(* ::Package:: *)

(* AnalyzeSectorsParallel.m
   ------------------------
   Drop-in replacement for LiteRed's AnalyzeSectors that batches the per-sector
   zero-test (`chzf`) across N Mathematica subkernels.

   Usage:
       Get["/path/to/LiteRed2025.m"];      (* load LiteRed normally *)
       Get["/path/to/AnalyzeSectorsParallel.m"];   (* patch *)
       LaunchKernels[];                    (* or LaunchKernels[N] *)
       NewDsBasis[...]; (* normal workflow *)
       AnalyzeSectors[basis];              (* now batched-parallel *)

   Falls back to plain sequential behaviour when $KernelCount == 0 — safe to
   load unconditionally.

   Only the FeynParUF branch (default) is parallelised. The IBP-corner branch
   stays sequential because Solvej carries LiteRed-private state that isn't
   trivially shareable across kernels.

   Correctness: the algorithm is the same bisection-by-classification as
   LiteRed's original (LiteRed2025.m:3041). Per outer iteration we now pick
   K = $KernelCount evenly-spaced candidates from the live `sectors` list and
   compute their verdicts in parallel; verdicts are then *applied* sequentially
   so the zero-/nonzero-cascade semantics are preserved (any candidate already
   removed by an earlier sibling's verdict is skipped). The output ZeroSectors/
   NonZeroSectors/SimpleSectors/BasisSectors are bit-identical to the serial
   version (verified on small topologies).

   Tunable: set $ASParBatchSize to override the default of $KernelCount.
*)

If[$VersionNumber < 10.0,
  Print["AnalyzeSectorsParallel.m: needs Mathematica >= 10.0"];
  Abort[]
];

(* Default batch size = 16 * $KernelCount: empirically the sweet spot on the
   gravity3l (15D) benchmark — large enough that the parallel chzf cost amortises
   the per-batch cascade work, small enough that few sibling candidates are
   wasted by absorption. Override with `$ASParBatchSize = N;` for very large
   topologies (22D+ may want N=256-1024). *)
$ASParBatchSize := If[$KernelCount > 0, 16 * $KernelCount, 1];

If[!ValueQ[$ASOriginalSaved],
  $ASOriginalDownValues = DownValues[AnalyzeSectors];
  $ASOriginalSaved = True;
];

(* Enter LiteRed`Private` so that:
   - jsectge / jsectle (defined in LiteRed`Private`) resolve normally
   - Module-local symbol names match those produced by the serial version
     (which is itself defined inside LiteRed`Private`) — important for the
     ZerojRule serialized form. *)
Begin["LiteRed`Private`"];

ClearAll[AnalyzeSectors];
Options[AnalyzeSectors] = {CutDs -> Automatic, FeynParUF -> True};

AnalyzeSectors[nm_, opts:OptionsPattern[]] :=
  AnalyzeSectors[nm, SectorsPattern[nm], opts];

AnalyzeSectors[nm_, patt_, OptionsPattern[]] := Module[
  {nds = Length@Ds@nm,
   nloops = Length@LMs@nm,
   nsects, nsects1,
   sectors, zsectors, nzsectors = {}, ssectors = {}, bsectors = {},
   dbase, s1, st, cds, ps, str, u, g, xs, x, x2,
   chzfPar, useFP, K, candidates, verdicts,
   t0, ti, n0, i, v, s1k,
   bvToInt, intToBv, sectorsInt, zsectorsInt, nzsectorsInt,
   candidatesInt, s1kInt},

  CurrentState[nm, AnalyzeSectors] = False;

  cds = OptionValue[CutDs] /. {None -> ConstantArray[0, nds], Automatic :> CutDs[nm]};
  ps  = Replace[PowerShifts[nm], {Except[0] -> 1}, {1}];
  useFP = TrueQ[OptionValue[FeynParUF]];

  If[useFP,
    {u, g, xs} = FeynParUF[js[nm, ##]&@@ConstantArray[1, nds],
                            NamingFunction->(Table[Unique[], {#}]&),
                            Function->False];
    g = Function[t, Append[# D[t,#]&/@xs, t]]/@MonomialList[u+g, xs];

    (* Hoist polynomial + options to globals and distribute to kernels once. *)
    $ASPar$g   = g;
    $ASPar$xs  = xs;
    $ASPar$cds = cds;
    $ASPar$ps  = ps;
    If[$KernelCount > 0,
      DistributeDefinitions[$ASPar$g, $ASPar$xs, $ASPar$cds, $ASPar$ps]
    ];

    chzfPar = Function[s,
      With[{m = BitOr[$ASPar$ps, s]},
        ($ASPar$cds =!= ($ASPar$cds * s)) ||
        MatrixRank[$ASPar$g /. Thread[$ASPar$xs -> m]] <= Count[m, 1]
      ]
    ];
    str = "FeynParUF (parallel batched)";
    ,
    (* IBP-corner branch — left sequential. *)
    If[!ValueQ[IBP@nm], Message[AnalyzeSectors::noibp, nm]; Return[$Failed]];
    chzfPar = Function[s,
      (cds =!= (cds * s)) ||
      ((dbase = {};
        Solvej[#, dbase, CheckZeroFunction->Factor1,
               SimplifyFunction->Factor1, SubstituteAlways->False]&/@IBP[nm]@@BitOr[ps, s];
        Collectj[j[nm, ##]&@@BitOr[ps, s] //. dbase, Factor1]) === 0)
    ];
    str = "IBP-corner (serial)";
  ];

  sectors = Cases[(IntegerDigits[#1, 2, nds]&)/@Range[0, 2^nds-1], patt];
  sectors = Developer`ToPackedArray[sectors];
  nsects  = Length@sectors;

  (* Encode each {0,1}-vector sector as a single integer (high bit = first slot).
     Cascade now uses BitOr-based subset tests on a packed integer vector — no
     per-candidate matrix allocation, no per-sector function call.

       jsectge[s1k - sec] (≡ sec ⊆ s1k bit-wise) ≡ BitOr[sec, s1k] == s1k
       jsectle[s1k - sec] (≡ s1k ⊆ sec bit-wise) ≡ BitOr[sec, s1k] == sec
  *)
  bvToInt[bv_]  := FromDigits[bv, 2];
  intToBv[i_]   := IntegerDigits[i, 2, nds];
  sectorsInt    = Developer`ToPackedArray[bvToInt /@ sectors];
  zsectorsInt   = {};
  nzsectorsInt  = {};

  Print["[AS-PAR] phase 1 start: ", nsects, " sectors; method=", str,
        ", kernels=", $KernelCount, ", batch=", $ASParBatchSize, ", t=", DateString[]];
  t0 = AbsoluteTime[];

  K = $ASParBatchSize;
  While[sectorsInt =!= {},
    Module[{n = Length@sectorsInt, idx},
      idx = If[K >= n,
        Range[n],
        DeleteDuplicates@Round@Rest@Subdivide[0, n, K+1]
      ];
      idx = Select[idx, 1 <= # <= n &];
      If[idx === {}, idx = {Ceiling[n/2]}];
      candidatesInt = sectorsInt[[idx]];
      candidates    = intToBv /@ candidatesInt;  (* chzfPar wants bit-vectors *)
    ];

    n0 = Length@sectorsInt;
    ti = AbsoluteTime[];

    verdicts = If[useFP && $KernelCount > 0 && Length@candidates > 1,
      ParallelMap[chzfPar, candidates],
      Map[chzfPar, candidates]
    ];

    Do[
      s1kInt = candidatesInt[[i]];
      If[!MemberQ[sectorsInt, s1kInt], Continue[]];  (* absorbed by a prior sibling *)
      v = verdicts[[i]];
      If[v,
        (* jsectge: keep sectors where BitOr[sec, s1k] == s1k, i.e. diff == 0. *)
        st = Unitize[BitOr[sectorsInt, s1kInt] - s1kInt];     (* 0 = subset *)
        zsectorsInt  = Join[zsectorsInt,  Pick[sectorsInt, st, 0]];
        sectorsInt   = Pick[sectorsInt, st, 1],
        (* jsectle: keep sectors where BitOr[sec, s1k] == sec, i.e. diff == 0. *)
        st = Unitize[BitOr[sectorsInt, s1kInt] - sectorsInt];  (* 0 = superset *)
        nzsectorsInt = Join[nzsectorsInt, Pick[sectorsInt, st, 0]];
        sectorsInt   = Pick[sectorsInt, st, 1]]
    , {i, Length@candidatesInt}];

    Print["[AS-PAR]   batch=", Length@candidates,
          " removed=", n0 - Length@sectorsInt,
          " chzf+classify=", Round[AbsoluteTime[] - ti, 0.01], "s",
          " remaining=", Length@sectorsInt,
          " zs=", Length@zsectorsInt, " nzs=", Length@nzsectorsInt];
  ];

  (* Decode back to bit-vector form for the rest of the algorithm. *)
  zsectors  = intToBv /@ zsectorsInt;
  nzsectors = intToBv /@ nzsectorsInt;

  Print["[AS-PAR] phase 1 done in ", Round[AbsoluteTime[] - t0, 0.01], "s; ",
        "zs=", Length@zsectors, " nzs=", Length@nzsectors];

  (* Phase 2: BasisSectors/SimpleSectors (sequential — usually << phase 1). *)
  sectors = Sort[nzsectors,
    Which[Plus@@#1 > Plus@@#2, False,
          Plus@@#1 < Plus@@#2, True,
          True, OrderedQ[{#1, #2}]]&];
  nsects1 = Length@sectors;
  Print["[AS-PAR] phase 2 start: ", nsects1, " nz sectors"];
  $LR$BT0 = AbsoluteTime[];
  $LR$BIter = 0;
  While[sectors =!= {},
    $LR$BIter++;
    s1 = First@sectors;
    sectors = Rest@sectors;
    (* Vectorised jsectge[s1 - #] over packed bsectors:
         Pick rows whose min(bsec - s1) is non-positive. Equivalent: jsectge[s1 - bsec]
         = Not[Or@@Negative[s1 - bsec]] = Min[s1 - bsec] >= 0 = Max[bsec - s1] <= 0.
         Using NonPositive on row-max keeps the work in C on the packed array.       *)
    st = If[bsectors === {}, {},
            Pick[bsectors, NonPositive[Max /@ (bsectors - ConstantArray[s1, Length[bsectors]])]]];
    If[st =!= {},
      If[s1 =!= BitOr@@st, AppendTo[bsectors, s1]],
      AppendTo[bsectors, s1]; AppendTo[ssectors, s1]]
  ];
  Print["[AS-PAR] phase 2 done in ", Round[AbsoluteTime[] - $LR$BT0, 0.01],
        "s; bsectors=", Length@bsectors, " ssectors=", Length@ssectors];

  (* Wrap into js[nm, ...] form and store. *)
  zsectors  = js[nm, ##]&@@@zsectors;
  nzsectors = js[nm, ##]&@@@nzsectors;
  ssectors  = js[nm, ##]&@@@ssectors;
  bsectors  = js[nm, ##]&@@@bsectors;

  If[SectorsPattern[nm] =!= patt, SectorsPattern[nm] ^= patt];
  ZeroSectors[nm]    ^= SortBy[zsectors,  {Count[#, 1], #}&];
  NonZeroSectors[nm] ^= SortBy[nzsectors, {Count[#, 1], #}&];

  sectors = {};
  While[zsectors =!= {},
    (AppendTo[sectors, #];
     zsectors = DeleteCases[zsectors, x2_/;x2 <= #])&[Last[zsectors]]];

  SimpleSectors[nm] ^= SortBy[ssectors, {Count[#, 1], #}&];
  BasisSectors[nm]  ^= SortBy[bsectors, {Count[#, 1], #}&];

  If[CutDs[nm] =!= cds, CutDs[nm] ^= cds];

  ZerojRule[nm] ^= jjj:j[#, __]:>0/;Module[{jjs = Rest[List@@jSector@jjj]},
                    (Or@@(BitOr[jjs, #]===#&/@#2))]&[nm, Rest/@List@@@sectors];

  CurrentState[nm, AnalyzeSectors] = True;
  LiteRedPrint["[parallel] " <> ToString@Length@ZeroSectors[nm] <>
               "/" <> ToString@Length@NonZeroSectors[nm] <>
               " zero/nz of " <> ToString@nsects <> " sectors."];

  If[Not@TrueQ@Not@BasisDirectory[nm], Quiet[DiskSave[nm, Save->"Basis"]]];
  Length@NonZeroSectors[nm]
];

End[];  (* LiteRed`Private` *)

Print["[AS-PAR] patch loaded. $KernelCount=", $KernelCount,
      "  $ASParBatchSize=", $ASParBatchSize,
      "  Call LaunchKernels[N] to enable N-fold batching."];

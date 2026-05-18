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

$ASParBatchSize := If[$KernelCount > 0, $KernelCount, 1];

If[!ValueQ[$ASOriginalSaved],
  $ASOriginalDownValues = DownValues[AnalyzeSectors];
  $ASOriginalSaved = True;
];

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
   t0, ti, n0, i, v, s1k},

  CurrentState[nm, AnalyzeSectors] = False;

  cds = OptionValue[CutDs] /. {None -> ConstantArray[0, nds], Automatic :> CutDs[nm]};
  ps  = Replace[PowerShifts[nm], {Except[0] -> 1}, {1}];
  useFP = TrueQ[OptionValue[FeynParUF]];

  If[useFP,
    {u, g, xs} = FeynParUF[j[nm, ##]&@@ConstantArray[1, nds],
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
  nsects  = Length@sectors;
  zsectors = {};

  Print["[AS-PAR] phase 1 start: ", nsects, " sectors; method=", str,
        ", kernels=", $KernelCount, ", batch=", $ASParBatchSize, ", t=", DateString[]];
  t0 = AbsoluteTime[];

  K = $ASParBatchSize;
  While[sectors =!= {},
    Module[{n = Length@sectors, idx},
      (* Pick K evenly spaced indices: the original code's "middle" + spread. *)
      idx = If[K >= n,
        Range[n],
        DeleteDuplicates@Round@Rest@Subdivide[0, n, K+1]
      ];
      idx = Select[idx, 1 <= # <= n &];
      If[idx === {}, idx = {Ceiling[n/2]}];
      candidates = sectors[[idx]];
    ];

    n0 = Length@sectors;
    ti = AbsoluteTime[];

    verdicts = If[useFP && $KernelCount > 0 && Length@candidates > 1,
      ParallelMap[chzfPar, candidates],
      Map[chzfPar, candidates]
    ];

    Do[
      s1k = candidates[[i]];
      If[!MemberQ[sectors, s1k], Continue[]];  (* absorbed by a prior sibling *)
      v = verdicts[[i]];
      If[v,
        st = jsectge[s1k - #]&/@sectors;
        zsectors  = Join[zsectors,  Pick[sectors, st]];
        sectors   = Pick[sectors, st, False],
        st = jsectle[s1k - #]&/@sectors;
        nzsectors = Join[nzsectors, Pick[sectors, st]];
        sectors   = Pick[sectors, st, False]]
    , {i, Length@candidates}];

    Print["[AS-PAR]   batch=", Length@candidates,
          " removed=", n0 - Length@sectors,
          " chzf+classify=", Round[AbsoluteTime[] - ti, 0.01], "s",
          " remaining=", Length@sectors,
          " zs=", Length@zsectors, " nzs=", Length@nzsectors];
  ];

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
    st = Select[bsectors, jsectge[s1 - #]&];
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

Print["[AS-PAR] patch loaded. $KernelCount=", $KernelCount,
      "  $ASParBatchSize=", $ASParBatchSize,
      "  Call LaunchKernels[N] to enable N-fold batching."];

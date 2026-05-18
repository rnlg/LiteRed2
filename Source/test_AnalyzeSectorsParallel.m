(* ::Package:: *)

(* test_AnalyzeSectorsParallel.m
   Quick correctness + speedup harness.

   Usage:
     wolframscript -file test_AnalyzeSectorsParallel.m <topology_dir> [kernels]

   <topology_dir> is a topology directory containing litered/generate_symmetries.m
   (the dataset convention). The script:
     1. Loads LiteRed.
     2. Runs AnalyzeSectors with the *serial* original; records ZeroSectors.
     3. Loads AnalyzeSectorsParallel.m, launches kernels, re-runs.
     4. Prints serial vs parallel timing and whether sector sets agree.
*)

If[Length[$ScriptCommandLine] < 2,
  Print["usage: wolframscript -file test_AnalyzeSectorsParallel.m <topology_dir> [kernels]"];
  Quit[1]];

topoDir = $ScriptCommandLine[[2]];
kernels = If[Length[$ScriptCommandLine] >= 3,
  ToExpression@$ScriptCommandLine[[3]],
  16];

literedDir = "/mnt/nvme1/marina/LiteRed2-logging/Source";
literedFiles = FileNames["LiteRed2*.m", literedDir];
If[literedFiles === {}, Print["No LiteRed found in ", literedDir]; Quit[1]];
literedMain = Last[literedFiles];

Print["Topology dir : ", topoDir];
Print["LiteRed      : ", literedMain];
Print["Kernels      : ", kernels];
Print["==============================================="];

(* --- Load LiteRed --- *)
Get[literedMain];
Quiet[Unprotect[$Notebooks, $FrontEnd]];
$Notebooks = False; $FrontEnd = Null;

(* --- Parse the topology's generate_symmetries.m to extract NewDsBasis call ---
   We reuse the basis setup verbatim by Get-ing the file, but stop before
   AnalyzeSectors. Trick: rebind AnalyzeSectors+FindSymmetries+GenerateIBP to
   no-ops, run the file, restore.
*)
genFile = FileNameJoin[{topoDir, "litered", "generate_symmetries.m"}];
If[!FileExistsQ[genFile], Print["No ", genFile]; Quit[1]];

(* Stub out the heavy calls so Get[genFile] just registers the basis. *)
$ORIG$AS = DownValues[AnalyzeSectors];
$ORIG$FS = DownValues[FindSymmetries];
$ORIG$GI = DownValues[GenerateIBP];
$ORIG$DS = DownValues[DiskSave];
ClearAll[AnalyzeSectors, FindSymmetries, GenerateIBP];
AnalyzeSectors[___] := Null;
FindSymmetries[___] := Null;
GenerateIBP[___]    := Null;
DiskSave[___]       := Null;

(* Run the topology's setup script — defines the basis. *)
SetDirectory[FileNameJoin[{topoDir, "litered"}]];
If[!DirectoryQ["symmetries_only"], CreateDirectory["symmetries_only"]];
SetDirectory["symmetries_only"];
(* Provide a fake $ScriptCommandLine to satisfy the file. *)
Block[{$ScriptCommandLine = {"stub", literedDir}, $InputFileName = genFile},
  Get[genFile]
];

(* Restore originals. *)
DownValues[AnalyzeSectors] = $ORIG$AS;
DownValues[FindSymmetries] = $ORIG$FS;
DownValues[GenerateIBP]    = $ORIG$GI;
DownValues[DiskSave]       = $ORIG$DS;

(* Identify the basis symbol the file registered. *)
basisCandidates = Cases[Names["Global`*"],
  s_String /; ValueQ@Symbol[s] && Head@Symbol[s] === Symbol &&
              !MemberQ[{"AnalyzeSectors","FindSymmetries","GenerateIBP"}, s] :>
              Symbol[s]];
basis = SelectFirst[Names["Global`*"], (ValueQ[Ds@Symbol[#]] && ValueQ[LMs@Symbol[#]])&, None];
If[basis === None, Print["Could not identify basis symbol"]; Quit[1]];
basisSym = Symbol[basis];
Print["Basis        : ", basisSym, "  (", Length@Ds@basisSym, " denominators, ",
      Length@LMs@basisSym, " loops)"];

(* --- Serial run --- *)
Print["\n--- SERIAL run ---"];
t1 = AbsoluteTime[];
AnalyzeSectors[basisSym];
tSerial = AbsoluteTime[] - t1;
serialZ = Sort[ZeroSectors[basisSym]];
serialNZ = Sort[NonZeroSectors[basisSym]];
Print["serial time  : ", Round[tSerial, 0.01], " s    ",
      "zs=", Length@serialZ, "  nz=", Length@serialNZ];

(* --- Reset state so AnalyzeSectors must do the work again --- *)
CurrentState[basisSym, AnalyzeSectors] = False;
ZeroSectors[basisSym]    =.;
NonZeroSectors[basisSym] =.;
SimpleSectors[basisSym]  =.;
BasisSectors[basisSym]   =.;

(* --- Load parallel patch and launch kernels --- *)
Get[FileNameJoin[{literedDir, "AnalyzeSectorsParallel.m"}]];
LaunchKernels[kernels];
Print["\n--- PARALLEL run ($KernelCount=", $KernelCount, ") ---"];
t2 = AbsoluteTime[];
AnalyzeSectors[basisSym];
tParallel = AbsoluteTime[] - t2;
parZ = Sort[ZeroSectors[basisSym]];
parNZ = Sort[NonZeroSectors[basisSym]];
Print["parallel time: ", Round[tParallel, 0.01], " s    ",
      "zs=", Length@parZ, "  nz=", Length@parNZ];

(* --- Comparison --- *)
Print["\n--- COMPARISON ---"];
matchZ = (serialZ === parZ);
matchNZ = (serialNZ === parNZ);
Print["ZeroSectors match    : ", matchZ];
Print["NonZeroSectors match : ", matchNZ];
Print["Speedup              : ", Round[tSerial / Max[tParallel, 0.0001], 0.01], "x"];

If[!matchZ || !matchNZ,
  Print["MISMATCH — investigation needed"];
  If[!matchZ,
    Print["  Z only in serial : ", Length@Complement[serialZ, parZ], " sectors"];
    Print["  Z only in parallel: ", Length@Complement[parZ, serialZ], " sectors"]];
  If[!matchNZ,
    Print["  NZ only in serial : ", Length@Complement[serialNZ, parNZ], " sectors"];
    Print["  NZ only in parallel: ", Length@Complement[parNZ, serialNZ], " sectors"]];
];

CloseKernels[];
Quit[]

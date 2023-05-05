(*
copyFile.m - written by Roman Lee.
This script assumes that the package is in a single file "PACKAGE*.m" 
(star stands for optional version number)
and copies this file into .Mathematica/Applications/PACKAGE.m

Modification of PACKAGE below should be sufficient when adjusting script to new package.
*)
PACKAGE="Mint";

SetDirectory[DirectoryName[$InputFileName]]
file = Join[FileNames[PACKAGE<>"*.wl"],FileNames[PACKAGE<>"*.m"]];
If[file==={},Print["No "<>PACKAGE<>"*.m or "<>PACKAGE<>"*.wl in "<>Directory[]<>". Changed nothing, quitting..."];Quit[]];
file=Last[file];
dest=$UserBaseDirectory<>"/Applications/"<>PACKAGE<>"."<>FileExtension[file];
Check[CopyFile[file,dest,OverwriteTarget->True],Print["Something went wrong when copying "<>file<>" into "<>dest<>". Quitting..."];Quit[]];
Print["Copied "<>file<>" into \033[1;34m"<>dest<>"\033[0m.\nTo load the package, use\n  \033[1;32m<<"<>PACKAGE<>"`\033[0m"]
Quit[]

(*
makeShortcut.m - written by Roman Lee.
This script assumes that the package is loaded from the
file "PACKAGE*.m" (star stands for optional version number).
and creates shortcut in .mathematica/Applications/PACKAGE/init.m

Modification of PACKAGE below should be sufficient when adjusting script to new package.
*)
PACKAGE="LiteRed2";

SetDirectory[DirectoryName[$InputFileName]]
appdir=$UserBaseDirectory <> "/Applications/"<>PACKAGE<>"/";
file = Join[FileNames[PACKAGE<>"*.wl"],FileNames[PACKAGE<>"*.m"]];
If[file==={},Print["No "<>PACKAGE<>"*.m or "<>PACKAGE<>"*.wl in "<>Directory[]<>". Changed nothing, quitting..."];Quit[]];
file=Last[file];
Quiet[CreateDirectory[appdir]];
init=OpenWrite[appdir<>"init.m"];
WriteString[init,"Begin[\""<>PACKAGE<>"`Private`\"]\n\
  Module[{path=\""<>Directory[]<>"/\",file=\""<>file<>"\"},\n\
    If[FileExistsQ[path<>file],\n\
      Get[path<>file],\n\
      Print[\"Error: can not find \"<>file<>\"\\nCheck if this file exists in \"<>path]\n\
    ]\n\
  ]"<>"\n\
End[];"
]
Close[init]
Print["Installed shortcut for "<>file<>".\nTo load the package, use\n  \033[1;31m<<"<>PACKAGE<>"`\033[1;0m"]
Quit[]

SetDirectory[DirectoryName[$InputFileName]]
appdir=$UserBaseDirectory <> "/Applications/LiteRed2/";
file = Last[FileNames["LiteRed*.m"]];
Quiet[CreateDirectory[appdir]];
init=OpenWrite[appdir<>"init.m"];
WriteString[init,"Begin[\"LiteRed2`Private`\"]\n\
  Module[{path=\""<>Directory[]<>"/\",file=\""<>file<>"\"},\n\
    If[FileExistsQ[path<>file],\n\
      Get[path<>file],\n\
      Print[\"Error: can not find \"<>file<>\"\\nCheck if this file exists in \"<>path]\n\
    ]\n\
  ]"<>"\n\
End[]"
]
Close[init]
Print["Installed shortcut for "<>file<>".\nTo load the package, use\n  <<LiteRed2`"]
Quit[]

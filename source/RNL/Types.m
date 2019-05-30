(* ::Package:: *)

(* ::Title:: *)
(*Types*)


(* ::Section:: *)
(*Notes*)


(* ::Text:: *)
(**)


(* ::Section:: *)
(*Preamble*)


$PrePrint=($PrePrint/.{HoldPattern[Types`Private`TypesPostFunction[x_]]->x});
(*Sacrifice possibility to reload the package in favour of posssibility to first switch off log by setting Types`TypesLog=False 
(Unprotect[#];Remove[#])&/@((#<>"*")&/@Contexts["Types`*"]);*)


BeginPackage["Types`"]
Off[General::"spell1"];


(* ::Section:: *)
(*Open all public names*)


(* ::Text:: *)
(*The expression has type Untyped if it is an undeclared variable or the function of all arguments of type  Untyped.*)
(*The expression has type Badformed if it the function of typed arguments, but there is no rule *)


Untyped::usage="Untyped is the type of all untyped expressions.";
Badformed::usage="All badly formed expressions have type Badformed";


Declare::usage="Declare[{\[ScriptV]\[ScriptA]\[ScriptR]11,\[ScriptV]\[ScriptA]\[ScriptR]12,\[Ellipsis]},\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]1,{\[ScriptV]\[ScriptA]\[ScriptR]21,\[ScriptV]\[ScriptA]\[ScriptR]22,\[Ellipsis]},\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]2] declares variables \[ScriptV]\[ScriptA]\[ScriptR]11,\[ScriptV]\[ScriptA]\[ScriptR]12,\[Ellipsis] as variables of type \[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]1, variables \[ScriptV]\[ScriptA]\[ScriptR]21,\[ScriptV]\[ScriptA]\[ScriptR]22,\[Ellipsis] 
as variables of type \[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]2 etc. and calls InitFunction[\[ScriptV]\[ScriptA]\[ScriptR]11,\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]1], InitFunction[\[ScriptV]\[ScriptA]\[ScriptR]12,\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]1],\[Ellipsis],InitFunction[\[ScriptV]\[ScriptA]\[ScriptR]21,\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]3]";
InitFunction::usage="InitFunction[\[ScriptV]\[ScriptA]\[ScriptR],\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]] is called by Declare[\[ScriptV]\[ScriptA]\[ScriptR],\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]]. Redefine it if you want to perform some operations when the variable is declared.";


TypeOf::usage="TypeOf[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives the type of \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR], \n 
TypeOf[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE]] gives True if TypeOf[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] is inside \[ScriptT]\[ScriptY]\[ScriptP]\[ScriptE].";
TypeX::usage="TypeX[<expr>] gives the pair {<type>,<expr>}";


VarQ::usage="VarQ[\[ScriptX]] gives True if \[ScriptX] is the variable, i.e. \[ScriptX] is a nonnumeric Symbol. Obsolete";
TypesCache::usage="TypesCache[0] switches off the cash of the function TypeX,\n
TypesCache[1] and TypesCache[2] switch on the cash of the function TypeX, the former activates clearing of the cash automatically.";
TypesClearCache::usage="TypesClearCache[] clears cash of TypeX."


TypeTable::usage="TypeTable[<op>] gives the lists of in- and out- types of the operation op[...]";
AddTypeRule::usage="AddTypeRule[<op>,<inpattern1>:><outtype1>,<inpattern2>:><outtype2>,...] adds entries into theTypeTable[op]";


TypesAbove::usage="TypesAbove[<type>] gives the list of all types, that are above <type>";
TypesBelow::usage="TypesBelow[<type>] gives the list of all types, that are below <type>";
TypeHierarchy::usage="TypeHierarchy[<t1>,<t2>,<t3>,...] defines the hierarchy of types <t1> below <t2> below <t3> etc";
ClearTypeHierarchy::usage="ClearTypeHierarchy[<t1>,<t2>,<t3>,...] removes  <t1>, <t2>, <t3> etc from hierarchy";
TypeLessQ::usage="TypeLessQ[<t1>,<t2>, ...] gives True if <t1> is below <t2> below <t3> etc";
TypeIntersect::usage="TypeIntersect[<t1>,<t2>, ...] gives the list of types that are below <t1>,<t2> etc";
TypesLog::usage="TypesLog=True turns on some log information of the package.";
If[!ValueQ[TypesLog],TypesLog=True];
Types::usage="Types package is written by Roman Lee. It  contains  tools for variable types definition, written \n"<>ToString[TableForm[Partition[Join[Names["Types`*"],{"","","","","",""}],5],
    TableSpacing->{0,3}]]


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


TypesPrint[x__]:=If[TypesLog,Print[x]];(*TypesPrint is equivalent to Print if TypesLog=True*)


TypesPrint[$Input]


TypesPrint["**************** \!\(\*StyleBox[\"Types 0.7\",FontWeight->\"Bold\"]\) ********************
Author: Roman N. Lee, Budker Institute of Nuclear Physics, Novosibirsk.
\!\(\*StyleBox[\"Types\",FontWeight->\"Bold\"]\) package contains tools for the variable types definition.
See ?Types`* for a list of functions. 
"]



(* ::Section:: *)
(*Procedures Definition*)


(* ::Subsection:: *)
(*VarQ*)


VarQ[x_Symbol]:=Not[NumericQ[x]];
VarQ[_]:=False;


(* ::Subsection:: *)
(*TypeTable*)


TypeTable[_]={{{Untyped,_}...}->Untyped,_List->Badformed};


(* ::Subsection::Closed:: *)
(*TypesClearList*)


(* ::Text:: *)
(*TypesClearList \:0438\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:0435\:0442\:0441\:044f \:0434\:043b\:044f \:0445\:0440\:0430\:043d\:0435\:043d\:0438\:044f \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0439, \:0442\:0438\:043f\:044b \:043a\:043e\:0442\:043e\:0440\:044b\:0445 \:0431\:044b\:043b\:0438 \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:044b \:0432 \:043f\:0440\:043e\:0446\:0435\:0441\:0441\:0435 \:0434\:0430\:043d\:043d\:043e\:0433\:043e \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:044f. TypesPostFunction "\:0437\:0430\:0431\:044b\:0432\:0430\:0435\:0442" \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:043d\:044b\:0435 \:0442\:0438\:043f\:044b.*)


TypesClearList={};TypesPostFunction=.;
Which[!ValueQ[$PrePrint],$PrePrint=TypesPostFunction[#]&,
Head[$PrePrint]===Function,$PrePrint=MapAt[Unevaluated[TypesPostFunction],$PrePrint,-1],
True,$PrePrint=TypesPostFunction[$PrePrint[#]]&/.HoldPattern[$PrePrint]->$PrePrint
];
TypesPostFunction=(Unset/@TypesClearList;TypesClearList={};#)&;
TypesCacheFunction=(AppendTo[TypesClearList,#1];#1=#2)&


(* ::Subsection:: *)
(*TypesCache, TypesClearCache*)


TypesCache[x:0|1|2]:=Switch[x,
0,TypesPostFunction=#&;TypesCacheFunction=#2&;,
1,TypesPostFunction=(Unset/@TypesClearList;TypesClearList={};#)&;TypesCacheFunction=(AppendTo[TypesClearList,#1];#1=#2)&;,
2,TypesPostFunction=#&;TypesCacheFunction=(AppendTo[TypesClearList,#1];#1=#2)&;];


TypesClearCache[]:=(Unset/@TypesClearList;TypesClearList={});


(* ::Subsection::Closed:: *)
(*TypeOf*)


TypeOf[expr_]:=First[TypeX[Unevaluated[expr]]];
TypeOf[a_,t_]:=MatchQ[TypeOf[Unevaluated[a]],TypesBelow[t]];


(* ::Subsection:: *)
(*TypeX*)


TypeX[{}]={{},{}};
TypeX[l_List]:=Transpose[TypeX/@ReleaseHold[Map[Unevaluated,Hold[l],{2}]]];
TypeX[nm_]/;!FreeQ[Unevaluated[nm],(_Pattern|_Blank|_BlankSequence|_BlankNullSequence|_Alternatives|_Optional|_PatternTest|_HoldPattern)]:={Untyped,nm};
TypeX[h_[args___]]:=TypesCacheFunction[HoldPattern[TypeX[h[args]]],
{Replace[List@@(TypeX/@Unevaluated/@Hold[args]),TypeTable[Unevaluated[h]]],h[args]}]
TypeX[nm_]:={Untyped,nm};


(* ::Subsection:: *)
(*AddTypeRule*)


ATR[h_Symbol,(in_):>(out_),n_]:=Module[{th=TypeTable[h],p},
If[(p=Position[th,a_/;(a[[1]]===in),{1},Heads->False])==={},
TypeTable[h]^=Insert[th,in:>out,{n}],
TypeTable[h]^=ReplacePart[th,in:>out,p]
]
];
ATR[h_,(in_):>(out_),n_]:=Module[{th=TypeTable[h],p},
If[(p=Position[th,a_/;(a[[1]]===in),{1},Heads->False])==={},
TypeTable[h]=Insert[th,in:>out,{n}],
TypeTable[h]=ReplacePart[th,in:>out,p]]
];
AddTypeRule[h_,io:((_:>_)..),Position->n_Integer]:=Module[{offset=2+Length[TypeTable[h]]},
Scan[ATR[h,#,Mod[n,offset]-offset]&,{io}]];
AddTypeRule[h_,io:((_:>_)..)]:=Scan[ATR[h,#,-3]&,{io}];


(* ::Subsection:: *)
(*InitFunction*)


SetAttributes[InitFunction,HoldFirst];InitFunction[h_,f_Function]:=ATR[h,f[-1],-3];


(* ::Subsection:: *)
(*Declare*)


SetAttributes[Declare,HoldAll];
Declare[x_List,t_]:=Scan[Declare[#,t]&,Map[Unevaluated,Hold[x],{2}],{2}];
Declare::info="The variable `1` is declared as `2`";
Off[Declare::info];
Declare[x_,t_]:=(TypeX[Unevaluated[a:x]]^={t,a};InitFunction[x,t];(Unset/@#;TypesClearList=Complement[TypesClearList,#])&[Select[TypesClearList,MemberQ[#,Unevaluated[x],Infinity]&]];Message[Declare::info,HoldForm[x],t])
Declare[x_,t_,r__]:=(Declare[x,t];Declare[r]);


(* ::Subsection:: *)
(*TypesAbove, TypesBelow,TypeIntersect,TypeHierarchy,TypeLessQ*)


TypesAbove[x_]:=Alternatives@@TA[x];
TypesBelow[x_]:=Alternatives@@TB[x];
TypeHierarchy=TH;
ClearTypeHierarchy=CTH;
TypeLessQ=TLQ;
TypeIntersect[x_]:=Alternatives@@TI[x];


TA[a_]={a};TB[a_]={a};


TH[t1_,t2_]:=(((TB[#]=Union[TB[#],TB[t1]])&/@TA[t2]);((TA[#]=Union[TA[t2],TA[#]])&/@TB[t1]);)
TH[t1_,t2_,t3__]:=(TH[t1,t2];TH[t2,t3])


CTH[t1_]:=(((TB[#]=DeleteCases[TB[#],t1])&/@TA[t1]);((TA[#]=DeleteCases[TA[#],t1])&/@TB[t1]);TA[t1]=.;TB[t1]=.;)
CTH[t1_,t2__]:=(CTH[t1];CTH[t2];)


TLQ[t1_,t2_]:=MemberQ[TB[t2],t1];
TLQ[t1_,t2_,t3__]:=TLQ[t1,t2]&&TLQ[t2,t3];
TI[t1__]:=Intersection@@(TB/@{t1});


(* ::Section:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*Protect public names*)


Protect[Untyped,Badformed,AddTypeRule,Declare, TypesBelow,TypesAbove,TypeLessQ,TypeIntersect,TypeHierarchy,ClearTypeHierarchy];



EndPackage[];


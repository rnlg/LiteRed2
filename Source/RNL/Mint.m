(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



Vectors`VectorsLog=False;


If[MemberQ[$ContextPath,"LiteRed`"],
Print[Style["Mint",{Bold,Green}],": ",Style["LiteRed",{Bold,Lighter@Red}]," is detected."];
Mint`Private`LiteRed=True
,
Mint`Private`LiteRed=False
];


Mint`$MintGroebner=GroebnerBasis;
Mint`$Singular=True;


Needs["Vectors`","RNL`Vectors`"];
Needs["LinearFunctions`","RNL`LinearFunctions`"];
Needs["Numbers`","RNL`Numbers`"];
Needs["Types`","RNL`Types`"];
If[Mint`Private`LiteRed,BeginPackage["Mint`",{"LiteRed`","Vectors`","LinearFunctions`","Numbers`","Types`"}],BeginPackage["Mint`",{"Vectors`","LinearFunctions`","Numbers`","Types`"}]];


$MintVersion="1.2";
$MintReleaseDate="20.02.2015";


Print["**************** ",Style["Mint v"<>ToString[$MintVersion],{Bold,Green}]," ********************\n\
Author: Roman N. Lee, Budker Institute of Nuclear Physics, Novosibirsk.\n\
Mint package is designed to determine the number of master integrals with given set of denominators.\n\
See ?Mint`* for a list of functions."];


LeadingMonomial;
PolyCoefficient;
ReducedGroebnerBasis;
ReducedPolyBasis;
LinearPolyBasis;
LinearDependentQ;
Kbase;
PMilnorNumber;
CriticalSet;
FindMIs;
CountMIs;


(*PolyNormalForm;*)
PermutationSymmetries;
PermutationSymmetrize;
(*SymmetricBasis;*)
Symmetric;
NamingFunction;
FeynParUF;


$MintGroebner


$MintMonomialOrder=Lexicographic;


Begin["`Private`"]


If[Permute[{2,3,1},{2,3,1}]==={3,1,2},
permute[x_,y_]:=Permute[x,PermutationPower[y,-1]],
permute[x_,y_]:=Permute[x,y]
]



LeadingMonomial::usage="LeadingMonomial[\!\(\*
StyleBox[\"poly\", \"TI\"]\),\!\(\*
StyleBox[\"{\", \"TI\"]\)\!\(\*
StyleBox[SubscriptBox[
StyleBox[\"x\", \"TI\"], \"1\"], \"TI\"]\)\!\(\*
StyleBox[\",\", \"TI\"]\)\!\(\*
StyleBox[SubscriptBox[
StyleBox[\"x\", \"TI\"], \"2\"], \"TI\"]\)\!\(\*
StyleBox[\",\", \"TI\"]\)\!\(\*
StyleBox[\"\[Ellipsis]\", \"TI\"]\)\!\(\*
StyleBox[\"}\", \"TI\"]\)] gives the list of the monomials of the polynomial. Threads over lists in the first argument.";


Options[LeadingMonomial]={MonomialOrder:>$MintMonomialOrder};


LeadingMonomial[p_List,vs_,opts:OptionsPattern[]]:=LeadingMonomial[#,vs,opts]&/@p;
LeadingMonomial[p_,vs_,opts:OptionsPattern[]]:=Replace[CoefficientRules[p,vs,OptionValue[MonomialOrder]],{{}->0,x_:>Times@@(vs^x[[1,1]])}];(*First@PadRight[MonomialList[p,vs,OptionValue[MonomialOrder]],1]*)


PolyCoefficient::usage="PolyCoefficient[\!\(\*
StyleBox[\"poly\", \"TI\"]\),{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives the coefficient of the polynomial in front of the monomial \!\(\*SubsuperscriptBox[
StyleBox[\"x\", \"TI\"], \(1\), SubscriptBox[
StyleBox[\"n\", \"TI\"], \"1\"]]\)\!\(\*SubsuperscriptBox[
StyleBox[\"x\", \"TI\"], \(2\), SubscriptBox[
StyleBox[\"n\", \"TI\"], \"2\"]]\)\[Ellipsis]. Threads over lists in the first argument.";
PolyCoefficient[p_List,xs_,e_List]:=PolyCoefficient[#,xs,e]&/@p
PolyCoefficient[p_,xs_,e_List]:=Replace[e,Append[CoefficientRules[p,xs],e->0]];


Options[ReducedPolyBasis]={MonomialOrder:>$MintMonomialOrder};


ReducedPolyBasis::usage="ReducedPolyBasis[{\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives a list of polynomials that form a reduced basis.";
ReducedPolyBasis[p_List,xs_,OptionsPattern[]]:=Module[{gb=p,t,k=0},
While[++k<= Length@gb,
t=Last@PolynomialReduce[gb[[k]],Delete[gb,k],xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[ReducedPolyBasis],Options[ReducedPolyBasis]]];
Which[t==0,gb=Delete[gb,k];k=0,
t=!=gb[[k]],gb[[k]]=t;k==0
]
];
gb
]


Options[LinearPolyBasis]={MonomialOrder:>$MintMonomialOrder};


LinearPolyBasis::usage="LinearPolyBasis[{\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives a list of polynomials that form a linear basis for the set of polynomials \!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], 
StyleBox[\"i\", \"TI\"]]\).";
LinearPolyBasis[{},xs_,OptionsPattern[]]:={};
LinearPolyBasis[p_List,xs_,OptionsPattern[]]:=Module[{es,ms},
es=First/@CoefficientRules[p.Table[Unique[],{Length@p}],xs,OptionValue[MonomialOrder]];(*(*Deleted 20.02.2016*)(*Reverse[Union@@(First/@CoefficientRules[#,xs,OptionValue[MonomialOrder]]&/@p)]*)(*/Deleted 20.02.2016*)*)
ms=Times@@(xs^#)&/@es;(*Monomials sorted from the most complicated*)
ms.#&/@DeleteCases[RowReduce[Outer[PolyCoefficient[#1,xs,#2]&,p,es,1]],{0...}]
]


LinearDependentQ::usage="LinearDependentQ[{\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives True if there is a linear relation among \!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], 
StyleBox[\"i\", \"TI\"]]\).";
LinearDependentQ[p_List,xs_]:=Module[{es=Reverse[Union@@(First/@CoefficientRules[#,xs]&/@p)]},
MemberQ[RowReduce[Outer[PolyCoefficient[#1,xs,#2]&,p,es,1]],{0...}]
]


Options[ReducedGroebnerBasis]={MonomialOrder:>$MintMonomialOrder};


ReducedGroebnerBasis::usage="ReducedGroebnerBasis[{\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives a list of polynomials that form a reduced Gr\[ODoubleDot]bner basis for the set of polynomials \!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], 
StyleBox[\"i\", \"TI\"]]\).";
ReducedGroebnerBasis[p_List,xs_,xe_List:{},opts:OptionsPattern[]]:=Module[{gb,t,k=0},
If[Mint`$Singular&&MemberQ[$ContextPath,"Singular`"],
PrintTemporary["Using Singular"];
If[xe==={},
gb=Check[Symbol["SingularStd"][p,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[ReducedGroebnerBasis],Options[Symbol["SingularSession"]]]],Abort[]],
gb=Check[Symbol["SingularEliminate"][p,xe,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[ReducedGroebnerBasis],Options[Symbol["SingularSession"]]]],Abort[]];
];
,
If[MatchQ[OptionValue[MonomialOrder],Lexicographic],
gb=p,
gb=GroebnerBasis[p,xs,xe]
];
gb=GroebnerBasis[gb,xs,xe,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[ReducedGroebnerBasis],Options[GroebnerBasis]]];
While[++k<= Length@gb,
t=Last@PolynomialReduce[gb[[k]],Delete[gb,k],xs];
Which[t==0,gb=Delete[gb,k];k=0,
t=!=gb[[k]],gb[[k]]=t;k==0
]
]
];
gb
]
(*/added 01.11.13*)


Options[Kbase]={MonomialOrder:>$MintMonomialOrder};


Kbase::usage="Kbase[{\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"x\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives a list of monomials which can not be reduced by {\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"poly\", \"TI\"], \(2\)]\),\[Ellipsis]}.";
Kbase[gb_,xs_List,OptionsPattern[]]:=Module[{gr,le,n=Length@xs,p,x,y},
If[Mint`$Singular&&MemberQ[$ContextPath,"Singular`"],
gr=Check[Reverse[Symbol["SingularKbase"][gb,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[Kbase],Options[Symbol["SingularSession"]]]]],Abort[]];
If[gr==={}&&\[Infinity]===Symbol["SingularVdim"][gb,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[Kbase],Options[Symbol["SingularSession"]]]],
gr=Indeterminate
]
,
le=CoefficientRules[#,xs,OptionValue[MonomialOrder]]&/@gb;
le=le[[All,1,1]];
If[MemberQ[le,{0..}],
gr={}
,
p=Plus@@@(Flatten@Cases[le,#,{1},1]&/@NestList[RotateRight,PadRight[{_},n],n-1]);
If[MemberQ[p,0],
gr=Indeterminate 
,
p=List@@@Flatten[Array[y@##&,p,0]];
Scan[(p=DeleteCases[p,x_/;And@@Thread[x>=#]])&,le];
gr=Times@@(xs^#)&/@p
];
]
];
gr
]


PermutationSymmetries::usage="PermutationSymmetries[\!\(\*
StyleBox[\"poly\", \"TI\"]\),\!\(\*
StyleBox[\"xs\", \"TI\"]\)] returns all permutations of \!\(\*
StyleBox[\"xs\", \"TI\"]\) which preserve the form of the polynomial.";
PermutationSymmetries[poly_,xs_]:=Module[{
b,
mg,
l(*active length*),
n=Length@xs,
cs,csn,xp,s,
f,
w,wt,
i
},
b=Max[Exponent[poly,xs]]+1;
f={Sort[First@#],Last@#}&;
mg=Map[Last@#*Times@@(xs^First@#)&,SplitBy[SortBy[CoefficientRules[poly,xs],f],f],{2}];(*monomial groups*)
cs={{{{},xs},mg}};(*candidates*)
(*now we add one variable*)
Do[w={};(*\:0432\:0435\:0441*)
csn={};
Scan[(Table[xp={Append[#[[1,1]],#[[1,2,i]]],Delete[#[[1,2]],i]};
s=SplitBy[Sort@#,First]&/@Map[{b^Range[0,l].Exponent[#,First[xp]],#}&,Last@#,{2}];
wt=Flatten[s[[All,All,All,1]],1];
Which[wt===w,AppendTo[csn,{xp,Flatten[s[[All,All,All,2]],1]}],
Order[w,wt]==1,csn={{xp,Flatten[s[[All,All,All,2]],1]}};w=wt
],{i,1,n-l}])&,cs];
cs=csn,
{l,0,n-1}];
cs=(First@#)&@@@cs/.Thread[xs->Range[n]];
permute[#,First@cs]&/@cs
]


PermutationSymmetrize::usage="PermutationSymmetrize[\!\(\*
StyleBox[\"expr\", \"TI\"]\),\!\(\*
StyleBox[\"xs\", \"TI\"]\),\!\(\*
StyleBox[\"perms\", \"TI\"]\)] symmetrizes \!\(\*
StyleBox[\"expr\", \"TI\"]\) with respect to all \!\(\*
StyleBox[\"perms\", \"TI\"]\) of \!\(\*
StyleBox[\"xs\", \"TI\"]\).";
PermutationSymmetrize[expr_,xs_,perm_]:=Module[{n},
If[ArrayDepth[perm]=!=2,Return[expr]];
xs=First@perm;
n=Length@perm;
Plus@@(expr/n/.Thread[xs->permute[xs,#]]&/@perm)
]


PMilnorNumber::ni="`1`: non-isolated critical points.";
PMilnorNumber::usage="PMilnorNumber[\!\(\*
StyleBox[\"poly\", \"TI\"]\),\!\(\*
StyleBox[\"xs\", \"TI\"]\)] returns Milnor number of \!\(\*
StyleBox[\"poly\", \"TI\"]\) up to symmetries and excluding those where \!\(\*
StyleBox[\"poly\", \"TI\"]\)=0.";
Options[PMilnorNumber]={Symmetric->True,PermutationSymmetries->Automatic,TransformationMatrix->None,MonomialOrder:>$MintMonomialOrder};
PMilnorNumber[poly_,xs_,OptionsPattern[]]:=Module[
{
tm=OptionValue[TransformationMatrix]/.None->False,
syms,
x0,
r,
g},
g=Expand[(D[poly,#])]&/@xs;
Monitor[
g=ReducedGroebnerBasis[Join[g,{x0*poly-1}],xs,{x0},Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[PMilnorNumber],Options[Kbase]]],
"Constructing Groebner basis..."];
r=Kbase[g,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[PMilnorNumber],Options[Kbase]]];
If[r===Indeterminate,Return[Indeterminate]];
If[OptionValue[Symmetric],
If[TrueQ[!tm],
syms=Thread[xs->Mint`Private`permute[xs,#]]&/@OptionValue[PermutationSymmetries];
If[syms===Automatic,
syms=Thread[xs->Mint`Private`permute[xs,#]]&/@PermutationSymmetries[poly,xs]]
,
syms=Thread[xs->Most[#].Append[xs,1]]&/@tm
];
(*tm=LinearPolyBasis[Last/@PolynomialReduce[Flatten[Collect[r-(r/.#),xs,Factor]&/@syms],g,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[PMilnorNumber],Options[PolynomialReduce]]],xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[PMilnorNumber],Options[LinearPolyBasis]]];
r=Complement[r,DeleteDuplicates[LeadingMonomial[#,xs,MonomialOrder\[Rule]OptionValue[MonomialOrder]]&/@tm]]*)
tm=LinearPolyBasis[Last/@PolynomialReduce[Flatten[Collect[r-(r/.#),xs,Factor]&/@syms],g,xs],xs];
r=Complement[r,DeleteDuplicates[LeadingMonomial[#,xs]&/@tm]]
];
Length@r
]


Options[CriticalSet]={MonomialOrder:>$MintMonomialOrder};


CriticalSet::usage="CriticalSet[\!\(\*
StyleBox[\"poly\", \"TI\"]\),\!\(\*
StyleBox[\"xs\", \"TI\"]\)] returns proper critical set of \!\(\*
StyleBox[\"poly\", \"TI\"]\).";
CriticalSet[poly_,xs_,opts:OptionsPattern[]]:=Module[
{
x0,
r,
g,sol},
g=Expand[(D[poly,#])]&/@xs;
Monitor[g=ReducedGroebnerBasis[Join[g,{x0*poly-1}],Join[xs,{x0}],{x0},Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[CriticalSet],Options[ReducedGroebnerBasis]]],"Constructing Groebner basis..."];Monitor[Solve[g==0,xs],"Solving system ..."]
]


If[!Mint`Private`LiteRed,
FeynParUF::usage="FeynParUF[{\!\(\*
StyleBox[\"dens\", \"TI\"]\)},{\!\(\*
StyleBox[\"lms\", \"TI\"]\)}] gives U and F polynomials, entering the Feynman parametrization of the integrals with denominators {\!\(\*
StyleBox[\"dens\", \"TI\"]\)} and loop momenta {\!\(\*
StyleBox[\"lms\", \"TI\"]\)}.";
Options[FeynParUF]={NamingFunction->(Array[ToExpression["x"<>ToString[#]]&,{#}]&)};
FeynParUF[ds_List,lms_List,OptionsPattern[]]:=Module[
{xs,den,t1,t2,dt2,a},
xs=OptionValue[NamingFunction][Length@ds,1&/@ds];
Declare[Evaluate@xs,Number];
den=Collect[LFDistribute[xs.ds,_sp],_sp];
t1=(D[den,#]/2/.{Derivative[0,1][sp]:>(#1&),Derivative[1,0][sp]:>(#2&)})&/@lms;
t2=(D[t1,#])&/@lms;
t1=t1/.Thread[lms->0];
dt2=Factor[Det[t2]];
If[dt2==0,Return[{0,0,xs}]];
t2=Together[(dt2*den/.Thread[lms->0])-LFDistribute@Inner[sp,t1,Together[dt2*Inverse[t2]].t1,Plus]];
{dt2,t2,xs}
]
]


FindMIs::usage="FindMIs[{\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives a list of the master integrals with denominators \!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(2\)]\),\[Ellipsis] and loop momenta \!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(2\)]\),\[Ellipsis].\n\
Each entry in the list has the form {\!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(2\)]\),\[Ellipsis]}, where \!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"n\", \"TI\"], \(2\)]\),\[Ellipsis] are the powers of the denominators.";
Options[FindMIs]={Method->"FeynParUF",Rule->{},MonomialOrder:>$MintMonomialOrder};
FindMIs[ds_List,lms_List,OptionsPattern[]]:=Module[{xs,poly,ps,t,i,r,c,mis={},is={}},
{poly,t,xs}=FeynParUF[ds,lms,NamingFunction->(Table[Unique["x"],{#}]&)]/.OptionValue[Rule];
poly=poly+t;
ps=PermutationSymmetries[poly,xs];
r=PMilnorNumber[poly,xs,Symmetric->True,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[FindMIs],Options[PMilnorNumber]]];
If[r===Indeterminate,Return[Indeterminate]];
t=i=Length@ds;
(*ps=ps/.Thread[xs\[Rule]Range[i]];*)
Monitor[
While[Length@mis<r,
If[is==={},is=-Sort[-Flatten[Permutations/@IntegerPartitions[t++,{i}],1]]];
c=First[is];
is=Complement[is,permute[c,#]&/@ps];
AppendTo[mis,c]
],
"Selecting masters..."
];
mis
];


If[Mint`Private`LiteRed,
FindMIs::usage=FindMIs::usage<>"\nFindMIs[\!\(\*
StyleBox[\"jsec\", \"TI\"]\):_js] finds masters in sector \!\(\*
StyleBox[\"jsec\", \"TI\"]\).";
FindMIs[jsec:js[nm_,n:(1|0)...],OptionsPattern[]]:=
Module[{mtd=OptionValue[Method],res,msg=False,xs,ds,jsyms,poly,gs,gb,ps,plane,t,c,r,rs={},x0,y0,i,pt={},mis={},is={}},
{poly,t,xs}=FeynParUF[jsec]/.OptionValue[Rule];
poly=poly+t;
ps=PermutationSymmetries[poly,xs];
If[mtd==="GramP",
(*GramP*)
jsyms=jSymmetries@@jsec;
If[Head@jsyms===jSymmetries,Message[CountMIs::wrng,jsyms];Abort[]];
(*t=Flatten@{Position[Rest@jsec,0],-1};
jsyms=DeleteDuplicates[(jsyms)\[LeftDoubleBracket]All,t,t\[RightDoubleBracket]];*)
t=Append[ReplacePart[j@@jsec,#->-1]&/@Flatten@Position[jsec,0],j@@jsec];
jsyms=Function[x,Outer[Coefficient[#1/.x,#2]&,t,t]]/@jsyms;
(*Quiet[Check[,msg=True,{PMilnorNumber::ni}]]*)
r=PMilnorNumber[poly,ds,Symmetric->True,TransformationMatrix->jsyms,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[FindMIs],Options[PMilnorNumber]]]
,
(*FeynParUF*)
(*Quiet[Check[,msg=True,{PMilnorNumber::ni}]]*)
r=PMilnorNumber[poly,xs,Symmetric->True,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[FindMIs],Options[PMilnorNumber]]]
];
If[msg,Message[PMilnorNumber::ni,jsec]];
If[r===Indeterminate,Return[Indeterminate]];
t=i=Length@xs;
(*ps=ps/.Thread[xs\[Rule]Range[i]];*)
Monitor[
While[Length@mis<r,
If[is==={},is=-Sort[-Flatten[Permutations/@IntegerPartitions[t++,{i}],1]]];
c=First[is];
is=Complement[is,permute[c,#]&/@ps];
AppendTo[mis,c]
],
"Selecting masters..."
];
ReplacePart[j[nm,n],Thread[Position[j[nm,n],1]->#]]&/@mis
]
]


CountMIs::usage="CountMIs[{\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(2\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(2\)]\),\[Ellipsis]}] gives the number of the master integrals with denominators \!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"d\", \"TI\"], \(2\)]\),\[Ellipsis] and loop momenta \!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(1\)]\),\!\(\*SubscriptBox[
StyleBox[\"l\", \"TI\"], \(2\)]\),\[Ellipsis].";
Options[CountMIs]={Symmetric->True,Method->"FeynParUF",Rule->{},MonomialOrder:>$MintMonomialOrder};
CountMIs[ds_List,lms_List,OptionsPattern[]]:=Module[{xs,poly,ps,t},
{poly,t,xs}=FeynParUF[ds,lms,NamingFunction->(Table[Unique["x"],{#}]&)]/.OptionValue[Rule];
poly=poly+t;
(*ps=PermutationSymmetries[poly,xs];*)
PMilnorNumber[poly,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[CountMIs],Options[PMilnorNumber]]]
];


If[Mint`Private`LiteRed,
CountMIs::usage=CountMIs::usage<>"\nCountMIs[\!\(\*
StyleBox[\"jsec\", \"TI\"]\):_js] finds masters in sector \!\(\*
StyleBox[\"jsec\", \"TI\"]\).";
CountMIs::wrng="Something wrong: `1` is not defined.";
CountMIs[jsec:js[nm_,n:(1|0)...],OptionsPattern[]]:=Module[{mtd=OptionValue[Method],res,msg=False,xs,ds,jsyms,poly,gs,gb,ps,plane,t,c,r,rs={},x0,y0,i,pt={},mis={},is={}},
{poly,t,xs}=FeynParUF[jsec]/.OptionValue[Rule];
poly=poly+t;
(*ps=PermutationSymmetries[poly,xs];*)
If[mtd==="GramP",
(*GramP*)
poly=GramP[jsec];
If[Head@poly===GramP,Message[CountMIs::wrng,poly];Abort[]];
{poly,ds}=poly/.OptionValue[Rule];
(*t=Flatten@{Position[Rest@jsec,0],-1};
jsyms=DeleteDuplicates[(jSymmetries@@jsec)\[LeftDoubleBracket]All,t,t\[RightDoubleBracket]];
Quiet[Check[r=PMilnorNumber[poly,ds,Symmetric\[Rule]OptionValue[Symmetric],TransformationMatrix\[Rule]If[OptionValue[Symmetric],jsyms,None]],msg=True,{PMilnorNumber::ni}]]*)
If[OptionValue[Symmetric],
jsyms=jSymmetries@@jsec;
If[Head@jsyms===jSymmetries,Message[CountMIs::wrng,jsyms];Abort[]];
(*t=Flatten@{Position[Rest@jsec,0],-1};
jsyms=DeleteDuplicates[(jsyms)\[LeftDoubleBracket]All,t,t\[RightDoubleBracket]];*)
t=Append[ReplacePart[j@@jsec,#->-1]&/@Flatten@Position[jsec,0],j@@jsec];
jsyms=Function[x,Outer[Coefficient[#1/.x,#2]&,t,t]]/@jsyms;
(*Quiet[Check[,msg=True,{PMilnorNumber::ni}]]*)
r=PMilnorNumber[poly,ds,TransformationMatrix->jsyms,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[CountMIs],Options[PMilnorNumber]]]
,
(*Quiet[Check[,msg=True,{PMilnorNumber::ni}]]*)
r=PMilnorNumber[poly,ds,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[CountMIs],Options[PMilnorNumber]]]
]
,
(*FeynParUF*)
(*Quiet[Check[,msg=True,{PMilnorNumber::ni}]]*)
r=PMilnorNumber[poly,xs,Sequence@@FilterRules[#1->OptionValue[#1]&@@@Options[CountMIs],Options[PMilnorNumber]]]
];
(*If[msg,Message[PMilnorNumber::ni,jsec]];*)
r
]
]


End[]


EndPackage[]

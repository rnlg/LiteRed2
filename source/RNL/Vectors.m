(* ::Package:: *)

(* ::Title:: *)
(*Vectors*)


(* ::Section:: *)
(*Preamble*)


(* ::Text:: *)
(*I do believe the way ValueQ works in Mma 12.2 is buggy, so at first ocassion I set the option Method->"Legacy"*)


If[$VersionNumber>=12.2,SetOptions[ValueQ,Method->"Legacy"]];


Types`TypesLog=False; 


(* ::Section:: *)
(*BeginPackage[Vectors`]*)


Needs["LinearFunctions`","RNL`LinearFunctions`"];
Needs["Numbers`","RNL`Numbers`"];
Needs["Types`","RNL`Types`"];
BeginPackage["Vectors`",{"LinearFunctions`","Numbers`","Types`"}]


$VectorsVersion="1.1";
$VectorsReleaseDate="11.01.2020";


Off[General::"spell1"];


(* ::Section:: *)
(*Open all public names*)


Vector::usage="Vector is a type of all vectors";
VectorIndex::usage="VectorIndex is a type of vector indices";
TComponent::usage="TComponent[{\[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR]},{\[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR]},{\[ScriptD]\[ScriptU]\[ScriptM]\[ScriptM]\[ScriptI]\[ScriptE]\[ScriptS]}] is a type of tensors\n, \[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR], \[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR], and \[ScriptD]\[ScriptU]\[ScriptM]\[ScriptM]\[ScriptI]\[ScriptE]\[ScriptS] are the upper, lower, and repeated indices";


VecQ::usage="VecQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives True if \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is of type Vector";
VecVarQ::usage="VecVarQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives True if \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is a variable of type Vector";
VecIndQ::usage="VecIndQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives True if \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is of type VectorIndex";
TCompQ::usage="TCompQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives True if \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is of type TComponent[...]";
TComponentQ::usage="TComponentQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],{\[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR]},{\[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR]}] gives true if TypeOf[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] matches TComponent[{\[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR]},{\[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR]},_List]";
VPolyQ::usage="VPolyQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptV]\[ScriptE]\[ScriptC]] gives True when \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is a polynomial in vector variable \[ScriptV]\[ScriptE]\[ScriptC].\n
VPolyQ[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives True when \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] is a polynomial in all vector variables";


VecVarsMark::usage=
  "VecVarsMark[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] marks all vector variables in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] with the tag VecVar";
Dummies::usage="Dummies[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives a list of all repeated indices of \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
VIContract::usage="VIContract[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] is a transparent tag to note that there is a contraction of the vector indices in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR], input it as \[LeftAngleBracket]\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]\[RightAngleBracket]";
VExpand::usage ="VExpand[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] expands expression \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR], distributing sp over Plus";
DummyEliminate::usage="DummyEliminate[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] eliminates repeated indices in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
ReduceDummies::usage="ReduceDummies[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] reduces the number of repeated indices in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
RemoveDummies::usage="RemoveDummies is an option for ReduceDummies and DummyEliminate";
SpEliminate::usage="SpEliminate[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] indexes all scalar products in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
VGrad::usage ="VGrad[\[ScriptF],{\[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR]},{\[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR]}][\[ScriptV]\[ScriptE]\[ScriptC]] is a (multiple) gradient of \[ScriptF] with respect to its argument.\n
\[ScriptU]\[ScriptP]\[ScriptP]\[ScriptE]\[ScriptR] and \[ScriptL]\[ScriptO]\[ScriptW]\[ScriptE]\[ScriptR] are the upper and lower indices, \[ScriptV]\[ScriptE]\[ScriptC] is an argument";
Partial::usage ="Partial[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\!\(\*SubscriptBox[\"\[ScriptR]\", \"\[Mu]\"]\)] gives the gradient of \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] with respect to \!\(\*SubscriptBox[\"\[ScriptR]\", \"\[Mu]\"]\)";
SetConstraints;PrintConstraints;ClearConstraints;
sp::usage="sp[\[ScriptV]\[ScriptE]\[ScriptC]1,\[ScriptV]\[ScriptE]\[ScriptC]2] is a scalar product of \[ScriptV]\[ScriptE]\[ScriptC]1 and \[ScriptV]\[ScriptE]\[ScriptC]2. It can be entered as \[ScriptV]\[ScriptE]\[ScriptC]1\[CenterDot]\[ScriptV]\[ScriptE]\[ScriptC]2";
SupIndex::usage="SupIndex[\[ScriptV]\[ScriptE]\[ScriptC],\[ScriptI]\[ScriptN]\[ScriptD]\[ScriptE]\[ScriptX]] determines the upper index of the vector";
SubIndex::usage="SupIndex[\[ScriptV]\[ScriptE]\[ScriptC],\[ScriptI]\[ScriptN]\[ScriptD]\[ScriptE]\[ScriptX]] determines the lower index of the vector";
LowerIndex::usage="LowerIndex[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[Mu]\[Rule]\[Nu]] turns (upper) index \[Mu] to lower index \[Nu] in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
UpperIndex::usage="UpperIndex[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[Mu]\[Rule]\[Nu]] turns (lower) index \[Mu] to upper index \[Nu] in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
UpperIndices::usage="UpperIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives a list of upper indexes of \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
LowerIndices::usage="LowerIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives a list of lower indexes of \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
Indices::usage="Indices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives a list {UpperIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]],LowerIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]]}" ;AllIndices::usage="AllIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] gives a list of all indices used in the \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
SwapIndices::usage="SwapIndices[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] swaps upper and lower indices in \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
MetricTensor::usage="MetricTensor[{},{\[Mu],\[Nu]}] represents the covariant metric tensor\n
Can be entered and printed as \!\(\*SubscriptBox[\"g\", 
RowBox[{\"\[Mu]\", \",\", \"\[Nu]\"}]]\)";
(*AntisymmetricPTensor::usage="AntisymmetricPTensor[{...},{...}] represents antisymmetric pseudo tensor, AntisymmetricPTensor[{1,2,..,n},{}]=1, where n is the dimension of the vector space (See ?SetDim)";*)
SetDim::usage="SetDim[\[ScriptD]\[ScriptI]\[ScriptM]] sets the dimension of the vector space equal to \[ScriptD]\[ScriptI]\[ScriptM].\n
SetDim[] unsets the dimension of the vector space";
VAverage::usage="VAverage[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptV]\[ScriptE]\[ScriptC]] averages \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] over the directions of vector \[ScriptV]\[ScriptE]\[ScriptC].\n
Works only for polynomials";
TensorSet::usage="TensorSet[{\[ScriptV]\[ScriptE]\[ScriptC]\[ScriptS]},{\[ScriptI]\[ScriptN]\[ScriptD]\[ScriptS]}] gives set of all tensors constructed of \[ScriptV]\[ScriptE]\[ScriptC]\[ScriptS] and metric tensor";
TSCollect::usage="TSCollect[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptF]\[ScriptU]\[ScriptN]\[ScriptC]] collects coefficients at the different tensor structures and applies \[ScriptF]\[ScriptU]\[ScriptN]\[ScriptC] on them";


Ort::usage="Ort[n] is a basis vector \!\(\*SubscriptBox[\(\[DoubleStruckE]\), \(n\)]\).";
OrtC::usage="OrtC[n] is a basis vector \!\(\*SuperscriptBox[\(\[DoubleStruckE]\), \(n\)]\).";


Vectors::usage=
  "Vectors package is written by Roman Lee. It makes some definitions for the elementary functions on vectors and contains some useful tools\n"<>
    ToString[TableForm[{Names["Vectors`*"]},TableSpacing->{0,3}]];


DAverage::usage="DAverage[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptV]\[ScriptE]\[ScriptC]] averages \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR] over the directions of vector \[ScriptV]\[ScriptE]\[ScriptC].\n\
Should work more effectively than VAverage.";
GramDeterminant::usage="GramDeterminant[\!\(\*SubscriptBox[\(\[ScriptV]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptV]\), \(2\)]\),\[Ellipsis]] is the determinant of the matrix {\!\(\*SubscriptBox[\(\[ScriptV]\), \(i\)]\)\[CenterDot]\!\(\*SubscriptBox[\(\[ScriptV]\), \(j\)]\)}.";
GramMatrix::usage="GramMatrix[\!\(\*SubscriptBox[\(\[ScriptV]\), \(1\)]\),\!\(\*SubscriptBox[\(\[ScriptV]\), \(2\)]\),\[Ellipsis]] is the Gram matrix {\!\(\*SubscriptBox[\(\[ScriptV]\), \(i\)]\)\[CenterDot]\!\(\*SubscriptBox[\(\[ScriptV]\), \(j\)]\)}.";


VectorsLog::usage="VectorsLog=True turns on some log information of the package.";If[!ValueQ[VectorsLog],VectorsLog=True];


Vectors`Private`VectorsPrint:=If[VectorsLog,Print[##]]&;(*VectorsPrint is equivalent to Print if VectorsLog=True*)
Vectors`Private`VectorsPrint[$Input];
Vectors`Private`VectorsPrint["****************",Style["Vectors v"<>ToString[$VectorsVersion],{Bold}],"********************\n\
Author:Roman N.Lee,Budker Institute of Nuclear Physics,Novosibirsk.\n\
Vectors package defines several types:Vector,VectorIndex,TComponent.\n\
Use Declare[{v1,v2,\[Ellipsis]},Vector] to declare variables v1,v2,\[Ellipsis] as vectors.\n\
See ?Vectors`* for a list of functions."];


(* ::Section:: *)
(*Begin[`Private`]*)


Begin["`Private`"]


VectorsPrint["Be careful:
I. If \!\(\*
StyleBox[\"v\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)is declared as \!\(\*
StyleBox[\"Vector\",\nFontWeight->\"Bold\"]\) and \!\(\*
StyleBox[\"\[Mu]\",\nFontWeight->\"Bold\"]\) as a \!\(\*
StyleBox[\"VectorIndex\",\nFontWeight->\"Bold\"]\)\*
StyleBox[\(\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\) \)]then the notation \!\(\*SuperscriptBox[
StyleBox[\"v\",\nFontWeight->\"Bold\"], 
StyleBox[\"\[Mu]\",\nFontWeight->\"Bold\"]]\) denotes not the power but the contravariant component.
II. Be sure to use VIContract[...] to avoid ambiguities in contraction of indices (it is also entered as \[LeftAngleBracket]...\[RightAngleBracket] )"];


(* ::Section:: *)
(*Add syntax rules for vectors*)


VectorIndex::wrngset="It is not allowed to assign a value to vector index `1`";
VectorIndex::value="`1` is valued, please, unset. Aborting...";


InitFunction[x_,VectorIndex]:=If[ValueQ[x],Message[VectorIndex::value,HoldForm[x]];Abort[];,x/:(TagSet|TagSetDelayed)[x,x,_]:=Message[VectorIndex::wrngset,x];
x/:(Set|SetDelayed)[x,_]:=Message[VectorIndex::wrngset,x];];


(*SetLinear[sp,{1,2},VecQ];*)
LFRules=Union[LFRules,{HoldPattern[sp[a_,b_Plus]]:>ReleaseHold[Hold[sp[a,#1]]&/@b],HoldPattern[sp[b_Plus,a_]]:>ReleaseHold[Hold[sp[#1,a]]&/@b],HoldPattern[sp[a_,b_?NumQ c_]]:>b*sp[a,c],HoldPattern[sp[b_?NumQ c_,a_]]:>b*sp[c,a],HoldPattern[VIContract[a_Plus]]:>VIContract/@a}];


VectorIndices=Alternatives[];


Unprotect[TypesAbove,TypesBelow];
(*TypesBelow[TComponent[{},{},a_List]]=Union[Alternatives[TComponent[{},{},a]],TypesBelow[Number]];*)
TypesBelow[Number]=Union[Alternatives[TComponent[{},{},_List]],TypesBelow[Number]];
Scan[(TypesAbove[#]=Union[TypesAbove[Number],TypesAbove[#]])&,TypesBelow[Number]];
Protect[TypesAbove,TypesBelow];


Block[{TypesBelow},Clear[TypesBelow];Unprotect[Times,Plus,Divide,Integrate];
ReleaseHold[Hold[
AddTypeRule[MetricTensor,
{{{VectorIndex...},a_List},{{VectorIndex...},b_List}}:>TComponent[Evaluate/@a,Evaluate/@b,{}],{{{Number...},_},{{Number...},_}}:>Number,{a:{{Number...,VectorIndex,Number...},_},{{Number...},_}}:>TComponent[Evaluate/@Cases[Transpose[a],{VectorIndex,b_}:>b],{},{}],{{{Number...},_},a:{{Number...,VectorIndex,Number...},_}}:>TComponent[{},Evaluate/@Cases[Transpose[a],{VectorIndex,b_}:>b],{}]];
AddTypeRule[Times,{{Number,_}...,{Vector,_},{Number,_}...}:>Vector];
AddTypeRule[Plus,{{Vector,_}..}:>Vector,Position->1];
AddTypeRule[SubIndex,{{Vector,_},{Number,_}}:>Number,{{Vector,_},{VectorIndex,i_}}:>TComponent[{},{Evaluate@i},{}]];AddTypeRule[SupIndex,{{Vector,_},{Number,_}}:>Number,{{Vector,_},{VectorIndex,i_}}:>TComponent[{Evaluate@i},{},{}]];
AddTypeRule[Plus,l1:{{Number,_}...,{TComponent[{},{},_List],_},{Number,_}...}:>TComponent[{},{},Union@@((#[[3]])&/@(Cases[(#[[1]]&/@l1),_TComponent]))],Position->1];(*Need position because of shading of Plus for repeateders*)
AddTypeRule[Plus,l1:{({TComponent[a_List,b_List,_List],_})..}:>TComponent[a,b,Union@@((#[[1,3]])&/@l1)]];
AddTypeRule[Times,l:{{Number|_TComponent,_}...,{_TComponent,_},{Number|_TComponent,_}...}:>TComponent@@Sort/@MapThread[Join,Apply[List,Cases[(#[[1]]&/@l),_TComponent],2]],Position->1];
(*Need position because of shading of Times for numbers*)
AddTypeRule[sp,{{Vector,_},{Vector,_}}:>Number];
AddTypeRule[Integrate,{{atr:(Number|Vector|_TComponent),_},{{Number..}|Number,_}..}:>atr];
]/.{a:(Number|Vector)->TypesBelow[a]}/.{(a_:>TypesBelow[b:(Number|Vector)])->(a:>b)}
];Protect[Times,Plus,Divide,Integrate];
]


(* ::Subsubsection:: *)
(*TComponent*)


HoldPattern[TComponent[_List,{___,a_,a_,___},_List]]=Badformed;
HoldPattern[TComponent[{___,a_,a_,___},_List,_List]]=Badformed
HoldPattern[TComponent[_List,{x___,a_,y___},{z___,a_,t___}]]=Badformed;
HoldPattern[TComponent[{x___,a_,y___},_List,{z___,a_,t___}]]=Badformed;
HoldPattern[TComponent[{x___,a_,y___},{z___,a_,t___},{u___}]]:=TComponent[{x,y},{z,t},{u,a}];


(*(*EuclideanSpace[Off]:=(Clear[TComponent];*)*)
(*(*TComponent[_List,{___,a_,a_,___},_List]=Badformed;TComponent[{___,a_,a_,___},_List,_List]=Badformed;TComponent[_List,{x___,a_,y___},{z___,a_,t___}]=Badformed;TComponent[{x___,a_,y___},_List,{z___,a_,t___}]=Badformed;TComponent[{x___,a_,y___},{z___,a_,t___},{u___}]:=TComponent[{x,y},{z,t},{u,a}];*)*)
(*(*(*LowerIndex=.;SubIndex[x_?VecQ,i_?VecIndQ]=.;*)*)
(*(*HoldPattern[MetricTensor[{a___},{b__}]]=.*)*)
(*(*MetricTensor/:HoldPattern[MetricTensor[{m1___,n_,m2___},{}]*MetricTensor[{m3___,n_,m4___},{}]]=.;MetricTensor/:HoldPattern[MetricTensor[{m1___,n_,m2___},k1_List]*MetricTensor[k2_List,{m3___,n_,m4___}]]:=MetricTensor[Join[{m1,m2},k2],Join[k1,{m3,m4}]];HoldPattern[TypeX[(SupIndex[x_?VecQ,i_?VecIndQ])^2]]=.;*))*)*)
(*(*EuclideanSpace[On]:=(Clear[TComponent];*)*)
(*(*TComponent[{b___},{a__},c_List]:=TComponent[{b,a},{},c];*)*)
(*(*TComponent[{x___,a_,y___,a_,z___},{},{b___}]:=TComponent[{x,y,z},{},{b,a}];TComponent[{x___,a_,y___},_List,{z___,a_,t___}]=Badformed;TComponent[_List,{x___,a_,y___},{z___,a_,t___}]=Badformed;*)*)
(*(*(*LowerIndex=UpperIndex;SubIndex[x_?VecQ,i_?VecIndQ]:=SupIndex[x,i];MetricTensor/:HoldPattern[MetricTensor[{m1___,n_,m2___},k1_List]*MetricTensor[k2_List,{m3___,n_,m4___}]]=.;*)*)
(*(*HoldPattern[MetricTensor[{a___},{b__}]]:=MetricTensor[{a,b},{}];*)*)
(*(*MetricTensor/:HoldPattern[MetricTensor[{m1___,n_,m2___},{}]*MetricTensor[{m3___,n_,m4___},{}]]:=MetricTensor[{m1,m2,m3,m4},{}];HoldPattern[TypeX[(a_?TCompQ)^2]]:={Join[#,#]&/@First[TypeX[Unevaluated[a]]],Unevaluated[a^2]}*);)*)*)


(* ::Section:: *)
(*Procedure definition*)


(* ::Subsection:: *)
(*Internal functions*)


VecQs[a_]:=(VecQ@@MakeExpression[a,StandardForm]);
VecIndQs[a_]:=(VecIndQ@@MakeExpression[a,StandardForm]);
TCQs[a_]:=MatchQ[First[ToExpression[RowBox[{"TypeX","[",RowBox[{"Unevaluated","[",a,"]"}],"]"}]]],_TComponent]
TCQ[x_]:=MatchQ[First[TypeX[x]],_TComponent]
rbn[v_,u_List,d_List]:=InterpretationBox[#,VGrad[v,u,d],Editable->False]&[RowBox[Join[(SuperscriptBox["\[EmptyDownTriangle]",ToBoxes[#]]&/@u),(SubscriptBox["\[EmptyDownTriangle]",ToBoxes[#]]&/@d),{ToBoxes[v]}]]]
rbn[v_,u_,d_]:=RowBox[{"VGrad","[",RowBox[{ToBoxes[v],",",ToBoxes[u],",",ToBoxes[d]}],"]"}];


NewVI[v_List,expr_]:=Module[{fvi=Complement[VectorIndices,AllIndices[expr]],l1,l2},If[(l1=Length[VectorIndices])>= (l2=Length[v]),Thread[v->Take[VectorIndices,l2]],Join[Thread[Take[v,l1]->VectorIndices],newvi[Take[v,l1-l2]]]]];
newvi[v__]:=(Declare[{##},VectorIndex];VectorIndices=Union[VectorIndices,Alternatives[##]];Rule@@@Transpose[{{v},{##}}])&@@(Unique["Vectors`\[Mu]"]&/@{v})
VISort[x_,y_]:=If[MatchQ[x,VectorIndices]&&!MatchQ[y,VectorIndices],False,If[MatchQ[y,VectorIndices]&&!MatchQ[x,VectorIndices],True,OrderedQ[{x,y}]]]


(* ::Subsection:: *)
(*Basic functions*)


(* ::Subsubsection:: *)
(*VExpand*)


(*VErule1:={
		a_*(b_+c__):>((a*#)&/@(b+c)),
		sp[a_?VecQ,b_?VecQ+c__?VecQ]:>(sp[a,#]&/@(b+c)),
		sp[a_?NumQ*b_?VecQ,c_?VecQ]:>a*sp[b,c],
		(a:(SupIndex|SubIndex))[b_?VecQ+c__?VecQ,d_]:>(a[#,d]&/@(b+c)),
		(a:(SupIndex|SubIndex))[b_?NumQ*c_?VecQ,d_]:>b*a[c,d]		
			};*)
VExpand[x_]:=LFDistribute[x,SupIndex|SubIndex|sp|Times]


(* ::Subsubsection:: *)
(*SupIndex,SubIndex*)


MakeExpression[SuperscriptBox[a_?VecQs,b_],StandardForm]:=MakeExpression[RowBox[{"SupIndex","[",RowBox[{a,",",b}],"]"}],StandardForm]
MakeBoxes[SupIndex[a_?VecQ,b_],StandardForm]:=SuperscriptBox[MakeBoxes[a,StandardForm],MakeBoxes[b,StandardForm]]
MakeBoxes[SupIndex[a_?VecQ,b_],TraditionalForm]:=SuperscriptBox[MakeBoxes[a,TraditionalForm],MakeBoxes[b,TraditionalForm]]
MakeExpression[SubscriptBox[a_?VecQs,b_],StandardForm]:=MakeExpression[RowBox[{"SubIndex","[",RowBox[{a,",",b}],"]"}],StandardForm]
MakeBoxes[SubIndex[a_?VecQ,b_],StandardForm]:=SubscriptBox[MakeBoxes[a,StandardForm],MakeBoxes[b,StandardForm]]
MakeBoxes[SubIndex[a_?VecQ,b_],TraditionalForm]:=SubscriptBox[MakeBoxes[a,TraditionalForm],MakeBoxes[b,TraditionalForm]]


Options[SubIndex]=Options[SupIndex]={AutoDeclareVI->True};


SubIndex[0,y_?VecIndQ]=SupIndex[0,y_?VecIndQ]=0;
HoldPattern[SupIndex[(x_Plus)?VecQ,y_?VecIndQ]]:=SupIndex[#,y]&/@x;
HoldPattern[SubIndex[(x_Plus)?VecQ,y_?VecIndQ]]:=SubIndex[#,y]&/@x;
HoldPattern[SupIndex[(x_?NumQ)*z_?VecQ,y_?VecIndQ]]:=x*SupIndex[z,y];
HoldPattern[SubIndex[(x_?NumQ)*z_?VecQ,y_?VecIndQ]]:=x*SubIndex[z,y];
SupIndex[v_,i_Symbol]/;(TypeOf[i]===Untyped&&(AutoDeclareVI/.Options[SupIndex])):=(Declare[i,VectorIndex];SupIndex[v,i])
SubIndex[v_,i_Symbol]/;(TypeOf[i]===Untyped&&(AutoDeclareVI/.Options[SubIndex])):=(Declare[i,VectorIndex];SubIndex[v,i])


(* ::Subsubsection:: *)
(*SetConstraints, PrintConstraints, ClearConstraints*)


SetAttributes[SetConstraints,HoldRest];
Options[SetConstraints]={Print->True};


SetConstraints::usage="SetConstraints[{p1,p2},sp[p1+p2]=1;sp[p1]=1;sp[p2]=0] resolves constraints with respect to scalar products and performes appropriate assignments";
SetConstraints::wrong="Something wrong in constraints `1`. Aborting...";


SetConstraints[{vs__?VecVarQ},cs_,OptionsPattern[]]:=Module[
{set,spp,equal,cns,x,eqs,vars,sol},
SetAttributes[set,HoldAll];
set[a_,set[b_,c_]]:=Sequence@@{set[a,c],set[b,c]};
cns=DeleteCases[ReleaseHold[Replace[Hold[cs],{Hold[CompoundExpression[x__]]:>Hold[{x}],Hold[x_]:>Hold[{x}]}]/.Set->set],Null];
If[!MatchQ[cns,{_set..}],Message[SetConstraints::wrong,HoldForm[cs]];Abort[]];
eqs=ReleaseHold[cns//.set->equal/.{x_equal:>Flatten[x]}]/.{HoldPattern[x_equal]:>(Equal@@(LFDistribute[#,sp]&/@x)),Null:>Sequence[]};
vars=Union@Cases[eqs,ss_sp?(Not[FreeQ[#,Alternatives[vs]]]&),\[Infinity]];
sol=Solve[eqs,vars];
If[Length[sol]=!=1,Message[SetConstraints::wrong,HoldForm[cs]];Abort[]];
If[sol==={{}},If[TrueQ@OptionValue[Print],Print["Executing nothing. Everything was already set?"]];Return[]];
sol=CompoundExpression@@@(HoldForm[#]&@(Set@@@Hold@@Append[First[sol],Null]));
If[TrueQ@OptionValue[Print],Print["Executing\n",sol]];
ReleaseHold[sol];
];


PrintConstraints::usage="PrintConstraints[{p1,p2}] prints available constraints for vectors p1,p2";


PrintConstraints[vl_List]:=PrintConstraints@@vl;
PrintConstraints[vs__?VecVarQ]:=Module[{dv=DownValues[sp],uv=UpValues/@{vs}},
dv=Delete[HoldForm[#1=#2],{1,1,0}]&@@@Select[dv,Not[FreeQ[#,Alternatives[vs]]]&];
uv=MapThread[Function[{s,v},(Delete[HoldForm[TagSet[v,#1,#2]],{1,2,0}]&@@@Select[s,Not[FreeQ[#,sp]]&])],{uv,{vs}}];
dv=Append[Flatten[{dv,uv}],Null];
If[dv=={Null},Print["No constraints found."],Print["Constraints found:\n",CompoundExpression@@@(HoldForm[#]&@dv)]]
]


Options[ClearConstraints]={Print->True};


ClearConstraints::usage="ClearConstraints[{p1,p2}] clears available constraints for vectors p1,p2";


ClearConstraints[{vs__},opts:OptionsPattern[]]:=ClearConstraints[vs,opts];
ClearConstraints[vs__?VecVarQ,opts:OptionsPattern[]]:=Module[{dv=DownValues[sp],uv=UpValues/@{vs}},
dv=Delete[HoldForm[Unset[#1]],{1,1,0}]&@@@Select[dv,Not[FreeQ[#,Alternatives[vs]]]&];
uv=MapThread[Function[{s,v},(Delete[HoldForm[TagUnset[v,#1]],{1,2,0}]&@@@Select[s,Not[FreeQ[#,sp]]&])],{uv,{vs}}];
dv=Append[Flatten[{dv,uv}],Null];
If[dv=={Null},If[TrueQ@OptionValue[Print],Print["No constraints found."]],If[TrueQ@OptionValue[Print],Print["Executing\n",CompoundExpression@@@(HoldForm[#]&@dv)]];ReleaseHold[dv]];
]


(* ::Subsubsection:: *)
(*sp*)


MakeBoxes[sp[a_?VecQ,b_?VecQ],StandardForm]:=RowBox[{Parenthesize[a, StandardForm, Times, Left],"\[CenterDot]",Parenthesize[b, StandardForm, Times, Right]}](*RowBox[{"(",MakeBoxes[a],"\[CenterDot]",MakeBoxes[b],")"}]*)
MakeBoxes[sp[a_?VecQ,b_?VecQ],TraditionalForm]:=RowBox[{Parenthesize[a, TraditionalForm, Times, Left],"\[CenterDot]",Parenthesize[b, TraditionalForm, Times, Right]}]
MakeBoxes[sp[a_?VecQ,a_?VecQ],TraditionalForm]:=SuperscriptBox[Parenthesize[a, TraditionalForm, Times, Left],"2"]
MakeExpression[RowBox[{lhs___,a_?VecQs,"\[CenterDot]",b_?VecQs,rhs___}],StandardForm]:=
MakeExpression[RowBox[{lhs,RowBox[{"sp","[",RowBox[{a,",",b}],"]"}],rhs}],StandardForm]


sp::set="Warning: assignment for `1`. Typically, it is not what you want.";


SetAttributes[sp,Orderless];
sp[0,_]=0;(*Special treatment of zero*)
sp[x_]:=sp[x,x];
sp/:Set[sp[x_],expr_]:=(sp[x,x]=expr);
sp/:SetDelayed[sp[x_],expr_]:=(sp[x,x]:=expr);
sp/:Unset[sp[x_]]:=Unset[sp[x,x]];


sp/:Set[sp[x:Except[_?VecVarQ],y_],expr_]:=(Message[sp::set,HoldForm[sp[x,y]]];setsp[x,y,expr]);
setsp[a_,b_,c_]:=Module[{r},sp/:Set[sp[x:Except[_?VecVarQ],y_],expr_]=.;r=(sp[a,b]=c);sp/:Set[sp[x:Except[_?VecVarQ],y_],expr_]:=(Message[sp::set,HoldForm[sp[x,y]]];setsp[x,y,expr]);r];


sp/:SetDelayed[sp[x:Except[_?VecVarQ],y_],expr_]:=(Message[sp::set,HoldForm[sp[x,y]]];setdsp[x,y,expr]);
setdsp[a_,b_,c_]:=Module[{r},sp/:SetDelayed[sp[x:Except[_?VecVarQ],y_],expr_]=.;sp[a,b]:=c;sp/:SetDelayed[sp[x:Except[_?VecVarQ],y_],expr_]:=(Message[sp::set,HoldForm[sp[x,y]]];setdsp[x,y,expr]);r];


(* ::Subsubsection:: *)
(*MetricTensor*)


MakeExpression[SuperscriptBox["g",RowBox[{\[Mu]_,",",\[Nu]_}]],StandardForm]:=
MakeExpression[RowBox[{"MetricTensor","[",
RowBox[{RowBox[{"{",RowBox[{\[Mu],",",\[Nu]}],"}"}],",",RowBox[{"{","}"}]}],"]"}],StandardForm]
MakeBoxes[MetricTensor[{\[Mu]_,\[Nu]_},{}],StandardForm]:=
SuperscriptBox["g",RowBox[{MakeBoxes[\[Mu],StandardForm],",",MakeBoxes[\[Nu],StandardForm]}]]
MakeExpression[SubscriptBox["g",RowBox[{\[Mu]_,",",\[Nu]_}]],StandardForm]:=
MakeExpression[RowBox[{"MetricTensor","[",
RowBox[{RowBox[{"{","}"}],",",RowBox[{"{",RowBox[{\[Mu],",",\[Nu]}],"}"}]}],"]"}],StandardForm]
MakeBoxes[MetricTensor[{},{\[Mu]_,\[Nu]_}],StandardForm]:=
SubscriptBox["g",RowBox[{MakeBoxes[\[Mu],StandardForm],",",MakeBoxes[\[Nu],StandardForm]}]]
MakeExpression[SubsuperscriptBox["g",\[Mu]_,\[Nu]_],StandardForm]:=
MakeExpression[RowBox[{"MetricTensor","[",RowBox[{RowBox[{"{",\[Nu],"}"}],",",RowBox[{"{",\[Mu],"}"}]}],"]"}],StandardForm]
MakeBoxes[MetricTensor[{\[Mu]_},{\[Nu]_}],StandardForm]:=
SubsuperscriptBox["g",MakeBoxes[\[Nu],StandardForm],MakeBoxes[\[Mu],StandardForm]];


MetricTensor[{a__},{}]/;!OrderedQ[{a}]:=MetricTensor[Sort[{a}],{}];
MetricTensor[{},{a__}]/;!OrderedQ[{a}]:=MetricTensor[{},Sort[{a}]];
MetricTensor/:HoldPattern[MetricTensor[{m1___,n_?VecIndQ,m2___},k1_List]*MetricTensor[k2_List,{m3___,n_,m4___}]]:=MetricTensor[Join[{m1,m2},k2],Join[k1,{m3,m4}]]


(* ::Subsubsection:: *)
(*SetDim*)


SetDim[n_Symbol]:=(Declare[n,Number];MetricTensor[]=n;
VectorsPrint["SetDim: The dimension is set to ",n];)
SetDim[n_]:=(MetricTensor[]=n;VectorsPrint["SetDim: The dimension is set to ",n];)
SetDim[]:=(MetricTensor[]=.;)


MetricTensor[{i_?VecIndQ},{i_}]=MetricTensor[]


SetDim[Global`\[ScriptCapitalD]];


(* ::Subsubsection:: *)
(*VecQ,VecIndQ,TCompQ,TComponentQ,VecVarQ,VPolyQ*)


VecQ[x_]:=TypeBelowQ[Unevaluated[x],Vector]
VecIndQ[x_]:=TypeBelowQ[Unevaluated[x],VectorIndex]
TCompQ[x_]:=TypeBelowQ[Unevaluated[x],_TComponent]
TComponentQ[x_,u_,d_]:=TypeBelowQ[Unevaluated[x],TComponent[u,d,_List]]
VecVarQ[x_Symbol]:=TypeBelowQ[Unevaluated[x],Vector];
VecVarQ[x_]=False;


VPolyQ[y_,x:{__?VecVarQ}]:=And@@(VPolyQ[y,#]&/@x);
VPolyQ[y_,x_?VecVarQ]:=True/;FreeQ[y,x];
VPolyQ[x_,x_?VecVarQ]=True;
VPolyQ[(SubIndex|SupIndex)[y_?VecQ,z_],x_?VecVarQ]:=VPolyQ[y,x]/;FreeQ[z,x];
VPolyQ[sp[a_,b_],x_?VecVarQ]:=True/;(VPolyQ[a,x]&&VPolyQ[b,x]);
VPolyQ[(Plus|Times)[a_,b__],x_?VecVarQ]:=True/;(VPolyQ[a,x]&&VPolyQ[Times[b],x]);
VPolyQ[Power[a_,b_Integer],x_?VecVarQ]:=True/;(VPolyQ[a,x]&&b>=0);
VPolyQ[_,_]=False;


VPolyQ[y:(_Times|_Plus|_sp)]:=And@@VPolyQ/@List@@y;
VPolyQ[Power[a_,b_Integer]]:=True/;(VPolyQ[a]&&b>=0);
VPolyQ[y_]:=True/;FreeQ[Level[y,{-1}],_?VecVarQ];
VPolyQ[(SubIndex|SupIndex)[y_?VecQ,z_?VecIndQ]]:=VPolyQ[y];
VPolyQ[_?VecVarQ]=True;VPolyQ[_]=False;


(* ::Subsubsection:: *)
(*VecVarsMark*)


VecVarsMark[x_]:=x/. a_?VecVarQ:>Vector[a]
VecVarsMark[x_,pat_]:=x/. a:(Alternatives[pat]?VecVarQ):>Vector[a]
HoldPattern[TypeX[Vector[x_]]]^={Vector,Unevaluated[x]};


(* ::Subsubsection:: *)
(*Ort,OrtC*)


Ort::usage="Ort[n] is a basis vector \!\(\*SubscriptBox[\(\[DoubleStruckE]\), \(n\)]\).";
OrtC::usage="OrtC[n] is a basis vector \!\(\*SuperscriptBox[\(\[DoubleStruckE]\), \(n\)]\).";


TypeX[Ort[x_?VecIndQ]]^:={Vector,Unevaluated[Ort[x]]};
TypeX[OrtC[x_?VecIndQ]]^:={Vector,Unevaluated[OrtC[x]]};
TypeX[Ort[x_Integer]]^:={Vector,Unevaluated[Ort[x]]};
TypeX[OrtC[x_Integer]]^:={Vector,Unevaluated[OrtC[x]]};


MakeBoxes[Ort[\[Mu]_?VecIndQ],StandardForm]:=SubscriptBox["\[DoubleStruckE]",MakeBoxes[\[Mu],StandardForm]];
MakeBoxes[Ort[\[Mu]_Integer],StandardForm]:=SubscriptBox["\[DoubleStruckE]",MakeBoxes[\[Mu],StandardForm]];
MakeBoxes[OrtC[\[Mu]_?VecIndQ],StandardForm]:=SuperscriptBox["\[DoubleStruckE]",MakeBoxes[\[Mu],StandardForm]];
MakeBoxes[OrtC[\[Mu]_Integer],StandardForm]:=SuperscriptBox["\[DoubleStruckE]",MakeBoxes[\[Mu],StandardForm]];
MakeExpression[SubscriptBox["\[DoubleStruckE]",\[Mu]_],StandardForm]:=MakeExpression[RowBox[{"Ort","[",\[Mu],"]"}],StandardForm]
MakeExpression[SuperscriptBox["\[DoubleStruckE]",\[Mu]_],StandardForm]:=MakeExpression[RowBox[{"OrtC","[",\[Mu],"]"}],StandardForm]


Quiet[sp[a_?VecQ,OrtC[\[Mu]_?VecIndQ]]:=SupIndex[a,\[Mu]];
sp[a_?VecQ,Ort[\[Mu]_?VecIndQ]]:=SubIndex[a,\[Mu]]];


SubIndex[Ort[\[Mu]_],\[Nu]_]:=MetricTensor[{},{\[Mu],\[Nu]}]
SupIndex[Ort[\[Mu]_],\[Nu]_]:=MetricTensor[{\[Nu]},{\[Mu]}]
SubIndex[OrtC[\[Mu]_],\[Nu]_]:=MetricTensor[{\[Mu]},{\[Nu]}]
SupIndex[OrtC[\[Mu]_],\[Nu]_]:=MetricTensor[{\[Mu],\[Nu]},{}]


(* ::Subsection:: *)
(*Manipulation with indices*)


(* ::Subsubsection:: *)
(*VIContract*)


MakeExpression[RowBox[{"\[LeftAngleBracket]",a_?TCQs,"\[RightAngleBracket]"}],StandardForm]:=MakeExpression[RowBox[{RowBox[{"VIContract","[",a,"]"}]}],StandardForm]
MakeBoxes[VIContract[a_?TCQ],StandardForm]:=RowBox[{"\[LeftAngleBracket]",MakeBoxes[a,StandardForm],"\[RightAngleBracket]"}]
MakeBoxes[VIContract[a_?TCQ],TraditionalForm]:=RowBox[{"\[LeftAngleBracket]",MakeBoxes[a,TraditionalForm],"\[RightAngleBracket]"}]


SetAttributes[VIContract,{OneIdentity,Flat}];
VIContract/:HoldPattern[TypeX[t:VIContract[v_]]]:=TypeX[Unevaluated[v]];
HoldPattern[Partial[VIContract[a_],b__]]:=VIContract[Partial[a,b]];
VIContract[x_]:=x/;!TypeBelowQ[x,TComponent[_,_,{__}]];
HoldPattern[VIContract'[x_]]=1;


(* ::Subsubsection:: *)
(*LowerIndex,UpperIndex*)


HoldPattern[LowerIndex[x:(_Plus|_VIContract),y_]]:=LowerIndex[#,y]&/@x;
HoldPattern[LowerIndex[SupIndex[(x_?VecQ),i_],i_->j_]]:=SubIndex[x,j];
HoldPattern[LowerIndex[MetricTensor[{k1___,i_,k2___},{l___}],i_->j_]]:=MetricTensor[{k1,k2},Sort[{j,l}]];
HoldPattern[LowerIndex[VGrad[f_,{k1___,i_,k2___},{l___}][a_],i_->j_]]:=VGrad[f,{k1,k2},{j,l}][a];
HoldPattern[LowerIndex[x_*z_,i_->j_]]:=x*LowerIndex[z,i->j]/;FreeQ[x,i];
HoldPattern[LowerIndex[x_,i_->j_]/;TypeBelowQ[x,TComponent[_,{___,i,___},_]]]:=(x/.i->j)
HoldPattern[LowerIndex[Integrate[exp_,a__],i_->j_]]:=Integrate[#,a]&[LowerIndex[exp,i->j]];
HoldPattern[LowerIndex[x_,i_->j_]/;(i=!=j)&&TypeBelowQ[x,TComponent[{___,i,___},_,_]]]:=MetricTensor[{},{i,j}]*x
LowerIndex[x_,i_?VecIndQ]:=LowerIndex[x,i->i]/;!MatchQ[Head[i],Rule|List];
LowerIndex[x_,i_List]:=Fold[LowerIndex,x,i];
HoldPattern[UpperIndex[x:(_Plus|_VIContract),y_]]:=UpperIndex[#,y]&/@x;
HoldPattern[UpperIndex[SubIndex[(x_?VecQ),i_],i_->j_]]:=SupIndex[x,j];
HoldPattern[UpperIndex[MetricTensor[{k___},{l1___,i_,l2___}],i_->j_]]:=MetricTensor[Sort[{j,k}],{l1,l2}];
HoldPattern[UpperIndex[VGrad[f_,{k___},{l1___,i_,l2___}][a_],i_->j_]]:=VGrad[f,{j,k},{l1,l2}][a];
HoldPattern[UpperIndex[x_*z_,i_->j_]]:=x*UpperIndex[z,i->j]/;FreeQ[x,i];
HoldPattern[UpperIndex[x_,i_->j_]/;TypeBelowQ[x,TComponent[{___,i,___},_,_]]]:=(x/.i->j)
HoldPattern[UpperIndex[Integrate[exp_,a__],i_->j_]]:=Integrate[#,a]&[UpperIndex[exp,i->j]]
HoldPattern[UpperIndex[x_,i_->j_]/;(i=!=j)&&TypeBelowQ[x,TComponent[_,{___,i,___},_]]]:=MetricTensor[{i,j},{}]*x
UpperIndex[x_,i_]:=UpperIndex[x,i->i]/;!MatchQ[Head[i],Rule|List];
UpperIndex[x_,i_List]:=Fold[UpperIndex,x,i];


(* ::Subsubsection:: *)
(*UpperIndices,LowerIndices,Indices,Dummies,AllIndices,SwapIndices*)


UpperIndices[expr_]:=uli[[1]]/;MatchQ[uli=TypeOf[expr],_TComponent];
LowerIndices[expr_]:=uli[[2]]/;MatchQ[uli=TypeOf[expr],_TComponent];
Indices[expr_]:=List@@uli[[{1,2}]]/;MatchQ[uli=TypeOf[expr],_TComponent];
Dummies[expr_]:=Module[{a=TypeOf[Unevaluated[expr]]},If[Head[a]===TComponent,Sort[a[[3]],VISort],{}]];
AllIndices[expr_]:=Union[Cases[{expr},_?VecIndQ,\[Infinity]]];
SwapIndices[expr_]:=UpperIndex[LowerIndex[expr,#1],#2]&@@Indices[expr];


(* ::Subsubsection:: *)
(*ReduceDummies*)


Options[ReduceDummies]={RemoveDummies->False};


rn[x_,{b__,a_}]:=(Do[If[y=Hold[Unevaluated[x]]/.a->{b}[[j]];MatchQ[TypeOf@@y,Number|_TComponent],AppendTo[ri,a];y=y[[1,1]];Break[],y=x],{j,1,Length[{b}]}];rn[y,{b}])
rn[x_,_]:=x;
f={#,","}&;
ReduceDummies[x_]:=ReduceDummies[x,True->True]
ReduceDummies[x_,opt_Rule]:=If[TypeBelowQ[x,(_TComponent|Number)],Module[{t},ri={};t=rn[x,Dummies[x]];((*Print["removing ",Sequence@@Flatten[f/@{##}]];*)If[TrueQ[RemoveDummies/.opt/.Options[ReduceDummies]],VectorIndices=DeleteCases[VectorIndices,Alternatives[##]];Remove[##]])&@@Cases[Union[ri],y_/;MatchQ[y,VectorIndices]];t],Print["Not a correct structure"];x]


(* ::Subsubsection:: *)
(*DummyEliminate*)


Options[DummyEliminate]={RemoveDummies->False};


VErule2:={SubIndex[x_?VecQ,n_?VecIndQ]*SupIndex[y_?VecQ,n_]:>(AppendTo[ni,n];sp[x,y]),HoldPattern[MetricTensor[{m1___,n_?VecIndQ,m2___},k1_List]]*HoldPattern[MetricTensor[k2_List,{m3___,n_,m4___}]]:>MetricTensor[Join[{m1,m2},k2],Join[k1,{m3,m4}]],
(HoldPattern[MetricTensor[{n_,m_}|{m_,n_},{}]]*x_):>(AppendTo[ni,n];UpperIndex[x,n->m])/;TypeBelowQ[x,HoldPattern[TComponent[{___},{___,n,___},_]]],(HoldPattern[MetricTensor[{m_},{n_}]]*x_):>(AppendTo[ni,n];UpperIndex[x,n->m])/;TypeBelowQ[x,HoldPattern[TComponent[{___,n,___},{___},_]]],(HoldPattern[MetricTensor[{},{n_,m_}|{m_,n_}]]*x_):>(AppendTo[ni,n];LowerIndex[x,n->m])/;TypeBelowQ[x,HoldPattern[TComponent[{___,n,___},{___},_]]],(HoldPattern[MetricTensor[{n_},{m_}]]*x_):>(AppendTo[ni,n];LowerIndex[x,n->m])/;TypeBelowQ[x,HoldPattern[TComponent[{___},{___,n,___},_]]]};
(*f={#,","}&;   *)         
DummyEliminate[x_Plus,opt:OptionsPattern[]]:=DummyEliminate[#,opt]&/@x;
DummyEliminate[x_,opt:OptionsPattern[]]:=ne[x,Alternatives@@Dummies[x],OptionValue[RemoveDummies]]
ne[x_,Alternatives[],opt_]:=x;
ne[(x_)[y__],n_,opt_]:=ne[#,n,opt]&/@x[y]/;!MatchQ[x,Times|SubIndex|SupIndex];
ne[x_*y_,n_,opt_]:=ne[x,n,opt]*y/;FreeQ[y,n];
ne[x_Plus*y__,n_,opt_]:=ne[Distribute[x*y],n,opt];
ne[x_,n_,opt_]:=(ni={};t=x//.VErule2;(
If[TrueQ[opt],VectorIndices=DeleteCases[VectorIndices,Alternatives[##]];Remove[##]])&@@
Cases[Union[ni],y_/;(FreeQ[t,y]&&MatchQ[y,VectorIndices])];t)


(* ::Subsubsection:: *)
(*SpEliminate*)


VErule3={HoldPattern[a_^(n_Integer)]:>If[n<0,Contract[SpEliminate[a^(-n)]]^(-1),SpEliminate[a]*SpEliminate[a^(n-1)]],sp[x_?VecQ,y_?VecQ]:>ReleaseHold[Hold[MetricTensor[{},{x1,x2}]*SupIndex[x,x1]*SupIndex[y,x2]]/.newvi[x1,x2]],x:(VGrad[v_,{i_?VecIndQ,j___},k_List][r_?VecQ]):>ReleaseHold[Hold[MetricTensor[{x1,i},{}]*LowerIndex[x,i->x1]]/.newvi[x1]],
x:(SubIndex[(v_?VecQ),i_?VecIndQ]):>ReleaseHold[Hold[MetricTensor[{},{x1,i}]*UpperIndex[x,i->x1]]/.newvi[x1]]}
SpEliminate[x_]:=x//.VErule3


SpEliminate[x_[y___]]:=SpEliminate/@x[y];
SpEliminate[x_]:=x;
SpEliminate[HoldPattern[a_^(n_Integer)]]:=If[n<0,VIContract[SpEliminate[a^(-n)]]^(-1),SpEliminate[a]*SpEliminate[a^(n-1)]];
SpEliminate[HoldPattern[sp[x_?VecQ,y_?VecQ]]]:=ReleaseHold[Hold[MetricTensor[{},{x1,x2}]*SupIndex[SpEliminate[x],x1]*SupIndex[SpEliminate[y],x2]]/.newvi[x1,x2]];
SpEliminate[HoldPattern[x:(VGrad[v_,{i_?VecIndQ,j___},k_List][r_?VecQ])]]:=ReleaseHold[Hold[MetricTensor[{x1,i},{}]*SpEliminate[LowerIndex[x,i->x1]]]/.newvi[x1]];
SpEliminate[HoldPattern[SubIndex[(v_?VecQ),i_?VecIndQ]]]:=ReleaseHold[Hold[MetricTensor[{},{x1,i}]*SupIndex[SpEliminate[v],x1]]/.newvi[x1]];


(* ::Subsection:: *)
(*Differentiation*)


(* ::Subsubsection:: *)
(*VGrad*)


MakeBoxes[VGrad[v_,u_,d_],StandardForm]:=rbn[v,u,d]


VGrad[VGrad[h_,a1_List,a2_List],b1_List,b2_List]:=VGrad[h,Sort[Join[a1,b1]],Sort[Join[a2,b2]]]
(*VGrad/:VGrad[V_,{},{a_}][x_?VecQ]:=(Declare[#,Vector];Partial[V[#],SupIndex[#,a]]/.#->x)&[Unique["Vectors`vec"]]/;Hold[V[x]]=!=Hold@@{V[x]}*)
VGrad/:VGrad[V_,a_List,b_List][x_?VecQ]:=(Declare[#,Vector];Partial[V[#],Sequence@@(Function[{c},SubIndex[#,c]]/@a),Sequence@@(Function[{c},SupIndex[#,c]]/@b)]/.#->x)&[Unique["Vectors`vec"]]/;Hold[V[x]]=!=Hold@@{V[x]}


TypeTable[VGrad[h_,a_List,b_List]]:={{{Vector,_}}->Module[{c = (TypeOf[h])}, If[MatchQ[c,Function[_Rule|_RuleDelayed]],c=Rule@@c[[1]];If[c == ({{Vector, _}} -> Number ),c= {{Vector, _}} -> TComponent[{}, {}, {}] ];If[MatchQ[c, {{Vector, _}} -> TComponent[_, _, _]],TComponent[Join[c[[2,1]], a], Join[c[[2,2]], b], c[[2,3]]], Badformed],Badformed]],{{Untyped,_}...}->Untyped,_List->Badformed}
VGrad/:HoldPattern[TypeX[gr:(VGrad[h_,a_List,b_List])]]:={Module[{c = (TypeOf[h])},If[MatchQ[c,Function[_Rule|_RuleDelayed]],c=Rule@@c[[1]];If[c == ({{Vector, _}} -> Number ),c= {{Vector, _}} ->TComponent[{}, {}, {}] ];If[MatchQ[c, {{Vector, _}} -> TComponent[_, _, _]],Function[Evaluate[{{Vector,_}}->TComponent[Join[c[[2,1]], a], Join[c[[2,2]], b], c[[2,3]]]]], Badformed],Badformed]],Unevaluated[gr]}


(* ::Subsubsection:: *)
(*Partial*)


Partial::"wrngarg"="`1` is not a correct form to differentiate over. Aborting...";


HoldPattern[Partial[x_,v1_,v2__]]:=Fold[Partial,x,{v1,v2}]
HoldPattern[Partial[x_List,v1__]]:=Partial[#,v1]&/@x


HoldPattern[Partial[x_,v_]/;!(MatchQ[v,(SupIndex|SubIndex)[_?VecVarQ,_?VecIndQ]])]:=(Message[Partial::"wrngarg",v];Abort[]);
(*Rules of the usual derivative*)
HoldPattern[Partial[x_Plus,v_]]:=(Partial[#,v]&/@x);
HoldPattern[Partial[x_,v_]]:=0/;FreeQ[x,v[[1]]];
HoldPattern[Partial[x_*y_,v_]]:=x*Partial[y,v]+y*Partial[x,v];
HoldPattern[Partial[Integrate[exp_,{x_,a_,b_}],v_]]:=(int[Partial[exp,v],{x,a,b}]/.{int->Integrate})+Partial[a,v]*(exp/.{x->a})-Partial[b,v]*(exp/.{x->b});HoldPattern[Partial[Integrate[exp_,{x_,a_,b_},c__List],v_]]:=(int[Partial[Integrate[exp,c],v],{x,a,b}]/.{int->Integrate})+Partial[a,v]*(Integrate[exp/.{x->a},c])-Partial[b,v]*(Integrate[exp/.{x->b},c]);
(*Rules which probably always hold for vectors*)
(*HoldPattern[Partial[a_,v_]]:=Partial[x,v]/;a=!=(x=VExpand[a]);(*Expand first*)*)
HoldPattern[Partial[sp[x_,y_],v_]]:=Partial[ReleaseHold[Hold[SupIndex[x,x1]*SubIndex[y,x1]]/.newvi[x1]],v]
Partial[x:(SupIndex|SubIndex)[v_,_],y:(SupIndex|SubIndex)[v_,_]]:=MetricTensor[Cases[#,SupIndex[_,k_]:>k],Cases[#,SubIndex[_,k_]:>k]]&[{x,y/.{SupIndex->SubIndex,SubIndex->SupIndex}}]
(*Some specific rules*)
HoldPattern[Partial[Power[x_,n_Integer],v_]]:=(n Power[x,n-1] Partial[x,v]);
HoldPattern[Partial[x:(y_[z__?NumQ])?NumQ,v_]]:=Sum[(Derivative@@ReplacePart[Table[0,{Length[{z}]}],1,i])[y][z]*Partial[{z}[[i]],v],{i,Length[{z}]}];
HoldPattern[Partial[x:(y_[z__?VecQ])?NumQ,v_]]:=ReleaseHold[Hold[VGrad[y,{},{x1}][z]*Partial[VExpand[SupIndex[z,x1]],v]]/.newvi[x1]];
HoldPattern[Partial[x:(y_[z__?VecQ])?TCompQ,v_]]:=ReleaseHold[Hold[VGrad[y,{},{x1}][z]*Partial[VExpand[SupIndex[z,x1]],v]]/.newvi[x1]];


(* ::Subsection:: *)
(*Tensor Structure*)


(* ::Subsubsection:: *)
(*DAverage*)


DAverage::invalid="Don't know how to average `1` over `2`. If the exression has undistributed  scalar products, you should try first LFDistribute[`1`,sp].";
DAverage[a_,p_List]:=Fold[DAverage,a,p];
DAverage[expr_,v_?VecVarQ]:=Module[
{lf,va,av,vav,res},
vav[]=1;
vav[x__]/;OddQ@Length@{x}=0;
vav[sp[v,x_],sp[v,y_]]:=(sp[v,v]sp[x,y])/MetricTensor[];
vav[(si:SubIndex|SupIndex)[v,x_],sp[v,y_]]:=(sp[v,v]si[y,x])/MetricTensor[];
vav[sp[v,y_],(si:SubIndex|SupIndex)[v,x_]]:=(sp[v,v]si[y,x])/MetricTensor[];
vav[SubIndex[v,x_],SubIndex[v,y_]]:=(sp[v,v]MetricTensor[{},{x,y}])/MetricTensor[];
vav[SupIndex[v,x_],SubIndex[v,y_]]:=(sp[v,v]MetricTensor[{x},{y}])/MetricTensor[];
vav[SubIndex[v,x_],SupIndex[v,y_]]:=(sp[v,v]MetricTensor[{y},{x}])/MetricTensor[];
vav[SupIndex[v,x_],SupIndex[v,y_]]:=(sp[v,v]MetricTensor[{x,y},{}])/MetricTensor[];
vav[x_,y__]:=MetricTensor[]/(MetricTensor[]+Length@{y}-1)*Sum[vav[x,{y}[[i]]]*vav@@Delete[{y},{i}],{i,Length@{y}}];
va[x__]:=vav@@Inner[Table[#1,{#2-1}]&,lf,{x},Join];
If[!And@@((MatchQ[Part[{expr},##]&@@MapAt[0&,#,{-1}],SupIndex|SubIndex|sp]&)/@Position[{expr},v]),
Message[DAverage::invalid,expr,v];Return[$Failed]];
lf=Union@Cases[{expr},(sp|SupIndex|SubIndex)[v,_]|sp[_,v],\[Infinity]]/.sp[v,v]->Sequence[];
If[lf=={},Return[expr]];
If[!PolynomialQ[expr,lf],Message[DAverage::invalid,expr,v];Return[$Failed]];
res=Plus@@Flatten[Array[va,Dimensions[#]]*#]&[CoefficientList[expr,lf]];
If[!FreeQ[res,_vav],Message[DAverage::invalid,expr,v];Return[$Failed]];
res
]


(* ::Subsubsection:: *)
(*VAverage*)


lpl[x_,p_]:=ReleaseHold[Hold[(Remove[x1];#)&@DummyEliminate[Expand[Partial[DummyEliminate[Partial[x,SupIndex[p,x1]]],SubIndex[p,x1]]]]]/.newvi[x1]]


VAverage[a_,p_List]:=Fold[VAverage,a,p];
VAverage[a_,p_]:=a/;FreeQ[a/.sp[p,p]->pp,p];
VAverage[a_,p_]:=0/;0===LFDistribute[a+(a/.p->-p)];
VAverage[a_Plus,p_]:=VAverage[#,p]&/@a;
VAverage[a_*b_,p_]:=a*VAverage[b,p]/;FreeQ[a/.sp[p,p]->pp,p];
VAverage[a_*b_Plus,p_]:=VAverage[Distribute[a*b],p];
VAverage[a_,p_]/;a=!=(c=LFDistribute[a,sp]):=VAverage[c,p];
VAverage[HoldPattern[a:Times[((sp[p_,_]|SubIndex[p_,_]|SupIndex[p_,_])^_.)..]],p_?VecVarQ]:=va[DeleteCases[FactorList[a],{1,1}],p];
VAverage[HoldPattern[a:((sp[p_,_]|SubIndex[p_,_]|SupIndex[p_,_])^_.)],p_?VecVarQ]:=va[DeleteCases[FactorList[a],{1,1}],p];


va[{},_]=1;
va[a:{a1_,an___},p_]:=1/(ReleaseHold[Hold[MetricTensor[{x1},{x1}]]/.newvi[x1]]+Plus@@Last/@a-2) (
If[Last@a1>=2,(Last@a1-1)*va1[First@a1,First@a1,p]*va[DeleteCases[{a1-{0,2},an},{_,0}],p],0]+
Sum[Last@a[[n]]*va1[First@a1,First@a[[n]],p]*va[DeleteCases[ReplacePart[{a1-{0,1},an},{n,2}->(Last@a[[n]]-1)],{_,0}],p],{n,2,Length[a]}])


va1[sp[p_,a_],sp[p_,b_],p_]:=sp[p,p] sp[a,b];
va1[sp[p_,a_],(x:SupIndex|SubIndex)[p_,b_],p_]:=sp[p,p] x[a,b];
va1[(x:SupIndex|SubIndex)[p_,b_],sp[p_,a_],p_]:=sp[p,p] x[a,b];
va1[x:(SupIndex|SubIndex)[p_,a_],y:(SupIndex|SubIndex)[p_,b_],p_]:=sp[p,p] MetricTensor[Cases[{x,y},SupIndex[p,z_]:>z],Cases[{x,y},SubIndex[p,z_]:>z]];


(*VAverage[y_,p_?VecVarQ]:=VAverage[y,p]=(VAnow=y;Factor[(Table[sp[p,p]^n/(4^n n! Pochhammer[1/2 MetricTensor[{x1},{x1}]/.newvi[x1],n]),{n,0,Length[#]-3}].Drop[#,-2])&[FixedPointList[lpl[#,p]&,y]/.{p->0}/.HoldPattern[Dot[___,0,___]]->0]])/;VPolyQ[y,p]*)


(* ::Subsubsection:: *)
(*TensorSet*)


TensorSet[{a__?VecVarQ},{}]={1};
TensorSet[{a__?VecVarQ},{\[Mu]_?VecIndQ}]:=SupIndex[#,\[Mu]]&/@{a}
TensorSet[{a__?VecVarQ},{\[Mu]_?VecIndQ,\[Nu]__?VecIndQ}]:=Join[Flatten[Table[MetricTensor[{\[Mu],{\[Nu]}[[i]]},{}]*TensorSet[{a},Delete[{\[Nu]},i]],{i,1,Length[{\[Nu]}]}]],Flatten[Outer[Times,TensorSet[{a},{\[Mu]}],TensorSet[{a},{\[Nu]}]]]]


(* ::Subsubsection:: *)
(*TSCollect*)


TSCollect[expr_,f_:Identity]:=Dot@@Transpose[Append[Reap[ccc[expr,1],_,{#1,f[Plus@@#2]}&][[2]],{0,0}]];
ccc[x_Plus,y_]:=ccc[#,y]&/@x;
ccc[x_?(FreeQ[#,_?TCompQ]&)*y_,z_]:=ccc[y,x*z];
ccc[x_Plus*y__,z_]:=ccc[Distribute[x*y],z];
ccc[x_,y_]:=(Sow[y,x]);


GramMatrix[v__]=Outer[sp,{v},{v}];


(* ::Subsubsection:: *)
(*GramDeterminant*)


GramDeterminant[v__]:=Det[Outer[sp,{v},{v}]]


(* ::Subsection:: *)
(*End[`Private`]*)


End[];


(* ::Subsection:: *)
(*Protect public names*)


Protect[VecQ,VecVarQ,VecVarsMark];


(* ::Subsection:: *)
(*EndPackage[Vectors`]*)


EndPackage[];

(* ::Package:: *)

(* ::Title:: *)
(*Linear functions*)


(Unprotect[#];Remove[#])&/@((#<>"*")&/@Contexts["LinearFunctions`*"]);


Needs["Numbers`","RNL`Numbers`"];
Needs["Types`","RNL`Types`"];
BeginPackage["LinearFunctions`",{"Numbers`","Types`"}];
Off[General::"spell1"];


LFRules::usage="LFRules is a list of rules used by LFDistribute";
LFDistribute::usage="LFDistribute[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] applies LFRules to \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\n
LFDistribute[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptP]\[ScriptA]\[ScriptT]] applies rules from LFRules containing \[ScriptP]\[ScriptA]\[ScriptT] to \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]";
LFNumsOut::usage="LFNumsOut[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR]] applies LFRules containing NumQ to \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\n
LFNumsOut[\[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR],\[ScriptP]\[ScriptA]\[ScriptT]] applies rules from LFRules containing NumQ and \[ScriptP]\[ScriptA]\[ScriptT] to \[ScriptE]\[ScriptX]\[ScriptP]\[ScriptR].\n
Basically, this function factorizes numbers in linear function";
SetLinear::usage="SetLinear[\[ScriptF],All] adds to LFRules distributive and \"numbers-out\" rules for function \[ScriptF] with respect to all its arguments\n
SetLinear[\[ScriptF],\[ScriptN]] adds to LFRules distributive and \"numbers-out\" rules for function \[ScriptF] with respect to its \[ScriptN]th argument\n
SetLinear[\[ScriptF],{\[ScriptN],\[ScriptM],..}] adds to LFRules distributive and \"numbers-out\" rules for function \[ScriptF] with respect to its \[ScriptN]th,\[ScriptM]th,.. arguments\n
SetLinear[\[ScriptF],\[ScriptF]\[ScriptO]\[ScriptR]\[ScriptM],\[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT]] adds to LFRules the rules corresponding to SetLinear[\[ScriptF],\[ScriptF]\[ScriptO]\[ScriptR]\[ScriptM]] with the restriction on the arguments which pass the \[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT]";
SetDistributive::usage="SetDistributive[\[ScriptF],All] adds to LFRules distributive rules for function \[ScriptF] with respect to all its arguments\n
SetDistributive[\[ScriptF],\[ScriptN]] adds to LFRules distributive rules for function \[ScriptF] with respect to its \[ScriptN]th argument\n
SetDistributive[\[ScriptF],{\[ScriptN],\[ScriptM],..}] adds to LFRules distributive rules for function \[ScriptF] with respect to its \[ScriptN]th,\[ScriptM]th,.. arguments\n
SetDistributive[\[ScriptF],\[ScriptF]\[ScriptO]\[ScriptR]\[ScriptM],\[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT]] adds to LFRules the rules corresponding to SetDistributive[\[ScriptF],\[ScriptF]\[ScriptO]\[ScriptR]\[ScriptM]] with the restriction on the arguments which pass the \[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT]";
SetLinearIn::usage="SetLinearIn[\[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT],\[ScriptF],..] does the same as SetLinear replacing NumQ by \[ScriptT]\[ScriptE]\[ScriptS]\[ScriptT]";
LinearRule::usage="Same syntax as in SetLinear, gives the rules which will be added to LFRules if using SetLinear.";
DistributiveRule::usage="Same syntax as in SetDistributive, gives the rules which will be added to LFRules if using SetDistributive.";
LinearFunctions::usage=
  "LinearFunctions package is written by Roman Lee. It introduces the function LFDistribute and some usefull tools "<>
    ToString[TableForm[
        Partition[Join[Names["LinearFunctions`*"],{"","","","","",""}],5],
        TableSpacing->{0,3}]];


Begin["`Private`"]


LFRules={};


LinearRule[f_,n_?(IntegerQ[#]&&Positive[#]&)]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;Length[{a}]==n-1,HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(Length[{a}]==n-1&&b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]/;Length[{a}]==n-1};
LinearRule[f_,n:{__?(IntegerQ[#]&&Positive[#]&)}]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;MemberQ[n-1,Length[{a}]],HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(MemberQ[n-1,Length[{a}]]&&b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]/;MemberQ[n-1,Length[{a}]]};
LinearRule[f_,All]:={HoldPattern[f[a___,b_Plus,c___]]:>Distribute[f[a,b,c]],HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]};
LinearRule[f_,n_?(IntegerQ[#]&&Positive[#]&),test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;(Length[{a}]==n-1&&test[b]),HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(Length[{a}]==n-1&&test[b]&&b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]/;(Length[{a}]==n-1&&test[b*d])};
LinearRule[f_,n:{__?(IntegerQ[#]&&Positive[#]&)},test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;(MemberQ[n-1,Length[{a}]]&&test[b]),HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(MemberQ[n-1,Length[{a}]]&&test[b]&&b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]/;(MemberQ[n-1,Length[{a}]]&&test[b*d])};
LinearRule[f_,All,test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;test[b],HoldPattern[f[a___,b_?NumQ,c___]]:>b*f[a,1,c]/;(test[b]&&b=!=1),HoldPattern[f[a___,b_?NumQ d_,c___]]:>b*f[a,d,c]/;test[b*d]};


DistributiveRule[f_,n_?(IntegerQ[#]&&Positive[#]&)]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;Length[{a}]==n-1};
DistributiveRule[f_,n:{__?(IntegerQ[#]&&Positive[#]&)}]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;MemberQ[n-1,Length[{a}]]};
DistributiveRule[f_,All]:={HoldPattern[f[a___,b_Plus,c___]]:>Distribute[f[a,b,c]]};
DistributiveRule[f_,n_?(IntegerQ[#]&&Positive[#]&),test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;(Length[{a}]==n-1&&test[b])};
DistributiveRule[f_,n:{__?(IntegerQ[#]&&Positive[#]&)},test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;(MemberQ[n-1,Length[{a}]]&&test[b])};
DistributiveRule[f_,All,test_]:={HoldPattern[f[a___,b_Plus,c___]]:>ReleaseHold[Hold[f[a,#,c]]&/@b]/;test[b]};


SetLinear[args__]:=(LFRules=Union[LFRules,LinearRule[args]])
SetLinearIn[test_,args__]:=(LFRules=Union[LFRules,LinearRule[args]/.{NumQ->test}]);
SetDistributive[args__]:=(LFRules=Union[LFRules,DistributiveRule[args]])


SetDistributive[Times,All]


LFDistribute[exp_]:=exp//.LFRules
LFDistribute[exp_,pat_]:=exp//.Select[LFRules,!FreeQ[#,pat]&]


LFNumsOut[exp_]:=LFDistribute[exp,NumQ]
LFNumsOut[exp_,pat_]:=LFDistribute[exp,x_/;!FreeQ[x,pat]&&!FreeQ[x,NumQ]]


End[];


EndPackage[];

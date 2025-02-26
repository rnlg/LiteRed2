(* ::Package:: *)

(Unprotect[#];Remove[#])&/@((#<>"*")&/@Contexts["Numbers`*"]);
Needs["Types`","RNL`Types`"];
BeginPackage["Numbers`",{"Types`"}];
Off[General::"spell1"];

NumQ::usage="NumQ[x] gives True if x is of type Number";
NumVarQ::usage="NumVarQ[x] gives True if x is a variable of type Number";
NumSymQ::usage="NumSymQ[x] gives True if x is  a variable or numeric of type Number";\

NumVarsMark::usage="NumVarsMark[expr] marks all Numeric variables in expr with the tag Num";\

NumSymsMark::usage="NumVarsMark[expr] marks all Numeric variables and explicit numeric expressions in expr with the tag Num";\

Number::usage="Number is a type of all numbers";
NumFuncs::usage="NumFuncs is a List of numeric functions"
Numbers::usage=
    "Numbers package is written by Roman Lee. It makes some definitions for the elementary functions and contains some useful tools\n"<>
      ToString[TableForm[
          Partition[Join[Names["Numbers`*"],{"","","","","",""}],5],
          TableSpacing->{0,3}]];

Begin["`Private`"]





NumFuncs1={Abs,AiryAi,AiryAiPrime,AiryBi,AiryBiPrime,ArcCos,ArcCosh,ArcCot,
      ArcCoth,ArcCsc,ArcCsch,ArcSec,ArcSech,ArcSin,ArcSinh,ArcTan,ArcTanh,Arg,
      Ceiling,Clip,Conjugate,Cos,Cosh,CoshIntegral,CosIntegral,Cot,Coth,Csc,
      Csch,DedekindEta,EllipticE,EllipticK,EllipticNomeQ,Erf,Erfc,Erfi,Exp,
      ExpIntegralEi,Factorial,Factorial2,Fibonacci,Floor,FractionalPart,
      FresnelC,FresnelS,Gamma,Im,IntegerPart,InverseEllipticNomeQ,
      KleinInvariantJ,Log,LogGamma,LogIntegral,Minus,ModularLambda,Re,
      RiemannSiegelTheta,RiemannSiegelZ,Round,Sec,Sech,Sign,Sin,Sinh,
      SinhIntegral,SinIntegral,Sqrt,Tan,Tanh,Zeta};
NumFuncs2={ArcTan,ArithmeticGeometricMean,BesselI,BesselJ,BesselK,BesselY,
      Beta,Binomial,ChebyshevT,ChebyshevU,Divide,EllipticE,EllipticF,
      EllipticPi,Erf,ExpIntegralE,Fibonacci,Gamma,GammaRegularized,
      GegenbauerC,HermiteH,Hypergeometric0F1,Hypergeometric0F1Regularized,
      InverseJacobiCD,InverseJacobiCN,InverseJacobiCS,InverseJacobiDC,
      InverseJacobiDN,InverseJacobiDS,InverseJacobiNC,InverseJacobiND,
      InverseJacobiNS,InverseJacobiSC,InverseJacobiSD,InverseJacobiSN,
      JacobiAmplitude,JacobiCD,JacobiCN,JacobiCS,JacobiDC,JacobiDN,JacobiDS,
      JacobiNC,JacobiND,JacobiNS,JacobiSC,JacobiSD,JacobiSN,JacobiZeta,
      LaguerreL,LegendreP,LegendreQ,Log,MathieuCharacteristicA,
      MathieuCharacteristicB,MathieuCharacteristicExponent,Mod,NevilleThetaC,
      NevilleThetaD,NevilleThetaN,NevilleThetaS,Pochhammer,PolyLog,Quotient,
      Subtract,Zeta};
NumFuncs3={Beta,BetaRegularized,EllipticPi,Gamma,GammaRegularized,GegenbauerC,
      Hypergeometric1F1,Hypergeometric1F1Regularized,HypergeometricU,
      LaguerreL,LegendreP,LegendreQ,LerchPhi,MathieuC,MathieuCharacteristicA,
      MathieuCharacteristicB,MathieuCharacteristicExponent,MathieuCPrime,
      MathieuS,MathieuSPrime,Mod,PolyLog,Quotient};
NumFuncs4={Beta,BetaRegularized,Hypergeometric2F1,
      Hypergeometric2F1Regularized,JacobiP,SphericalHarmonicY};
NumFuncs0={Max,Min,Multinomial,Plus,Power,Times,UnitStep};

DownValues[TypeX]=
    Insert[DownValues[TypeX],
      HoldPattern[TypeX[x_?NumericQ]]:>{Number,Unevaluated[x]},-3];

Block[{TypesBelow},Clear[TypesBelow];
  (Unprotect[#];
        Declare[#,({{TypesBelow[Number],_}...}:>Number)&];
        Protect[#])&/@Union[NumFuncs1,NumFuncs2,NumFuncs3,NumFuncs4,NumFuncs0];
]


NumFuncs=Union[NumFuncs1,NumFuncs2,NumFuncs3,NumFuncs4,NumFuncs0];



NumQ[x_]:=TypeBelowQ[Unevaluated[x],Number]
NumVarQ[x_]:=
  AtomQ[Unevaluated[x]]&&(!NumericQ[Unevaluated[x]])&&ExpressionType[x]===Number
NumSymQ[x_]:=(AtomQ[Unevaluated[x]]&&ExpressionType[Unevaluated[x]]===Number)||
    NumericQ[Unevaluated[x]]

NumVarsMark[x_]:=x/. a_?NumVarQ:>Number[a]
NumVarsMark[x_,pat_]:=
  x/. a:(Alternatives[pat]?NumVarQ):>Number[a]
NumSymsMark[x_]:=x/. a_?NumSymQ:>Number[a]
NumSymsMark[x_,pat_]:=x/. a:(Alternatives[pat]?NumSymQ):>Number[a]

End[];

Protect[NumQ,NumVarQ,NumSymQ,NumVarsMark,NumSymsMark,Number,NumFuncs]

EndPackage[];

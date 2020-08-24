(* ::Package:: *)

SetOptions[GraphPlot,MultiedgeStyle->0.3,VertexRenderingFunction->({If[#2>0,Black,Blue],Disk[#1,0.03]}&),
EdgeRenderingFunction->(Replace[#3,{

"\[Infinity]"->{Thickness[0.015],Black,Line[#1]},
"-\[Infinity]"->{Thickness[0.015],Red,Line[#1]},

"1"->{Thickness[0.005],Black,Line[#1]},
"-1"->{Thickness[0.005],Red,Line[#1]},
"0"->{Thickness[0.005],Black,Dashed,Line[#1]},
"-0"->{Thickness[0.005],Red,Dashed,Line[#1]},

(*with dots*)
{"\[Infinity]",k_}:>{Thickness[0.015],Black,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},
{"-\[Infinity]",k_}:>{Thickness[0.015],Red,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},

{"1",k_}:>{Thickness[0.005],Black,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},
{"-1",k_}:>{Thickness[0.005],Red,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},

{"0",k_}:>{Thickness[0.005],Black,Dashed,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},
{"-0",k_}:>{Thickness[0.005],Red,Dashed,Arrowheads[Table[{0.008,i/(k+1),Graphics[{Black,Disk[]}]},{i,k}]],Arrow[#1]},

"p"->{Thickness[0.005],Blue,Arrow[#1]},
"Z"->{Thickness[0.015],Blue,Arrow[#1]}
}]&)];

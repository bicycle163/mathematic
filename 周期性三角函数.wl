(* ::Package:: *)

f[x_]=ArcSin[Sin[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]


Clear["Global`*"]
f[x_]=ArcCos[Sin[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]


Clear["Global`*"]
f[x_]=ArcCos[Cos[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]


Clear["Global`*"]
f[x_]=ArcTan[Sin[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]


Clear["Global`*"]
f[x_]=ArcTan[Tan[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]


f[x_]=2/Pi*ArcSin[Sin[x]];
Plot[{f[x]},{x,-2Pi,2 Pi}]

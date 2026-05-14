(* ::Package:: *)

Clear["Global`*"]

f[t_]:=2/Pi ArcSin[Sin[t]]

S[n_,t_]:=8/Pi^2 Sum[(-1)^k Sin[(2 k+1) t]/(2 k+1)^2,{k,0,(n-1)/2}](*\:9636\:6570\:4e3a2k+1\:ff0c\:4ee4n=2k+1\:ff0c\:6240\:4ee5k=\:ff08n-1\:ff09/2\:ff0c\:6c42\:51fa\:4e0a\:9650*)

Plot[{f[t],S[3,t],S[5,t],S[7,t]},{t,-2 Pi,2 Pi},PlotRange->{-2,2},PlotLegends->{"\:539f\:51fd\:6570","3\:9636\:5c55\:5f00","5\:9636\:5c55\:5f00","7\:9636\:5c55\:5f00"},AxesLabel->{"t","f(t)"}]

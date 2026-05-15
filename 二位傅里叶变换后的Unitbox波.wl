(* ::Package:: *)

Clear["Global`*"]
f[x_,y_]=UnitBox[x] UnitBox[y];
g[wx_,wy_]=FourierTransform[f[x,y],{x,y},{wx,wy}];
Plot3D[Evaluate[g[wx,wy]],{wx,-20,20},{wy,-20,20},PlotRange->All]


ContourPlot[Evaluate[g[wx,wy]],{wx,-20,20},{wy,-20,20},Contours->30,PlotLegends->Automatic]

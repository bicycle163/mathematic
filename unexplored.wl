(* ::Package:: *)

(* Extracted from: \:672a\:63a2\:7d22.nb *)


(* ===== Input Cell 1 ===== *)

ClearAll["Global`*"]
z1=1+I Sqrt[3];
{Re[z1],Im[z1],Abs[z1],Arg[z1]}
Assuming[0<=\[Alpha]<2 Pi,z2=1-Cos[\[Alpha]]+I Sin[\[Alpha]];
{ComplexExpand[Re[z2]],ComplexExpand[Im[z2]],FullSimplify[Abs[z2]],FullSimplify[Arg[z2]]}]
Assuming[Element[x,Reals],z3=Exp[I Sin[x]];
{ComplexExpand[Re[z3]],ComplexExpand[Im[z3]],FullSimplify[Abs[z3]],FullSimplify[Arg[z3]]}]



(* ===== Input Cell 2 ===== *)

ClearAll["Global`*"]

sol1=z/.Solve[z^2+1==0,z]
pts1=({Re[#],Im[#]}&)/@sol1

ListPlot[pts1,AxesLabel->{"Re","Im"},PlotStyle->{Red,PointSize[0.03]},GridLines->Automatic,PlotRange->All,AspectRatio->1,PlotLabel->"Solutions of z^2 + 1 = 0"]



(* ===== Input Cell 3 ===== *)

sol2=z/.Solve[z^3+8==0,z]
pts2=({Re[#],Im[#]}&)/@sol2

ListPlot[pts2,AxesLabel->{"Re","Im"},PlotStyle->{Blue,PointSize[0.03]},GridLines->Automatic,PlotRange->All,AspectRatio->1,PlotLabel->"Solutions of z^3 + 8 = 0"]



(* ===== Input Cell 4 ===== *)

ClearAll["Global`*"]

T0=1;
\[Omega]0=200 Pi;

U[t_]:=Exp[-t^2/T0^2+I \[Omega]0 t];

(*\:5f3a\:5ea6*)
Plot[Abs[U[t]]^2,{t,-2,2},AxesLabel->{"t","|U(t)|^2"},PlotLabel->"Intensity of Gaussian Wave",PlotRange->All]

(*\:76f8\:4f4d*)
Plot[Arg[U[t]],{t,-2,2},AxesLabel->{"t","Arg[U(t)]"},PlotLabel->"Phase of Gaussian Wave",PlotRange->All]



(* ===== Input Cell 5 ===== *)

ClearAll["Global`*"]

tri[t_]:=TriangleWave[2 t/Pi]

Plot[tri[t],{t,-20,20},AxesLabel->{"t","f(t)"},PlotLabel->"Triangular Wave (Period = Pi)",PlotRange->All]



(* ===== Input Cell 6 ===== *)

ClearAll["Global`*"]

a=1;
U[t_]:=Exp[-a Abs[t]];

Plot[U[t],{t,-10,10},AxesLabel->{"t","U(t)"},PlotLabel->"Decay Wave U(t)=Exp[-a|t|]",PlotRange->All]



(* ===== Input Cell 7 ===== *)

ClearAll["Global`*"]

rect[t_]:=Piecewise[{{1,Abs[t]<=2}},0]

Plot[rect[t],{t,-6,6},AxesLabel->{"t","f(t)"},PlotLabel->"Rectangular Pulse (Width = 4)",PlotRange->{-0.2,1.2}]



(* ===== Input Cell 8 ===== *)

ClearAll["Global`*"]

Integrate[Exp[I t]/Exp[I t]*I Exp[I t],{t,0,2 Pi}]



(* ===== Input Cell 9 ===== *)

ClearAll["Global`*"]

z[t_]:=Exp[I t];
Integrate[(1/Abs[z[t]]) z'[t],{t,0,2 Pi}]



(* ===== Input Cell 10 ===== *)

ClearAll["Global`*"]

f[z_]:=z Abs[z]^2

(*\:5199\:6210 x,y \:5f62\:5f0f*)
u[x_,y_]=ComplexExpand[Re[f[x+I y]]]
v[x_,y_]=ComplexExpand[Im[f[x+I y]]]

(*Cauchy-Riemann \:6761\:4ef6*)
cr1=FullSimplify[D[u[x,y],x]==D[v[x,y],y]]
cr2=FullSimplify[D[u[x,y],y]==-D[v[x,y],x]]

(*\:8054\:7acb\:6c42\:89e3*)
Solve[{D[u[x,y],x]==D[v[x,y],y],D[u[x,y],y]==-D[v[x,y],x]},{x,y},Reals]



(* ===== Input Cell 11 ===== *)

Print["f(z)=z|z|^2 \:4ec5\:5728 z=0 \:5904\:6ee1\:8db3 C-R \:6761\:4ef6\:ff0c\:56e0\:6b64\:4ec5\:5728 z=0 \:5904\:53ef\:5bfc\:ff1b\:5728\:4efb\:4f55\:5f00\:533a\:57df\:5185\:90fd\:4e0d\:89e3\:6790\:3002"]



(* ===== Input Cell 12 ===== *)

ClearAll["Global`*"]

expr=((Conjugate[z] z+2 z-Conjugate[z]-2)/(z^2-1));

exprXY=ComplexExpand[expr/.z->x+I y];

Limit[exprXY,{x,y}->{1,0}]



(* ===== Input Cell 13 ===== *)

ClearAll["Global`*"]

an[n_]:=1/n^3;
R1=1/Limit[Abs[an[n]]^(1/n),n->Infinity]



(* ===== Input Cell 14 ===== *)

ClearAll["Global`*"]

bn[n_]:=1/n!;
R2=Infinity



(* ===== Input Cell 15 ===== *)

ClearAll["Global`*"]

Series[Exp[z] Sin[z],{z,0,10}]
Normal[%]



(* ===== Input Cell 16 ===== *)

Series[Log[z+2],{z,0,10}]
Normal[%]



(* ===== Input Cell 17 ===== *)

Series[1/(1+z^2)^2,{z,0,10}]
Normal[%]



(* ===== Input Cell 18 ===== *)

SetOptions[FourierTransform,FourierParameters->{0,1}];
SetOptions[InverseFourierTransform,FourierParameters->{0,1}];



(* ===== Input Cell 19 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

FourierTransform[Sin[t]^2,t,\[Omega]]



(* ===== Input Cell 20 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

f[t_]:=1/2 (DiracDelta[t+a]+DiracDelta[t-a]+DiracDelta[t+a/2]+DiracDelta[t-a/2]);

FourierTransform[f[t],t,\[Omega]]



(* ===== Input Cell 21 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

f[t_]:=DiracDelta[t-1] (t-2)^2 Cos[t];

FourierTransform[f[t],t,\[Omega]]



(* ===== Input Cell 22 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

FourierTransform[Sign[t],t,\[Omega]]



(* ===== Input Cell 23 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

Assuming[\[Beta]>0,FourierTransform[Exp[-\[Beta] Abs[t]],t,\[Omega]]]



(* ===== Input Cell 24 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

FourierTransform[UnitStep[t] Cos[3 t],t,\[Omega]]



(* ===== Input Cell 25 ===== *)

ClearAll["Global`*"]
SetOptions[InverseFourierTransform,FourierParameters->{0,1}];

InverseFourierTransform[1/(2+I \[Omega]),\[Omega],t]



(* ===== Input Cell 26 ===== *)

ClearAll["Global`*"]
SetOptions[InverseFourierTransform,FourierParameters->{0,1}];

InverseFourierTransform[(2 Sin[3 (\[Omega]-2 Pi)])/(\[Omega]-2 Pi),\[Omega],t]



(* ===== Input Cell 27 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

f[t_,b_]:=Piecewise[{{2,Abs[t]<=b}},0]

F[\[Omega]_,b_]=FullSimplify[FourierTransform[f[t,b],t,\[Omega]]]



(* ===== Input Cell 28 ===== *)

Plot[Evaluate[Abs[F[\[Omega],1]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","|F(\[Omega])|"},PlotLabel->"Magnitude Spectrum (b = 1)",PlotRange->All]



(* ===== Input Cell 29 ===== *)

Plot[Evaluate[Arg[F[\[Omega],1]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","Arg[F(\[Omega])]"},PlotLabel->"Phase Spectrum (b = 1)",PlotRange->All]



(* ===== Input Cell 30 ===== *)

Plot[Evaluate[Abs[F[\[Omega],3]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","|F(\[Omega])|"},PlotLabel->"Magnitude Spectrum (b = 3)",PlotRange->All]



(* ===== Input Cell 32 ===== *)

Plot[Evaluate[Arg[F[\[Omega],3]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","Arg[F(\[Omega])]"},PlotLabel->"Phase Spectrum (b = 3)",PlotRange->All]



(* ===== Input Cell 33 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

b=1;
\[Omega]d=4;

g[t_]:=f[t,b] Exp[I \[Omega]d t];
G[\[Omega]_]=FullSimplify[FourierTransform[g[t],t,\[Omega]]]

Plot[Evaluate[Abs[G[\[Omega]]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","|G(\[Omega])|"},PlotLabel->"Magnitude Spectrum of f(t) Exp[i \[Omega]d t]",PlotRange->All]



(* ===== Input Cell 34 ===== *)

Plot[Evaluate[Arg[G[\[Omega]]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","Arg[G(\[Omega])]"},PlotLabel->"Phase Spectrum of f(t) Exp[i \[Omega]d t]",PlotRange->All]



(* ===== Input Cell 35 ===== *)

ClearAll["Global`*"]
SetOptions[FourierTransform,FourierParameters->{0,1}];

b=1;
C=1;

h[t_]:=f[t,b] Exp[I C t^2];

H[\[Omega]_]=FourierTransform[h[t],t,\[Omega]];

Plot[Evaluate[Abs[H[\[Omega]]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","|H(\[Omega])|"},PlotLabel->"Magnitude Spectrum of f(t) Exp[i C t^2]",PlotRange->All]



(* ===== Input Cell 36 ===== *)

Plot[Evaluate[Arg[H[\[Omega]]]],{\[Omega],-20,20},AxesLabel->{"\[Omega]","Arg[H(\[Omega])]"},PlotLabel->"Phase Spectrum of f(t) Exp[i C t^2]",PlotRange->All]


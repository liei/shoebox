(* ::Package:: *)

normalize[lst_] := lst/Total[lst]; 

forward[f_,A_,Bs_] := FoldList[#1.Transpose[A].#2 &,f,Bs];

scaledForward[f_,A_,Bs_] := Transpose[FoldList[{normalize[#],Total[#]}&[#2.Transpose[A].#1[[1]]] &,{Bs[[1]].f,Total[Bs[[1]].f]},Bs[[2;;-1]]]];

backward[b_,A_,Bs_] := Reverse[FoldList[A.#2.#1 &,b,Reverse[Bs]]];

scaledBackward[b_,A_,Bs_,l_] := Reverse[FoldList[A.#2[[1]].#1 / #2[[2]] &,b / l[[-1]],Reverse[Thread[{Bs,l[[1;;-2]]}]]]];

likelihood[f_,A_,Bs_] :=  Fold[#1 + Log[Total[#2]] &,0,forward[f,A,Bs]]

forwardBackward[fInit_,A_,Bs_] := Module[{f,b,likelihood},
	f = forward[fInit,A,Bs];
	b = normalize /@ backward[Table[1,{Length[fInit]}],A,Bs];
	likelihood = Fold[#1 + Log[Total[#2]] &,0,f];
	f normalize /@ f;
	Return[{f,b,likelihood}];
	(*Return[normalize[Times @@ #] & /@ Transpose[{f,b}]];*)
];

scaledForwardBackward[fInit_,A_,Bs_] := Module[{f,l,b,likelihood},
	{f,l} = scaledForward[fInit,A,Bs];
	b = scaledBackward[Table[1,{Length[fInit]}],A,Bs,l];
	Return[{f,b}];
];




(* ::Input:: *)
(*A  = {{0.7,0.3},{0.3,0.7}};*)
(*U  = {{0.9,0.0},{0.0,0.2}};*)
(*NU = {{0.1,0.0},{0.0,0.8}};*)
(*Bs = {U,U,NU,U,U};*)
(**)


findCalcB[mu_, sigma_] := Function[frame,DiagonalMatrix[Table[PDF[MultinormalDistribution[mu[[i]],sigma[[i]]],frame],{i,1,Length[mu]}]]];

newModel[{A_,mu_,sigma_}] := Module[{calcB},
	calcB = findCalcB[mu,sigma];
	Return[Function[frames,(forward[Table[0.5,Length[A]],A,calcB /@ frames])[[2]]]];
];

addModel[classifier_,word_,model_] := Append[classifier,{word,model}];

classify[classifier_,frames_] := Ordering[{#[[1]], #[[2]][frames]} & /@ classifier,1,#1[[2]] > #2[[2]] &];

emInit[Os_, N_] := Module[{A,prior, mu, sigma,D,mean},
	D = Length[Os[[1]]];
	(* Generate a random prior *)
	prior = normalize[RandomReal[{0.01, 0.99},N]];
	(* Generate a random transition matrix A *)
	A = normalize /@ (RandomReal[{0.01, 0.99}, {N,N}]);
	(* mu, for each state *)
	mu = Table[RandomReal[{0.01,0.99}] * Mean[#] & /@ Transpose[Os],{N}];
	(* sigma, for each state *)
	sigma = Table[#.Transpose[#] &[RandomReal[{0.01,0.99},{D,D}]],{N}];
	Return[{prior,A,mu,sigma,Os}];
];

emStep[{prior_,A_,mu_,sigma_,Os_}] := Module[{T,S,F,r,f,t,muj,sum,\[Gamma],\[Xi],likelihood,gamma,xi,Aden,Anum,i,j,t,k,l,calcB,newPi,newA,newMu,newSigma},
	(* Do step *) 
	T = Length[Os];
	S = Length[A];
	F = Length[Os[[1]]];
	
	calcB = findCalcB[mu,sigma];
	{f,r} = scaledForwardBackward[prior,A,calcB /@ Os];
	\[Gamma] = normalize[Times @@ #] & /@ Transpose[{f,r}];

	newMu = Table[
		Total[Table[(\[Gamma][[t,j]] * Os[[t]]),{t,T}]] / Total[Table[\[Gamma][[t,j]],{t,T}]],
		{j,S}
	];

	newSigma = Table[
		Total[Table[\[Gamma][[t,j]] * Transpose[{Os[[t]] - newMu[[j]]}].{Os[[t]] - newMu[[j]]},{t,T}]] / Total[Table[\[Gamma][[t,j]],{t,T}]],
		{j,S}
	];

	newPi = \[Gamma][[1]];
	
	\[Xi] = Table[
		sum = Total[Table[f[[k]] * A[[k,l]] * calcB[[Os[[t]]][[l,l]]] * r[[t,l]] ,{l,S},{k,S}]];
		Table[	
			(f[[t,i]] * A[[i,j]] * calcB[Os[[t]]][[j,j]] * r[[t+1,j]]) / sum
			{i,1,Length[A]},
			{j,1,Length[A]}		
		],
		{t,1,Length[Os] - 1}
	];

	newA = Table[
		Total[Table[\[Xi][[t,i,j]],{t,T}]] / Total[Table[\[Gamma][[t,j]],{t,T}]];
		{i,S},
		{j,S}
	];

	Return[{newPi,newA,newMu,newSigma,Os}];
];

baumWelch[frames_,N_] := Module[{pi,A,mu,sigma,diff,lastLikelihood,test},
	diff = 0.00001;
	lastLikelihood = 0;
	test[{pi_,A_,mu_,sigma_,Os_}] := Module[{logLike,continue},
		logLike = likelihood[pi,A,findCalcB[mu,sigma] /@ Os];
		continue = (logLike - lastLikelihood) < diff; 		
		lastLikelihood = logLike;
		Return[continue];
	];
	{pi,A,mu,sigma,frames} = Nest[emStep,{emInit[frames,N]},test];
	Return[{A,mu,sigma}];
];
	


(* ::Input:: *)
(*-*)


(* ::Input:: *)
(*Os = {{3,2},{3,4},{2,3}}*)


(* ::Input:: *)
(*{{3,2},{3,4,5},{2,3,4}}*)


(* ::Input:: *)
(*emInit[Os,3]*)


(* ::Input:: *)
(*emStep[%]*)


(* ::InheritFromParent:: *)
(**)


(* ::Input:: *)
(*Transpose[{{a,b}}].{{x,y}}*)

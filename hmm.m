(* ::Package:: *)

normalize[lst_] := lst/Total[lst]; 

forward[f_,A_,Bs_] := FoldList[normalize[#1.Transpose[A].#2] &,f,Bs];

backward[b_,A_,Bs_] := Reverse[FoldList[normalize[A.#2.#1] &,b,Bs]];

forwardBackward[fInit_,A_,Bs_] := Module[{f,b},
	f = forward[fInit,A,Bs];
	b = backward[Table[1,{Length[fInit]}],A,Bs];
	Return[normalize[Times @@ #] & /@ Transpose[{f,b}]];
];

A  = {{0.7,0.3},{0.3,0.7}};
U  = {{0.9,0.0},{0.0,0.2}};
NU = {{0.1,0.0},{0.0,0.8}};
Bs = {U,U,NU,U,U};


calcB[frame, mu, sigma] := Module[{multiNormal},
        DiagonalMatrix[Table[PDF[MultinormalDistribution[mu[[i]], sigma[[i]]],
frame],{i,1,Length[mu]}]];
];

newClassifier[{A_,mu_,sigma_}] := Function[frames,forward[{0.5,0.5},A,calcB[mu,sigma,#] & /@ frames]];

emInit[frames_, N_] := Module[{A,prior, mu, sigma},
	
	(* Generate a random prior *)
	prior = Transpose[#/Total[#] & /@ (RandomReal[{0.01, 0.99}, {1,N}])];
	(* Generate a random transition matrix A *)
	A = #/Total[#] & /@ (RandomReal[{0.01, 0.99}, {N,N}]);
	(* mu, for each state *)
	mu = (#/Total[#] &[RandomReal[{-0.99,0.99},N]]) * Mean[O];
	(* sigma *)
	sigma = Sqrt[(#/Total[#] &[RandomReal[{0.1,0.99},N]]) * Covariance[O]];

   Return[{prior, A, mu,sigma}];
	*)
];

emStep[{prior_,A_,mu_,sigma_}] := Module[{},
	(* Do step *)
	Return[{prior,A,mu,sigma}];
];

baumWelch[frames_,N_] := Module[{state,A,mu,sigma},
	state = emInit[frames,N];
	Nest[emStep,state,True,15];
	Return[{A,mu,sigma}];
];

 

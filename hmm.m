(* ::Package:: *)

forward[f_,A_,Bs_] := Module[{newf},
	newf = f.A.B;
	newf = newf/Total[newf];
	Return[forward[newf,A,B]];
];

A  = {{0.7,0.3},{0.3,0.7}};
U  = {{0.9,0.0},{0.0,0.2}};
NU = {{0.1,0.0},{0.0,0.8}};
Bs = {U,U,NU,U,U};

Print[forward[{0.5,0.5},A,Bs]


backward[b_,A_,B_] := Module[{},
	Print["backward"];
]; 











newClassifier[word_,trainingSet_] := Module[{returnFunction,A,B},
	A = calcA[trainingSet];
	B = calcB[trainingSet]; 
	returnFunction = Function[O,
		
		Return[forward[A,B,{0.5,0.5},O][[1]]];
	];

 Return[returnFunction];
];

emInit[O_, N_] := Module[{A,prior, mu, sigma},
	(* Generate a random prior *)
	prior = Transpose[#/Total[#] & /@ (RandomReal[{0.01, 0.99}, {1,N}])];
	
	(* Generate a random transition matrix A *)
	A = #/Total[#] & /@ (RandomReal[{0.01, 0.99}, {N,N}]);
	Covariance[]
	(* mu, for each state *)
	mu = (#/Total[#] &[RandomReal[{-0.99,0.99},N]]) * Mean[O];
	(* sigma *)
	sigma = Sqrt[(#/Total[#] &[RandomReal[{0.1,0.99},N]]) * Covariance[O]];

   Return[{prior, A, mu,sigma}];
];

emStep[] := Module[{},
	DoStep[];
];

baumWelch[O_,N_] := Module[{init},
	state = emInit[O,N];
	For[ (*as many times as it takes!*)
		state = emStep[state];
	]
];

O = {{4,2,2},{4,2,1}{5,7,2}}
emInit[O,3]




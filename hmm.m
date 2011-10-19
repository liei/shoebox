(* ::Package:: *)

forward[A_,B_,f_,O_] := Module[{},
	Print["forward"];
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

O = {{4,2,2},{4,2,1}{5,7,2}}
emInit[O,3]




(* ::Package:: *)

(* Returns the featuresof the file found at path *)
extract[path_] := Module[{sampleRate, data, frameSize, overlap,peaks},

Print[path];

(*Import the data, grab the sampling rate*)
sampleRate = Import[path, "SampleRate"];
data = Flatten@Import[path,"Data"];

(*Divide soundfile into slices. Perform fourier transform on the slices,
and make a spectrogram*)

(*Number of data points in each slice. A resolution of 100 slices per second.*)
frameSize = 80;
overlap = 20;

(*Apply a Hamming window to each segment*)

frames = Table[data[[i;;i+frameSize-1]],{i,1,Length[data]-frameSize,frameSize-overlap}];


hamming[x_] := Table[(0.54+0.46*Cos[2*\[Pi]*i/(Length[x])]) * x[[i]], {i, 1, Length[x]}];
frames = Map[hamming,frames];
(*Apply fourier transform to each window*)
frames = Fourier /@ frames;
frames = Abs[frames];
frames = Map[DeleteDuplicates, frames];
Print[ListLinePlot[frames[[20]]]];
frames = Take[Sort[Flatten[Position[Partition[Differences[#], 2, 1], {x_, y_} /; Sign[x] > Sign[y]] + 1],Function[{e1,e2},#[[e1]] > #[[e2]]]],3] & /@ frames;
Return[frames];
];
path = "git/shoebox/training/go/go_0.wav";
features = extract[Directory[] <> "/" <> path];
Print[features];





(* ::InheritFromParent:: *)
(**)
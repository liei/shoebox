(* ::Package:: *)

(* Returns the featuresof the file found at path *)
features[path] := Module[{sampleRate, data, frameSize, overlap},

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
frames = Map[Fourier,frames];

frames = Abs[frames];
frames = Function[x, x/2N[Pi,6]]/@frames;
frames = Map[DeleteDuplicates, frames];
findPeaks[list, peaks] := Sort[list, Greater][[1;;peaks]];

features = Table[findPeaks[frames[[i]], 6], {i, 1, Length[frames]}];

]
path = "d:\\go_0.wav";
features = features[path]









(* ::Package:: *)

(* Returns the features of the file found at path *)
extract[path_] := Module[{sampleRate, data, frameSize,overlap},
	(*Import the data, grab the sampling rate*)
	sampleRate = Import[path, "SampleRate"];
	data = Flatten@Import[path,"Data"];

	(*Number of data points in each slice. A resolution of 100 slices per second. With a sample rate of 8000Hz*)
	frameSize = 80;
	overlap = 20;

	(*Divide soundfile into slices. Perform fourier transform on the slices, and make a spectrogram*)
	frames = Table[data[[i;;i+frameSize-1]],{i,1,Length[data]-frameSize,frameSize-overlap}];

	(*Apply a Hamming window to each frame*)
	hamming[frame_] := Table[(0.54+0.46*Cos[2*\[Pi]*i/(Length[frame])]) * frame[[i]], {i, 1, Length[frame]}];
	frames = hamming /@ frames;

	(*Apply fourier transform to each window*)
	frames = Fourier /@ frames;
	(* Get theabsolute value *)
	frames = Abs[frames];
	frames = Map[DeleteDuplicates, frames];
	(*Find all peaks, sort them by amplitude, desc, and take the 3 first frequencies*)
	frames = Take[Sort[Flatten[Position[Partition[Differences[#], 2, 1], {x_, y_} /; Sign[x] > Sign[y]] + 1],Function[{e1,e2},#[[e1]] > #[[e2]]]],3] & /@ frames;
	Return[frames];
];




(* ::InheritFromParent:: *)
(**)

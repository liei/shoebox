(* ::Package:: *)

(* Returns the spectrogram of the file found at path *)
spectrogram[path] = Module[{sampleRate, data}, 

(*Import the data, grab the sampling rate*)
sampleRate = Import["/home/liei/git/shoebox/trainfiles/go/go_0.wav", "SampleRate"];
data = Flatten@Import["/home/liei/git/shoebox/trainfiles/go/go_0.wav","Data"];

(*Divide soundfile into slices. Perform fourier transform on the slices,
and make a spectrogram*)

(*Number of data points in each slice. A resolution of 100 slices per second.*)
sliceSize = 80;
overlap = 20;

(*Apply a Hammond window to each segment*)

slices = Table[data[[i;;i+sliceSize-1]],{i,1,Length[data]-sliceSize,sliceSize-overlap}];


hamming[x_] := Table[(0.54+0.46*Cos[2*\[Pi]*i/(Length[x])]) * x[[i]], {i, 1, Length[x]}];
slices = Map[hamming,slices];
slices = Map[Fourier,slices];

(*Apply fourier transform to each window*)

slices = Abs[slices];
slices = Function[x, x/2N[Pi,6]]/@slices;
Return[slices];
];
slices = spectrogram["/home/liei/git/shoebox/trainfiles/go/go_0.wav"];
ListContourPlot[slices]


(* ::InheritFromParent:: *)
(*spectrogram["/home/liei/git/shoebox/trainfiles/go/go_0.wav"]*)

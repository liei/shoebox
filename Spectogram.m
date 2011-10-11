(* ::Package:: *)

(* Returns the spectrogram of the file found at path *)
spectrogram[path] := Module[{sampleRate, data}, 

(*Import the data, grab the sampling rate*)
sampleRate = Import[path, "SampleRate"];
data = Flatten@Import[path,"Data"];

(*Divide soundfile into slices. Perform fourier transform on the slices,
and make a spectrogram*)

(*Number of data points in each slice. A resolution of 100 slices per second.*)
sliceSize = sampleRate / 100;

slices = Table[Fourier[data[[t;; t+sliceSize-1]] ], {t, 1, 
Length[data] - sliceSize, sliceSize}];

slices = Abs[slices];
slices = Function[x, x/2N[Pi,6]]/@slices;
slices;
ListContourPlot[slices]
]
spectrogram["c:/trainingData/go_0.wav"]


(* ::InheritFromParent:: *)
(*spectrogram["c:/trainingData/go_0.wav"]*)

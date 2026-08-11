(* Export held StandardForm input cells from a Mathematica notebook as .wl. *)
marker = FirstPosition[$CommandLine, "--"];
arguments = If[Head[marker] === Missing,
  {},
  Drop[$CommandLine, marker[[1]]]
];
If[Length[arguments] < 2 || Length[arguments] > 3,
  Print["usage: export_notebook_inputs.wls input.nb output.wl [cell-directory]"];
  Exit[2]
]

input = arguments[[1]];
output = arguments[[2]];
cellDirectory = If[Length[arguments] == 3, arguments[[3]], ""];
notebook = Import[input, "Notebook"];
boxes = Cases[notebook, Cell[BoxData[body_], "Input", ___] :> body, Infinity];

held = Quiet[Check[ToExpression[#, StandardForm, HoldComplete], $Failed] & /@ boxes];
held = Flatten[held];
held = DeleteCases[held, $Failed];
held = DeleteCases[held, HoldComplete[Null]];
text = (StringDrop[
      StringDrop[ToString[#, InputForm], StringLength["HoldComplete["]],
      -1] & /@ held);
Export[output, StringRiffle[text, StringJoin[FromCharacterCode[10], FromCharacterCode[10]]], "Text"];
If[cellDirectory != "",
  CreateDirectory[cellDirectory, CreateIntermediateDirectories -> True];
  MapIndexed[
    Export[
      FileNameJoin[{cellDirectory,
        "cell-" <> IntegerString[First[#2], 10, 3] <> ".wl"}],
      #1, "Text"] &,
    text]
];
Print["exported ", Length[text], " input cells"];

BeginPackage["JerryI`Notebook`ManipulateUtils`", {
    "JerryI`Notebook`Graphics2D`",
    "Notebook`Kernel`Inputs`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`WLJS`Transport`"
}]

ManipulatePlot::usage = "ManipulatePlot[f_, {x, min, max}, {p1, min, max}, ...] an interactive plot of a function f[x, p1] with p1 given as a parameter"

Unprotect[Manipulate]
ClearAll[Manipulate]

Manipulate[__] := Style["Not supported! Please, use ManipulatePlot or general dynamics", Background->Yellow]

Begin["`Internal`"]

If[$VersionNumber < 13.3,
  RealValuedNumericQ = NumericQ
];

(* convert parameters to objects *)
makeVariableObject[{s_Symbol, min_, max_}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[((max-min)/50.0)], "Initial" -> N[((min + max)/2.0)]|>

makeVariableObject[{{s_Symbol, init_}, min_, max_}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[((max-min)/50.0)], "Initial" -> N[init]|>

makeVariableObject[{s_Symbol, min_, max_, step_}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[step], "Initial" -> Round[(min + max)/2.0 // N, step]|>

makeVariableObject[{{s_Symbol, init_}, min_, max_, step_}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[step], "Initial" -> Round[init // N, step]|>

makeVariableObject[{s_Symbol}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->-1, "Max"->1, "Step" -> 0.1, "Initial" -> 0.|>
makeVariableObject[{{s_Symbol, init_}}] := <|"Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->-1, "Max"->1, "Step" -> 0.1, "Initial" -> N[init]|>

SetAttributes[makeVariableObject, HoldAll]


makeFunction[f_, variables__] := With[{v = variables}, 
  Function[variables, f] 
]

SetAttributes[makeFunction, HoldFirst]
SetAttributes[TempHeld, HoldAll]


ManipulatePlot[f_, {t_Symbol, tmin_?NumericQ, tmax_?NumericQ}, paramters:{_Symbol | {_Symbol, _?NumericQ}, ___?NumericQ}.., OptionsPattern[]] := 
With[{
  vars = Map[makeVariableObject, Unevaluated @ List[paramters]], 
  plotPoints = OptionValue["SamplingPoints"]
},

  With[{
    (* wrap f to a pure function *)
    anonymous = With[{s = Extract[#, "Symbol", TempHeld] &/@ Join[{<|"Symbol":>t|>}, vars]},

                  makeFunction[f, s] /. {TempHeld[x_] -> x} // Quiet
              ],
    
    size = OptionValue[ImageSize],

    transitionType = OptionValue[TransitionType],
    transitionDuration = OptionValue[TransitionDuration],

    axes = OptionValue[AxesLabel],
    prolog = OptionValue[Prolog],
    epilog = OptionValue[Epilog],
    style = {OptionValue[PlotStyle]}//Flatten
  },
    Module[{pts, plotRange = OptionValue[PlotRange], sampler},

      
      sampler[args_] := Select[
        Table[{t, anonymous @@ Join[{t}, args]}, {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[# // Flatten, RealValuedNumericQ]&];

      (* test sampling of f *)
      pts = sampler[#["Initial"] &/@ vars];

      If[plotRange === Automatic,
        plotRange = 1.1 {MinMax[pts[[All,1]]], MinMax[pts[[All,2]] // Flatten]};
      ];



      With[{
        opts = Sequence[
          ImageSize->size, 
          PlotRange->plotRange, 
          Axes->True, 
          TransitionType->transitionType, 
          TransitionDuration->transitionDuration, 
          Epilog -> epilog,
          Prolog -> prolog,
          AxesLabel -> axes
        ],
        traces = Length[{pts[[1,2]]} // Flatten],
        length = plotPoints
      },
      
        (* two cases: single curve or multiple *)
        If[Depth[pts] === 3,
          singleTrace[anonymous, t, tmin, tmax, length, style, vars, opts]
        ,
          multipleTraces[anonymous, traces, t, tmin, tmax, length, style, vars, opts]
        ]

      ]


    ]
  ]
]


singleTrace[anonymous_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, Global`pts, sampler},
      (* sampling of f *)
      sampler[a_] := Select[
        Table[{t, anonymous @@ Join[{t}, a]}, {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&];

      Global`pts = sampler[#["Initial"] &/@ vars];
      
      (* controls *)
      sliders = InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])] &/@ vars;
      sliders = InputGroup[sliders];
      
      (* update pts when dragged *)
      EventHandler[sliders, Function[data, Global`pts = sampler[data]]];


      Row[{
          Graphics[{AbsoluteThickness[2], style[[1]], Line[Global`pts // Offload]}, opts],
          sliders
      }]
]

multipleTraces[anonymous_, traces_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, sampler, Global`pts},

      sampler[a_] := Select[
        Table[anonymous @@ Join[{t}, a], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&] // Transpose;

      Global`pts = sampler[#["Initial"] &/@ vars];
      
      sliders = InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])] &/@ vars;
      sliders = InputGroup[sliders];
      
      EventHandler[sliders, Function[data, Global`pts = sampler[data]]];


      Row[{
          Graphics[{AbsoluteThickness[2], 
            (* combine contstant X axis list with different dynamic Y lists *)
            Table[With[{
              i = i,
              color = If[i > Length[style], style[[1]], style[[i]]],
              xaxis = Table[t, {t, tmin, tmax, (tmax-tmin)/plotPoints}]
            },
              
              {color, Line[With[{
                points = Transpose[{xaxis, Global`pts[[i]]}]
              },
                points
              ]]} // Offload
            ]
            , {i, traces}]
          }, opts],
          sliders
      }]
]

SetAttributes[singleTrace, HoldAll]
SetAttributes[multipleTraces, HoldAll]

Options[ManipulatePlot] = {PlotRange -> Automatic, "SamplingPoints" -> 200.0, ImageSize -> {400, 300}, PlotStyle->ColorData[97, "ColorList"], TransitionType->"Linear", TransitionDuration->50, Epilog->{}, Prolog->{}, AxesLabel->{}};

SetAttributes[ManipulatePlot, HoldAll]

End[]
EndPackage[]

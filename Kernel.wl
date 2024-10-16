BeginPackage["JerryI`Notebook`ManipulateUtils`", {
    "JerryI`Notebook`Graphics2D`",
    "Notebook`Kernel`Inputs`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Language`",
    "JerryI`Misc`WLJS`Transport`",
    "Notebook`Editor`Boxes`",
    "Notebook`EditorUtils`"
}]

ManipulatePlot::usage = "ManipulatePlot[f_, {x, min, max}, {p1, min, max}, ...] an interactive plot of a function f[x, p1] with p1 given as a parameter"
ManipulateParametricPlot::usage = ""

AnimatePlot::usage = "AnimatePlot[f_, {x, min, max}, {t, min, max}]"
AnimateParametricPlot::usage = ""

ListAnimatePlot::usage = ""

Unprotect[Animate]
ClearAll[Animate]

Unprotect[Refresh]

Animate[__] := (
  Message["Not supported in WLJS. Use AnimatePlot or general dynamics"];
  Style["Not supported! Please, use AnimatePlot or general dynamics", Background->Yellow]
)

Manipulator[_] := (
  Message["Not supported in WLJS. Use AnimatePlot or general dynamics"];
  Style["Not supported! Please, use AnimatePlot or general dynamics", Background->Yellow]
)



Unprotect[ListAnimate]
ClearAll[ListAnimate]

Unprotect[Animator]
ClearAll[Animator]

Animator[_] := (
  Message["Not supported in WLJS. Use AnimatePlot or general dynamics"];
  Style["Not supported! Please, use AnimatePlot or general dynamics", Background->Yellow]
)

Animator[__] := (
  Message["Not supported in WLJS. Use AnimatePlot or general dynamics"];
  Style["Not supported! Please, use AnimatePlot or general dynamics", Background->Yellow]
)

ListAnimate[__] := (
  Message["Not supported in WLJS. Use AnimatePlot or general dynamics"];
  Style["Not supported! Please, use AnimatePlot or general dynamics", Background->Yellow]
)

AnimatePlot;

RefreshBox;

Begin["`Internal`"]

Unprotect[Manipulate]
ClearAll[Manipulate]

ClearAll[Manipulate]

Manipulate`Cached = <||>;

useCache[hash_, f_, values_] := If[KeyExistsQ[Manipulate`Cached[hash], values],
  Manipulate`Cached[hash, values]
,
  With[{r = f @ values},
    Manipulate`Cached[hash, values] = r;
    r
  ]
]

Manipulate[f_, parameters:{_Symbol | {_Symbol, _?NumericQ}, ___?NumericQ}.., OptionsPattern[] ] := Module[{Global`code, sliders}, With[{
  vars = Map[makeVariableObject, Unevaluated @ List[parameters] ],
  hash = Hash[{f, parameters}]
},
 
    Manipulate`Cached[hash] = <||>; (* look up table *)

    With[{
    (* wrap f to a pure function *)
    anonymous = With[{s = Extract[#, "Symbol", TempHeld] &/@ vars},

                  makeFunction[f, s] /. {TempHeld[x_] -> x} // Quiet
              ]
    },

      test =vars;
      
      Global`code = useCache[hash, ToString[anonymous @@ #, StandardForm]&, (#["Initial"] &/@ vars) ];
      
      (* controls *)
      sliders = InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])] &/@ vars;
      sliders = InputGroup[sliders];
      
      (* update pts when dragged *)
      EventHandler[sliders, Function[data, Global`code = useCache[hash, ToString[anonymous @@ #, StandardForm]&, data] ] ];


      Column[{
          sliders,
          EditorView[Global`code // Offload]
      }]
    ]
]]

SetAttributes[Manipulate, HoldAll]

Refresh /: MakeBoxes[Refresh[expr_, updateInterval_, OptionsPattern[] ], StandardForm ] := With[{
  interval = If[MatchQ[updateInterval, _Quantity], UnitConvert[updateInterval, "Milliseconds"] // QuantityMagnitude, updateInterval 1000],
  event = CreateUUID[],
  evaluated = expr
},
  LeakyModule[{
    Global`str = ToString[evaluated, StandardForm]
  },

      EventHandler[event, Function[Null,
        Global`str = ToString[expr, StandardForm]
      ] ];

    With[{
      editor = EditorView[Global`str // Offload, "ReadOnly"->True, "ForceUpdate"->True] // CreateFrontEndObject
    },
    
        ViewBox[evaluated, RefreshBox[editor, event, interval] ]
    ]
  ]
] // Quiet

Refresh /: MakeBoxes[Refresh[expr_, ev_String | ev_EventObject, OptionsPattern[] ], StandardForm ] := With[{
  event = CreateUUID[],
  evaluated = expr
},
  LeakyModule[{
    Global`str = ToString[evaluated, StandardForm]
  },
  
    EventHandler[ev, Function[Null,
        Global`str = ToString[expr, StandardForm]
    ] ];

    With[{
      editor = EditorView[Global`str // Offload, "ReadOnly"->True, "ForceUpdate"->True] // CreateFrontEndObject
    },

      ViewBox[evaluated, RefreshBox[editor, event, 0] ]
    ]
  ]
] // Quiet

SetAttributes[Refresh, HoldFirst]

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

ManipulateParametricPlot[all__] := manipulatePlot[xyChannel, all]

ManipulatePlot[all__] := manipulatePlot[yChannel, all]

yChannel[t_, y_] := {t, y}
xyChannel[t_, y_] := y

manipulatePlot[tracer_, f_, {t_Symbol, tmin_?NumericQ, tmax_?NumericQ}, paramters:{_Symbol | {_Symbol, _?NumericQ}, ___?NumericQ}.., OptionsPattern[] ] := 
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
        Table[tracer[t, anonymous @@ Join[{t}, args] ], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
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
          singleTrace[tracer, anonymous, t, tmin, tmax, length, style, vars, opts]
        ,
          multipleTraces[tracer, anonymous, traces, t, tmin, tmax, length, style, vars, opts]
        ]

      ]


    ]
  ]
]


singleTrace[tracer_, anonymous_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, Global`pts, sampler},
      (* sampling of f *)
      sampler[a_] := Select[
        Table[tracer[t, anonymous @@ Join[{t}, a] ], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&];

      Global`pts = sampler[#["Initial"] &/@ vars];
      
      (* controls *)
      sliders = InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])] &/@ vars;
      sliders = InputGroup[sliders];
      
      (* update pts when dragged *)
      EventHandler[sliders, Function[data, Global`pts = sampler[data] ] ];


      Row[{
          Graphics[{AbsoluteThickness[2], style[[1]], Line[Global`pts // Offload]}, opts],
          sliders
      }]
]

multipleTraces[tracer_, anonymous_, traces_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, sampler, Global`pts},

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

Options[manipulatePlot] = {PlotRange -> Automatic, "SamplingPoints" -> 200.0, ImageSize -> {400, 300}, PlotStyle->ColorData[97, "ColorList"], TransitionType->"Linear", TransitionDuration->50, Epilog->{}, Prolog->{}, AxesLabel->{}};
Options[ManipulatePlot] = Options[manipulatePlot]
Options[ManipulateParametricPlot] = Options[manipulatePlot]

SetAttributes[ManipulatePlot, HoldAll]
SetAttributes[manipulatePlot, HoldAll]

animatePlot;
AnimatePlot;

Options[animatePlot] = Join[Options[manipulatePlot], {AnimationRate -> 24}];
Options[AnimatePlot] = Options[animatePlot];
Options[ListAnimatePlot] = Join[Options[animatePlot], {InterpolationOrder -> 1}];

animatePlot[tracer_, f_, {t_Symbol, tmin_?NumericQ, tmax_?NumericQ}, paramters:{_Symbol | {_Symbol, _?NumericQ}, ___?NumericQ}.., OptionsPattern[] ] := 
With[{
  vars = Map[makeVariableObject, Unevaluated @ List[paramters] ], 
  plotPoints = OptionValue["SamplingPoints"]
},

  If[Length[List[paramters] ] > 1, Return[Style["Use single parameter for the animation", Background->Yellow] ] ];

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
    rate = OptionValue[AnimationRate],
    style = {OptionValue[PlotStyle]}//Flatten
  },
    Module[{pts, plotRange = OptionValue[PlotRange], sampler},

      
      sampler[args_] := Select[
        Table[tracer[t, anonymous @@ Join[{t}, args] ], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
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
          Prolog -> prolog,
          AxesLabel -> axes
        ],
        traces = Length[{pts[[1,2]]} // Flatten],
        length = plotPoints
      },
      
        (* two cases: single curve or multiple *)
        If[Depth[pts] === 3,
          singleAnimatedTrace[tracer, anonymous, t, tmin, tmax, length, style, vars, Epilog -> epilog, AnimationRate -> rate, opts]
        ,
          multipleAnimatedTraces[tracer, anonymous, traces, t, tmin, tmax, length, style, vars, Epilog -> epilog, AnimationRate -> rate, opts]
        ]

      ]


    ]
  ]
]

singleAnimatedTrace[tracer_, anonymous_, t_, tmin_, tmax_, plotPoints_, style_, vars_, Rule[Epilog, epilog_], Rule[AnimationRate, rate_], opts__] := Module[{dataset = {}, sliders, Global`pts, sampler, ranges},
      (* sampling of f *)
      sampler[a_] := Select[
        Table[tracer[t, anonymous @@ Join[{t}, a] ], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&];

      Global`pts = sampler[#["Initial"] &/@ vars];
      
      (* ranges *)
      ranges = With[{j =First[vars]}, Table[{i}, {i, j["Min"], j["Max"], j["Step"]}] ];
      
      dataset = sampler /@ ranges;

      Graphics[{AbsoluteThickness[2], style[[1]], Line[Global`pts // Offload]}, Epilog->{Animate`Shutter[Global`pts, dataset, rate], epilog}, opts]
]

SetAttributes[Animate`Shutter, HoldFirst];

multipleAnimatedTraces[tracer_, anonymous_, traces_, t_, tmin_, tmax_, plotPoints_, style_, vars_, Rule[Epilog, epilog_], Rule[AnimationRate, rate_], opts__] := Module[{sliders, ranges, dataset, sampler, Global`pts},

      sampler[a_] := Select[
        Table[anonymous @@ Join[{t}, a], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&] // Transpose;

      Global`pts = sampler[#["Initial"] &/@ vars];
      
      ranges = With[{j =First[vars]}, Table[{i}, {i, j["Min"], j["Max"], j["Step"]}] ];
      
      dataset = sampler /@ ranges;


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
              ] ]} // Offload
            ]
            , {i, traces}]
      }, Epilog->{Animate`Shutter[Global`pts, dataset, rate], epilog}, opts]
]

SetAttributes[singleAnimatedTrace, HoldAll]
SetAttributes[multipleAnimatedTraces, HoldAll]

AnimatePlot[all__] := animatePlot[yChannel, all]
SetAttributes[AnimatePlot, HoldAll]

AnimateParametricPlot[all__] := animatePlot[xyChannel, all]
SetAttributes[AnimateParametricPlot, HoldAll]


ListAnimatePlot[list_List, opts: OptionsPattern[] ] := With[{intOrder = OptionValue[InterpolationOrder]},
  Switch[ArrayDepth[list // First],
    1,
      (* single y traces *)
      With[{t = Interpolation[Transpose[{Range[Length[#] ], #}], InterpolationOrder->intOrder] &/@ list},
        Module[{func,x,i},
          func[x_?NumberQ, j_?NumberQ] := t[[j // Round]][x];
          AnimatePlot[func[x, i], {x, 1, Length[list // First]}, {i, 1, Length[list], 1}, opts]
        ]
        
      ]
    ,

    2,
      If[Length[list // First // First] > 2,
        (* multiple y traces *)
        
With[{t = Map[Function[{l}, Interpolation[Transpose[{Range[Length[#] ], #}], InterpolationOrder->intOrder] &/@ l], list]},
        Module[{func,x,i},
          func[xx_?NumberQ, jj_?NumberQ] := Map[Function[k, k[xx]], t[[jj // Round]]];
          AnimatePlot[func[x, i], {x, 1, Length[list // First // First]}, {i, 1, Length[list], 1}]
        ]
        
      ]
        
      ,
        (* single xy traces *)

      With[{t = Interpolation[#, InterpolationOrder->intOrder] &/@ list},
        Module[{func,x,i},
          func[x_?NumberQ, j_?NumberQ] := t[[j // Round]][x];
          AnimatePlot[func[x, i], {x, list[[1,All,1]] // Min, list[[1,All,1]] // Max}, {i, 1, Length[list], 1}, opts]
        ]
        
      ]

      ]

    ,

    3,
      (* multiple xy traces *)

With[{t = Map[Function[{l}, Interpolation[#, InterpolationOrder->intOrder] &/@ l], list]},
        Module[{func,x,i},
          func[xx_?NumberQ, jj_?NumberQ] := Map[Function[k, k[xx]], t[[jj // Round]]];
          AnimatePlot[func[x, i], {x, list[[1,1,All,1]] // Min, list[[1,1,All,1]] // Max}, {i, 1, Length[list], 1}]
        ]
        
      ]

      
  ]
]


End[]
EndPackage[]

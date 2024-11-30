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

(* register globally, cuz Refresh is also originally a system function *)
System`RefreshBox;

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

SetAttributes[TempHeld, HoldAll]


checkIfFunction[_Symbol] := False;
checkIfFunction[_] := True

checkIfFunction[_Notebook`Editor`Internal`$PreviousOut] := False

SetAttributes[checkIfFunction, HoldFirst]

Manipulate[f_, parameters:({_Symbol | {_Symbol, _?NumericQ} | {_Symbol, _?NumericQ, _String}, ___?NumericQ} | {_Symbol | {_Symbol, _} | {_Symbol, _, _String}, _List}).., OptionsPattern[] ] := Module[{Global`code, sliders}, With[{
  vars = Map[makeVariableObject, Unevaluated @ List[parameters] ],
  hash = Hash[{f, parameters}]
},

    
 
    If[!AllTrue[vars, !FailureQ[#] &] || vars === $Failed,
      Return[$Failed];
    ];

    Manipulate`Cached[hash] = <||>; (* look up table *)

    With[{
    (* wrap f into a pure function *)
    anonymous = With[{s = Extract[#, "Symbol", Hold] &/@ vars},

                  With[{vlist = Hold[s] /. {Hold[u_Symbol] :> u}, checked = checkIfFunction[f]},
                    makeFunction[vlist, f,  checked]
                  ]
              ]
    },

      (* 
         ExpressionJSON works with contexts in a bit sketchy way if it is executed from a package. 
         Use Global` or System` or any other contexts explicitly to void conflicts for dynamic symbols
      *)
      Global`code = useCache[hash, ToString[anonymous @@ #, StandardForm]&, (#["Initial"] &/@ vars) ];
      
      (* controls *)
      sliders = Switch[#["Controller"],
                  InputRange,
                    InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"]), "Topic"->{Null, "Default"}],

                  InputSelect,
                    InputSelect[#["List"], #["Initial"], "Label"->(#["Label"])],
                  
                  _,
                    Null
                ] &/@ vars;

      sliders = InputGroup[sliders];
      
      (* update expression when any slider is dragged *)
      EventHandler[sliders, Function[data, Global`code = useCache[hash, ToString[anonymous @@ #, StandardForm]&, data] ] ];


      Column[{
          sliders,
          EditorView[Global`code // Offload] (* EditorView works only with strings *)
      }]
    ]
]]

SetAttributes[Manipulate, HoldAll]
(*
Unprotect[Refresh]

Options[Refresh] = {UpdateInterval -> 1}




*)

Refresh::usage = "Refresh[expr_, UpdateInterval_] creates a dynamic widget, which reevalues expr every UpdateInterval (in seconds or Quantity[]). Refresh[expr_, ev_EventObject] is updated by external event object ev"

(* Refresh[expr_, Rule[UpdateInterval, updateInterval_Quantity] | Rule[UpdateInterval, updateInterval_?NumericQ] ] := Refresh[expr, updateInterval ] *)

Refresh /: MakeBoxes[Refresh[expr_, updateInterval_Quantity | updateInterval_?NumericQ, OptionsPattern[] ], StandardForm ] := With[{
  interval = If[MatchQ[updateInterval, _Quantity], UnitConvert[updateInterval, "Milliseconds"] // QuantityMagnitude, updateInterval 1000],
  event = CreateUUID[],
  evaluated = expr
},
  (* We need LeakyModule to fool WL's Garbage collector *)
  LeakyModule[{
    (* 
       ExpressionJSON works with contexts in a bit sketchy way if it is executed from a package. 
       Use Global` or System` or any other contexts explicitly to void conflicts for dynamic symbols
    *)    
    Global`str = ToString[evaluated, StandardForm]
  },

  (* event is fired from JS side (RefreshBox) *)
      EventHandler[event, Function[Null,
        Global`str = ToString[expr, StandardForm]
      ] ];

    With[{
      editor = EditorView[Global`str // Offload, "ReadOnly"->True] 
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
  
  (* event is fired from WL side *)
    EventHandler[ev, Function[Null,
        Global`str = ToString[expr, StandardForm]
    ] ];

    With[{
      editor = EditorView[Global`str // Offload, "ReadOnly"->True] 
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

makeVariableObject[{s_Symbol, list_List}] := <|"Controller"->InputSelect, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "List"->list, "Initial" -> First[list]|>

makeVariableObject[{{s_Symbol, init_}, list_List}] := <|"Controller"->InputSelect, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "List"->list, "Initial" -> init|>

makeVariableObject[{{s_Symbol, init_, label_String}, list_List}] := <|"Controller"->InputSelect, "Symbol" :> s, "Label"->label, "List"->list, "Initial" -> init|>



makeVariableObject[{s_Symbol, min_, max_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[((max-min)/50.0)], "Initial" -> N[((min + max)/2.0)]|>

makeVariableObject[{{s_Symbol, init_}, min_, max_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[((max-min)/50.0)], "Initial" -> N[init]|>

makeVariableObject[{{s_Symbol, init_, label_String}, min_, max_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->label, "Min"->N[min], "Max"->N[max], "Step" -> N[((max-min)/50.0)], "Initial" -> N[init]|>


makeVariableObject[{s_Symbol, min_, max_, step_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[step], "Initial" -> Round[(min + max)/2.0 // N, step]|>

makeVariableObject[{{s_Symbol, init_}, min_, max_, step_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->N[min], "Max"->N[max], "Step" -> N[step], "Initial" -> Round[init // N, step]|>

makeVariableObject[{{s_Symbol, init_, label_String}, min_, max_, step_}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->label, "Min"->N[min], "Max"->N[max], "Step" -> N[step], "Initial" -> Round[init // N, step]|>


makeVariableObject[{s_Symbol}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->-1, "Max"->1, "Step" -> 0.1, "Initial" -> 0.|>
makeVariableObject[{{s_Symbol, init_}}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->ToString[Unevaluated[s]], "Min"->-1, "Max"->1, "Step" -> 0.1, "Initial" -> N[init]|>


makeVariableObject[{{s_Symbol, init_, label_String}}] := <|"Controller"->InputRange, "Symbol" :> s, "Label"->label, "Min"->-1, "Max"->1, "Step" -> 0.1, "Initial" -> N[init]|>


makeVariableObject[__] := (
  Message[ManipulatePlot::badargs, "does not match the pattern"];
  $Failed
)

SetAttributes[makeVariableObject, HoldAll]


ClearAll[makeFunction];
makeFunction[Hold[list_], f_, ___] := If[MatchQ[list, {__Symbol}],
  With[{l = list, ff = f},
    Function @@ {l, ff}
  ]
,

  Internal`LocalizedBlock[list,
    Function[list, f]
  ]
]

makeFunction[Hold[list_], f_, True] := If[MatchQ[list, {__Symbol}],
  With[{l = list, ff = f},
    Function[list, f]
  ]
,

  Internal`LocalizedBlock[list,
    Function[list, f]
  ]
]

SetAttributes[makeFunction, HoldAll]

ManipulateParametricPlot[all__] := manipulatePlot[xyChannel, all]

ManipulatePlot[all__] := manipulatePlot[yChannel, all]

yChannel[t_, y_] := {t, y}
xyChannel[t_, y_] := y

ManipulatePlot::badargs = "Unsupported sequence of arguments: `1`";

manipulatePlot[__] := (
  Message[ManipulatePlot::badargs, "???"];
  $Failed
) 

manipulatePlot::nonreal = "The result function does not return real numbers"

manipulatePlot[tracer_, f_, {t_Symbol, tmin_?NumericQ, tmax_?NumericQ}, paramters:({_Symbol | {_Symbol, _?NumericQ} | {_Symbol, _?NumericQ, _String}, ___?NumericQ} | {_Symbol | {_Symbol, _} | {_Symbol, _, _String}, _List}).., OptionsPattern[] ] := 
With[{
  vars = Map[makeVariableObject, Unevaluated @ List[paramters]], (* convert all parameters, ranges to associations *)
  plotPoints = OptionValue["SamplingPoints"]
},


  If[!AllTrue[vars, !FailureQ[#] &] || vars === $Failed,
    Return[$Failed];
  ];

  With[{
    (* wrap f to a pure function *)
    anonymous = With[{s = Extract[#, "Symbol", Hold] &/@ Join[{<|"Symbol":>t|>}, vars]},
                  With[{vlist = Hold[s] /. {Hold[u_Symbol] :> u}},
                     makeFunction[vlist, f]
                  ]
              ],
    
    
    
    size = OptionValue[ImageSize],

    transitionType = OptionValue[TransitionType],
    transitionDuration = OptionValue[TransitionDuration],

    axes = OptionValue[AxesLabel],
    prolog = OptionValue[Prolog],
    epilog = OptionValue[Epilog],
    style = {OptionValue[PlotStyle]}//Flatten
  },

    test = anonymous;
    
    Module[{pts, plotRange = OptionValue[PlotRange], sampler},


      sampler[args_] := Select[
        Table[tracer[t, anonymous @@ Join[{t}, args] ], {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[# // Flatten, RealValuedNumericQ]&];

      (* test sampling of f *)
      pts = sampler[#["Initial"] &/@ vars];

      If[Length[pts] == 0,
        Message[manipulatePlot::nonreal];
        Return[$Failed];
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


singleTrace[tracer_, anonymous_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, Global`pts, sampler, plotRange},
      (* sampling of f *)
      sampler[a_] := Select[
        Table[tracer[t, anonymous @@ Join[{t}, a] ]// N, {t, tmin, tmax, (tmax-tmin)/plotPoints}] 
      , AllTrue[#, RealValuedNumericQ]&];


    (* 
       ExpressionJSON works with contexts in a bit sketchy way if it is executed from a package. 
       Use Global` or System` or any other contexts explicitly to void conflicts for dynamic symbols
    *) 
      Global`pts = sampler[#["Initial"] &/@ vars];

      If[Length[Global`pts] == 0,
        Message[manipulatePlot::nonreal];
        Return[$Failed];
      ];

      plotRange = Lookup[Association[opts], PlotRange, Automatic];

      If[plotRange === Automatic,
        plotRange = With[{p = {MinMax[Global`pts[[All,1]]], MinMax[Global`pts[[All,2]] // Flatten]}},
          {(p[[1]] - Mean[p[[1]]]) 1.1 + Mean[p[[1]]],  (p[[2]] - Mean[p[[2]]]) 1.1 + Mean[p[[2]]]}
        ];
      ];
      
      (* controls *)
      sliders = Switch[#["Controller"],
                  InputRange,
                    InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])],

                  InputSelect,
                    InputSelect[#["List"], #["Initial"], "Label"->(#["Label"])],
                  
                  _,
                    Null
                ] &/@ vars;

      sliders = InputGroup[sliders];
      
      (* update pts when dragged *)
      EventHandler[sliders, Function[data, Global`pts = sampler[data] ] ];


      Row[{
          Graphics[{AbsoluteThickness[2], style[[1]], Line[Global`pts // Offload]}, opts, PlotRange->plotRange],
          sliders
      }]
]

multipleTraces[tracer_, anonymous_, traces_, t_, tmin_, tmax_, plotPoints_, style_, vars_, opts__] := Module[{sliders, sampler, Global`pts, plotRange},

      sampler[a_] := Select[
        Table[anonymous @@ Join[{t}, a]// N, {t, tmin, tmax, (tmax-tmin)/plotPoints}]
      , AllTrue[#, RealValuedNumericQ]&] // Transpose;


    (* 
       ExpressionJSON works with contexts in a bit sketchy way if it is executed from a package. 
       Use Global` or System` or any other contexts explicitly to void conflicts for dynamic symbols
    *) 
      Global`pts = sampler[#["Initial"] &/@ vars];


      plotRange = Lookup[Association[opts], PlotRange, Automatic];

      If[plotRange === Automatic,
        plotRange = {
          With[{p = {tmin, tmax}},
            (p - Mean[p]) 1.1 + Mean[p]
          ],
          With[{p = MinMax[Global`pts // Flatten]},
            (p - Mean[p]) 1.1 + Mean[p]
          ]
        };
      ];      
      
      sliders = Switch[#["Controller"],
                  InputRange,
                    InputRange[#["Min"], #["Max"], #["Step"], #["Initial"], "Label"->(#["Label"])],

                  InputSelect,
                    InputSelect[#["List"], #["Initial"], "Label"->(#["Label"])],
                  
                  _,
                    Null
                ] &/@ vars;

      sliders = InputGroup[sliders];
      
      EventHandler[sliders, Function[data, Global`pts = sampler[data] ] ];


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
              ] ] } // Offload
            ]
            , {i, traces}]
          }, opts, PlotRange->plotRange],
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

SetAttributes[animatePlot, HoldAll]
SetAttributes[AnimatePlot, HoldAll]

Options[animatePlot] = Join[Options[manipulatePlot], {AnimationRate -> 24}];
Options[AnimatePlot] = Options[animatePlot];
Options[ListAnimatePlot] = Join[Options[animatePlot], {InterpolationOrder -> 1}];

animatePlot[tracer_, f_, {t_Symbol, tmin_?NumericQ, tmax_?NumericQ}, paramters:({_Symbol | {_Symbol, _?NumericQ} | {_Symbol, _?NumericQ, _String}, ___?NumericQ} | {_Symbol | {_Symbol, _} | {_Symbol, _, _String}, _List}).., OptionsPattern[] ] := 
With[{
  vars = Map[makeVariableObject, Unevaluated @ List[paramters] ], 
  plotPoints = OptionValue["SamplingPoints"]
},

  If[!AllTrue[vars, !FailureQ[#] &] || vars === $Failed,
    Return[$Failed];
  ];

  If[Length[List[paramters] ] > 1, Return[Style["Use single parameter for the animation", Background->Yellow] ] ];

  With[{
    (* wrap f to a pure function *)
    anonymous = With[{s = Extract[#, "Symbol", Hold] &/@ Join[{<|"Symbol":>t|>}, vars]},
                  With[{vlist = Hold[s] /. {Hold[u_Symbol] :> u}},
                    makeFunction[vlist, f]
                  ]
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

(* ::Package:: *)

Colors = {ColorData["DeepSeaColors"][0.01], Darker[Red], 
   ColorData[35][4], ColorData[35][6], Black, ColorData[35][2], 
   Darker[Magenta]};
genColors[n_] := 
 Table[{Colors[[Mod[i, 5]]], 
   If[i == 1, Opacity[1], Opacity[0.55]]}, {i, 1, n}]
genColors2[n_] := Table[{Opacity[0.6]}, {i, 1, n}]
initPols = 
  Map[#1 == 0 &, ToExpression[Import["InitPols.txt", "Lines"]]];
wuCharSet = 
  Map[ #1 == 0 &, ToExpression[ Import["AscChain.txt", "Lines"]]];
n = Length[wuCharSet];
variablesEval = 
  Table[ToExpression[
    Import[ToString[StringForm["CoeffsStep_`1`.txt", i]], 
     "Data"]], {i, 0, n - 2}];
initVars = 
  Drop[Drop[StringSplit[Import["SymVars.txt"], ""], 1], -1] // 
   ToExpression;


evalPlot[vars_, \[Lambda]_, system_] := 
    Module[{functions, n, m, aux, style}, Clear[x0, x1, x2];
      n = Length[initVars];
      m = Length[system];
      aux = Join[Table[initVars[[i]] -> vars[[i]], {i, n}]];
      style = genColors[m];
      functions = system /. aux;
      ContourPlot3D[
        Evaluate[
          
     functions], {x0, -\[Lambda], \[Lambda]}, {x1, -\[Lambda], \
     \
 \[Lambda]}, {x2, -\[Lambda], \[Lambda]}, PlotRange -> All, 
        ContourStyle -> style, Mesh -> None, 
        BaseStyle -> {FontSize -> 20, 
      FontFamily -> "Latin Modern Roman", 
            Black}, AxesLabel -> {"x", "y", "t"}, 
        FormatType -> TraditionalForm, Axes -> False, 
        AxesStyle -> {Black}, Boxed -> False, 
        LabelStyle -> Directive[Black, Bold]]];
evalPlot2[vars_, \[Lambda]_, system_] := 
    Module[{functions, n, m, aux, style}, Clear[x0, x1, x2];
      n = Length[initVars];
      m = Length[system];
      aux = Join[Table[initVars[[i]] -> vars[[i]], {i, n}]];
      style = genColors2[m];
      functions = system /. aux;
      ContourPlot3D[
        Evaluate[
          
     functions], {x0, -\[Lambda], \[Lambda]}, {x1, -\[Lambda], \
     \
 \[Lambda]}, {x2, -\[Lambda], \[Lambda]}, PlotRange -> All, 
        ContourStyle -> style, Mesh -> False, 
        BaseStyle -> {FontSize -> 20, 
      FontFamily -> "Latin Modern Roman", 
            Black}, AxesLabel -> {"x", "y", "t"}, 
        FormatType -> TraditionalForm, Axes -> True, 
    AxesStyle -> {Black},
         Boxed -> False, LabelStyle -> Directive[Black, Bold]]];



dynComp[rang_, system_, system2_, 
  vars_, \[Lambda]1_, \[Lambda]2_] := 
  With[{value = Module[{func, n, m, aux, style}, Clear[x, y, z];
          n = Length[vars];
          m = Length[system];
          aux = Join[Table[vars[[i]] -> vars[[i]], {i, n}]];
          style = genColors[m];
          func = system /. aux;
          ContourPlot3D[
            
      Evaluate[func /. {x0 -> x, x1 -> y, x2 -> z}], {x, -rang, 
              rang}, {y, -rang, rang}, {z, -rang, rang}, 
      PlotRange -> All, 
            ContourStyle -> style, Mesh -> None, 
            BaseStyle -> {FontSize -> 20, 
                FontFamily -> "Latin Modern Roman", Black}, 
            AxesLabel -> {"x", "y", "t"}, 
      FormatType -> TraditionalForm, 
            Axes -> True, AxesStyle -> {Black}, Boxed -> False, 
            LabelStyle -> Directive[Black, Bold]]], 
      value2 = Module[{func, n, m, aux, style}, Clear[x, y, z];
          n = Length[vars];
          m = Length[system2];
          aux = Join[Table[vars[[i]] -> vars[[i]], {i, n}]];
          style = genColors2[m];
          func = system2 /. aux;
          ContourPlot3D[
            
      Evaluate[func /. {x0 -> x, x1 -> y, x2 -> z}], {x, -rang, 
              rang}, {y, -rang, rang}, {z, -rang, rang}, 
      PlotRange -> All, 
            ContourStyle -> style, Mesh -> None, 
            BaseStyle -> {FontSize -> 20, 
                FontFamily -> "Latin Modern Roman", Black}, 
            AxesLabel -> {"x", "y", "t"}, 
      FormatType -> TraditionalForm, 
            Axes -> True, AxesStyle -> {Black}, Boxed -> False, 
            LabelStyle -> Directive[Black, Bold]]], 
      controls = 
        Sequence @@ 
          Table[{vars[[i]], \[Lambda]1, \[Lambda]2}, {i, 1, 
              Length[vars]}]}, 
  Manipulate[Show[value, value2], controls]]
       



dynPlot[rang_, system_, vars_, \[Lambda]1_, \[Lambda]2_] := 
  With[{value = Module[{func, n, m, aux, style}, Clear[x, y, z];
          n = Length[initVars];
          m = Length[system];
          aux = Join[Table[initVars[[i]] -> vars[[i]], {i, n}]];
          style = genColors[m];
          func = system /. aux;
          ContourPlot3D[
            
      Evaluate[func /. {x0 -> x, x1 -> y, x2 -> z}], {x, -rang, 
              rang}, {y, -rang, rang}, {z, -rang, rang}, 
      PlotRange -> All, 
            ContourStyle -> style, Mesh -> None, 
            BaseStyle -> {FontSize -> 20, 
                FontFamily -> "Latin Modern Roman", Black}, 
            AxesLabel -> {"x", "y", "t"}, 
      FormatType -> TraditionalForm, 
            Axes -> True, AxesStyle -> {Black}, Boxed -> False, 
            LabelStyle -> Directive[Black, Bold]]], 
      controls = 
        Sequence @@ 
          Table[{vars[[i]], \[Lambda]1, \[Lambda]2}, {i, 1, 
              Length[vars]}]}, Manipulate[value, controls]]


compPlot[vars_, rang_, system1_, system2_] := 
  Module[{pl1, pl2}, pl1 = evalPlot[vars, rang, system1];
    pl2 = evalPlot2[vars, rang, system2];
    Show[pl1, pl2]]

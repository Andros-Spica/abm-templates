;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  <MODEL NAME>
;;  Copyright (C) <YEAR> <AUTHORS (EMAIL)>
;;  Based on the 'terrainBuilder' template by Andreas Angourakis (andros.spica@gmail.com), 2018
;;  available at https://www.github.com/Andros-Spica/abm-templates
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ coreBuilders coreBuilder ]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  ;;; default constants
  totalPatches
  totalUsefulPatches
  maxDist

  ;;; modified parameters
  maxPotential
  numCores
  potentialGr
  numNullBodies
  numNullPatches

  ;;; variables
  ;;;; auxiliar
  cores

  ;;;; counters and final measures
  totalPotential
  maxDistanceToCore
]

;;; agents variables

patches-own
[
  potential
  nearestCore
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-constants

  set-parameters

  setup-patches

  update-counters

  refresh-view

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.
  ; In this example, the constants depend on the size of the dimensions (x,y)
  set totalPatches count patches
  ; maximum distance
  set maxDist sqrt (((max-pxcor - min-pxcor) ^ 2) + ((max-pxcor - min-pxcor) ^ 2))

end

to set-parameters

  ; set random seed
  random-seed SEED

  parameters-check-1

  ;;; setup parameters depending on the type of experiment
  if (typeOfExperiment = "user-defined")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set maxPotential max_potential
    set numCores num_cores
    set potentialGr potential_decay_gradient
    set numNullBodies num_null_bodies
	  set numNullPatches round (num_null_patches * (count patches) / 100)
  ]
  if (typeOfExperiment = "random")
  [
    ;;; load parameters from user interface
    set maxPotential 1 + random max_potential
    set numCores 1 + random num_cores
    set potentialGr random-gamma 2 (1 / potential_decay_gradient)
    set numNullBodies random (1 + num_null_bodies)
	  set numNullPatches (count patches) * random (1 + num_null_patches) / 100
  ]

  parameters-check-2

end

to parameters-check-1

  ;;; initial parameter check (avoiding division per zero error
  check-par-is-positive "potential_decay_gradient" potential_decay_gradient

end

to parameters-check-2

  ;;; check number of null bodies/patches
  if (numNullBodies = 0) [ set numNullPatches 0 print "Warning: there is no null bodies, so the number of null patches will be set at 0" ]
  if (numNullPatches = 0) [ set numNullBodies 0 print "Warning: there is no null patches, so the number of null bodies will be set at 0" ]

end

to check-par-is-positive [ parName parValue ]

  if (parValue <= 0)
  [
    print (word "ERROR: " parName " must be greater than zero")
    stop
  ]

end

to setup-patches

  create-core-patches

  diffuse-potential

  create-null-patches

  set totalUsefulPatches count patches with [potential > 0]
  set totalPotential sum [potential] of patches

end

to create-core-patches

  ;;; create cores
  set cores nobody

  ; points
  if (layout-scenario = "point cores")
  [
    create-point-cores
  ]
  ; lines
  if (layout-scenario = "line cores")
  [
  	create-line-cores
  ]
  ; fractal
  ; get example from library

  ask cores
  [
  	set potential maxPotential
  ]

  ask patches
  [
    set nearestCore min-one-of patches with [ potential = maxPotential ] [distance myself]
  ]

  set maxDistanceToCore max [distance nearestCore] of patches

end

to create-point-cores

  let aCore nobody

  repeat numCores
  [
    ifelse (cores = nobody)
    [
      set aCore one-of patches
    ]
    [ set aCore one-of patches with [not member? self cores] ]
		set cores (patch-set cores aCore)
	]

end

to create-line-cores

  let aCore nobody

  repeat numCores
  [
    ifelse (cores = nobody)
    [
      set aCore one-of patches
    ]
    [ set aCore one-of patches with [not member? self cores] ]
		  set cores (patch-set cores aCore)
		  ask aCore [ sprout-coreBuilders 1 [ face one-of neighbors4 with [not member? self cores] ] ]
	]
  ask coreBuilders
  [
    repeat count patches
    [
      ifelse ( xcor > (min-pxcor) and ycor > (min-pycor) and xcor < (max-pxcor) and ycor < (max-pycor) )
      [
        forward 1
        set aCore patch-here
        set cores (patch-set cores aCore)
        rt 10 * (1 - random-float 2)
        ;face one-of neighbors4 with [not member? self cores]
      ]
      [
        die
      ]
    ]
  ]

end

to diffuse-potential

  ask patches
  [
  	let me self
	  let distanceToNearestCore distance min-one-of cores [distance me]
	  set potential maxPotential * get-value-in-gradient distanceToNearestCore potentialGr maxDist
  ]

end

to create-null-patches

  let aNullPatch nobody
  let nullPatches nobody
  ;;; create null bodies
  repeat numNullBodies
  [
		ifelse (nullPatches = nobody)
    [
      set aNullPatch one-of patches
    ]
    [
      set aNullPatch one-of patches with [not member? self nullPatches]
    ]
    ask aNullPatch [ set potential 0 ]
		set nullPatches (patch-set nullPatches aNullPatch)
  ]
  ;;; set null patches
  repeat (numNullPatches - numNullBodies)
  [
  	ask one-of patches with [not member? self nullPatches and any? neighbors4 with [potential = 0] ] [
      set potential 0
      set nullPatches (patch-set nullPatches self)
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-counters

  set totalPotential sum [potential] of patches

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view

  ask patches
  [
    ifelse (potential > 0)
    [
      let potLevel ((maxPotential - potential) / maxPotential)
      set pcolor 52 + 5 * potLevel
    ]
    [
      set pcolor 5
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to export-map

  let fname (word "run_" behaviorspace-run-number "_" ticks "_map_potential.png")
  export-view fname

  set fname (word "run_" behaviorspace-run-number "_" ticks "_map_potential.csv")
  file-open fname
  file-print (word "posX;posY;potential")
  ask patches [
    file-print (word pxcor ";" pycor ";" potential)
  ]
  file-close

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auxiliary math functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-value-in-gradient [ input gradient maximum ]

  report e ^ ( - input / ((gradient / 100) * maximum) )

end
@#$#@#$#@
GRAPHICS-WINDOW
306
10
739
444
-1
-1
10.63
1
12
1
1
1
0
0
0
1
0
39
0
39
1
1
1
ticks
30.0

BUTTON
201
89
256
122
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
247
288
304
325
patches
count patches
0
1
9

MONITOR
232
326
304
363
useful patches
totalUsefulPatches
0
1
9

INPUTBOX
9
269
76
329
num_cores
3.0
1
0
Number

SLIDER
8
231
263
264
potential_decay_gradient
potential_decay_gradient
0
100
16.0
1
1
% maxDist
HORIZONTAL

INPUTBOX
77
268
199
328
max_potential
20.0
1
0
Number

INPUTBOX
12
381
105
441
num_null_bodies
3.0
1
0
Number

CHOOSER
11
181
149
226
layout-scenario
layout-scenario
"point cores" "line cores"
1

SLIDER
116
379
275
412
num_null_patches
num_null_patches
0
100
10.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
19
158
169
178
Terrain builder
16
0.0
1

MONITOR
16
335
78
372
NIL
numCores
0
1
9

MONITOR
78
335
159
372
NIL
maxPotential
0
1
9

MONITOR
159
335
231
372
NIL
potentialGr
4
1
9

MONITOR
113
420
196
457
NIL
numNullBodies
0
1
9

MONITOR
196
420
285
457
NIL
numNullPatches
0
1
9

TEXTBOX
201
64
258
82
Controls
14
0.0
1

PLOT
303
446
743
566
distance to nearest core
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (ceiling maxDistanceToCore)\nset-histogram-num-bars 20" ";set-plot-y-range -0.01 (max (list countLandUseH countLandUseF))"
PENS
"pen" 1.0 1 -16777216 true "histogram [distance nearestCore] of patches" "histogram [distance nearestCore] of patches"

CHOOSER
8
39
149
84
typeOfExperiment
typeOfExperiment
"random" "user-defined"
1

TEXTBOX
8
19
166
38
Experiment configuration
14
0.0
1

INPUTBOX
48
90
110
150
seed
0.0
1
0
Number

MONITOR
198
471
292
508
NIL
maxDistanceToCore
4
1
9

MONITOR
210
515
276
552
NIL
totalPotential
4
1
9

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

invisible
true
0

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

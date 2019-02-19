;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  <MODEL NAME>
;;  Copyright (C) <YEAR> <AUTHORS (EMAIL)>
;;  Based on the 'Land Use' template by Andreas Angourakis (andros.spica@gmail.com)
;;  last update Feb 2019
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

extensions [vid]

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ groups group ]

breed [ pointers pointer ]

breed [ coreBuilders coreBuilder ]

breed [ labelpositions labelposition ]

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
  ;;;; general
  intGrowth
  extGrowth
  initLandUse
  initGroups

  ;;;; spatial relations
  disPenGr
  maxDistInitGroupsToNearestCore
  maxDistBetweenInitGroups

  ;;;; land use potential
  maxPotential
  numCores
  potentialGr
  numNullBodies
  numNullPatches

  ;;;; group dynamics
  effectivenessGr
  maxGroupChangeRate

  ;;; variables
  ;;;; auxiliar
  display_modeLabelLimit
  cores
  defender
  contender
  indepOfCont
  index_of_opportunity
  ratio_of_intensities
  incentives_to_relinquish

  ;;;; counters and final measures
  landUsePotential
  landUseIntensity
  numberGroups
  competitions
  landUseChangeEvents
  meanGroupSize
  bigGroupSize
  intensiveGroupSize
  meanGroupIntensity
  intensiveGroupIntensity
  vastGroupIntensity
  meanGroupEffectiveness
  bigGroupEffectiveness
  maxDistanceToCore
  meanDistanceToNearestCore
]

;;; agents variables

groups-own
[
  flag
  groupSize groupEffectiveness
  groupIntensity groupIntensity

  ;;;; auxiliar
  groupDemand
  groupDemandRemain
]

patches-own
[
  potential nearestCore intensity
  landUse myGroup

  ;;;; auxiliar
  contenders
]

pointers-own [ group-helper ]

labelpositions-own [ name ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-constants

  set-parameters

  setup-patches

  setup-groups

  setup-display-labels

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
    ;;; load parameters from user interface
    ; general
    set intGrowth intrinsic_growth_rate
    set extGrowth extrinsic_growth_rate
    set initLandUse round ( init_%landUse * (count patches) / 100)
	  set initGroups initial_number_groups
    ; spatial relations
    set disPenGr distance_penalty_gradient
    set maxDistInitGroupsToNearestCore (max_%distance_initial_groups_core * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    set maxDistBetweenInitGroups (max_%distance_between_initial_groups * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    ; land use potential
    set maxPotential max_potential
    set numCores num_cores
    set potentialGr potential_decay_gradient
    set numNullBodies num_null_bodies
	  set numNullPatches round (num_null_patches * (count patches) / 100)
    set effectivenessGr effectiveness_gradient
    set maxGroupChangeRate max_group_change_rate
  ]
  if (typeOfExperiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    ; general
    set intGrowth intrinsic_growth_rate
    set extGrowth extrinsic_growth_rate
    set initLandUse round ( (random-float init_%landUse) * (count patches) / 100)
	  set initGroups 1 + random initial_number_groups
	  ; spatial relations
    set disPenGr 0.01 + random-float 99.99 ; random-gamma 2 (1 / distance_penalty_gradient)
    set maxDistInitGroupsToNearestCore 25 + random (max_%distance_initial_groups_core - 25)
    set maxDistBetweenInitGroups 20 + random (max_%distance_between_initial_groups - 20)
    set maxDistInitGroupsToNearestCore (maxDistInitGroupsToNearestCore * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    set maxDistBetweenInitGroups (maxDistBetweenInitGroups * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
	  ; land use potential
    set maxPotential 1 + random max_potential
    set numCores 1 + random num_cores
    set potentialGr random-gamma 2 (1 / potential_decay_gradient)
    set numNullBodies random (1 + num_null_bodies)
	  set numNullPatches (count patches) * random (1 + num_null_patches) / 100
    ; group dynamics
    set effectivenessGr 0.01 + random-float 99.99
    set maxGroupChangeRate random-float max_group_change_rate
  ]
  if (typeOfExperiment = "defined by expNumber")
  [
    ;load-experiment
  ]

  parameters-check-2

end

to parameters-check-1

  ;;; check if values were reset to 0
  ;;; and set default values
  if (endSimulation = 0)                        [ set endSimulation                      1000 ]
  if (init_%landUse = 0)                        [ set init_%landUse                        20 ]
  if (initial_number_groups = 0)                [ set initial_number_groups                 5 ]
  if (intrinsic_growth_rate = 0)                [ set intrinsic_growth_rate                 0.01 ]
  if (extrinsic_growth_rate = 0)                [ set extrinsic_growth_rate                 0.001 ]
  if (max_%distance_initial_groups_core = 0)    [ set max_%distance_initial_groups_core    33 ]
  if (max_%distance_between_initial_groups = 0) [ set max_%distance_between_initial_groups 33 ]
  if (num_cores = 0)                            [ set num_cores                             5 ]
  if (max_potential = 0)                        [ set max_potential                        10 ]
  if (potential_decay_gradient = 0)             [ set potential_decay_gradient             20 ]
  if (num_null_bodies = 0)                      [ set num_null_bodies                       3 ]
  if (num_null_patches = 0)                     [ set num_null_patches                     20 ]
  if (distance_penalty_gradient = 0)            [ set distance_penalty_gradient            20 ]
  if (effectiveness_gradient = 0)               [ set effectiveness_gradient               20 ]
  if (max_group_change_rate = 0)                [ set max_group_change_rate                 3 ]

  ;;; initial parameter check (avoiding division per zero error
  check-par-is-positive "distance_penalty_gradient" distance_penalty_gradient
  check-par-is-positive "potential_decay_gradient" potential_decay_gradient

end

to parameters-check-2

  ;;; check number of initial centers and land use units
  if (initLandUse >= count patches) [print "ERROR: initial number of land use units is too high compare to the number of patches" stop ]
  if (initGroups >= count patches) [print "ERROR: initial number of centers is too high compare to the number of patches" stop ]
  if (initGroups >= (count patches) - numNullPatches ) [print "ERROR: initial number of centers is too high compare to the number of useful patches (too many null patches)" stop ]

  if (initLandUse < 3 * initGroups) [print "Warning: initial number of centers is too high compare to the initial number of land use units. Some groups will be initialised empty" ]
  if (initGroups = 0) [ set initLandUse 0 print "Warning: there are no groups, so the initial proportion of land use will be set at 0" ]
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

end

to create-core-patches

  ;;; create cores
  set cores (patch-set)

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
    ifelse (not any? cores)
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
    ifelse (not any? cores)
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
	  set potential maxPotential * e ^ ( - distanceToNearestCore / potentialGr)
  ]

end

to create-null-patches

  let aNullPatch nobody
  let nullPatches (patch-set)
  ;;; create null bodies
  repeat numNullBodies
  [
		ifelse (not any? nullPatches)
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

to setup-groups

  ;;; create agents and set land use according to the parameter setting

  setup-centres

  setup-landUse

  update-groups

end

to setup-centres

  ;;; centers:
  let candidates nobody
  ask patches
  [
  	let me self
	  let distanceToNearestCore distance min-one-of cores [distance me]
	  if ( distanceToNearestCore <= maxDistInitGroupsToNearestCore ) [ set candidates (patch-set candidates me) ]
  ]
  if (initGroups >= count candidates) [print "ERROR: initial number of centers is too high compare to the allowed distance to cores" stop ]
  repeat initGroups
  [
    if (any? groups)
    [
      ask groups [
        set candidates candidates in-radius maxDistBetweenInitGroups
        ]
    ]
    if (not any? candidates with [ not any? turtles-here]) [print "ERROR: initial number of centers is too high compare to the allowed distance between groups" stop ]
    ask one-of candidates with [ not any? turtles-here ]
    [
      sprout-groups 1 [ set-flag ]
    ]
    ;;; visualize distance constrictions (activate the following line and set slow speed)
    ; ask candidates [set pcolor pcolor + 1]
  ]

end

to set-flag

  set flag random-float 140
  while [remainder flag 10 < 2 or remainder flag 10 > 8] [ set flag random-float 140 ]

end

to setup-LandUse

  ;;; land use:
  let numLandUse 0

  ask patches [ set intensity 0 set landUse false set myGroup nobody set contenders (turtle-set) ]

  if (any? groups)[
    repeat (initLandUse)
    [
      ask one-of groups
      [
        if (numLandUse < initLandUse)
        [
          ask min-one-of patches with [ landUse = false and potential > 0 ] [distance myself / potential]
          [
            set intensity intensity + 1
            set landUse true
            set myGroup myself
            set numLandUse numLandUse + 1
          ]
        ]
      ]
    ]
  ]

end

to setup-display-labels

  ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.97) ) (min-pycor + round ((max-pycor - min-pycor) * 0.97) )
  [
    sprout-labelpositions 1 [ set name "display mode" set label display_mode set shape "invisible" ]
  ]
  ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.97) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
  [
    sprout-labelpositions 1 [ set name "time" set label "time: 0" set shape "invisible" ]
  ]
  ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.25) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
  [
    sprout-labelpositions 1 [ set name "farming" set shape "invisible" ]
  ]
  ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.83) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
  [
    sprout-labelpositions 1 [ set name "group" set shape "invisible" ]
  ]
  ask labelpositions [ set label-color 8 ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  reset-counters

  growth

  landUse-expansion

  check-competition

  change_groups

  update-counters

  refresh-view

  tick

  if (display-labels = true) [ ask labelpositions with [ name = "time" ] [ set label (word "time: " ticks) ] ]
  if (ticks = endSimulation) [stop]

end

to reset-counters

  ;;; counters are used througout the procedures, so they must be set to 0 at the beggining of each step
  set competitions 0
  set landUseChangeEvents 0

  ask groups [ set groupDemand 0 ]

end

to growth

  ;;; Intrinsic Demand
  ask groups with [groupIntensity > 0]
  [
    set groupDemand 0
    let myLand patches with [landUse = true and myGroup = myself]
    ask myLand
    [
      let effGroupIntensity round (diminishing-returns intensity potential)
      repeat effGroupIntensity
      [
        if ( random-float 1 <= intGrowth )
        [
          ask myGroup [ set groupIntensity groupIntensity + 1 set groupDemand groupDemand + 1 ]
        ]
      ]
    ]
  ]

  ;;; Extrinsic Demand
  let ext (round (extGrowth * sum [potential - intensity] of patches) )
  repeat ext
  [
    ask one-of groups
    [
      set groupIntensity groupIntensity + 1
      set groupDemand groupDemand + 1
    ]
  ]

end

to landUse-expansion

  let growingGroups groups with [ groupDemand > 0 ]
  ask growingGroups [ set groupDemandRemain groupDemand ]

  repeat sum [groupDemandRemain] of growingGroups
  [
    ask one-of growingGroups with [groupDemandRemain > 0]
    [
      let me self
      let myLand patches with [landUse = true and myGroup = myself]
      let availableLand patches with [ myGroup = nobody and potential > 0 ]
      let othersLand patches with [ myGroup != me and potential > 0 ]

      ifelse ( any? availableLand )
      [
        ;;; one of the nearest, most productive, available land is used
        ask min-one-of availableLand [ distance me / potential ]
        [
          if (landUse = false) [ set landUseChangeEvents landUseChangeEvents + 1 ]
          set myGroup me set landUse true set intensity 1
        ]
      ]
      [
        ;;; if the territory is saturated
        ifelse ( any? othersLand )
        [
          ;;; if there are other groups
          ;;; the group presses for extending over one of the nearest and most productive lands of other groups
          let aPatch min-one-of othersLand [ distance me / potential ]
          if ([landUse] of aPatch = true)
          [
            ask aPatch [ set contenders (turtle-set contenders me) ]
          ]
        ]
        [
          ;;; the group can't extend any more
          ;;; instead, units are concentrated within the land already used
          ifelse (any? myLand)
          [
            ask min-one-of myLand [intensity / potential] [ set intensity intensity + 1 ]
          ]
          [
            ;;; if the group do not own any land unit (stakeholder comes from extrinsic growth), the unit do not apply
            set groupIntensity groupIntensity - 1
          ]
        ]
      ]
      set groupDemandRemain groupDemandRemain - 1
    ]
  ]

end

to check-competition

  ask patches with [ landUse = true and any? contenders ]
  [
    ;;; the center assigned is the one that is effectively using the land
    set defender myGroup
    repeat count contenders
    [
      set competitions (competitions + 1)
      set contender one-of contenders
      ;;; remove contender from the respective contenders agent-set
      set contenders contenders with [self != contender]
      resolve-competition
    ]
  ]

  update-groups

end

to resolve-competition

  ;;; set conditions of competition

  ; define intensities
  let supportDef ( [groupIntensity * groupEffectiveness] of defender ) * get-value-in-gradient (distance defender) disPenGr maxDist
  let supportCon ( [groupIntensity * groupEffectiveness] of contender ) * get-value-in-gradient (distance contender) disPenGr maxDist
;  print (word "supportDef: " supportDef " ; supportCon: " supportCon)
  ;;; a contender is the one attempting to expand, thus it is the one to make a informed decision
  set ratio_of_intensities  (supportCon /(supportCon + supportDef))

;  print (word defender " vs " contender " in " [patch-here] of defender )

  ;;; Does the competitive situation evolves into land use change event?
  ifelse ( random-float 1 < ratio_of_intensities)
  [
    ;;; extending whichever land use is encouraged
    set myGroup contender
    set intensity 1

    set landUse true
    ;;; Hence, there is land use change
    set landUseChangeEvents landUseChangeEvents + 1
  ]
  [
    ;;; extending whichever land use is discouraged
    ;;; instead, units are concentrated within the land already used
    let myLand patches with [landUse = true and myGroup = contender]
    ifelse (any? myLand)
    [
      ask min-one-of myLand [intensity] [ set intensity intensity + 1 ]
    ]
    [
      ;;; if the group do not own any land unit with farming (stakeholder comes from extrinsic growth or farming lands were lost), the unit do not apply
      ask contender [ set groupIntensity groupIntensity - 1 ]
    ]
  ]

end

to change_groups

  ask groups [
    ;;; each patch of a group will assess their will (maxGroupChangeRate)
    ;;; and their freedom, which is inversaly related to the group influence (1 - ([groupIntensity * groupEffectiveness] of myGroup) *  get-value-in-gradient (distance myGroup) disPenGr maxDist ) )
    ;;; to change groups, possibly forming a new group
    let me self
    let myLand patches with [myGroup = me]
    let newGroupPatches (patch-set)
    ask myLand [
      if ( random-float 1 < maxGroupChangeRate * (1 - ([groupIntensity * groupEffectiveness] of myGroup) * get-value-in-gradient (distance myGroup) disPenGr maxDist))
      [
        set newGroupPatches (patch-set newGroupPatches self)
      ]
    ]
    if (any? newGroupPatches) [
      ;;; if there are any patches escaping this group...
      ;;; the viability of the possible new group is calculated for each patch and compared to the most influent group
      let intensityNewGroup (sum [intensity] of newGroupPatches)
      let effNewGroup get-value-in-gradient (sum [intensity] of newGroupPatches) effectivenessGr (sum [potential] of patches)
      let newCenter patch (round mean [pxcor] of newGroupPatches) (round mean [pycor] of newGroupPatches)
      let newGroup nobody
      ask newGroupPatches [
        let thisPatch self
        let influenceNewGroup intensityNewGroup * effNewGroup * (e ^ ( - (distance newCenter) / (disPenGr * maxDist) ) )
        let mostInfluentGroup max-one-of groups [groupIntensity * groupEffectiveness * ( e ^ ( - (distance thisPatch) / (disPenGr * maxDist) ) )]
        let influenceOtherGroup [groupIntensity * groupEffectiveness * ( e ^ ( - (distance thisPatch) / (disPenGr * maxDist) ) )] of mostInfluentGroup
        ifelse (influenceOtherGroup > influenceNewGroup) [
          set myGroup mostInfluentGroup
        ]
        [
          if (newGroup = nobody) [
            sprout-groups 1 [ set-flag set newGroup self ]
          ]
          set myGroup newGroup
        ]
      ]
    ]
  ]
  update-groups

end

to update-groups

  ask groups
  [
    let me self

    set groupSize count patches with [ myGroup = me ]

    set groupIntensity sum [intensity] of patches with [myGroup = myself and landUse = true]

    set groupEffectiveness get-value-in-gradient groupIntensity effectivenessGr (sum [potential] of patches)
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-counters

  set numberGroups count groups with [groupSize > 0]

  set landUsePotential sum [potential] of patches
  set landUseIntensity sum [intensity] of patches with [ landUse = true]

  if (any? groups)[
    set meanGroupSize mean [[groupSize] of myGroup] of patches with [ landUse = true ]
    set bigGroupSize [groupSize] of max-one-of groups [groupSize]
    set intensiveGroupSize [groupSize] of max-one-of groups [groupIntensity]
    set meanGroupIntensity mean [[groupIntensity] of myGroup] of patches with [ landUse = true ]
    set intensiveGroupIntensity [groupIntensity] of max-one-of groups [groupIntensity]
    set vastGroupIntensity [groupIntensity] of max-one-of groups [groupSize]

    set meanGroupEffectiveness mean [[groupEffectiveness] of myGroup] of patches with [ landUse = true ]
    set bigGroupEffectiveness [groupEffectiveness] of max-one-of groups [groupIntensity]

    set meanDistanceToNearestCore mean [ distance nearestCore ] of patches with [ landUse = true ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view

  set display_modeLabelLimit 0

  refresh-to-display-mode

  refresh-group-territory-pointers

  refresh-group-centres

  refresh-display-labels

end

to refresh-to-display-mode

  ;;; set patch color depending on the display mode selector
  if (display_mode = "land use potential")
  [
    ask patches [
      let potLevel ((maxPotential - potential) / maxPotential)
      if ( landUse = true )
      [
        set pcolor 52 + 5 * potLevel
      ]
      if ( landUse = false )
      [
        set pcolor 32 + 5 * potLevel
        if (potential = 0) [ set pcolor 5 ]
      ]
    ]
    set display_modeLabelLimit 0.7
  ]
  if (display_mode = "land use intensity")
  [
    if (any? patches with [myGroup != nobody])
    [
      let maxPatchIntensity max [intensity] of patches
      ask patches with [myGroup != nobody]
      [
        let strength intensity / maxPatchIntensity
        set pcolor (79 - (1 - strength) * 9 )
        if ( landUse = "N" ) [ set pcolor 10 ]
      ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.7
  ]
  if (display_mode = "group intensity")
  [
    if (any? patches with [myGroup != nobody])
    [
      let maxGroupIntensity max [groupIntensity] of groups
      ask patches with [myGroup != nobody]
      [
        let strength ( [groupIntensity] of myGroup ) / maxGroupIntensity
        set pcolor (119 - (1 - strength) * 9 )
        if ( landUse = "N" ) [ set pcolor 10 ]
      ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.74
  ]
  if (display_mode = "group effectiveness")
  [
    if (any? patches with [myGroup != nobody])
    [
      let maxEffectiveness max [groupEffectiveness] of groups
      ask patches with [myGroup != nobody]
      [
        let strength ( [groupEffectiveness] of myGroup ) / maxEffectiveness
        set pcolor (129 - (1 - strength) * 9 )
        if ( landUse = "N" ) [ set pcolor 10 ]
      ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.68
  ]
  if (display_mode = "distance penalization")
  [
    if (any? patches with [myGroup != nobody])
    [
      let maxDistPen max [get-value-in-gradient (distance myGroup) disPenGr maxDist] of patches with [myGroup != nobody]
      ask patches with [myGroup != nobody]
      [
        let strength get-value-in-gradient (distance myGroup) disPenGr maxDist / maxDistPen
        set pcolor (89 - (1 - strength) * 9 )
        if ( landUse = "N" ) [ set pcolor 10 ]
      ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.68
  ]
  if (display_mode = "group strength (effec*intens*distPen)") [
    if (any? patches with [myGroup != nobody])
    [
      let maxStrength max [( [groupIntensity * groupEffectiveness] of myGroup ) * ( e ^ ( - (distance myGroup) / (disPenGr * maxDist) ) )] of patches with [myGroup != nobody]
      ask patches with [myGroup != nobody]
      [
        let strength ( [groupIntensity * groupEffectiveness] of myGroup ) * ( e ^ ( - (distance myGroup) / (disPenGr * maxDist) ) ) / maxStrength
        set pcolor (19 - (1 - strength) * 9 )
        if ( landUse = "N" ) [ set pcolor 10 ]
      ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.44
  ]
  if (display_mode = "group territory")
  [
    if (any? groups)
    [
      ask patches with [myGroup != nobody] [ set pcolor [flag] of myGroup ]
    ]
    if (any? patches with [myGroup = nobody]) [ ask patches with [myGroup = nobody] [ set pcolor brown ] ]
    set display_modeLabelLimit 0.76
  ]

end

to refresh-group-territory-pointers

  ;;; mark group territory using the "pointers" agents
  ifelse (mark-territory? = true)
  [
    if (any? patches with [myGroup != nobody])
    [
      ask patches with [myGroup != nobody]
      [
        ifelse ( landUse != "N" and not any? groups-here )
        [
          ifelse (any? pointers-here)
          [
            ask one-of pointers-here [ face [myGroup] of patch-here ]
          ]
          [
            sprout-pointers 1
            [
              set color black
              set shape "line"
              set size 0.5
              face [myGroup] of patch-here
            ]
          ]
        ]
        [
          if (any? pointers-here) [ ask pointers-here [die] ]
        ]
      ]
    ]
    ask patches with [myGroup = nobody] [ ask pointers-here [die] ]
  ]
  [
    if (any? pointers) [ ask pointers [ die ] ]
  ]

  ;;; hide pointers that overlap with group centres
  if (any? groups)
  [
    ask groups [
      if (any? pointers-here) [ ask pointers-here [die] ]
      ifelse ( groupSize = 0 ) [ set shape "x" set color 5 set size 1 ] [
         set shape "house" set color 25 set size 2
      ]
      set hidden? false
    ]
  ]
  if (any? pointers with [ group-helper = true ])
  [
    ask pointers with [ group-helper = true ] [ if (not any? groups-here) [die] ]
  ]

end

to refresh-group-centres

  ;;; show/hide group centres
  ifelse ( hide-centers? = true )
  [
    if (any? groups) [ ask groups [ set hidden? true ] ]
    if (any? pointers with [ group-helper = true ])
    [ ask pointers with [ group-helper = true ] [ set hidden? true ] ]
  ]
  [
    if (any? groups) [ ask groups [ set hidden? false ] ]
    if (any? pointers with [ group-helper = true ])
    [ ask pointers with [ group-helper = true ] [ set hidden? false ] ]
  ]

end

to refresh-display-labels

  ;;; show/hide extra display labels
  ifelse (display-labels = true)
  [
    ask patches with [ (pycor > (min-pycor + round ((max-pycor - min-pycor) * 0.95)) - 1) and (pxcor > min-pxcor + round ((max-pxcor - min-pxcor) * display_modeLabelLimit)) ]
    [
      set pcolor black
      if (any? groups-here = true) [ ask groups-here [ set hidden? true ] ]
      if (any? pointers-here with [ group-helper = true ])
      [ ask pointers-here with [ group-helper = true ] [ set hidden? true ] ]
    ]
    ask labelpositions with [ name = "display mode" ] [ set label display_mode ]
    let bigGroupSizeLabel ""
    if (bigGroupSize != "")
    [ set bigGroupSizeLabel (precision (100 * bigGroupSize / totalUsefulPatches) 2) ]
    let intensiveGroupIntensityLabel ""
    if (intensiveGroupIntensity != "") [ set intensiveGroupIntensityLabel (precision intensiveGroupIntensity 2) ]
    ask patches with [ pycor < (min-pycor + round ((max-pycor - min-pycor) * 0.03)) + 1 ]
    [
      set pcolor black
      if (any? groups-here = true) [ ask groups-here [ set hidden? true ] ]
      if (any? pointers-here with [ group-helper = true ])
      [ ask pointers-here with [ group-helper = true ] [ set hidden? true ] ]
    ]
    ask labelpositions with [ name = "landUse" ]
    [ set label (word "Intensity (%): " (precision (100 * landUseIntensity / landUsePotential) 2) ) ]
    ask labelpositions with [ name = "group" ]
    [ set label (word "max. group size (%): " bigGroupSizeLabel ", intensity: " intensiveGroupIntensityLabel) ]
  ]
  [
    ask labelpositions with [ name = "display mode" ] [ set label "" ]
    ask labelpositions with [ name = "time" ] [ set label "" ]
    ask labelpositions with [ name = "farming" ] [ set label "" ]
    ask labelpositions with [ name = "group" ] [ set label "" ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to export-map

  let fname (word "run_" behaviorspace-run-number "_" ticks "_map.png")
  export-view fname

;  set fname (word "run_" behaviorspace-run-number "_" ticks "_map_landuse")
;  gis:set-transformation (list min-pxcor max-pxcor min-pycor max-pycor) (list min-pxcor max-pxcor min-pycor max-pycor)
;  let landscape gis:patch-dataset pcolor
;  gis:store-dataset landscape fname

  set fname (word "run_" behaviorspace-run-number "_" ticks "_map_landuse.csv")
  file-open fname
  file-print (word "posX;posY;landUse;center;centX;centY")
  ask patches [
    file-print (word pxcor ";" pycor ";" landUse ";" myGroup ";" [xcor] of myGroup ";" [ycor] of myGroup)
  ]
  file-close

;  set fname (word "run_" behaviorspace-run-number "_" ticks "_map_Fcenters")
;  set landscape gis:turtle-dataset FarmingCenters
;  gis:store-dataset landscape fname
;
;  set fname (word "run_" behaviorspace-run-number "_" ticks "_map_Hcenters")
;  set landscape gis:turtle-dataset HerdingCenters
;  gis:store-dataset landscape fname

end

to load-experiment

  ;;; this procedure loads the values of each (explored) parameter from a csv file placed in local folder called "exp".
  ;;; Note that the setup will use the value set by the user for any parameter not included here.

  let FilePath "exp//"
  let filename (word FilePath "exp_" expNumber ".csv")
  file-open filename
  while [not file-at-end?]
  [
    set intGrowth file-read
    set extGrowth file-read
    set initLandUse file-read
    set initGroups file-read

    ; spatial relations
    set disPenGr file-read
    set maxDistInitGroupsToNearestCore file-read
    set maxDistBetweenInitGroups file-read

    ; land use potential
    set maxPotential file-read
    set numCores file-read
    set potentialGr file-read
    set numNullBodies file-read
    set numNullPatches file-read

    ; group dynamics
    set effectivenessGr file-read
    set maxGroupChangeRate file-read

    set endSimulation file-read
  ]
  file-close

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; movie generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to generate-animation

  setup
  vid:start-recorder
  repeat endSimulation [ go vid:record-view ]
  vid:save-recording  (word "run_" behaviorspace-run-number ".mov")
  vid:reset-recorder

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auxiliary math functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report diminishing-returns [ inputValue scaleNumber ]

    let factor inputValue / scaleNumber
    let pos (sqrt(8 * factor + 1) - 1) / 2
    let output pos * scaleNumber

    report output

end

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
239
35
294
68
NIL
setup
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
241
76
296
109
NIL
go
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
0

BUTTON
241
109
296
142
NIL
go
T
1
T
OBSERVER
NIL
3
NIL
NIL
0

SLIDER
6
621
231
654
intrinsic_growth_rate
intrinsic_growth_rate
0
0.1
0.01
0.001
1
NIL
HORIZONTAL

PLOT
741
34
996
157
land use
ticks
patches
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"potential" 1.0 0 -16777216 true "plot landUsePotential" "plot landUsePotential"
"intensity" 1.0 0 -14439633 true "plot landUseIntensity" "plot landUseIntensity"

PLOT
739
158
1032
392
events
ticks
events
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"competitions" 1.0 0 -2064490 true "" "plot competitions"
"landUseChangeEvents" 1.0 0 -5825686 true "" "plot landUseChangeEvents"

SLIDER
6
654
231
687
extrinsic_growth_rate
extrinsic_growth_rate
0
0.1
0.001
0.001
1
NIL
HORIZONTAL

PLOT
741
416
901
536
group size
NIL
frequency
0.0
10.0
0.0
10.0
false
false
"set-plot-x-range 0 (bigGroupSize + 1)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups" "set-plot-x-range 0 (bigGroupSize + 1)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups"
PENS
"default" 1.0 1 -16050907 true "histogram [groupSize] of groups with [groupSize > 0]" "histogram [groupSize] of groups with [groupSize > 0]"

PLOT
902
416
1062
536
group intensity
NIL
frequency
0.0
10.0
0.0
10.0
false
false
"set-plot-x-range 0 (intensiveGroupIntensity + 1)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups" "set-plot-x-range 0 (intensiveGroupIntensity + 1)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups"
PENS
"default" 1.0 1 -11783835 true "histogram [groupIntensity] of groups with [groupSize > 0]" "histogram [groupIntensity] of groups with [groupSize > 0]"

INPUTBOX
4
385
125
445
initial_number_groups
5.0
1
0
Number

MONITOR
231
618
285
655
NIL
intGrowth
4
1
9

MONITOR
231
654
285
691
NIL
extGrowth
4
1
9

MONITOR
995
34
1052
71
patches
count patches
0
1
9

MONITOR
996
71
1068
108
useful patches
totalUsefulPatches
0
1
9

MONITOR
742
556
902
593
NIL
meanGroupSize
2
1
9

MONITOR
904
557
1063
594
meanGroupIntensity
meanGroupIntensity
2
1
9

INPUTBOX
4
325
82
385
init_%landUse
10.0
1
0
Number

SLIDER
368
666
606
699
distance_penalty_gradient
distance_penalty_gradient
0.01
100
10.0
0.01
1
% maxDist
HORIZONTAL

SLIDER
8
452
226
485
max_%distance_initial_groups_core
max_%distance_initial_groups_core
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
8
521
281
554
max_%distance_between_initial_groups
max_%distance_between_initial_groups
20
100
50.0
1
1
NIL
HORIZONTAL

CHOOSER
6
178
225
223
display_mode
display_mode
"land use potential" "land use intensity" "group territory" "group intensity" "group effectiveness" "distance penalization" "group strength (effec*intens*distPen)"
0

BUTTON
231
184
307
217
refresh view
refresh-view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
604
665
661
702
NIL
disPenGr
2
1
9

SWITCH
7
230
148
263
mark-territory?
mark-territory?
0
1
-1000

SWITCH
6
263
147
296
hide-centers?
hide-centers?
1
1
-1000

MONITOR
9
555
132
592
NIL
maxDistBetweenInitGroups
17
1
9

MONITOR
8
484
157
521
NIL
maxDistInitGroupsToNearestCore
17
1
9

SLIDER
345
718
540
751
max_group_change_rate
max_group_change_rate
0
100
10.0
0.01
1
%
HORIZONTAL

INPUTBOX
316
475
383
535
num_cores
3.0
1
0
Number

SLIDER
515
512
718
545
potential_decay_gradient
potential_decay_gradient
0
100
12.0
1
1
NIL
HORIZONTAL

INPUTBOX
384
474
506
534
max_potential
20.0
1
0
Number

INPUTBOX
308
592
401
652
num_null_bodies
3.0
1
0
Number

MONITOR
83
337
150
374
NIL
initLandUse
0
1
9

MONITOR
126
400
191
437
NIL
initGroups
0
1
9

CHOOSER
545
466
683
511
layout-scenario
layout-scenario
"point cores" "line cores"
1

SLIDER
401
605
560
638
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
326
448
476
468
Terrain builder
16
0.0
1

TEXTBOX
8
601
158
619
Growth rates
14
0.0
1

TEXTBOX
5
308
155
326
Initial conditions
14
0.0
1

SLIDER
10
719
255
752
effectiveness_gradient
effectiveness_gradient
0.01
100
20.0
0.01
1
% totalPatches
HORIZONTAL

MONITOR
256
716
336
753
NIL
effectivenessGr
0
1
9

MONITOR
540
717
648
754
NIL
maxGroupChangeRate
4
1
9

MONITOR
323
541
385
578
NIL
numCores
0
1
9

MONITOR
385
541
466
578
NIL
maxPotential
0
1
9

MONITOR
574
547
646
584
NIL
potentialGr
4
1
9

MONITOR
563
604
646
641
NIL
numNullBodies
0
1
9

MONITOR
646
604
735
641
NIL
numNullPatches
0
1
9

TEXTBOX
15
698
165
716
Group dynamics
14
0.0
1

MONITOR
995
112
1066
149
NIL
numberGroups
0
1
9

PLOT
1067
10
1274
150
number of groups
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "plot numberGroups" "plot numberGroups"

PLOT
1071
416
1231
536
group effectiveness
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 1.01\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups" "set-plot-x-range 0 1.01\nset-histogram-num-bars 10\nset-plot-y-range -0.01 numberGroups"
PENS
"default" 1.0 1 -16777216 true "histogram [groupEffectiveness] of groups with [groupSize > 0]" "histogram [groupEffectiveness] of groups with [groupSize > 0]"

MONITOR
1072
557
1232
594
NIL
meanGroupEffectiveness
4
1
9

TEXTBOX
748
396
898
414
group-level histograms
14
0.0
1

TEXTBOX
743
537
1118
557
group-level mean variables weighted by extent (patches)
14
0.0
1

MONITOR
1074
594
1232
631
NIL
bigGroupEffectiveness
4
1
9

MONITOR
904
594
1063
631
NIL
intensiveGroupIntensity
0
1
9

MONITOR
742
593
902
630
NIL
bigGroupSize
0
1
9

MONITOR
904
631
1063
668
NIL
vastGroupIntensity
0
1
9

MONITOR
742
630
902
667
NIL
intensiveGroupSize
0
1
9

TEXTBOX
239
10
296
28
Controls
14
0.0
1

PLOT
1038
158
1270
278
distance to nearest core
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 0 (ceiling maxDistanceToCore)\nset-histogram-num-bars 20" ""
PENS
"centres" 1.0 1 -7500403 true "histogram [[distance nearestCore] of patch-here] of groups" "histogram [[distance nearestCore] of patch-here] of groups"
"patches" 1.0 1 -12087248 true "histogram [distance nearestCore] of patches" "histogram [distance nearestCore] of patches"

MONITOR
1075
285
1227
322
NIL
meanDistanceToNearestCore
4
1
9

CHOOSER
8
39
149
84
typeOfExperiment
typeOfExperiment
"random" "user-defined" "defined by expNumber"
1

TEXTBOX
746
12
896
30
main global variables
14
0.0
1

MONITOR
914
253
1019
290
landUseChangeEvents
landUseChangeEvents
0
1
9

MONITOR
928
214
1000
251
competitions
competitions
0
1
9

SWITCH
160
230
289
263
display-labels
display-labels
1
1
-1000

INPUTBOX
7
84
88
144
endSimulation
1000.0
1
0
Number

INPUTBOX
88
84
154
144
expNumber
0.0
1
0
Number

TEXTBOX
8
19
166
38
Experiment configuration
14
0.0
1

TEXTBOX
84
154
234
172
Display settings
14
0.0
1

INPUTBOX
154
84
216
144
seed
324.0
1
0
Number

BUTTON
151
267
285
300
record simulation (movie)
generate-animation
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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

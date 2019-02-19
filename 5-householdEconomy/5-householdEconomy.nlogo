;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  <MODEL NAME>
;;  Copyright (C) <YEAR> <AUTHORS (EMAIL)>
;;  Based on the 'landUseABM' template by Andreas Angourakis (andros.spica@gmail.com), 2018
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

breed [ households household ]

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
  stockStepsToExpire
  maturityAge                     ; defaults to 15 years old; it affects the minimum age acceptable for individuals to keep a household without older individuals

  ;;; demography tables
  fertilityTable
  nuptialityTable-women nuptialityTable-men
  mortalityTable-women mortalityTable-men

  ;;; modified parameters
  ;;;; initial conditions
  initGroups
  maxDistInitGroupsToNearestCore
  maxDistBetweenInitGroups
  initialNumHouseholds
  householdInitialAgeDistribution ; (list minimum maximum)
  maxCoupleCountDistribution      ; (list minimum maximum)

  excessNutritionDimReturns

  ;;;; spatial relations
  disPenGr

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

  ;;;; demography
  ;;;; these are lists of string-coded individuals
  womenToMarry                   ; single individuals selected to marry ("<household who> <member index>")
  menToMarry
  orphanList                     ; orphan children left without adults ("<sex> <age>")

  ;;;; land use
  cores
  defender
  contender
  indepOfCont
  index_of_opportunity
  ratio_of_intensities
  incentives_to_relinquish

  ;;;; counters and final measures

  ;;;;; deomgraphy
  totalHouseholds                ; count households (integer)
  totalIndividuals               ; sum of members of households (integer)
  totalPopulationGrowth          ; % of last totalIndividuals
  totalWomen                     ; sum of female members of households (integer)
  totalMen                       ; sum of male members of households (integer)
  femaleRatio                    ; total number of women over total number of individuals (float)
  womenAgeStructure              ; merge list of all households female members ages (list of integers)
  menAgeStructure                ; merge list of all households male members ages (list of integers)
  womenFirstAgeGroup             ; count of women with age from 0 to 4 years old (base of typical population pyramid) (integer)
  menFirstAgeGroup               ; count of men with age from 0 to 4 years old (base of typical population pyramid) (integer)
  womenBirths                    ; number of births (integer)
  menBirths
  womenDeaths                    ; number of deaths (integer)
  menDeaths
  womenIn                        ; number of individuals entering the system (generated for couple creation with external population) (integer)
  menIn
  womenOut                       ; number of individuals exiting the system (due to couple creation with external population) (integer)
  menOut
  totalOrphans                   ; number of children moving from an household where all adults are dead

  ;;;;; land use
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

households-own
[
  hh_myGroup
  hh_labour ; total labour value of household members (see function in get-labour)
  hh_stock ; storage of patch production; there is a separated stock per each valid year (list of integers)
  hh_nutrition ; degree to which household members have their nutrition needs satisfied (see function in hh_update-nutrition);
               ; it bends the curve of mortality (see function in get-net-mortality) (float between -1 and 1, with 0 => default mortality curve)
  hh_consumptionLevel ; trait type variable; states the level of consumption considered normal for the household; 1 means one unit of stock per member (positive float)

  hh_householdAge ; number of years during which the household existed (integer)
  hh_maxCoupleCount ; max. number of couples accepted within a household (integer)
  hh_membersAge ; ages of every household member (list of integer)
  hh_membersSex ; sex of every household member (list of true/false, i.e. is female?)
  hh_membersMarriage ; couple index of every member (list of integers; 0-Inf: couple index, -1: member is single)

  ;;;; auxiliar
  hh_memberDataToDelete
]

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
  potential nearestCore
  intensity ; intensity sustained by every household invested in a patch (list)
  returns ; returns obtained by every household invested in a patch (list)
  landUse
  myGroup
  myHouseholds ; IDs of households invested in a patch (list)
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

  build-demography-tables

  setup-patches

  setup-groups

  reset-counters

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

  ; parameter set to default value (constant)
  set stockStepsToExpire 2
  set maturityAge 15

end

to set-parameters

  ; set random seed
  random-seed SEED

  parameters-check-1

  ;;; setup parameters depending on the type of experiment
  set householdInitialAgeDistribution read-from-string ( word "[" household-initial-age-distribution "]")
  set maxCoupleCountDistribution read-from-string ( word "[" max-couple-count-distribution "]")

  if (typeOfExperiment = "user-defined")
  [
    ;;; load parameters from user interface
    ; initial conditions
	  set initGroups initial_number_groups
    set maxDistInitGroupsToNearestCore (max_%distance_initial_groups_core * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    set maxDistBetweenInitGroups (max_%distance_between_initial_groups * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    set initialNumHouseholds initial-num-households
    ; spatial relations
    set disPenGr distance_penalty_gradient
    ; land use potential
    set maxPotential max_potential
    set numCores num_cores
    set potentialGr potential_decay_gradient
    set numNullBodies num_null_bodies
	  set numNullPatches round (num_null_patches * (count patches) / 100)
    set effectivenessGr effectiveness_gradient
    set maxGroupChangeRate max_group_change_rate
    ; demography
    set excessNutritionDimReturns excess-nutrition-diminishing-returns
  ]
  if (typeOfExperiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    ; initial conditions
    set initialNumHouseholds 1 + random initial-num-households ; at least one household
    set householdInitialAgeDistribution (list
      (1 + random (item 0 householdInitialAgeDistribution))   ; minimum
      (
        (item 0 householdInitialAgeDistribution)
        + 1
        + random ((item 1 householdInitialAgeDistribution) - (item 0 householdInitialAgeDistribution))
      )   ; maximum
      )
    set maxCoupleCountDistribution (list
      (1 + random (item 0 maxCoupleCountDistribution))   ; minimum
      (
        (item 0 maxCoupleCountDistribution)
        + 1
        + random ((item 1 maxCoupleCountDistribution) - (item 0 maxCoupleCountDistribution))
      )   ; maximum
      )
	  set initGroups 1 + random initial_number_groups
	  set maxDistInitGroupsToNearestCore 25 + random (max_%distance_initial_groups_core - 25)
    set maxDistBetweenInitGroups 20 + random (max_%distance_between_initial_groups - 20)
    set maxDistInitGroupsToNearestCore (maxDistInitGroupsToNearestCore * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
    set maxDistBetweenInitGroups (maxDistBetweenInitGroups * (sqrt ((max-pxcor ^ 2) + (max-pycor ^ 2))) / 100)
	  ; spatial relations
    set disPenGr 0.01 + random-float 99.99 ; random-gamma 2 (1 / distance_penalty_gradient)
    ; land use potential
    set maxPotential 1 + random max_potential
    set numCores 1 + random num_cores
    set potentialGr random-gamma 2 (1 / potential_decay_gradient)
    set numNullBodies random (1 + num_null_bodies)
	  set numNullPatches (count patches) * random (1 + num_null_patches) / 100
    ; group dynamics
    set effectivenessGr 0.01 + random-float 99.99
    set maxGroupChangeRate random-float max_group_change_rate
    ; demography
    set excessNutritionDimReturns 1 + random 6
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
  if (initial_number_groups = 0)                [ set initial_number_groups                 5 ]
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

  if (average-births-per-woman = 0)             [ set average-births-per-woman              8 ]
  if (cdmlt-level = 0)                          [ set cdmlt-level                           8 ]
  if (c1-women = 0)                             [ set c1-women                              0.9 ]
  if (c1-men = 0)                               [ set c1-men                                0.85 ]
  if (mu-women = 0)                             [ set mu-women                             15 ]
  if (mu-men = 0)                               [ set mu-men                               20 ]
  if (sigma1-women = 0)                         [ set sigma1-women                          5 ]
  if (sigma1-men = 0)                           [ set sigma1-men                            2 ]
  if (sigma2-women = 0)                         [ set sigma2-women                          2 ]
  if (sigma2-men = 0)                           [ set sigma2-men                           10 ]

  ;;; string type inputs (vector of values)
  if (initial-num-households = 0)               [ set initial-num-households               25 ]
  if (household-initial-age-distribution = "0")   [ set household-initial-age-distribution   "0 30" ]
  if (max-couple-count-distribution = "0")        [ set max-couple-count-distribution        "1 6" ]

  ;;; initial parameter check (avoiding division per zero error
  check-par-is-positive "distance_penalty_gradient" distance_penalty_gradient
  check-par-is-positive "potential_decay_gradient" potential_decay_gradient

end

to parameters-check-2

  ;;; initial parameter check (e.g., avoiding division per zero error)
  check-par-is-positive "initialNumHouseholds" initialNumHouseholds

  ;;; check if given min values are less than max values
  check-par-is-range "householdInitialAgeDistribution" householdInitialAgeDistribution
  check-par-is-range "maxCoupleCountDistribution" maxCoupleCountDistribution

  ;;; check number of initial centers and land use units
  if (initGroups >= count patches) [print "ERROR: initial number of centers is too high compare to the number of patches" stop ]
  if (initGroups >= (count patches) - numNullPatches ) [print "ERROR: initial number of centers is too high compare to the number of useful patches (too many null patches)" stop ]

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

to check-par-is-range [ parName parListMinMax ]

  if ( item 0 parListMinMax > item 1 parListMinMax)
  [
    print (word "ERROR: " parName " minimum (first item) must be less than or equal to maximum (second item)")
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

  setup-households

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

to setup-households

  ;;; create households
  repeat initialNumHouseholds
  [
    create-households 1
    [
      hh_initialise
    ]
  ]

end

to setup-LandUse

  ;;; land use:
  ask patches [ set intensity (list) set returns (list) set landUse false set myGroup nobody set myHouseholds (list) set contenders (turtle-set) ]

  if (any? groups and any? households)[
    ask groups
    [
      let thisGroup self
      ask households with [ hh_myGroup = thisGroup ]
      [
        let thisHousehold self
        ask max-one-of patches with [ potential > 0 ] [ get-patch-attractiveness thisGroup ([hh_labour] of thisHousehold) ]
          [
            set intensity lput ([hh_labour] of thisHousehold) intensity
            set landUse true
            set myGroup thisGroup
            set myHouseholds lput ([who] of thisHousehold) myHouseholds
          ]
      ]
    ]
  ]

  ask patches with [ landUse = true ] [ update-patch-returns ]

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

  update-group-economy

  update-household-demography

  growth

  landUse-expansion

  check-competition

  change_groups

  update-counters

  refresh-view

  tick

  if (display-labels = true) [ ask labelpositions with [ name = "time" ] [ set label (word "time: " ticks) ] ]
  if (ticks = endSimulation) [stop]
  if (numberGroups = 0) [stop]

end

;============GLOBAL===============================

to update-group-economy

  patch-production

  update-household-stocks

  household-consumption

end

to patch-production

  ask patches with [landUse = true] [ update-patch-returns ]

end

to update-patch-returns

  set returns (list)

  foreach n-values length myHouseholds [ j -> j ]
  [
    i ->
    set returns lput (get-patch-return item i myHouseholds) returns
  ]

end

to-report get-patch-return [ householdWho ]

  ; get household index in "myHouseholds" variable of this patch
  let householdIndex position householdWho myHouseholds

  ; get the intensity invested by the other households that have "arrived first"
  let intensityFirstServed 0
  foreach n-values length myHouseholds [ j -> j ]
  [
    i ->
    if (i < householdIndex) [ set intensityFirstServed intensityFirstServed + item i intensity ]
  ]

  ; calculate the return obtained by the household discounting the returns obtained by others "arriving first"
  report (diminishing-returns (item householdIndex intensity + intensityFirstServed) potential) - diminishing-returns intensityFirstServed potential

end

to update-household-stocks

  ask households
  [
    hh_update-stock
  ]

end

to household-consumption

ask households
  [
    hh_update-nutrition
  ]

end

to growth

  ;;; Intrinsic Demand
  ask groups with [groupIntensity > 0]
  [
    set groupDemand 0
    let myLand patches with [landUse = true and myGroup = myself]
    ask myLand
    [
      let effGroupIntensity round (diminishing-returns sum intensity potential)
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
  let ext (round (extGrowth * sum [potential - sum intensity] of patches) )
  repeat ext
  [
    ask one-of groups
    [
      set groupIntensity groupIntensity + 1
      set groupDemand groupDemand + 1
    ]
  ]

end

to-report get-patch-attractiveness [ aGroup intensityToInvest ]

  ; calculate the return that would be obtained by investing "intensityToInvest", discounting the returns already obtained by the other households that have "arrived first"
  let potentialReturn (diminishing-returns (intensityToInvest + sum intensity) potential) - (diminishing-returns (sum intensity) potential)

  report potentialReturn / (1 + distance aGroup)

end



;============LAND USE & GROUP DYNAMICS===============================

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

;============HOUSEHOLD DEMOGRAPHY GLOBAL===============================

to update-household-demography
;print "-------apply-mortality----"
  apply-mortality
;print "-------apply-nuptiality----"
  apply-nuptiality
;print "-------apply-fertility----"
  apply-fertility
;print "-------update-households-labour----"
  update-household-labour

end

to apply-mortality

  ; initialize population lists of orphan children for this year
  set orphanList (list)

  ask households
  [
    hh_aging
  ]

  if (any? households) ; do it only in case there is any household left
  [
    ; distribute orphans randomly among surviving households
    foreach n-values length orphanList [ j -> j ]
    [
      i ->

      ask one-of households
      [
        hh_add-orphan (item i orphanList)
      ]
    ]
  ]

end

to apply-nuptiality

  ; initialize population lists of marrying candidates for this year
  set womenToMarry (list)
  set menToMarry (list)

  ; fill lists
  ask households
  [
    hh_set-members-to-marry

    set hh_memberDataToDelete (list)
  ]

  ; identify internal and external matches
  let internalMatchIndex 0
  let externalMatchIndex 0
  let womenWithoutMatch (length womenToMarry > length menToMarry)
  ifelse (womenWithoutMatch)
  [
    set internalMatchIndex (n-values length menToMarry [ i -> i ])
    set externalMatchIndex (n-values (length womenToMarry - length menToMarry) [ i -> i + length menToMarry])
    ;print "There are extra women to marry"
    ; EXAMPLE:
    ; for length menToMarry = 10 and length womenToMarry = 15
    ; internalMatchIndex = [ 0 1 2 3 4 5 6 7 8 9 ]
    ; externalMatchIndex = [ 10 11 12 13 14 ]
  ]
  [
    set internalMatchIndex (n-values length womenToMarry [ i -> i ])
    set externalMatchIndex (n-values (length menToMarry - length womenToMarry) [ i -> i + length womenToMarry])
    ;print "There are extra men to marry"
  ]

  ;print (word "internal" internalMatchIndex)
  ;print (word "external" externalMatchIndex)

  ; match every women to marry with a men to marry internally
  foreach internalMatchIndex
  [
    i ->
    create-couple i
  ]

  ; iterate for every women or men with no match internally, "searching" for an external match
  foreach externalMatchIndex
  [
    i ->
    create-couple-external i womenWithoutMatch
  ]

  ; delete recently married individuals from their parent household
  ask households
  [
    hh_delete-members-in-queue
  ]

end

to apply-fertility

  ask households
  [
    hh_reproduction
  ]

end

to create-couple [ indexInSinglesList ]

  ; load woman and man data according to index of the womenToMarry or menToMarry lists
  let womanData item indexInSinglesList womenToMarry ; list holding the household and member index
  let manData item indexInSinglesList menToMarry

  let womanHousehold item 0 womanData
  let womanIndex item 1 womanData
  let manHousehold item 0 manData
  let manIndex item 1 manData

  ;print (word "index " womanIndex " in " [hh_membersAge] of womanHousehold ", female, " womanHousehold)
  ;print (word "index " manIndex " in " [hh_membersAge] of manHousehold ", male, " manHousehold)

  if (residence-rule = "patrilocal-patrilineal")
  [
    ask manHousehold
    [
      hh_try-to-add-couple manIndex womanData
    ]
  ]
  if (residence-rule = "matrilocal-matrilineal")
  [
    ask womanHousehold
    [
      hh_try-to-add-couple womanIndex manData
    ]
  ]

end

to create-couple-external [ indexInSinglesList singleSex ]

  let singleData 0  ; list holding the household and member index

  ; load single woman or man data according to index of the womenToMarry or menToMarry lists
  ifelse (singleSex)
  [
    ; there are extra women to marry
    set singleData item indexInSinglesList womenToMarry
  ]
  [
    ; there are extra men to marry
    set singleData item indexInSinglesList menToMarry
  ]

  let singleHousehold item 0 singleData
  let singleIndex item 1 singleData

  ;print (word "Member " singleIndex " in " singleHousehold " (ages = " [hh_membersAge] of singleHousehold ", sex = " [hh_membersSex] of singleHousehold ") is marring outside the system."  )

  ; find out if the new couple imply a new individual to be entering the system
  let newIndividualIn
  (
    ; a new individual comes in either when...
    ; ...the single is a woman and the residence rule is matrilocal (husband comes in)
    (singleSex and residence-rule = "matrilocal-matrilineal")
    or
    ; ...the single is a men and the residence rule is patrilocal (wife comes in)
    ((not singleSex) and residence-rule = "patrilocal-patrilineal")
  )

  ifelse (newIndividualIn)
  [
    ; a new individual is entering the system...

    ; try to add a new couple in singleHousehold
    ask singleHousehold
    [
      hh_try-to-add-couple singleIndex [ -1 -1 ] ; pass spouse data as a NULL value (messaging it should be generated)
    ]

    ; account for the new individual coming in
    ifelse (not singleSex)
    [
      set womenIn womenIn + 1 ; if single is male, the new individual is female
    ]
    [
      set menIn menIn + 1
    ]
  ]
  [
    ; the single individual is the one moving, so exiting the system...

    ;print (word "individual from " singleHousehold " moving out the system: is female = " singleSex ", age = " (item singleIndex [hh_membersAge] of singleHousehold) )

    ; add the single individual (and any children left alone) to the deletion queue of singleHousehold
    let childrenMovingOut 0
    ask singleHousehold
    [
      ; add the single individual to the deletion queue
      set hh_memberDataToDelete lput singleIndex hh_memberDataToDelete

      ; since children can be left in a household without adults AND be set to marry to an individual outside the system in the same time step,
      ; we need to remove duplicated indexes before using hh_memberDataToDelete in hh_check-infant-only
      set hh_memberDataToDelete remove-duplicates hh_memberDataToDelete

      if (hh_check-infant-only)
      [
        ;print (word "all remaining individuals in " singleHousehold " are children: ages including single adult = " hh_membersAge )
        ; add all members to deletion queue since the household would have only children (assuming they are moving out with the single individual)
        set hh_memberDataToDelete n-values (length hh_membersAge) [ i -> i ]

        set childrenMovingOut hh_count-children
      ]
    ]

    ; account for the single individual going out
    ifelse (singleSex)
    [
      set womenOut womenOut + 1 + childrenMovingOut
    ]
    [
      set menOut menOut + 1 + childrenMovingOut
    ]
  ]

end

to update-household-labour

  ask households
  [
    hh_update-labour
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOUSEHOLDS DEMOGRAPHY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialisation ==============================================================================
to hh_initialise

  ;;; Initialization of households

  set hh_householdAge hh_get-initial-age
  set hh_maxCoupleCount hh_get-initial-max-couple-count

  hh_initialise-members

  hh_update-labour

  set hh_stock hh_get-initial-stock

  set hh_consumptionLevel random-gamma (1 * 1 / 0.1) (1 / (0.1 / 1))
  ; from NetLogo Dictionary on random-gamma:
  ; Note: for results with a given mean and variance, use inputs as follows: alpha = mean * mean / variance; lambda = 1 / (variance / mean).
  ; for now, the aim here is that most households have a consumptionLevel not far from 1 (mean)

  set hh_myGroup one-of groups

  ;set hidden? true
  move-to hh_myGroup

end

to-report hh_get-initial-stock

  let initialStock (list)

  repeat stockStepsToExpire
  [
    set initialStock lput (2 * (length hh_membersAge)) initialStock
  ]

  report initialStock

end

to-report hh_get-initial-age

  report item 0 householdInitialAgeDistribution + random (item 1 householdInitialAgeDistribution - item 0 householdInitialAgeDistribution + 1)

end

to-report hh_get-initial-max-couple-count

  report item 0 maxCoupleCountDistribution + random (item 1 maxCoupleCountDistribution - item 0 maxCoupleCountDistribution + 1)

end

to hh_initialise-members

  hh_reset-members

  ; assures that at least two member are above (fertility age + household age) and have different sex
  foreach (list 0 1)
  [
    ?1 ->
    set hh_membersSex lput (?1 = 0) hh_membersSex
    let marriageAge (hh_get-initial-marriage-age (item ?1 hh_membersSex))
    set hh_membersAge lput (hh_householdAge + marriageAge) hh_membersAge
    set hh_membersMarriage lput 0 hh_membersMarriage ; this couple will have the 0 index
  ]

  ; calculate offspring and offspring age
  let offspringProb 0
  let i hh_householdAge
  repeat hh_householdAge
  [
    ; get the probability of the couple having descendence for a past year i
    ; i.e. woman fertility at the corresponding age
    set offspringProb get-fertility ((item 0 hh_membersAge) - i)

    ; test the probability and add the offspring with the corresponding age
    if (random-float 1 < offspringProb)
    [
      hh_add-offspring i ; a child with i years old
    ]
    set i i - 1
  ]

end

to-report hh_get-initial-marriage-age [ isFemale ]

  ; get a marriage age as a stochastic function of nuptiality rates in earlier years
  let notMarried true
  let i 0

  while [notMarried AND i < 100]
  [
    if ((get-nuptiality isFemale i) > random-float 1)
    [
      set notMarried false
    ]
    set i i + 1
  ]

  report i

end

; Main procedures ==============================================================================

to hh_update-labour

  set hh_labour sum map [ i -> get-labour i ] hh_membersAge

end

to hh_update-stock

  ; get patch production this timestep
  let thisHousehold self
  let productionThisStep sum [ get-patch-return ([who] of thisHousehold) ] of patches with [ member? ([who] of thisHousehold) myHouseholds ]

  ; age stocks (starting with the oldest)
  let i stockStepsToExpire - 1
  while [ i > 0 ]
  [
    let newValue item (i - 1) hh_stock
    set hh_stock replace-item i hh_stock newValue
    set i i - 1
  ]

  ; place the last harvest as the younger stock
  set hh_stock replace-item 0 hh_stock productionThisStep

end

to hh_update-nutrition

  let numberOfPeople length hh_membersAge

  let consumed min (list sum hh_stock (hh_consumptionLevel * numberOfPeople))
  ; the amount of stock consumed depends on
  ; 1) the current amount of stock and
  ; 2) the normal level of consumption for the household, proportional to the number of members
  ; EXAMPLE: numberOfPeople = 10 and consumptionLevel = 1.5 -> normal consumption = 15
  ; if stock = 5, then consumed = 5 (minimum value between 5 and 15)
  ; if stock = 20, then consumed = 15 (minimum value between 20 and 15)

  ;discount consumed stock !!!!

  set hh_nutrition ((consumed - numberOfPeople) / numberOfPeople) ^ 3
  ; to visualise the function output:
  ; In https://www.wolframalpha.com: plot ((x - 10)/10)^3 x=0..20
  ; In R: plot(0:20, ((0:20 - 10)/10)^3, main = "consumed = ((stock - numPeople) / numPeople)^3")
print (word "numberOfPeople = " numberOfPeople ", sum hh_stock = " (sum hh_stock) ", (hh_consumptionLevel * numberOfPeople) = " (hh_consumptionLevel * numberOfPeople) ", hh_nutrition = " hh_nutrition)
end

to hh_aging

  ; household aging
  set hh_householdAge hh_householdAge + 1

  ; members aging
  hh_update-members-age

  ; apply mortality rate
  hh_update-members-survival

end

to hh_update-members-age

  ; household members aging
  set hh_membersAge map [ i -> i + 1 ] hh_membersAge

end

to hh_update-members-survival

  ; applies age-specific mortality rates and delete dying members

  let membersIndex (n-values length hh_membersAge [ i -> i ])

  ; define a list with true/false values flagging which members is dying during the current year
  let dying? map
  [
    i ->
    ( random-float 1 < (get-net-mortality (item i hh_membersSex) (item i hh_membersAge) hh_nutrition ))
  ] membersIndex

  ; iterate for the members from last to first, eliminating those that should be dying
  let index (length membersIndex) - 1
  repeat length membersIndex
  [
    if (item index dying?)
    [
      ; add to mortality
      ifelse (item index hh_membersSex)
      [ set womenDeaths womenDeaths + 1 ]
      [ set menDeaths menDeaths + 1 ]

      ;print (word "member from " self " dies with age " (item index hh_membersAge))
      hh_delete-member index
    ]
    set index index - 1
  ]

  ; update the index of members
  set membersIndex (n-values length hh_membersAge [ i -> i ])

  ; update hh_membersMarriage to consider any widow/widower as single
  let hh_membersMarriageCopy hh_membersMarriage
  foreach membersIndex
  [
    i ->
    if (item i hh_membersMarriage != -1                                           ; if the member is married
      and length filter [j -> j = item i hh_membersMarriage] hh_membersMarriage = 1) ; and her/his partner died this year (so she/he is the only member with the marriage index)
    [
      set hh_membersMarriage replace-item i hh_membersMarriage -1
      ;print (word "a member of " self " has lost her/his partner.")
    ]
  ]

  ; check if the household is left without any adult
  if (hh_check-infant-only)
  [
    ; there is no adult in this household, so the children will enter the orphanList so they can be adopted later
    ;print (word "all remaining individuals in " self " are children: is female = " hh_membersSex ", ages = " hh_membersAge ". They are now in the orphan list.")

    ; iterate for the members from last to first
    set index (length membersIndex) - 1
    repeat length membersIndex
    [
      ; add children data (sex, age) to the orphan list (they are distributed among other households once aging procedures are done)
      set orphanList lput (list (item index hh_membersSex) (item index hh_membersAge) ) orphanList

      ; and delete them from the current household (eventualy erasing it)
      hh_delete-member index

      set index index - 1
    ]
  ]

end

to hh_set-members-to-marry

  ; applies age-specific nuptiality rates

  let membersIndex (n-values length hh_membersAge [ i -> i ])

  ; iterate for each member, finding which one is marrying during the current year, according to age cohort and sex
  foreach membersIndex
  [
    i ->
    if (item i hh_membersMarriage = -1) ; member is not married
    [
      let sex item i hh_membersSex

      if (random-float 1 < get-nuptiality sex (item i hh_membersAge))  ; passes nuptiality test according to age
      [
        ifelse (sex)
        [
          set womenToMarry lput (list self i) womenToMarry
        ]
        [
          set menToMarry lput (list self i) menToMarry
        ]
      ]
    ]
  ]

end

to hh_try-to-add-couple [ selfIndex spouseData ]

  ; check if new couple fits in self's household
  let tooManyCouples (hh_count-couples + 1) > hh_maxCoupleCount

  ifelse (tooManyCouples)
  [
    ; the new couple must found a new household, descending from self's household
    ;print "household fission"
    hh_household-fission selfIndex spouseData
  ]
  [
    ; the new couple can stay in the self's parent household
    hh_add-couple selfIndex spouseData
  ]

end

to hh_household-fission [ selfIndex spouseData ]

  ; creates a new household descending from self household,
  ; adding self and spouse and
  ; deleting them from the respective parent households

  ;print (word self " is fissioning")

  let selfHousehold self

  ; create new household w/ self and spouse
  hatch 1
  [
    set hh_householdAge 0
    ; inherit hh_maxCoupleCount value from self's parent

    hh_reset-members

    ; add self
    hh_add-member-from (list selfHousehold selfIndex)
    ; add spouse
    hh_add-spouse 0 spouseData ; 0 is the index of the self individual in the new household

    ; account for the new couple
    set hh_membersMarriage lput 0 hh_membersMarriage ; 0 because this is the first couple of the new household
    set hh_membersMarriage lput 0 hh_membersMarriage
  ]

end

to hh_add-couple [ selfIndex spouseData ]

  ; add spouse to self's household
  ;print (word "adding couple to " self)

  hh_add-spouse selfIndex spouseData

  ; account for the new couple
  let newCoupleIndex 1 + max hh_membersMarriage ; create a new couple index
  set hh_membersMarriage replace-item selfIndex hh_membersMarriage newCoupleIndex ; update self's marriage status
  set hh_membersMarriage lput newCoupleIndex hh_membersMarriage ; add another corresponding to the spouse

end

to hh_reproduction

  ; iterate for the members, up to the number of couples,
  ; testing women for the corresponding fertility rate
  ; and generates a new born individual if passing test.

  let membersIndex (n-values length hh_membersAge [ i -> i ])
  let couplesToTest hh_count-couples

  foreach membersIndex
  [
    i ->
    ; there is still a couple to consider and the member is female
    if (couplesToTest > 0 and item i hh_membersSex)
    [
      if (random-float 1 < get-fertility (item i hh_membersAge))
      [
        hh_add-offspring 0 ; add a newborn
        ;print (word "a new member is born in " self)
      ]
      set couplesToTest couplesToTest - 1
    ]
  ]

end

to hh_add-spouse [ selfIndex spouseData ]

  ; if spouseData is NULL -> [ -1 -1 ]
  ifelse (item 0 spouseData = -1)
  [
    ; The spouse is entering the system:
    ; generate and add spouse's sex and age
    set hh_membersSex lput (not item selfIndex hh_membersSex) hh_membersSex ; opposite sex from selfIndex's
    set hh_membersAge lput (hh_get-initial-marriage-age (last hh_membersSex)) hh_membersAge ; get the age as a function of sex-specific nuptiality

    ;print (word "new spouse added to " self ": is female = " (last hh_membersSex) ", age = " (last hh_membersAge))
  ]
  [
    ; The spouse is already in the system:
    ; copy spouse
    hh_add-member-from spouseData

    ;print (word "spouse moving from " (Household (item 1 spouseData)) " to " self ": is female = " (last hh_membersSex) ", age = " (last hh_membersAge))
  ]

end

to hh_add-member-from [ memberData ]

  let aHousehold item 0 memberData
  let index item 1 memberData

  set hh_membersAge lput item index ([hh_membersAge] of aHousehold) hh_membersAge
  set hh_membersSex lput item index ([hh_membersSex] of aHousehold) hh_membersSex

  ; add member to deletion queue of original parent household
  ask aHousehold
  [
    set hh_memberDataToDelete lput index hh_memberDataToDelete
  ]

end

to hh_add-offspring [ initialAge ]

  ; add a newborn to the household
  set hh_membersAge lput initialAge hh_membersAge
  set hh_membersSex lput (random 2 = 0) hh_membersSex
  set hh_membersMarriage lput -1 hh_membersMarriage ; any offspring will be single

  ifelse (last hh_membersSex)
  [ set womenBirths womenBirths + 1 ]
  [ set menBirths menBirths + 1 ]

end

to hh_add-orphan [ orphanData ]

  ; add an orphan child to the household
  ; orphanData is assumed to be: [ <sex> <age> ]

  set hh_membersSex lput (item 0 orphanData) hh_membersSex
  set hh_membersAge lput (item 1 orphanData) hh_membersAge
  set hh_membersMarriage lput -1 hh_membersMarriage ; any orphan is assumed single
  ;(when children "marry", they will share the household with the spouse. If they are found an orphan is because the spouse is either dead or is also an orphan)

  ;print (word "the orphan (is female = " (item 0 orphanData) ", age = " (item 1 orphanData) ") is adopted by " self ": are females = " hh_membersSex ", ages = " hh_membersAge )

end

to hh_delete-members-in-queue

  ; delete members in queue following decresing order (so indexes still to go remain valid)
  foreach sort-by > hh_memberDataToDelete
  [
    i ->
    ; delete member from this household
    hh_delete-member i
  ]

  ; reset queue
  set hh_memberDataToDelete (list)

end

to hh_delete-member [ index ]

  set hh_membersAge remove-item index hh_membersAge
  set hh_membersSex remove-item index hh_membersSex
  set hh_membersMarriage remove-item index hh_membersMarriage
  ; if the member was married, it will imply that there will be an odd number of married members,
  ; thus discounting a couple in hh_count-couples

  if (length hh_membersAge = 0) [ die ] ; delete empty household

end

to-report hh_check-infant-only

  ; filter out the members selected to be deleted
  let membersAgeWithoutQueuToDelete hh_membersAge

  foreach sort-by > hh_memberDataToDelete
  [
    i ->
    set membersAgeWithoutQueuToDelete remove-item i membersAgeWithoutQueuToDelete
  ]

  if (length membersAgeWithoutQueuToDelete = 0) [ report false ] ; report false in case there is no members that are not in queue to deletion

  report reduce and (map [i -> i < maturityAge] membersAgeWithoutQueuToDelete)

end

to-report hh_count-children

  report length filter [ i -> i < maturityAge ] hh_membersAge

end

to-report hh_count-couples

  let marriedMembers filter [ i -> (i >= 0) ] hh_membersMarriage

  report floor ((length marriedMembers) / 2)

end

to hh_reset-members

  set hh_membersAge (list)
  set hh_membersSex (list)
  set hh_membersMarriage (list)

  set hh_memberDataToDelete (list)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Demographic rates 'getters' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-labour [ age ]

  let labour 0

  ;;; labour value function (TO DO: implement a more elegant function)
  ifelse (age < 15);maturityAge)
  [ set labour (age / 15) ^ 10 ]
  [
    ifelse (age < 45)
    [ set labour 1 ]
    [ set labour (45 / age) ^ 10 ]
  ]

  report max (list 0 labour)

end

to-report get-fertility [ age ]

  report item age fertilityTable

end

to-report get-nuptiality [ isFemale age ]

  ifelse (isFemale)
  [
    report item age nuptialityTable-women
  ]
  [
    report item age nuptialityTable-men
  ]

end

to-report get-mortality [ isFemale age ]

  ifelse (isFemale)
  [
    report item age mortalityTable-women
  ]
  [
    report item age mortalityTable-men
  ]

end

to-report get-net-mortality [ isFemale age nutrition ]

  let mort get-mortality isFemale age

  let nutEff nutrition * (1 - mort)
  if (nutrition > 0) [ set nutEff nutEff * excessNutritionDimReturns ]

  report mort - nutEffect

  ; to visualise the function output in R see "demoTables/nutritionEffect.R"

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters

  ;;; counters are used througout the procedures, so they must be set to 0 at the beggining of each step
  set competitions 0
  set landUseChangeEvents 0

  ask groups [ set groupDemand 0 ]

  set womenBirths 0
  set menBirths 0
  set womenDeaths 0
  set menDeaths 0

end

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

  set totalHouseholds count households
  let oldTotalIndividual totalIndividuals
  set totalIndividuals 0
  set totalWomen 0
  set totalMen 0
  set femaleRatio -1
  set womenAgeStructure (list)
  set menAgeStructure (list)
  set womenFirstAgeGroup 0
  set menFirstAgeGroup 0

  ask households
  [
    foreach (n-values length hh_membersAge [j -> j])
    [
      i ->
      ifelse (item i hh_membersSex)
      [
        set totalWomen totalWomen + 1
        set womenAgeStructure lput item i hh_membersAge womenAgeStructure
        if (item i hh_membersAge < 5)
        [ set womenFirstAgeGroup womenFirstAgeGroup + 1 ]
      ]
      [
        set totalMen totalMen + 1
        set menAgeStructure lput item i hh_membersAge menAgeStructure
        if (item i hh_membersAge < 5)
        [ set menFirstAgeGroup menFirstAgeGroup + 1 ]
      ]
      set totalIndividuals totalIndividuals + 1
    ]
  ]

  carefully [ set totalPopulationGrowth 100 * (totalIndividuals - oldTotalIndividual) / oldTotalIndividual ] [ ]

  carefully [ set femaleRatio totalWomen / totalIndividuals ] [ set femaleRatio "" ]

  carefully [ set totalOrphans length orphanList ] [ set totalOrphans 0 ]

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
      let maxPatchIntensity max [sum intensity] of patches
      ask patches with [myGroup != nobody]
      [
        let strength sum intensity / maxPatchIntensity
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

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    plotxy j i
    set j j + 1
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEMOGRAPHY TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to build-demography-tables

  ;;; load demographic data tables into lists

  ;=======FERTILITY========================================================

  build-fertility-tables

  ;=======NUPTIALITY========================================================

  build-nuptiality-tables

  ;=======MORTALITY========================================================

  build-mortality-tables

end


to build-fertility-tables

  set fertilityTable load-betaDist-table

end

to-report load-betaDist-table

  ;;; this function calculate the age-specific fertility rate 'ASFR' (probabilities for a woman to give birth between at age i)
  ;;; using a beta distribution scaled to the average number of offspring per woman during the entire fertile period
  ;;; Beta distribution is either obtained with the external R script or by using the 'rngs' community extension.
  ;;; NOTE: 2.1 of offspring alive during fertile period is the minimum replacement rate (i.e. fertility must compensate infant mortality)
  ;;; Approach taken from Peter Ellis
  ;;; http://freerangestats.info/blog/2018/06/26/fertility-rate
  ;;; https://github.com/ellisp/blog-source/tree/master/_working/0122-demographics

  let birthProbs (list)

  let FilePath "demoTables//"
  let filename (word FilePath "betaDist.txt")
  let temp 0
  file-open filename
  set temp file-read ; skips column name

  while [not file-at-end?]
  [
    set temp file-read ; this line skips the row identifier
    set birthProbs lput file-read birthProbs ; load row datum
  ]
  file-close

  set birthProbs map [ i -> i * average-births-per-woman / (length filter [j -> j > 0] birthProbs) ] birthProbs

  report birthProbs

end

to build-nuptiality-tables

  set nuptialityTable-women load-nuptiality-model-table true

  set nuptialityTable-men load-nuptiality-model-table false

end

to-report load-nuptiality-model-table [ isFemale ]

  ;;; The following correspond to the first parametric model
  ;;; for fitting the age-specific distributions of marriages, mentioned in page 133 of:
  ;;; Peristeva and Kostaki
  ;;; "A parametric model for estimating nuptiality patterns in modern populations"
  ;;; Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].
  ;;; use "demoTables/compareNuptialityModel.R" to test shapes

  let c1 c1-women
  let sigma-list (list sigma1-women sigma2-women)
  let mu mu-women
  if (not isFemale)
  [
    set c1 c1-men
    set sigma-list (list sigma1-men sigma2-men)
    set mu mu-men
  ]

  let marriageProbs (list)

  foreach n-values 151 [ i -> i ]
  [
    i ->
    let sigma item 1 sigma-list
    if (i < mu) [ set sigma item 0 sigma-list ]

    set marriageProbs lput (
      c1 * exp (-1 * (((i - mu) / sigma) ^ 2))
    ) marriageProbs
  ]

  report marriageProbs

end

to build-mortality-tables

  set mortalityTable-women load-coale-demeny-table true

  set mortalityTable-men load-coale-demeny-table false

end

to-report load-coale-demeny-table [ isFemale ]

  ;;; Coale-Demeny Life Tables Model
  ;;; tables generated with 'cdmlt' functions in 'demoR' package
  ;;; demoR package version 0.6.0 (2018-09-13)
  ;;; by James Holland Jones and collaborators
  ;;; Their source:
  ;;; Coale, A., P. Demeny, and B. Vaughn. 1983.
  ;;; Regional model life tables and stable populations.
  ;;; 2nd ed. New York: Academic Press.

  ;;; Tables were generated using the R script 'importCoaleDemenyLifeTables.R'
  ;;; included in the demoTables folder.

  ;;; this function assumes there is a text file (.../demoTables/cdmlt<coale-demeny-region><sex>.txt)
  ;;; containing a matrix with prob. of death for life-expentancy-level (rows) by age cohort (columns).
  ;;; values of the first row and columns should be skipped

  let sex "F"
  if (not isFemale) [ set sex "M" ]

  let nqx (list)

  ; read file corresponding to coale-demeny-region and sex
  let FilePath "demoTables//"
  let filename (word FilePath "cdmlt" (first coale-demeny-region) sex ".txt")

  file-open filename

  ; skip first line (header)
  let temp file-read-line

  ; read lines and get the one corresponding to coale-demeny-life-expectancy-level
  let i 1
  while [not file-at-end?]
  [
    ; read line
    set temp read-from-string (word "[" file-read-line "]")
    ; select value corresponding to coale-demeny-life-expectancy-level (because the first item is 0, the values skips the first column or row names)
    set nqx lput (item cdmlt-level temp) nqx

;    if (read-from-string item 0 temp = cdmlt-level)
;    [
;      set nqx remove-item 0 temp
;    ]
    set i i + 1
  ]

  file-close

  report nqx

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
1075
35
1368
171
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

PLOT
952
196
1112
316
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
1113
196
1273
316
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
6
534
127
594
initial_number_groups
0.0
1
0
Number

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
953
334
1113
371
NIL
meanGroupSize
2
1
9

MONITOR
1115
335
1274
372
meanGroupIntensity
meanGroupIntensity
2
1
9

SLIDER
368
666
606
699
distance_penalty_gradient
distance_penalty_gradient
0.01
100
0.0
0.01
1
% maxDist
HORIZONTAL

SLIDER
6
595
224
628
max_%distance_initial_groups_core
max_%distance_initial_groups_core
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
6
664
279
697
max_%distance_between_initial_groups
max_%distance_between_initial_groups
20
100
0.0
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
227
184
308
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
1
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
7
698
130
735
NIL
maxDistBetweenInitGroups
17
1
9

MONITOR
6
627
155
664
NIL
maxDistInitGroupsToNearestCore
17
1
9

SLIDER
360
769
555
802
max_group_change_rate
max_group_change_rate
0
100
0.0
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
0.0
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
0.0
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
0.0
1
0
Number

INPUTBOX
308
592
401
652
num_null_bodies
0.0
1
0
Number

MONITOR
128
549
193
586
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
0.0
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
5
308
155
326
Initial conditions
14
0.0
1

SLIDER
349
730
594
763
effectiveness_gradient
effectiveness_gradient
0.01
100
0.0
0.01
1
% totalPatches
HORIZONTAL

MONITOR
595
727
675
764
NIL
effectivenessGr
0
1
9

MONITOR
555
768
663
805
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
354
709
504
727
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
745
197
944
337
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
1282
196
1442
316
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
1283
335
1443
372
NIL
meanGroupEffectiveness
4
1
9

TEXTBOX
959
176
1109
194
group-level histograms
14
0.0
1

TEXTBOX
954
315
1329
335
group-level mean variables weighted by extent (patches)
14
0.0
1

MONITOR
1285
372
1443
409
NIL
bigGroupEffectiveness
4
1
9

MONITOR
1115
372
1274
409
NIL
intensiveGroupIntensity
0
1
9

MONITOR
953
371
1113
408
NIL
bigGroupSize
0
1
9

MONITOR
1115
409
1274
446
NIL
vastGroupIntensity
0
1
9

MONITOR
953
408
1113
445
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
1373
36
1610
171
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
1551
100
1703
137
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
1250
130
1355
167
landUseChangeEvents
landUseChangeEvents
0
1
9

MONITOR
1264
91
1336
128
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
0.0
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
0.0
1
0
Number

BUTTON
151
267
308
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

TEXTBOX
953
445
1184
470
household-level histograms
14
0.0
1

PLOT
953
463
1113
583
household size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -1 (1 + max [length hh_membersAge] of households)\nset-histogram-num-bars 10\nset-plot-y-range -1 totalHouseholds" "set-plot-x-range -1 (1 + max [length hh_membersAge] of households)\nset-histogram-num-bars 10\nset-plot-y-range -1 totalHouseholds"
PENS
"default" 1.0 1 -16777216 true "" "histogram [length hh_membersAge] of households"

PLOT
746
337
946
487
number of household
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
"default" 1.0 0 -16777216 true "plot totalHouseholds" "plot totalHouseholds"

PLOT
1114
463
1274
583
household stocks
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -1 (1 + max [sum hh_stock] of households)\nset-histogram-num-bars 10\nset-plot-y-range -1 totalHouseholds" "set-plot-x-range -1 (1 + max [sum hh_stock] of households)\nset-histogram-num-bars 10\nset-plot-y-range -1 totalHouseholds"
PENS
"default" 1.0 1 -16777216 true "" "histogram [sum hh_stock] of households"

PLOT
1276
463
1436
583
household labour
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 (0.01 + max [hh_labour] of households)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 totalHouseholds" "set-plot-x-range -0.01 (0.01 + max [hh_labour] of households)\nset-histogram-num-bars 10\nset-plot-y-range -0.01 totalHouseholds"
PENS
"default" 1.0 1 -16777216 true "" "histogram [hh_labour] of households"

PLOT
746
488
946
638
number of individuals
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
"default" 1.0 0 -16777216 true "plot totalIndividuals" "plot totalIndividuals"

INPUTBOX
4
394
172
454
household-initial-age-distribution
0
1
0
String

INPUTBOX
3
452
172
512
max-couple-count-distribution
0
1
0
String

INPUTBOX
1
334
171
394
initial-num-households
0.0
1
0
Number

TEXTBOX
10
512
188
534
<minimum><SPACE><maximum>
9
0.0
1

MONITOR
170
355
284
392
NIL
initialNumHouseholds
0
1
9

MONITOR
172
413
310
450
NIL
householdInitialAgeDistribution
0
1
9

MONITOR
172
474
302
511
NIL
maxCoupleCountDistribution
0
1
9

CHOOSER
220
1195
408
1240
residence-rule
residence-rule
"matrilocal-matrilineal" "patrilocal-patrilineal"
0

SLIDER
222
932
521
965
average-births-per-woman
average-births-per-woman
0
30
0.0
0.001
1
default: 8
HORIZONTAL

PLOT
13
878
213
1028
fertility (prob. of giving birth)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"clear-plot \nset-plot-y-range -0.001 (precision (max fertilityTable + 0.001) 0.01)" ""
PENS
"default" 1.0 0 -5298144 true "" "plot-table fertilityTable"

PLOT
13
1029
213
1179
mortality (prob. dying)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"clear-plot\nset-plot-y-range -0.001 (precision (max (list max mortalityTable-women max mortalityTable-men) + 0.001) 0.01)" ""
PENS
"default" 1.0 0 -5298144 true "" "plot-table mortalityTable-women"
"pen-1" 1.0 0 -14070903 true "" "plot-table mortalityTable-men"

SLIDER
219
1061
430
1094
cdmlt-level
cdmlt-level
1
25
0.0
1
1
from 1 to 25
HORIZONTAL

CHOOSER
220
1097
384
1142
coale-demeny-region
coale-demeny-region
"west" "north" "east" "south"
3

PLOT
13
1180
213
1330
nuptiality (prob. marrying)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"clear-plot \nset-plot-y-range -0.001 (precision (max (list max nuptialityTable-women max nuptialityTable-men) + 0.001) 0.01)" ""
PENS
"default" 1.0 0 -5298144 true "" "plot-table nuptialityTable-women"
"pen-1" 1.0 0 -14070903 true "" "plot-table nuptialityTable-men"

SLIDER
221
1241
444
1274
c1-women
c1-women
0
1
0.0
0.0011
1
(default: 0.85)
HORIZONTAL

SLIDER
220
1276
443
1309
c1-men
c1-men
0
1
0.0
0.001
1
(default: 0.85)
HORIZONTAL

SLIDER
445
1241
661
1274
mu-women
mu-women
0
40
0.0
0.001
1
(default: 20)
HORIZONTAL

SLIDER
444
1276
661
1309
mu-men
mu-men
0
40
0.0
0.001
1
(default: 20)
HORIZONTAL

SLIDER
662
1241
879
1274
sigma1-women
sigma1-women
0
2 * 5
0.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
664
1277
879
1310
sigma1-men
sigma1-men
0
2 * 5
0.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
880
1242
1114
1275
sigma2-women
sigma2-women
0
2 * 5
0.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
880
1276
1113
1309
sigma2-men
sigma2-men
0
2 * 5
0.0
0.001
1
(default: 5)
HORIZONTAL

TEXTBOX
19
850
275
884
Household demography models
14
0.0
1

BUTTON
260
864
357
897
refresh tables
build-demography-tables\nupdate-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1035
639
1505
759
Age structure
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-histogram-num-bars 20\nset-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)" "set-plot-y-range 0 10\n;set-histogram-num-bars 20\nset-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)"
PENS
"women" 1.0 1 -2674135 true "" "histogram womenAgeStructure"
"men" 1.0 1 -13345367 true "" "histogram menAgeStructure"

MONITOR
1426
693
1499
738
NIL
femaleRatio
4
1
11

MONITOR
1050
588
1151
633
women death rate
womenDeaths / totalWomen
4
1
11

MONITOR
1249
588
1349
633
men death rate
menDeaths / totalMen
4
1
11

PLOT
746
638
1030
788
Births and deaths
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"women deaths" 1.0 0 -11053225 true "plot womenDeaths" "plot womenDeaths"
"men deaths" 1.0 0 -4539718 true "plot menDeaths" "plot menDeaths"
"births" 1.0 0 -14439633 true "plot womenBirths + menBirths" "plot womenBirths + menBirths"
"deaths" 1.0 0 -16777216 true "plot womenDeaths + menDeaths" "plot womenDeaths + menDeaths"
"women births" 1.0 0 -13210332 true "plot womenBirths" "plot womenBirths"
"men births" 1.0 0 -11881837 true "plot menBirths" "plot menBirths"

MONITOR
954
588
1047
633
women birth rate
womenBirths / totalWomen
4
1
11

MONITOR
1153
588
1246
633
men birth rate
menBirths / totalMen
4
1
11

SLIDER
20
752
322
785
excess-nutrition-diminishing-returns
excess-nutrition-diminishing-returns
0
1
0.2
0.001
1
default: 0.2
HORIZONTAL

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

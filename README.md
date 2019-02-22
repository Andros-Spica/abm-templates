# Collection of templates for agent-based models using NetLogo

This is a collection of templates focused on modelling agropastoral systems in prehistory as agent-based models in NetLogo. Besides setting a general framework for creating models in NetLogo, these templates include models covering themes from land use and settlement patterns to household decision-making and cultural dynamics.

This effort is framed by the modelling program followed as part of the [TwoRains ERC project](https://www.arch.cam.ac.uk/research/projects/two-rains) (see also [blog](https://tworains.wordpress.com/)). Each template is the consolidation of a stage of the modelling process in TwoRains, though there are many elements owned to my previous experiences in agent-based modelling (ABM). Specifically, **Terrain Builder** and **Land Use** templates are simplifications of the models of the *Musical Chairs* series, created for the SimulPast project (2011-2017, funded by Spanish Gov.). There are also many bits of code and programming practices that I picked up during my exchanges with the ABM community.

This collection is intended as a open resource for anyone creating (or willing to create) agent-based models in NetLogo. All templates can be copied, re-used, modified and redistributed, given complainance to GNU GPL v3 license. In accordance with this same spirit, please direct to me any comments, criticism, suggestions. The repository is also open to collaborators interested into extending the template collection.

## Basic ("0-basic.nlogo")

A general template for structuring NetLogo models (exemplifies general practices such as GNU license, UI setting, commentary, procedures for parameter checks, structuring procedures using commentary, etc.)

## Growers ("1-growers.nlogo")

Template illustrating the implementation of a density-dependent population growth.

## Terrain Builder ("2-terrainBuilder.nlogo")

Template including a submodel to generate local-scale inland terrain. "Terrain" is expressed as different potential return for a certain land use. This potential return is highest in "core" patches (darker green). Core patches may be distributed in point or linear patterns ("cores"). The potential is zero in "null patches" (grey), which may constitute separate "null bodies". Patches that are neither cores or part of null bodies have a potential that decreases with distance to the nearest core patch.

This template contain examples of data exporting and using helper agents (i.e. agents used temporally to create patterns).

## Land Use ("2-2-landUse.nlogo")

Template seting up a framework for land use competition amoung territorial agropastoral groups. The Terrain Builder submodel is included to generate different spatial settings. Each turn, groups need to allocate an amount of intensity units, which increases slowly every turn.

Available options are: 

1. to place a new intensity unit in an unoccupied patch (if any),  
2. accumulate it within patches already in use by the group (intensification, penalised by diminishing returns), or  
3. create a competitive situation by trying to use patches of another group (increased risk).

Group efficiency is represented as dependent on group size (total number of intensity units) and group influence over patches as a function of distance from the group centre. Groups of patches receiving low influence from their group may join another group or found a new group.

This template introduces how to create multiple modes of visualisation or display, import parameter settings from files and generate video animations of a simulation.

## Household Demography ("3-householdDemography.nlogo")

This template sets a general demographic model representing individual births (fertility submodel), deaths (mortality submodel), and match-making (nuptiality submodel). Individuals are represented not as separate agents, but as values in lists (e.g. age, sex) placed inside household agents.

The fertility submodel is an approximation to the age-specific patterns observed in human populations (beta function curve). The mortality submodel is the Coale-Demeny Life Tables Model, available in the demoR package in R. This model was built based on a large set of 20th-century demographic data (Coale, Demeny and Vaughn, 1983). The nuptiality submodel corresponds to the first parametric model presented in Peristeva and Kostaki (2015).

This submodel is implemented as sex-specific in order to allow for different patterns of female and male nuptiality. The model system is kept open by introducing and erasing individuals due to marriages to individuals outside the population.

The template includes a folder ("demoTables") containing files associated to the three submodels. Those include:

- R scripts:
  -  "demographyModels.R": performs a step-wise graphical demostration of the three models.
  -  "exportBetaDistribution.R": creates and exports to "betaDist.txt" a vector of values corresponding to a beta distribution for ages between 0 and 150, to be used as a age-specific fertility model.
  -  "exportCoaleDemenyLifeTables.R": creates sex-specific life tables according to the Coale-Demeny model using the 'demogR' package. It then exports these tables to separate name-coded files (e.g., "cdmlteM.txt").

- txt files: tables 


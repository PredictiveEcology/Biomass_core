---
title: "Biomass_core"
author:
  - Jean Marchal (<jean.d.marchal@gmail.com>)
  - Ceres Barros (<cbarros@mail.ubc.ca>)
  - Eliot McIntire (<eliot.mcintire@canada.ca>)
  - Yong Luo
  - Alex M. Chubaty (<achubaty@for-cast.ca>)
date: "11 May 2021"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



[![Gitter](https://badges.gitter.im/PredictiveEcology/LandR_Biomass.svg)](https://gitter.im/PredictiveEcology/LandR_Biomass?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

**This documentation is not complete. If there are discrepancies, omissions, or it is unclear, this is expected**

# Overview

A biomass succession model derived and modified from LANDIS-II Biomass Succession v3.2.1.
Biomass_core simulates forest succession in a spatially explicit manner (per pixel) by reproducing the population dynamics (growth and mortality), dispersal and competition for light resources of tree species. 
Like in LANDIS-II, dynamics are simulated in terms of their biomass per cohort (a combination of species and age), following a `biomass ~ age` curve  that is influenced by species-specific parameters such as growth, mortality, maximum biomass, and species traits such as longevity and tolerance to shade (see [LANDIS-II Biomass Succession v3.2 User Guide](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v3.2%20User%20Guide.docx) and [Scheller and Mladenoff (2004)](https://pdfs.semanticscholar.org/4d38/d0be6b292eccd444af399775d37a757d1967.pdf).
One of three dispersal algorithms are available: 'no dispersal', 'universal dispersal'(every species can disperse across the entire landscape) and 'ward dispersal' (species disperse according to a dispersal kernel, with mean and maximum distances taken from LANDIS-II trait tables), the latter being used by default.
The biggest differences between Biomass_core and LANDIS-II lie on how the model is parameterized and initialized and how different processes have been compartmentalized in order for higher flexibility.

## Inputs and parametrisation

`Biomass_core` is capable of running on dummy datasets from which it estimates parameters linked to vegetation growth and seed germination (such as the maximum biomass per species, per pixel, and the probability of seed germination - *i.e.*, species establishment probability not due to resprouting), but also builds and initializes forest communities (based on biomass, age, species composition, land cover and ecological zones like ecodistricts.

Ideally, however, the user should supply realistic versions of these data and the essential initialization objects that `Biomass_core` requires to run.


```
## defineParameter: 'growthInitialTime' is not of specified type 'numeric'.
```

```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName                       |objectClass              |desc                                                                                                                                                                                                                                                                                                         |sourceURL                                                                                                                                                       |
|:--------------------------------|:------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------|
|biomassMap                       |RasterLayer              |total biomass raster layer in study area (in g/m2), filtered for pixels covered by cohortData. Only used if `P(sim)$initialBiomassSource == 'biomassMap'`                                                                                                                                                    |                                                                                                                                                                |
|cceArgs                          |list                     |a list of quoted objects used by the `growthAndMortalityDriver` `calculateClimateEffect` function                                                                                                                                                                                                            |NA                                                                                                                                                              |
|cohortData                       |data.table               |Columns: B, pixelGroup, speciesCode, Indicating several features about ages and current vegetation of stand                                                                                                                                                                                                  |NA                                                                                                                                                              |
|ecoregion                        |data.table               |ecoregion look up table                                                                                                                                                                                                                                                                                      |https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt              |
|ecoregionMap                     |RasterLayer              |ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table. Defaults to a dummy map matching rasterToMatch with two regions                                                                                                                                                            |NA                                                                                                                                                              |
|lastReg                          |numeric                  |an internal counter keeping track of when the last regeneration event occurred                                                                                                                                                                                                                               |NA                                                                                                                                                              |
|minRelativeB                     |data.frame               |table defining the cut points to classify stand shadeness                                                                                                                                                                                                                                                    |NA                                                                                                                                                              |
|pixelGroupMap                    |RasterLayer              |initial community map that has mapcodes match initial community table                                                                                                                                                                                                                                        |NA                                                                                                                                                              |
|rasterToMatch                    |RasterLayer              |a raster of the studyArea in the same resolution and projection as biomassMap                                                                                                                                                                                                                                |NA                                                                                                                                                              |
|species                          |data.table               |a table that has species traits such as longevity, shade tolerance, etc. Default is partially based on Dominic Cir and Yan's project                                                                                                                                                                         |https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv                                                                    |
|speciesEcoregion                 |data.table               |table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time. Defaults to a dummy table based on dummy data os biomass, age, ecoregion and land cover class                                                                                                            |NA                                                                                                                                                              |
|speciesLayers                    |RasterStack              |cover percentage raster layers by species in Canada species map. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived species cover maps from 2001 using a cover threshold of 10 - see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata |http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/                      |
|sppColorVect                     |character                |A named vector of colors to use for plotting. The names must be in sim$speciesEquivalency[[sim$sppEquivCol]], and should also contain a color for 'Mixed'                                                                                                                                                    |NA                                                                                                                                                              |
|sppEquiv                         |data.table               |table of species equivalencies. See LandR::sppEquivalencies_CA.                                                                                                                                                                                                                                              |NA                                                                                                                                                              |
|studyArea                        |SpatialPolygonsDataFrame |Polygon to use as the study area. Defaults to an area in Southwestern Alberta, Canada.                                                                                                                                                                                                                       |NA                                                                                                                                                              |
|studyAreaReporting               |SpatialPolygonsDataFrame |multipolygon (typically smaller/unbuffered than studyArea) to use for plotting/reporting. Defaults to an area in Southwestern Alberta, Canada.                                                                                                                                                               |NA                                                                                                                                                              |
|sufficientLight                  |data.frame               |table defining how the species with different shade tolerance respond to stand shadeness. Default is based on LANDIS-II Biomass Succession v6.2 parameters                                                                                                                                                   |https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt |
|treedFirePixelTableSinceLastDisp |data.table               |3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred                                                                                      |NA                                                                                                                                                              |

Below are two lists of input objects that the user should supply to `Biomass_core` in order for it to run on a realistic setting (this is not, however, an exhaustive list of inputs) .

Required inputs    | Description
------------------ | -------------------------------------------------------------------------------
`biomassMap`       | map of total biomass in the study area
`ecoregion`        | a table listing values in `ecoregionMap` and their state (active or not)
`minRelativeB`     | table defining the cut points to classify stand shadiness
`rasterToMatch`    | the map of the study area.
`species`          | a table of species traits (based on LANDIS-II species traits for [v3.2.1] (https://github.com/LANDIS-II-Foundation/Extensions-Succession-Archive/blob/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt))
`sufficientLight`  | table defining species responses to shade according to shade tolerance levels

Init. objects      | Description
------------------ | -------------------------------------------------------------------------------
`cohortData`       | table with cohort information by pixelGroup 
`ecoregionMap`     | map of ecological zones in the study area
`pixelGroupMap`    | map of pixelGroup
`speciesEcoregion` | a table of species traits by for ecological zone in ecoregionMap

For the beginner user, we suggest running `Biomass_core` without supplying any inputs and inspecting the above mentioned objects to understand their structure and format.
The user can later either feed these objects via `simInit`, or make a module that makes them and provides necessary inputs to `Biomass_core` (see e.g. [Boreal_Biomass_coreDataPrep](https://github.com/eliotmcintire/Boreal_Biomass_coreDataPrep))

Besides the above mentioned inputs, `Biomass_core` uses the following parameters, which can be changed by the user if need be:


```
## defineParameter: 'growthInitialTime' is not of specified type 'numeric'.
```

```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|paramName                 |paramClass |default      |min |max |paramDesc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-------------------------|:----------|:------------|:---|:---|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|calcSummaryBGM            |character  |end          |NA  |NA  |A character vector describing when to calculate the summary of biomass, growth and mortality Currently any combination of 5 options is possible: 'start'- as before vegetation succession events, i.e. before dispersal, 'postDisp' - after dispersal, 'postRegen' - after post-disturbance regeneration (currently the same as 'start'), 'postGM' - after growth and mortality, 'postAging' - after aging, 'end' - at the end of vegetation succesion events, before plotting and saving. The 'end' option is always active, being also the default option.                                                           |
|calibrate                 |logical    |FALSE        |NA  |NA  |Do calibration? Defaults to FALSE                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|cohortDefinitionCols      |character  |pixelGro.... |NA  |NA  |cohortData columns that determine what constitutes a cohort This parameter should only be modified if additional modules are adding columns to cohortData                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|cutpoint                  |numeric    |1e+10        |NA  |NA  |A numeric scalar indicating how large each chunk of an internal data.table is, when processing by chunks                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|gmcsGrowthLimits          |numeric    |66.66666.... |NA  |NA  |if using LandR.CS for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model                                                                                                                                                                                                                                                                     |
|gmcsMortLimits            |numeric    |66.66666.... |NA  |NA  |if using LandR.CS for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model                                                                                                                                                                                                                                                                     |
|gmcsMinAge                |numeric    |21           |0   |NA  |if using LandR.CS for climate-sensitive growth and mortality, the minimum age for which to predict climate-sensitive growth and mortality. Young stands (< 30) are poorly represented by the PSP data used to parameterize the model.                                                                                                                                                                                                                                                                                                                                                                                  |
|growthAndMortalityDrivers |character  |LandR        |NA  |NA  |package name where the following functions can be found: calculateClimateEffect, assignClimateEffect (see LandR.CS for climate sensitivity, leave default if none desired)                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|growthInitialTime         |numeric    |start(sim)   |NA  |NA  |Initial time for the growth event to occur                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|initialBiomassSource      |character  |cohortData   |NA  |NA  |Currently, there are three options: 'spinUp', 'cohortData', 'biomassMap'. If 'spinUp', it will derive biomass by running spinup derived from Landis-II. If 'cohortData', it will be taken from the 'cohortData' object, i.e., it is already correct, by cohort. If 'biomassMap', it will be taken from `sim$biomassMap`, divided across species using `sim$speciesLayers` percent cover values `spinUp` uses `sim$standAgeMap` as the driver, so biomass is an output. That means it will be unlikely to match any input information about biomass, unless this is set to TRUE, and a `sim$rawBiomassMap` is supplied. |
|keepClimateCols           |logical    |FALSE        |NA  |NA  |include growth and mortality predictions in `cohortData`?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|minCohortBiomass          |numeric    |0            |NA  |NA  |cohorts with biomass below this threshold are removed. Not a LANDIS-II BSM param.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|mixedType                 |numeric    |2            |NA  |NA  |How to define mixed stands: 1 for any species admixture; 2 for deciduous > conifer. See `?vegTypeMapGenerator`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|plotOverstory             |logical    |FALSE        |NA  |NA  |swap max age plot with overstory biomass                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|seedingAlgorithm          |character  |wardDisp.... |NA  |NA  |choose which seeding algorithm will be used among noDispersal, universalDispersal, and wardDispersal (default). Species dispersal distances (in the 'species' table) are based on LANDIS-II parameters.                                                                                                                                                                                                                                                                                                                                                                                                                |
|spinupMortalityfraction   |numeric    |0.001        |NA  |NA  |defines the mortality loss fraction in spin up-stage simulation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|sppEquivCol               |character  |Boreal       |NA  |NA  |The column in sim$specieEquivalency data.table to use as a naming convention                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|successionTimestep        |numeric    |10           |NA  |NA  |defines the simulation time step, default is 10 years. Note that growth and mortality always happen on a yearly basis.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|vegLeadingProportion      |numeric    |0.8          |0   |1   |a number that define whether a species is leading for a given pixel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|.maxMemory                |numeric    |5            |NA  |NA  |maximum amount of memory (in GB) to use for dispersal calculations.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|.plotInitialTime          |numeric    |start(sim)   |NA  |NA  |Vector of length = 1, describing the simulation time at which the first plot event should occur. Set to NA to turn plotting off.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|.plotInterval             |numeric    |NA           |NA  |NA  |defines the plotting time step. If NA, the default, .plotInterval is set to successionTimestep.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|.plotMaps                 |logical    |TRUE         |NA  |NA  |Controls whether maps should be plotted or not                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|.saveInitialTime          |numeric    |NA           |NA  |NA  |Vector of length = 1, describing the simulation time at which the first save event should occur. Set to NA if no saving is desired. If not NA, then saving will occur at .saveInitialTime with a frequency equal to .saveInterval                                                                                                                                                                                                                                                                                                                                                                                      |
|.saveInterval             |numeric    |NA           |NA  |NA  |defines the saving time step. If NA, the default, .saveInterval is set to successionTimestep.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|.studyAreaName            |character  |NA           |NA  |NA  |Human-readable name for the study area used. If NA, a hash of studyArea will be used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|.useCache                 |character  |.inputOb.... |NA  |NA  |Internal. Can be names of events or the whole module name; these will be cached by SpaDES                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|.useParallel              |ANY        |2            |NA  |NA  |Used only in seed dispersal. If numeric, it will be passed to data.table::setDTthreads and should be <= 2; If TRUE, it will be passed to `parallel:makeCluster`; and if a cluster object, it will be passed to `parallel::parClusterApplyB`.                                                                                                                                                                                                                                                                                                                                                                           |

## Initialization

Unlike the initialization in LANDIS-II, which "iterates the number of time steps equal to the maximum cohort age for each site", beginning at t - oldest cohort age and adding cohorts at the appropriate time ([Scheller & Miranda 2016](https://studylib.net/doc/6761603/landis-ii-biomass-succession-v3.2-user-guide)), Biomass_core initializes the simulation by deriving initial biomasses from available data.

## Compartmentalisation and modularity
While in LANDIS-II phenomena like growth and mortality occur as part of the same process (or function from a coding perspective), in Biomass_core they occur separately and sequentially. This means that they can be more easily upgraded/changed. The same happens with post-disturbance regeneration. Unlike in LANDIS-II, post-disturbance regeneration is not part of Biomass_core *per se*, but rather belongs to a separate module, that needs to be loaded should the user want to simulate disturbance effect (*i.e.*, fire disturbances). Again, this enables higher flexibility when swapping between different approaches to regeneration.
For instance, default (*i.e.* not climate sensitive) growth and mortality functions are part of the `LandR` R package, which needs to be loaded prior to running `Biomass_core`. Should the user wish to change the growth/mortality algorithms, they would need to provide compatible functions (with the same names) to the simulation via `simInit` - user-provided functions will replace those loaded with a package.
  + Note: The `LandR` package provides other supporting functions and objects to the simulation, and still needs to be loaded prior to running `Biomass_core`.

# Simulation flow

## No disturbances 

Biomass_core itself does not simulate disturbances, or their effect on vegetation (*i.e.* post-disturbance mortality and regeneration). The general flow of Biomass_core processes is:

1. Preparation of necessary objects for the simulation - either by accessory data prep. modules, or Biomass_core itself (using LANDIS-II test parameters and dummy data for stand age, biomass and land cover and ecological zoning)
2. Seed dispersal - see Section 4.5.1 Seeding of the LANDIS-II Model v7.0 Description(https://drive.google.com/file/d/15gSueug-Rj9I2RZqdroDbad-k53Jq7j3/view) for details
  + Seed dispersal can be a slow process and has been adapted to occur every 10 years. The user can set it to occur more often, but this should not make much of a difference to model outputs, because age classes are meant to be collapsed to tens.
3. Growth and mortality - based on [Scheller and Mladenoff (2004)](https://pdfs.semanticscholar.org/4d38/d0be6b292eccd444af399775d37a757d1967.pdf)
  + unlike dispersal, growth and mortality should occur every year
4. Ageing - based on [Scheller and Mladenoff (2004)](https://pdfs.semanticscholar.org/4d38/d0be6b292eccd444af399775d37a757d1967.pdf)
  + follows the same frequency as dispersal, collapsing ages to classes with resolution = to this frequency
5. Preparation of visual/saved outputs
... (repeat 2-4) ...

## With disturbances 

Note that should a post-disturbance regeneration module be used, regeneration will occur after the disturbance, but *before* dispersal and background vegetation growth and mortality. Hence, the disturbance should take place either at the very beginning or at the very end of each simulation time step. The general flow of Biomass_core processes when disturbances are included (by linking other modules) is:

1. Preparation of necessary objects for the simulation - either by accessory prep. data modules, or Biomass_core itself (using LANDIS-II test parameters and dummy data.)
2. Disturbances - simulated by a disturbance module
3. Post-disturbance regeneration - simulated by a regeneration module (`Biomass_regeneration` is an optional download)
4. Seed dispersal - see Section 4.5.1 Seeding of the LANDIS-II Model v7.0 Description(https://drive.google.com/file/d/15gSueug-Rj9I2RZqdroDbad-k53Jq7j3/view) for details
5. Growth, ageing and mortality - based on [Scheller and Mladenoff (2004)](https://pdfs.semanticscholar.org/4d38/d0be6b292eccd444af399775d37a757d1967.pdf)
6. Preparation of visual/saved outputs
... (repeat 2-6) ...

# Usage example

## Load SpaDES


```r
library(SpaDES)
library(LandR)

## April 1st, 2019:
##  growth/mortality processes still in separate module, but will be ported back to Biomass_core soon.
moduleName <- c("Biomass_core")  
spadesModulesDirectory <- ".." # In general, a module code will be controlled at one level above the source code
```

## Get the module

See [SpaDES-modules repository](https://github.com/PredictiveEcology/SpaDES-modules) to see how to download this and other SpaDES modules. Alternatively, it can be forked or cloned from github.com directly.

## Setup simulation


```r
setPaths(inputPath = "inputs", 
         cachePath = "cache", 
         modulePath = spadesModulesDirectory, 
         outputPath = "outputs")

times <- list(start = 0, end = 1)

studyArea <- Cache(randomStudyArea, size = 1e7) # cache this so it creates a random one only once on a machine

# Pick the species you want to work with -- using the naming convention in "Boreal" column of LandR::sppEquivalencies_CA
speciesNameConvention <- "Boreal"
speciesToUse <- c("Pice_Gla", "Popu_Tre", "Pinu_Con")

sppEquiv <- LandR::sppEquivalencies_CA[get(speciesNameConvention) %in% speciesToUse]
# Assign a colour convention for graphics for each species
sppColorVect <- LandR::sppColors(sppEquiv, speciesNameConvention,
                                  newVals = "Mixed", palette = "Accent")

## Usage example
modules <- as.list(moduleName)
objects <- list(studyArea = studyArea, sppEquiv = sppEquiv, sppColorVect = sppColorVect)
paths <- getPaths()

successionTimestep <- 1L

## keep default values for most parameters 
## (ommitted from this list)
parameters <- list(
  Biomass_core = list(
    ".plotInitialTime" = times$start
    , "sppEquivCol" = speciesNameConvention
    , "successionTimestep" = successionTimestep*10
    , ".saveInitialTime" = 1
    , ".useCache" = FALSE 
    , ".useParallel" = FALSE
  )
)

mySim <- simInit(times = times, params = parameters, modules = modules, objects = objects, paths = paths)
mySimOut <- spades(mySim, debug = TRUE)
```

# Events
Events are scheduled as follows:

- Module initiation
- Seed dispersal (every 10 time steps)
- Mortality and growth
- Reclassification of age cohorts (every 10 time steps)
- Summary tables of regeneration (summaryRegen), biomass, age, growth and mortality (summaryBGM*)
- Plots of maps (plotMaps) and averages (plotAvgs)
- Save

# Output data

The module produces the following outputs:


```
## defineParameter: 'growthInitialTime' is not of specified type 'numeric'.
```

```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName                       |objectClass |desc                                                                                                                                                                                                                    |
|:--------------------------------|:-----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|activePixelIndex                 |integer     |internal use. Keeps track of which pixels are active                                                                                                                                                                    |
|activePixelIndexReporting        |integer     |internal use. Keeps track of which pixels are active in the reporting study area                                                                                                                                        |
|ANPPMap                          |RasterLayer |ANPP map at each succession time step                                                                                                                                                                                   |
|cohortData                       |data.table  |age cohort-biomass table hooked to pixel group map by pixelGroupIndex at succession time step                                                                                                                           |
|inactivePixelIndex               |logical     |internal use. Keeps track of which pixels are inactive                                                                                                                                                                  |
|inactivePixelIndexReporting      |integer     |internal use. Keeps track of which pixels are inactive in the reporting study area                                                                                                                                      |
|lastFireYear                     |numeric     |Year of the most recent fire year                                                                                                                                                                                       |
|lastReg                          |numeric     |an internal counter keeping track of when the last regeneration event occurred                                                                                                                                          |
|minRelativeB                     |data.frame  |define the cut points to classify stand shadeness                                                                                                                                                                       |
|mortalityMap                     |RasterLayer |Mortality map at each succession time step                                                                                                                                                                              |
|pixelGroupMap                    |RasterLayer |updated community map at each succession time step                                                                                                                                                                      |
|regenerationOutput               |data.table  |TODO: description needed                                                                                                                                                                                                |
|reproductionMap                  |RasterLayer |Regeneration map at each succession time step                                                                                                                                                                           |
|simulatedBiomassMap              |RasterLayer |Biomass map at each succession time step                                                                                                                                                                                |
|simulationOutput                 |data.table  |contains simulation results by ecoregion (main output)                                                                                                                                                                  |
|simulationTreeOutput             |data.table  |Summary of several characteristics about the stands, derived from cohortData                                                                                                                                            |
|species                          |data.table  |a table that has species traits such as longevity, shade tolerance, etc. Currently obtained from LANDIS-II Biomass Succession v.6.0-2.0 inputs                                                                          |
|speciesEcoregion                 |data.table  |define the maxANPP, maxB and SEP change with both ecoregion and simulation time                                                                                                                                         |
|speciesLayers                    |RasterStack |biomass percentage raster layers by species in Canada species map                                                                                                                                                       |
|spinupOutput                     |data.table  |Spin-up output                                                                                                                                                                                                          |
|summaryBySpecies                 |data.table  |The total species biomass, average age and aNPP across the landscape (used for plotting and reporting).                                                                                                                 |
|summaryBySpecies1                |data.table  |No. pixels of each leading vegetation type (used for plotting and reporting).                                                                                                                                           |
|summaryLandscape                 |data.table  |The averages of total biomass, age and aNPP across the landscape (used for plotting and reporting).                                                                                                                     |
|treedFirePixelTableSinceLastDisp |data.table  |3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred |
|vegTypeMap                       |RasterLayer |Map of leading species in each pixel, colored according to sim$sppColorVect                                                                                                                                             |

# Links to other modules

Intended to be used with other landscape modules, such as LandMine, FireSense, Biomass_borealDataPrep, and possibly many others.


## Getting help

- <https://gitter.im/PredictiveEcology/LandR_Biomass>


---
title: "LandR _Biomass_core_ Manual"
subtitle: "v.1.3.3"
date: "Last updated: 2022-02-25"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_Biomass_core.bib
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

# LandR *Biomass_core* Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:moduleName) *Biomass_core*



[![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.png)](http://commonmark.org) [![Generic badge](https://img.shields.io/badge/Get%20help-Report%20issues-%3CCOLOR%3E.png)](https://github.com/PredictiveEcology/Biomass_core/issues)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

**This documentation is work in progress. Potential discrepancies and omissions may exist for the time being. If you find any, do contact us using the link above\^\^**

#### Authors:

Yong Luo <yluo1@lakeheadu.ca> [aut], Eliot J B McIntire <eliot.mcintire@canada.ca> [aut, cre], Jean Marchal <jean.d.marchal@gmail.com> [ctb], Alex M. Chubaty <achubaty@for-cast.ca> [ctb], Ceres Barros <cbarros@mail.ubc.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

LandR *Biomass_core* (hereafter *Biomass_core*) is the core forest succession simulation module of the LandR ecosystem of `SpaDES` modules [see @ChubatyMcIntire2019]. It simulates tree cohort ageing, growth, mortality and competition for light resources, as well as seed dispersal (Fig. \@ref(fig:fig-Biomass-core)), in a spatially explicit manner and using a yearly time steps. The model is based on the LANDIS-II Biomass Succession Extension v.3.2.1 [LBSE; @SchellerMiranda2015], with a few changes (see [Differences between *Biomass_core* and LBSE]). Nonetheless, the essential functioning of the succession model still largely follows its LANDIS-II counterpart, and we refer the reader to the corresponding LANDIS-II BSE manual [@SchellerMiranda2015] for a detailed reading of the mechanisms implemented in the model.

<div class="figure" style="text-align: center">
<img src="figures/Biomass_coreSchematic.png" alt="(ref:moduleName) simulates tree cohort growth, mortality, recruitment and dispersal dynamics, as a function of  cohort ageing and competition for light (shading) and space, as well as disturbances like fire (simulated using other modules)." width="60%" />
<p class="caption">(\#fig:fig-Biomass-core)(ref:moduleName) simulates tree cohort growth, mortality, recruitment and dispersal dynamics, as a function of  cohort ageing and competition for light (shading) and space, as well as disturbances like fire (simulated using other modules).</p>
</div>

### Module inputs and parameters

*Biomass_core* is capable of running on dummy datasets from which it estimates parameters linked to vegetation growth and seed germination (such as the maximum biomass per species, per pixel, and the probability of seed germination -- *i.e.*, species establishment probability not due to resprouting), but also builds and initializes forest communities (based on biomass, age, species composition, land cover and ecological zones like ecodistricts.

Ideally, however, the user should supply realistic versions of these data and the essential initialization objects that *Biomass_core* requires to run.

Table \@ref(tab:moduleInputs-Biomass-core) shows a full list of input objects that *Biomass_core* expects. Of these, the only input that **must** be provided (*i.e.*, *Biomass_core* does not have a default for) is `studyArea`.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-Biomass-core)List of (ref:moduleName) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> biomassMap </td>
   <td style="text-align:left;"> total biomass raster layer in study area (in g/m^2), filtered for pixels covered by cohortData. Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cceArgs </td>
   <td style="text-align:left;"> a list of quoted objects used by the `growthAndMortalityDriver` `calculateClimateEffect` function </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> `data.table` with cohort-level information on age and biomass, by pixelGroup and ecolocation (i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer), `ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in g/m^2), `age` (integer in years) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregion </td>
   <td style="text-align:left;"> ecoregion look up table </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionMap </td>
   <td style="text-align:left;"> ecoregion map that has mapcodes match ecoregion table and `speciesEcoregion` table. Defaults to a dummy map matching `rasterToMatch` with two regions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lastReg </td>
   <td style="text-align:left;"> an internal counter keeping track of when the last regeneration event occurred </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minRelativeB </td>
   <td style="text-align:left;"> table defining the relative biomass cut points to classify stand shadeness </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> initial community map that has mapcodes match initial community table </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> a raster of the `studyArea` in the same resolution and projection as `biomassMap` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> a table that has species traits such as longevity, shade tolerance, etc. Default is partially based on Dominic Cyr and Yan Boulanger's project </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesEcoregion </td>
   <td style="text-align:left;"> table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time. Defaults to a dummy table based on dummy data os biomass, age, ecoregion and land cover class </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesLayers </td>
   <td style="text-align:left;"> percent cover raster layers of tree species in Canada. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived species cover maps from 2001 using a cover threshold of 10 - see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppColorVect </td>
   <td style="text-align:left;"> A named vector of colors to use for plotting. The names must be in `sim$speciesEquivalency[[sim$sppEquivCol]]`, and should also contain a color for 'Mixed' </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> table of species equivalencies. See `LandR::sppEquivalencies_CA`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> Polygon to use as the study area. Must be provided by the user </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaReporting </td>
   <td style="text-align:left;"> multipolygon (typically smaller/unbuffered than studyArea) to use for plotting/reporting. Defaults to `studyArea`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sufficientLight </td>
   <td style="text-align:left;"> table defining how the species with different shade tolerance respond to stand shade. Default is based on LANDIS-II Biomass Succession v6.2 parameters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treedFirePixelTableSinceLastDisp </td>
   <td style="text-align:left;"> 3 columns: `pixelIndex`, `pixelGroup`, and `burnTime`. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred </td>
  </tr>
</tbody>
</table>

Of the above, we draw particular attention to the the following inputs, which are crucial to run *Biomass_core* on a realistic setting (see [Input objects] section of the manual for further detail):

-   Spatial layers: `ecoregionMap`, `studyArea`

-   Trait and parameter tables: `ecoregion`, `minRelativeB`, `species`, `speciesEcoregion`, `sufficientLight`, `sppEquiv`, `sppColorVect`

-   Cohort-simulation related: `cohortData`, `pixelGroupMap`

For the beginner user, we suggest running *Biomass_core* without supplying any inputs and inspecting the above mentioned objects to understand their structure and format. The user can later either feed these objects via `simInit`, or make a module that makes them and provides necessary inputs to *Biomass_core* (see e.g. [*Biomass_borealDataPrep*](https://github.com/PredictiveEcology/Biomass_borealDataPrep))

Besides the above mentioned inputs, *Biomass_core* uses several other parameters, which can be changed by the user if need be (Table \@ref(tab:moduleParams-Biomass-core)). Please see the [Parameters] section of the manual for a list of the most useful parameters.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-Biomass-core)List of (ref:moduleName) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> calcSummaryBGM </td>
   <td style="text-align:left;"> A character vector describing when to calculate the summary of biomass, growth and mortality Currently any combination of 5 options is possible: 'start'- as before vegetation succession events, i.e. before dispersal, 'postDisp' - after dispersal, 'postRegen' - after post-disturbance regeneration (currently the same as 'start'), 'postGM' - after growth and mortality, 'postAging' - after aging, 'end' - at the end of vegetation succesion events, before plotting and saving. The 'end' option is always active, being also the default option. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> calibrate </td>
   <td style="text-align:left;"> Do calibration? Defaults to `FALSE` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortDefinitionCols </td>
   <td style="text-align:left;"> `cohortData` columns that determine what constitutes a cohort This parameter should only be modified if additional modules are adding columns to cohortData </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cutpoint </td>
   <td style="text-align:left;"> A numeric scalar indicating how large each chunk of an internal data.table is, when processing by chunks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsGrowthLimits </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsMortLimits </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsMinAge </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, the minimum age for which to predict climate-sensitive growth and mortality. Young stands (&lt; 30) are poorly represented by the PSP data used to parameterize the model. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> growthAndMortalityDrivers </td>
   <td style="text-align:left;"> package name where the following functions can be found: `calculateClimateEffect`, `assignClimateEffect` (see `LandR.CS` for climate sensitivity equivalent functions, or leave default if this is not desired) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> growthInitialTime </td>
   <td style="text-align:left;"> Initial time for the growth event to occur </td>
  </tr>
  <tr>
   <td style="text-align:left;"> initialBiomassSource </td>
   <td style="text-align:left;"> Currently, there are three options: 'spinUp', 'cohortData', 'biomassMap'. If 'spinUp', it will derive biomass by running spinup derived from Landis-II. If 'cohortData', it will be taken from the `cohortData` object, i.e., it is already correct, by cohort. If 'biomassMap', it will be taken from `sim$biomassMap`, divided across species using `sim$speciesLayers` percent cover values 'spinUp' uses `sim$standAgeMap` as the driver, so biomass is an output . That means it will be unlikely to match any input information about biomass, unless this is set to 'biomassMap', and a `sim$biomassMap` is supplied. Only the 'cohortData' option is currently active. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> keepClimateCols </td>
   <td style="text-align:left;"> include growth and mortality predictions in `cohortData`? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minCohortBiomass </td>
   <td style="text-align:left;"> cohorts with biomass below this threshold (g/m^2) are removed. Not a LANDIS-II BSE parameter. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mixedType </td>
   <td style="text-align:left;"> How to define mixed stands: 1 for any species admixture; 2 for deciduous &gt; conifer. See `?LandR::vegTypeMapGenerator`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> plotOverstory </td>
   <td style="text-align:left;"> swap max age plot with overstory biomass </td>
  </tr>
  <tr>
   <td style="text-align:left;"> seedingAlgorithm </td>
   <td style="text-align:left;"> choose which seeding algorithm will be used among 'noDispersal', 'universalDispersal', and 'wardDispersal' (default). See Scheller &amp; Miranda (2015) - Biomass Succession extension, v3.2.1 User Guide </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spinupMortalityfraction </td>
   <td style="text-align:left;"> defines the mortality loss fraction in spin up-stage simulation. Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> The column in `sim$specieEquivalency` data.table to use as a naming convention </td>
  </tr>
  <tr>
   <td style="text-align:left;"> successionTimestep </td>
   <td style="text-align:left;"> defines the simulation time step, default is 10 years. Note that growth and mortality always happen on a yearly basis. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vegLeadingProportion </td>
   <td style="text-align:left;"> a number that define whether a species is leading for a given pixel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .maxMemory </td>
   <td style="text-align:left;"> maximum amount of memory (in GB) to use for dispersal calculations. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> Vector of length = 1, describing the simulation time at which the first plot event should occur. To plotting off completely use `P(sim)$.plots`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> defines the plotting time step. If `NA`, the default, .plotInterval is set to successionTimestep. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set. Note that plots (or their data) saving will ONLY occur at `end(sim)`. If `NA` plotting is off completely (this includes plot saving). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotMaps </td>
   <td style="text-align:left;"> Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> Vector of length = 1, describing the simulation time at which the first save event should occur. Set to `NA` if no saving is desired. If not `NA`, then saving will occur at `P(sim)$.saveInitialTime` with a frequency equal to `P(sim)$.saveInterval` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> defines the saving time step. If `NA`, the default, .saveInterval is set to `P(sim)$successionTimestep`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> Human-readable name for the study area used. If `NA`, a hash of `studyArea` will be used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> Internal. Can be names of events or the whole module name; these will be cached by `SpaDES` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useParallel </td>
   <td style="text-align:left;"> Used only in seed dispersal. If numeric, it will be passed to `data.table::setDTthreads` and should be &lt;= 2; If `TRUE`, it will be passed to `parallel::makeCluster`; and if a cluster object, it will be passed to `parallel::parClusterApplyB`. </td>
  </tr>
</tbody>
</table>

### Events

Events are scheduled as follows:

-   Module initiation (`init` event)
-   Seed dispersal (every `successionTimestep`; `Dispersal` event)
-   Mortality and growth (`mortalityAndGrowth` event)
-   Reclassification of age cohorts (every `successionTimestep`; `cohortAgeReclassification` event)
-   Summary tables of regeneration (`summaryRegen` event), biomass, age, growth and mortality (`summaryBGM\*` event)
-   Plots of maps (`plotMaps` event) and averages (`plotAvgs` and `plotSummaryBySpecies` events)
-   Save (`save`)

### Module outputs

The module produces the following outputs types. -- Plotting -- live and/or saved plot objects/images (depending on `.plots`)

-- Saved biomass, mortality, leading vegetation raster layers -- Whatever objects supplied to `outputs` argument in `simInit`, that are within the `simList` object.

All `simList` objects that are changed by *Biomass_core* (*i.e.*, the definition of a module output) are listed in Table \@ref(tab:moduleOutputs-Biomass-core).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-Biomass-core)List of (ref:moduleName) output objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> activePixelIndex </td>
   <td style="text-align:left;"> internal use. Keeps track of which pixels are active </td>
  </tr>
  <tr>
   <td style="text-align:left;"> activePixelIndexReporting </td>
   <td style="text-align:left;"> internal use. Keeps track of which pixels are active in the reporting study area </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ANPPMap </td>
   <td style="text-align:left;"> ANPP map at each succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> `data.table` with cohort-level information on age, biomass, aboveground primary productivity (year's biomass gain) and mortality (year's biomass loss), by pixelGroup and ecolocation (i.e., `ecoregionGroup`). Contains at least the following columns: `pixelGroup` (integer), `ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in g/m^2), `age` (integer in years), `mortality` (integer in g/m^2), `aNPPAct` (integer in g/m^2). May have other columns depending on additional simulated processes (i.e., cliamte sensitivity; see, e.g., `P(sim)$keepClimateCols`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionMap </td>
   <td style="text-align:left;"> ecoregion map that has mapcodes match `ecoregion` table and `speciesEcoregion` table. Defaults to a dummy map matching rasterToMatch with two regions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> inactivePixelIndex </td>
   <td style="text-align:left;"> internal use. Keeps track of which pixels are inactive </td>
  </tr>
  <tr>
   <td style="text-align:left;"> inactivePixelIndexReporting </td>
   <td style="text-align:left;"> internal use. Keeps track of which pixels are inactive in the reporting study area </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lastFireYear </td>
   <td style="text-align:left;"> Year of the most recent fire year </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lastReg </td>
   <td style="text-align:left;"> an internal counter keeping track of when the last regeneration event occurred </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minRelativeB </td>
   <td style="text-align:left;"> define the cut points to classify stand shade </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mortalityMap </td>
   <td style="text-align:left;"> Mortality map at each succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> updated community map at each succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> regenerationOutput </td>
   <td style="text-align:left;"> If `P(sim)$calibrate == TRUE`, an summary of seed dispersal and germination success (i.e., number of pixels where seeds successfully germinated) per species and year. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> reproductionMap </td>
   <td style="text-align:left;"> Regeneration map at each succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simulatedBiomassMap </td>
   <td style="text-align:left;"> Biomass map at each succession time step (in g/m^2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simulationOutput </td>
   <td style="text-align:left;"> contains simulation results by ecoregion (main output) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simulationTreeOutput </td>
   <td style="text-align:left;"> Summary of several characteristics about the stands, derived from `cohortData` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> a table that has species traits such as longevity, shade tolerance, etc. Currently obtained from LANDIS-II Biomass Succession v.6.0-2.0 inputs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesEcoregion </td>
   <td style="text-align:left;"> define the maxANPP, maxB and SEP change with both ecoregion and simulation time </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesLayers </td>
   <td style="text-align:left;"> biomass percentage raster layers by species in Canada species map </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spinupOutput </td>
   <td style="text-align:left;"> Spin-up output </td>
  </tr>
  <tr>
   <td style="text-align:left;"> summaryBySpecies </td>
   <td style="text-align:left;"> The total species biomass (in g/m^2 as in `cohortData`), average age and aNPP (in g/m^2 as in `cohortData`), across the landscape (used for plotting and reporting). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> summaryBySpecies1 </td>
   <td style="text-align:left;"> No. pixels of each leading vegetation type (used for plotting and reporting). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> summaryLandscape </td>
   <td style="text-align:left;"> The averages of total biomass (in tonnes/ha , not g/m^2 like in `cohortData`), age and aNPP (also in tonnes/ha) across the landscape (used for plotting and reporting). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treedFirePixelTableSinceLastDisp </td>
   <td style="text-align:left;"> 3 columns: `pixelIndex`, `pixelGroup`, and `burnTime`. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vegTypeMap </td>
   <td style="text-align:left;"> Map of leading species in each pixel, colored according to `sim$sppColorVect`. Species mixtures calculated according to `P(sim)$vegLeadingProportion` and `P(sim)`$mixedType. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Intended to be used with other landscape modules, such as *LandMine*, *fireSense*, *Biomass_borealDataPrep*, *Biomass_regeneration* and possibly many others. You can see all *potential* module linkages within the LandR ecosystem [here](https://rpubs.com/PredictiveEcology/LandR_Module_Ecosystem). Select *Biomass_core* from the dropdown menu to see linkages with *Biomass_core*.

### Getting help

-   <https://github.com/PredictiveEcology/Biomass_core/issues>

## Module manual

### Introduction

LandR *Biomass_core* (hereafter *Biomass_core*) a forest landscape model based on the LANDIS-II Biomass Succession Extension v.3.2.1 model [LBSE; @SchellerMiranda2015]. It is the core forest succession model of the LandR ecosystem of `SpaDES` modules. Similarly to the LBSE, *Biomass_core* simulates changes in tree cohort aboveground biomass (g/m^2^.) by calculating growth, mortality and recruitment as functions of pixel and species characteristics, competition and disturbances (Fig. \@ref(fig:fig-Biomass-core)). Specifically, growth is driven by both invariant (`growthcurve`) and spatially varying species growth traits (maximum biomass, `maxB`, and maximum annual net primary productivity, `maxANPP`), while mortality depends only on invariant species traits (`age`, `longevity` and `mortalityshape`). Disturbances (e.g., fire) can also cause cohort mortality, but are simulated in separate modules (e.g., *Biomass_regeneration* simulates the death of all cohorts immediately after a fire). The parameters `growthcurve` and `mortalityshape` directly influence the shape of species growth curves, by determining how fast they grow and how soon age mortality starts with respect to longevity. Cohort recruitment is determined by available "space" (*i.e.*, pixel shade), invariant species traits (regeneration mode, age at maturity, shade tolerance) and spatially varying traits (species establishment probability, `SEP`). The available "growing space" is calculated as species `maxB` minus the occupied biomass (summed across other cohorts and species). If there is "space", a cohort can establish from one of three recruitment modes: serotiny, resprouting and germinating. Serotiny and resprouting occur only in response to fire and are simulated in two separate, but interchangeable modules, *Biomass_regeneration* and *Biomass_regenerationPM*. Germination occurs if seeds are made available from local sources (the pixel), or via seed dispersal. Seed dispersal can be of three modes: 'no dispersal', 'universal dispersal' (only interesting for dummy case studies) or 'ward dispersal' [@SchellerMiranda2015]. The 'ward dispersal' algorithm describes a flexible kernel that calculates the probability of a species colonising a neighbour pixel as a function of distance from the source and dispersal-related (and invariant) species traits, and is used by default. We refer the reader to @SchellerMiranda2015, @SchellerDomingo2011 and @SchellerDomingo2012 for further details with respect to the mechanisms implemented in the module.

### Differences between *Biomass_core* and LBSE

#### Algorithm changes

Upon porting LBSE into R, we made six minor modifications to the original model's algorithms to better reflect ecological processes. This did not result in dramatic changes in simulation outputs and we note that these changes might also have been implemented in more recent versions of LBSE.

First, for each year and community (*i.e.*, 'pixel group' in *Biomass_core*, see below), LBSE calculates the competition index for a cohort sequentially (*i.e.*, one cohort at a time) after updating the growth and mortality (*i.e.*, the biomass gain and loss, respectively) of other cohorts, and with the calculation sequence following cohort age in descending order, but no explicit order of species. This sorting of growth and mortality calculations from oldest to youngest cohorts in LBSE was aimed at capturing size-asymmetric competition between cohorts, under the assumption that older cohorts have priority for growing space given their greater height (Scheller pers. comm.). We felt that sequential, within-year growth, death and recruitment may be not ecologically accurate, and that the size-asymmetric competition was being accounted for twice, as the calculation of the competition index already considers the competitive advantage of older cohorts [as shown in the User's Guide; @SchellerMiranda2015]. Hence, in *Biomass_core* growth, mortality and the competition index are calculated at the same time across all cohorts and species.

Second, the unknown species-level sorting mechanism contained within LBSE (which changed depending on the species order in the input species list file), led to different simulation results depending on the input species list file (e.g., Table \@ref(tab:tableLBSEtest1) and Fig. \@ref(fig:figLBSEtest1)). The calculation of competition, growth and mortality for all cohorts at the same time also circumvented this issue.

Third, in LBSE the calculation of total pixel biomass for the purpose of calculating the initial biomass of a new cohort included the (previously calculated) biomass of other new cohorts when succession time step = 1, but not when time step was \> 1. This does not reflect the documentation in the User's Guide, which stated that "Bsum [total pixel biomass] is the current total biomass for the site (not including other new cohorts)" [@SchellerMiranda2015, pp. 4], when the succession time step was set to 1. Additionally, together with the lack of explicit ordering, it generated different results in terms of the biomass assigned to each new cohort (e.g. Table \@ref(tab:tableLBSEtest2) and Fig. \@ref(fig:figLBSEtest2)). In *Biomass_core* the initial biomass of new cohorts is no longer calculated sequentially (as with competition, growth and mortality), and thus the biomass of new cohorts is never included in the calculation of total pixel biomass.

Fourth, in LBSE, serotiny and resprouting could not occur in the same pixel following a fire, with serotiny taking precedence if activated. We understand that this provides an advantage to serotinous species, which could perhaps be disadvantaged with respect to fast-growing resprouters. However, we feel that it is ecologically more realistic that serotinous and resprouter species be able to both regenerate in a given community following a fire and allow the competition between serotinous and resprouting species to arise from species traits. **Note that this change was implemented in the *Biomass_regeneration* and *Biomass_regenerationPM* modules.**

Fifth, in *Biomass_core*, species shade tolerance values can have decimal values to allow for finer adjustments of between-species competition.

Sixth, we added a new parameter called `minCohortBiomass`, that allows the user to control cohort removal bellow a certain threshold of biomass. In some simulation set-ups, we noticed that *Biomass_core* (and LBSE) were able to generate many very small cohorts in the understory that, due to cohort competition, were not able to gain biomass and grow. However, because competition does not increase mortality, only decreases growth, these cohorts survived at very low biomass levels until they reached sufficient age to suffer age-related mortality. We felt this is unlikely to be realistic in many cases. By default, this parameter is left at 0 to follow LBSE behaviour (*i.e.*, no cohorts removal based on minimum biomass).

#### Other enhancements

In addition to the five minor changes in growth, mortality and regeneration, we separated the components that govern vegetation responses to disturbances -- only fire at the moment -- into two independent modules, used interchangeably, and implemented hashing, caching and testing to improve the model's computational efficiency and insure its performance.

##### Modularity

Unlike in LBSE, post-disturbance regeneration is not part of *Biomass_core* *per se*, but belongs to two separate modules, used interchangeably ([*Biomass_regeneration*](https://github.com/PredictiveEcology/Biomass_regeneration/blob/master/Biomass_regeneration.Rmd) and [*Biomass_regenerationPM*](https://github.com/PredictiveEcology/Biomass_regenerationPM/blob/master/Biomass_regenerationPM.Rmd)). These need to be loaded and added to the "modules folder" of the project in case the user wants to simulate forest responses to disturbances (only fire disturbances at the moment). Again, this enables higher flexibility when swapping between different approaches to regeneration. For instance, default (*i.e.*, not climate sensitive) growth and mortality functions are part of the `LandR` R package, which needs to be loaded prior to running *Biomass_core*. Should the user wish to change the growth/mortality algorithms, they would need to provide compatible functions (with the same names) to the simulation via `simInit` -- user-provided functions will replace those loaded with a package <!-- we should have an example of this-->. Note that the `LandR` package provides other supporting functions and objects to the simulation, and still needs to be loaded prior to running *Biomass_core*.

##### Hashing

Our first strategy to improve simulation efficiency in *Biomass_core* was to use a hashing mechanism [@YangEtAl2011]. Instead of assigning a key to each pixel in a raster and tracking the simulation for each pixel in a lookup table, we indexed pixels using a *pixelGroup* key that contained unique combinations of ecolocation and community, and tracked and stored simulation data for each *pixelGroup* (Fig. \@ref(fig:figLBSEtest3)). Ecolocation (called 'ecoregion' in LBSE and in model objects) is a spatial unit with similar biophysical characteristics. In our applications, we define ecolocation as the combination of land-cover types from the Land Cover Map of Canada 2005 (v1) and ecodistricts from the National Ecological Framework for Canada (<!--add source-->). Hence, these ecolocations contain relatively fine scale land cover information plus coarse scale regional information. In turn, community is the species composition and age structure of a particular pixel. This algorithm was able to ease the computational burden by significantly reducing the size of the lookup table and speeding-up the simulation process. After recruitment and disturbance events, pixels are rehashed into new pixel groups.

##### Caching

The second strategy aimed at improving model efficacy was the implementation of caching, and data-driven parametrisation and initialisation. Caching automatically archives outputs of a given function to disk (or memory) and reads them back when subsequent calls of this function are given identical inputs. All caching operations were achieved using the `reproducible` R package [@McIntireChubaty2020]. In the current version of *Biomass_core*, the spin-up phase was replaced by data-driven landscape initialisation and many model parameters were derived from data, using "data modules" (e.g., *Biomass_borealDataPrep*). To avoid having to repeat data downloads and treatment, statistical estimation of parameters and landscape initialisation every time the simulation is re-run under the same conditions (*i.e.*, no data or algorithm changes), many of these pre-simulation steps are automatically cached. This means that the pre-simulation phase is significantly faster upon a second call when inputs have not changed (e.g., the input data and parametrisation methods), and when inputs do change only directly affected steps are re-run (see main text for examples). When not using data modules, *Biomass_core* still relies on caching for the preparation of its theoretical inputs.

##### Testing

Finally, we implemented code testing, to facilitate bug detection by comparing the outputs of functions [etc.] to expected outputs [@Wickham2011]. We built and integrated code tests in *Biomass_core* and across all LandR modules and the `LandR` R package <!-- package name may change --> and the in the form of assertions and integration tests. Assertions are run automatically during simulations (but can be turned off), while integration are be run manually. Tests were also implemented in R package dependencies of *Biomass_core*, such as the `LandR` R package <!-- package name may change --> and `SpaDES`, which are routinely tested using GitHub Actions continuous integration (CI) or automated checks on CRAN. For the `LandR` R package, we use GitHub Actions CI to automatically test for installation and execution errors.

Finally, because *Biomass_core* (and all other LandR modules) code is hosted in public GitHub repositories, there is a potentially high number of users that can identify issues and contribute to improve module code.

#### Performance and accuracy of *Biomass_core* with respect to LBSE {data-link="Algorithm changes"}

In the recoding of *Biomass_core*, we ensured similar outputs of each demographic process (namely, growth, mortality and recruitment) to the outputs from its counterpart in LBSE, using integration tests. Here, we report the comparisons of the overall simulation (i.e., including all demographic processes) between LBSE and *Biomass_core* using three randomly generated initial communities (Tables \@ref(tab:tableLBSEtest3)-\@ref(tab:tableLBSEtest5)). The remaining input parameters were taken from a LANDIS-II training course (Tables \@ref(tab:tableLBSEtest6)-\@ref(tab:tableLBSEtest9)), and contained species attributes information of 16 common tree species in boreal forests and 2 ecolocations. We ran simulations for 1000 years, with a succession time step of 10 and three repetitions, which were enough to account for the variability produced by stochastic processes. Seed dispersal was set as "ward dispersal".

The results suggested that *Biomass_core* had a good agreement with LBSE using the three randomly generated initial communities (Fig. \@ref(fig:figLBSEtest4)), with very small deviations for LBSE-generated biomasses. Notably, the mean differences between LBSE and *Biomass_core* were 0.03% (range: -0.01% \~ 0.13%), 0.03% (range: -0.01% \~ 0.11%) and 0.05% (-0.02% \~ 0.15%) for each initial community, respectively (right panels in Fig. \@ref(fig:figLBSEtest4) of this appendix).

To examine how running time changed with map size, we ran simulations using maps with increasing number of pixels from 22,201 to 638,401. All maps were initialised with a single ecolocation and 7 different communities. Simulations were run for 120 years using a succession time step of 10 and replicated three times. To eliminate the effect of hardware on running time, we used machines that were all purchased at the same time, with equal specifications and running Windows 7. Each simulation ran on 2 CPU threads with a total RAM of 4000 Mb. For both LBSE and *Biomass_core*, the simulation time increased linearly with number of pixels, but the increase rate was smaller for *Biomass_core* (Fig. \@ref(fig:figLBSEtest5)a). This meant that while both models had similar simulation efficiencies in small maps (\< 90,000 pixels), as map size increased *Biomass_core* was \~2 times faster than LBSE (maps \> 100,000 pixels; Fig. \@ref(fig:figLBSEtest5)a). *Biomass_core* also scaled better with map size, as LBSE speeds fluctuated between 19 to 25 seconds per 1,000 pixels across all map sizes, while *Biomass_core* decreased from 21 to 11 seconds per 1,000 pixels from smaller to larger maps (Fig. \@ref(fig:figLBSEtest5)b).

### Initialization, inputs and parameters

Unlike the initialization in LBSE, which "iterates the number of time steps equal to the maximum cohort age for each site", beginning at *t* -- oldest cohort age and adding cohorts at the appropriate time [@SchellerMiranda2015], *Biomass_core* initializes the simulation by deriving initial biomasses from available data, using data modules. If data modules are not available, *Biomass_core* initializes itself with theoretical data.

To be initialized, *Biomass_core* requires the following input objects and parameters:

#### Input objects

All of *Biomass_core*'s input objects have (theoretical) defaults that are produced automatically by the module (when running the `.inputObjects` function during the `simInit` call, and in the `init` event during the `spades` call -- see `?SpaDES.core::events` and `SpaDES.core::simInit`). We suggest that new users run *Biomass_core* by itself supplying only a `studyArea` object. This will enable them to become familiar with all the input objects before attempting to supply their own, or combine *Biomass_core* with data modules.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs2-Biomass-core)List of (ref:moduleName) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> biomassMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> total biomass raster layer in study area (in g/m^2), filtered for pixels covered by cohortData. Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated. </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cceArgs </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> a list of quoted objects used by the `growthAndMortalityDriver` `calculateClimateEffect` function </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> `data.table` with cohort-level information on age and biomass, by pixelGroup and ecolocation (i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer), `ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in g/m^2), `age` (integer in years) </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregion </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> ecoregion look up table </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> ecoregion map that has mapcodes match ecoregion table and `speciesEcoregion` table. Defaults to a dummy map matching `rasterToMatch` with two regions </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lastReg </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> an internal counter keeping track of when the last regeneration event occurred </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minRelativeB </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> table defining the relative biomass cut points to classify stand shadeness </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> initial community map that has mapcodes match initial community table </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> a raster of the `studyArea` in the same resolution and projection as `biomassMap` </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> a table that has species traits such as longevity, shade tolerance, etc. Default is partially based on Dominic Cyr and Yan Boulanger's project </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesEcoregion </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time. Defaults to a dummy table based on dummy data os biomass, age, ecoregion and land cover class </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesLayers </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> percent cover raster layers of tree species in Canada. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived species cover maps from 2001 using a cover threshold of 10 - see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata </td>
   <td style="text-align:left;"> http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppColorVect </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> A named vector of colors to use for plotting. The names must be in `sim$speciesEquivalency[[sim$sppEquivCol]]`, and should also contain a color for 'Mixed' </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of species equivalencies. See `LandR::sppEquivalencies_CA`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> Polygon to use as the study area. Must be provided by the user </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaReporting </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> multipolygon (typically smaller/unbuffered than studyArea) to use for plotting/reporting. Defaults to `studyArea`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sufficientLight </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> table defining how the species with different shade tolerance respond to stand shade. Default is based on LANDIS-II Biomass Succession v6.2 parameters </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treedFirePixelTableSinceLastDisp </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> 3 columns: `pixelIndex`, `pixelGroup`, and `burnTime`. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Of the inputs in Table \@ref(tab:moduleInputs2-Biomass-core), the following are particularly important and deserve special attention:

-   **Spatial layers**

    -   `ecoregionMap` -- a raster layer with ecolocation IDs (note that the term "ecoregion" was inherited from LBSE and kept as is for consistency with original LBSE code). Ecolocations group pixels or similar biophysical conditions using up to two levels of grouping. In many of our applications, we use the Natural Ecoregion classification of Canada as the first grouping level and a land-cover classification as the second level. The raster layer must be defined as a categorical variable, with an associated Raster Attribute Table (RAT; see, e.g., `raster::ratify`). The RAT must contain the columns: `ID` (the value in the raster layer), `ecoregion` (the first level of ecolocation grouping) and `ecoregionGroup` (the full ecolocation "name" written as \<firstlevel_secondlevel>). Note that `ecoregionGroup` usually originated from combining two raster layers and, thus, the grouping level IDs are also integers. For instance, if Natural Ecoregion `2` has land-cover types `1`, `2` and `3`, the RAT will contain `ID = {1,2,3}`, `ecoregion = {2}` and `ecoregionGroup = {2_1, 2_2, 2_3}`. All ecolocations are listed in the `ecoregion` `data.table`.

    -   `rasterToMatch` -- a RasterLayer, with a given resolution and projection determining the pixels (*i.e.*, non NA values) where forest dynamics will be simulated. Needs to match `studyArea`. If not supplied, *Biomass_core* attempts to produce it, using `biomassMap` as the template for spatial resolution and projection.

    -   `studyArea` -- shapefile. A `SpatialPolygonsDataFrame` with a single polygon determing the where the simulation will take place. This is the only input object that **must be supplied by the user**.

-   **Species traits and other parameter tables**

    -   `ecoregion` -- `data.table` listing all ecolocation "names" (*ecoregionGroup* column; see `ecoregionMap` above for details) and their state (active -- `yes` -- or inactive -- `no`)

    -   `minRelativeB` -- `data.table` of minimum relative biomass values. This is a spatially variant trait used to determine the shade level in each pixel [see @SchellerMiranda2015, pp. 14], yet in our applications we often keep values constant across ecolocations. The table must contain the following columns:

        -   *ecoregionGroup --* character. Ecolocation names. See `ecoregionMap` and `ecoregion` objects above.

        -   *X0-X5* -- six numeric columns, one per shade class (no-shade, 0, to maximum shade, 5), with 0 to 1 values determining the minimum threshold of biomass (relative to the species/ecolocation `maxB`) necessary to reach a given shade-level. This means that shade-levels are determined on a species by species basis [see @SchellerMiranda2015, pp. 14]

    -   `species` -- `data.table` of *invariant species traits*. There are species traits that do no vary spatially, nor temporally (e.g., longevity). The table must contain the following trait values (*i.e.*, columns) in order to run *Biomass_core* (note that columns should follow the data type indicated):

        -   *speciesCode --* character. Species ID.

        -   *longevity --* integer. Maximum age in years [see @SchellerDomingo2011, pp. 18].

        -   *sexualmature --* integer. Age at sexual maturity in years [see @SchellerDomingo2011, pp. 18].

        -   *shadetolerance --* integer OR numeric. *Relative* shade tolerance (see [Algorithm changes]).

        -   *seeddistance_eff --* integer. Eeffective seed distance in meters. [see @SchellerDomingo2011, pp. 18]

        -   *seeddistance_max --* integer. Maximum seed distance in meters. Note that is the pixel size is larger than the maximum seed distance, the species will not be able to disperse to neighbouring pixels [see @SchellerDomingo2011, pp. 18].

        -   *mortalityshape --* integer. Shape of growth curve determining how quickly mortality begins [see @SchellerMiranda2015, pp. 15].

        -   *growthcurve --* numeric. Shape of growth curve determining ANPP reaches its maximum [see @SchellerMiranda2015, pp. 16].

    -   `speciesEcoregion` -- `data.table` of *spatiotemporally-varying species traits*. There are species traits vary spatially and, potentially, temporally. The table must contain the following columns in order to run *Biomass_core*:

        -   *ecoregionGroup* -- character. Ecolocation names. See `ecoregionMap` and `ecoregion` objects above.

        -   *speciesCode* -- character. Species ID.

        -   *establishprob* -- numeric. Species establishment probability (`SEP`) for a given species in an ecolocation and, potentially year. SEP influences the success of incoming seed germination, given pixel biophysical characteristics (note that *actual* success is determined by both SEP and light conditions in the pixel) [see @SchellerMiranda2015, pp. 18].

        -   *maxB* -- integer. Maximum biomass for a given species in an ecolocation in units of g biomass / m^2^. Note that the actual maximum biomass reached by a species in a pixel may exceed `maxB` because `maxB` is applied at the cohort level an species may have several cohorts in a given pixel [see @SchellerMiranda2015, pp. 18].

        -   *maxANPP* -- numeric. Maximum aboveground net primary productivity in units of g biomass / m^2^ / year, by default it is calculated as 1/30 of `maxB` [see @SchellerMiranda2015, pp. 18]

        -   *year* -- integer. Used when varying `SEP`, `maxB` and `maxANPP` values in time. Otherwise, use fill all lines with `0`.

    -   `sufficientLight` -- `data.table` defining the probability of germination for a species, given its `shadetolerance` level (see `species` above) and the shade level in the pixel (see `minRelativeB` above). Must contain columns:

        -   *speciesshadetolerance* -- integer. Species shade tolerance levels, from 1-5 (all levels must be present in this table).

        -   *X0-X5* -- six integer columns, one per shade class (no-shade, 0, to maximum shade, 5), filled with 0s OR 1s values determining the probability of germination (or resprouting) for a species given a shade-level[see @SchellerMiranda2015, pp. 14]. Unlike LBSE, species `shadetolerance` values can take decimal values between 1-5, in which case the resulting probability of germination in a given pixel is interpolated between the corresponding lower and upper shade tolerance values.

    -   `sppEquiv` -- a `data.table` of species name equivalencies between various conventions. It must contain the columns *LandR* (species IDs following in LandR format) *EN_generic_short* (short generic species names in English -- or any other language; used for plotting), *Type* (type of species, *Conifer* or *Deciduous*, as in "broadleaf") and *Leading* (same as *EN_generic_short* but with "leading" appended -- e.g., "Poplar leading") <!-- we should add an assertion that checks whether these columns are present early on!!! -->. See `?LandR::sppEquivalencies_CA` for more information.

    -   `sppColorVect` -- character. A named vector of colours used to plot species dynamics. Should contain one colour per species in the `species` table and, potentially a colour for species mixtures (named "Mixed"). Vector names must follow `species$speciesCode`.

-   **Cohort-simulation-related objects**

    -   `cohortData` -- a `data.table` containing initial cohort information per *pixelGroup* (see `pixelGroupMap` below). This table is updated during the simulation as cohort dynamics are simulated. Must contain the following columns

        -   *pixelGroup* -- integer. *pixelGroup* ID. See [Hashing].

        -   *ecoregionGroup* -- character. Ecolocation names. See `ecoregionMap` and `ecoregion` objects above.

        -   *speciesCode* -- character. Species ID.

        -   *age* -- integer. Cohort age.

        -   *B* -- integer. cohort biomass in g/m^2^.

        -   *mortality* -- integer. cohort dead biomass in the current year in g/m^2^. Should be filled with 0s in initial conditions.

        -   *aNPPAct* -- integer. Actual aboveground net primary productivity of the current year in g/m^2^. Hence *B* is the result of the previous year's *B* minus *mortality* plus *aNPPAct*. See "1.1.3 Cohort growth and ageing" section of @SchellerMiranda2015.

    -   `pixelGroupMap` -- a raster layer with *pixelGroup* IDs per pixel. Pixels are always grouped based on identical *ecoregionGroup*, *speciesCode*, *age* and *B* composition, even if the user supplies other initial groupings (e.g., this is possible in the *Biomass_borealDataPrep* data module).

#### Parameters

Table \@ref(tab:moduleParams2-Biomass-core) lists all parameters used in *Biomass_core*. Note that a few of these parameters are only relevant when simulating climate effects of cohort growth and mortality, which require also loading the `LandR.CS` R package. Like with input objects, default values are supplied for all parameters and we suggest the user becomes familiarized with them before attempting any changes. We also note that the `"spin-up"` and `"biomassMap"` options for the `initialBiomassSource` are currently deactivated, since *Biomass_core* no longer generates initial cohort biomass conditions using a spin-up based on initial stand age like LANDIS-II (`"spin-up"`), nor does it attempt to fill initial cohort biomasses using `biomassMap` (`"biomassMap"`). A list of useful parameters and their description is shown in Table \@ref(tab:tableUsefulParams).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams2-Biomass-core)List of (ref:moduleName) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> calcSummaryBGM </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> end </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A character vector describing when to calculate the summary of biomass, growth and mortality Currently any combination of 5 options is possible: 'start'- as before vegetation succession events, i.e. before dispersal, 'postDisp' - after dispersal, 'postRegen' - after post-disturbance regeneration (currently the same as 'start'), 'postGM' - after growth and mortality, 'postAging' - after aging, 'end' - at the end of vegetation succesion events, before plotting and saving. The 'end' option is always active, being also the default option. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> calibrate </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Do calibration? Defaults to `FALSE` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortDefinitionCols </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> pixelGro.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> `cohortData` columns that determine what constitutes a cohort This parameter should only be modified if additional modules are adding columns to cohortData </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cutpoint </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1e+10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A numeric scalar indicating how large each chunk of an internal data.table is, when processing by chunks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsGrowthLimits </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 66.66666.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsMortLimits </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 66.66666.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, a percentile is used to estimate the effect of climate on growth/mortality (currentClimate/referenceClimate). Upper and lower limits are suggested to circumvent problems caused by very small denominators as well as predictions outside the data range used to generate the model </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gmcsMinAge </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if using `LandR.CS` for climate-sensitive growth and mortality, the minimum age for which to predict climate-sensitive growth and mortality. Young stands (&lt; 30) are poorly represented by the PSP data used to parameterize the model. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> growthAndMortalityDrivers </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> LandR </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> package name where the following functions can be found: `calculateClimateEffect`, `assignClimateEffect` (see `LandR.CS` for climate sensitivity equivalent functions, or leave default if this is not desired) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> growthInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> start(sim) </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Initial time for the growth event to occur </td>
  </tr>
  <tr>
   <td style="text-align:left;"> initialBiomassSource </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Currently, there are three options: 'spinUp', 'cohortData', 'biomassMap'. If 'spinUp', it will derive biomass by running spinup derived from Landis-II. If 'cohortData', it will be taken from the `cohortData` object, i.e., it is already correct, by cohort. If 'biomassMap', it will be taken from `sim$biomassMap`, divided across species using `sim$speciesLayers` percent cover values 'spinUp' uses `sim$standAgeMap` as the driver, so biomass is an output . That means it will be unlikely to match any input information about biomass, unless this is set to 'biomassMap', and a `sim$biomassMap` is supplied. Only the 'cohortData' option is currently active. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> keepClimateCols </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> include growth and mortality predictions in `cohortData`? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minCohortBiomass </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> cohorts with biomass below this threshold (g/m^2) are removed. Not a LANDIS-II BSE parameter. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mixedType </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> How to define mixed stands: 1 for any species admixture; 2 for deciduous &gt; conifer. See `?LandR::vegTypeMapGenerator`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> plotOverstory </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> swap max age plot with overstory biomass </td>
  </tr>
  <tr>
   <td style="text-align:left;"> seedingAlgorithm </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> wardDisp.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> choose which seeding algorithm will be used among 'noDispersal', 'universalDispersal', and 'wardDispersal' (default). See Scheller &amp; Miranda (2015) - Biomass Succession extension, v3.2.1 User Guide </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spinupMortalityfraction </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the mortality loss fraction in spin up-stage simulation. Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Boreal </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The column in `sim$specieEquivalency` data.table to use as a naming convention </td>
  </tr>
  <tr>
   <td style="text-align:left;"> successionTimestep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the simulation time step, default is 10 years. Note that growth and mortality always happen on a yearly basis. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vegLeadingProportion </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.8 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> a number that define whether a species is leading for a given pixel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .maxMemory </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> maximum amount of memory (in GB) to use for dispersal calculations. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> start(sim) </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Vector of length = 1, describing the simulation time at which the first plot event should occur. To plotting off completely use `P(sim)$.plots`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the plotting time step. If `NA`, the default, .plotInterval is set to successionTimestep. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> object </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set. Note that plots (or their data) saving will ONLY occur at `end(sim)`. If `NA` plotting is off completely (this includes plot saving). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotMaps </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Vector of length = 1, describing the simulation time at which the first save event should occur. Set to `NA` if no saving is desired. If not `NA`, then saving will occur at `P(sim)$.saveInitialTime` with a frequency equal to `P(sim)$.saveInterval` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the saving time step. If `NA`, the default, .saveInterval is set to `P(sim)$successionTimestep`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used. If `NA`, a hash of `studyArea` will be used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> .inputOb.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Internal. Can be names of events or the whole module name; these will be cached by `SpaDES` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useParallel </td>
   <td style="text-align:left;"> ANY </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used only in seed dispersal. If numeric, it will be passed to `data.table::setDTthreads` and should be &lt;= 2; If `TRUE`, it will be passed to `parallel::makeCluster`; and if a cluster object, it will be passed to `parallel::parClusterApplyB`. </td>
  </tr>
</tbody>
</table>

::: fullwidth
+------------------------+-------------------------------------------------------------------------------------------------+
| Required inputs        | Description                                                                                     |
+:=======================+:================================================================================================+
| Plotting & saving      |                                                                                                 |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.plots`               | activates/deactivates plotting and defines type fo plotting (see `?Plots`)                      |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.plotInitialTime`     | defines when plotting starts                                                                    |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.plotInterval`        | defines plotting frequency                                                                      |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.plotMaps`            | activates/deactivates map plotting                                                              |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.saveInitialTime`     | defines when saving starts                                                                      |
+------------------------+-------------------------------------------------------------------------------------------------+
| `.saveInterval`        | defines saving frequency                                                                        |
+------------------------+-------------------------------------------------------------------------------------------------+
| Simulation             |                                                                                                 |
+------------------------+-------------------------------------------------------------------------------------------------+
| `seedingAlgorithm`     | dispersal type (see above)                                                                      |
+------------------------+-------------------------------------------------------------------------------------------------+
| `successionTimestep`   | defines frequency of dispersal/local recruitment event (growth and mortality are always yearly) |
+------------------------+-------------------------------------------------------------------------------------------------+
| Other                  |                                                                                                 |
+------------------------+-------------------------------------------------------------------------------------------------+
| `mixedType`            | how mixed forest stands are defined                                                             |
+------------------------+-------------------------------------------------------------------------------------------------+
| `vegLeadingProportion` | relative biomass threshold to consider a species "leading" (*i.e.*, dominant)                   |
+------------------------+-------------------------------------------------------------------------------------------------+
:::

: (#tab:tableUsefulParams) Useful *Biomass_core* parameters.

### Simulation flow

#### No disturbances

*Biomass_core* itself does not simulate disturbances, or their effect on vegetation (*i.e.*, post-disturbance mortality and regeneration). The general flow of *Biomass_core* processes is:

1.  Preparation of necessary objects for the simulation -- either by accessory data prep. modules, or *Biomass_core* itself (using LANDIS-II test parameters and dummy data for stand age, biomass and land cover and ecological zoning)
2.  Seed dispersal -- see @SchellerDomingo2012 for details

-   Seed dispersal can be a slow process and has been adapted to occur every 10 years. The user can set it to occur more often, but this should not make much of a difference to model outputs, because age classes are meant to be collapsed to tens.

3.  Growth and mortality -- based on @SchellerMladenoff2004

-   unlike dispersal, growth and mortality should occur every year

4.  Ageing -- based on @SchellerMiranda2015

-   follows the same frequency as dispersal, collapsing ages to classes with resolution = to this frequency

5.  Preparation of visual/saved outputs ... (repeat 2-4) ...

#### With disturbances

Note that should a post-disturbance regeneration module be used (e.g., *Biomass_regeneration*), regeneration will occur after the disturbance, but *before* dispersal and background vegetation growth and mortality. Hence, the disturbance should take place either at the very beginning or at the very end of each simulation time step. The general flow of *Biomass_core* processes when disturbances are included (by linking other modules) is:

1.  Preparation of necessary objects for the simulation -- either by accessory prep. data modules, or *Biomass_core* itself (using LANDIS-II test parameters and dummy data.)
2.  Disturbances -- simulated by a disturbance module
3.  Post-disturbance regeneration -- simulated by a regeneration module (*Biomass_regeneration* is an optional download)
4.  Seed dispersal -- see @SchellerDomingo2012 for details
5.  Growth, ageing and mortality -- based on @SchellerMiranda2015
6.  Preparation of visual/saved outputs ... (repeat 2-6) ...

## Usage example

### Load `SpaDES`


```r
library(SpaDES)

## make sure all necessary packages are installed
SpaDES.install::makeSureAllPackagesInstalled(modulePath = "../Biomass_core/")

moduleName <- c("Biomass_core")  
spadesModulesDirectory <- ".." # In general, a module code will be controlled at one level above the source code
```

### Get the module

See [SpaDES-modules repository](https://github.com/PredictiveEcology/SpaDES-modules) to see how to download this and other `SpaDES` modules. Alternatively, it can be forked or cloned from its [GitHub repository](https://github.com/PredictiveEcology/Biomass_core/) directly.

### Setup simulation


```r
tempDir <- tempdir()
setPaths(inputPath = file.path(tempDir, "inputs"), 
         cachePath = file.path(tempDir, "cache"), 
         modulePath = spadesModulesDirectory, 
         outputPath = file.path(tempDir, "outputs"))

times <- list(start = 0, end = 30)

studyArea <- Cache(randomStudyArea, size = 1e7) # cache this so it creates a random one only once on a machine

# Pick the species you want to work with – using the naming convention in "Boreal" column of LandR::sppEquivalencies_CA
speciesNameConvention <- "Boreal"
speciesToUse <- c("Pice_Gla", "Popu_Tre", "Pinu_Con")

sppEquiv <- LandR::sppEquivalencies_CA[get(speciesNameConvention) %in% speciesToUse]
# Assign a colour convention for graphics for each species
sppColorVect <- LandR::sppColors(sppEquiv, speciesNameConvention,
                                 newVals = "Mixed", palette = "Set1")

## Usage example
modules <- as.list(moduleName)
objects <- list(studyArea = studyArea, sppEquiv = sppEquiv, sppColorVect = sppColorVect)
paths <- getPaths()

successionTimestep <- 10L

## keep default values for most parameters 
## (omitted from this list)
parameters <- list(
  Biomass_core = list(
    "sppEquivCol" = speciesNameConvention
    , "successionTimestep" = successionTimestep
    , ".plots" = c("screen", "object")
    , ".plotInitialTime" = times$start
    , ".plots" = c("screen", "png")
    , ".saveInitialTime" = times$start
    , ".useCache" = "init"
    , ".useParallel" = FALSE
  )
)

outputs <- data.frame(expand.grid(objectName = "cohortData",
                                  saveTime = unique(seq(times$start, times$end, by = 1)),
                                  eventPriority = 1,
                                  stringsAsFactors = FALSE))

graphics.off()
```

### Run simulation


```r
mySim <- simInitAndSpades(times = times,
                          params = parameters, 
                          modules = modules, 
                          objects = objects, 
                          paths = paths,
                          outputs = outputs,
                          debug = TRUE)
```

<div class="figure">
<img src="figures/Biomass_coreOutPlots1.png" alt="(ref:moduleName) automatically generates simulation visuals of species dynamics across the landscape in terms of total biomass, number of presences and age and productivity (above), as well as yearly plots of total biomass, productivity, mortality, reproduction and leading species in each pixel (below)." width="479" /><img src="figures/Biomass_coreOutPlots2.png" alt="(ref:moduleName) automatically generates simulation visuals of species dynamics across the landscape in terms of total biomass, number of presences and age and productivity (above), as well as yearly plots of total biomass, productivity, mortality, reproduction and leading species in each pixel (below)." width="305" />
<p class="caption">(\#fig:fig-Biomass-coreOutPlots)(ref:moduleName) automatically generates simulation visuals of species dynamics across the landscape in terms of total biomass, number of presences and age and productivity (above), as well as yearly plots of total biomass, productivity, mortality, reproduction and leading species in each pixel (below).</p>
</div>

## Appendix

### Tables

| Input order 1 |          |     |            | Input order 2 |          |     |            |
|:--------------|:---------|:----|:-----------|:--------------|:---------|:----|:-----------|
| Community     | Input    | Age | Processing | Community     | Input    | Age | Processing |
|               | order    |     | order      |               | order    |     | order      |
| 1             | abiebals | 20  | poputrem   | 1             | pinustro | 20  | thujocci   |
| 1             | acerrubr | 20  | querelli   | 1             | poputrem | 20  | tiliamer   |
| 1             | acersacc | 20  | pinuresi   | 1             | acerrubr | 20  | querelli   |
| 1             | betualle | 20  | pinustro   | 1             | pinubank | 20  | querrubr   |
| 1             | betupapy | 20  | tiliamer   | 1             | betualle | 20  | betupapy   |
| 1             | fraxamer | 20  | tsugcana   | 1             | piceglau | 20  | fraxamer   |
| 1             | piceglau | 20  | querrubr   | 1             | pinuresi | 20  | tsugcana   |
| 1             | pinubank | 20  | thujocci   | 1             | acersacc | 20  | abiebals   |
| 1             | pinuresi | 20  | acersacc   | 1             | querelli | 20  | acerrubr   |
| 1             | pinustro | 20  | betualle   | 1             | querrubr | 20  | pinubank   |
| 1             | poputrem | 20  | abiebals   | 1             | thujocci | 20  | pinustro   |
| 1             | querelli | 20  | acerrubr   | 1             | tiliamer | 20  | poputrem   |
| 1             | querrubr | 20  | piceglau   | 1             | tsugcana | 20  | pinuresi   |
| 1             | thujocci | 20  | pinubank   | 1             | abiebals | 20  | acersacc   |
| 1             | tiliamer | 20  | betupapy   | 1             | betupapy | 20  | betualle   |
| 1             | tsugcana | 20  | fraxamer   | 1             | fraxamer | 20  | piceglau   |

: (#tab:tableLBSEtest1) Input order and processing order (as determined by LBSE) for the same community used to assess the impact of sequential calculation of the competition index, combined with a lack of explicit species ordering. The input order was the order of species in the initial communities table input file. The processing order was the order used in the simulation, which was obtained from `Landis-log.txt` when `CalibrateMode` was set to 'yes'. Species starting ages are also shown.

| Input order 1 |          |     |            | Input order 2 |          |     |            |
|:--------------|:---------|:----|:-----------|:--------------|:---------|:----|:-----------|
| Community     | Input    | Age | Processing | Community     | Input    | Age | Processing |
|               | order    |     | order      |               | order    |     | order      |
| 1             | abiebals | 1   | poputrem   | 1             | pinustro | 1   | thujocci   |
| 1             | acerrubr | 1   | querelli   | 1             | poputrem | 1   | tiliamer   |
| 1             | acersacc | 1   | pinuresi   | 1             | acerrubr | 1   | querelli   |
| 1             | betualle | 1   | pinustro   | 1             | pinubank | 1   | querrubr   |
| 1             | betupapy | 1   | tiliamer   | 1             | betualle | 1   | betupapy   |
| 1             | fraxamer | 1   | tsugcana   | 1             | piceglau | 1   | fraxamer   |
| 1             | piceglau | 1   | querrubr   | 1             | pinuresi | 1   | tsugcana   |
| 1             | pinubank | 1   | thujocci   | 1             | acersacc | 1   | abiebals   |
| 1             | pinuresi | 1   | acersacc   | 1             | querelli | 1   | acerrubr   |
| 1             | pinustro | 1   | betualle   | 1             | querrubr | 1   | pinubank   |
| 1             | poputrem | 1   | abiebals   | 1             | thujocci | 1   | pinustro   |
| 1             | querelli | 1   | acerrubr   | 1             | tiliamer | 1   | poputrem   |
| 1             | querrubr | 1   | piceglau   | 1             | tsugcana | 1   | pinuresi   |
| 1             | thujocci | 1   | pinubank   | 1             | abiebals | 1   | acersacc   |
| 1             | tiliamer | 1   | betupapy   | 1             | betupapy | 1   | betualle   |
| 1             | tsugcana | 1   | fraxamer   | 1             | fraxamer | 1   | piceglau   |

: (#tab:tableLBSEtest2) Input order and processing order (as determined by LBSE) for the same community used to assess the impact of setting the succession time step to 1, combined with a lack of explicit species ordering. The input order was the order of species in the initial communities table input file. The processing order was the order used in the simulation, which was obtained from `Landis-log.txt` when `CalibrateMode` was set to 'yes'. Species starting ages are also shown.

| Community | Species  | Age 1 | Age 2 | Age 3 | Age 4 | Age 5 | Age 6 | Age 7 |
|:----------|:---------|:------|:------|:------|:------|:------|:------|:------|
| 0         | betupapy | 1     | 37    | 45    | 46    | 85    | NA    | NA    |
| 0         | piceglau | 27    | 73    | 153   | 256   | 270   | NA    | NA    |
| 0         | pinustro | 157   | 159   | 181   | 220   | 223   | 303   | 307   |
| 0         | querrubr | 80    | 102   | 127   | 152   | 206   | 227   | NA    |
| 1         | acerrubr | 3     | 91    | 126   | 145   | NA    | NA    | NA    |
| 1         | acersacc | 138   | 144   | 276   | NA    | NA    | NA    | NA    |
| 1         | betualle | 24    | 106   | 136   | 149   | 279   | NA    | NA    |
| 1         | piceglau | 27    | 67    | 70    | 153   | NA    | NA    | NA    |
| 1         | pinubank | 3     | 10    | 24    | 31    | 71    | NA    | NA    |
| 1         | querelli | 92    | 224   | 234   | NA    | NA    | NA    | NA    |
| 1         | thujocci | 73    | 146   | 262   | NA    | NA    | NA    | NA    |
| 2         | fraxamer | 108   | 118   | 137   | 147   | 204   | NA    | NA    |
| 2         | piceglau | 40    | 128   | 131   | 159   | 174   | NA    | NA    |
| 2         | pinustro | 78    | 156   | 237   | 245   | 270   | NA    | NA    |
| 2         | querelli | 67    | 97    | 186   | 292   | NA    | NA    | NA    |
| 2         | tiliamer | 70    | 103   | 121   | 152   | 178   | 180   | 245   |
| 3         | acerrubr | 5     | 83    | 125   | 126   | 127   | NA    | NA    |
| 3         | pinuresi | 1     | 25    | 42    | 49    | 76    | 79    | 103   |
| 3         | poputrem | 4     | 9     | 62    | NA    | NA    | NA    | NA    |
| 3         | querelli | 101   | 104   | 167   | 226   | NA    | NA    | NA    |
| 3         | tsugcana | 37    | 135   | 197   | 404   | 405   | NA    | NA    |
| 4         | acerrubr | 15    | 29    | 63    | 70    | 105   | 133   | NA    |
| 4         | piceglau | 67    | 132   | 189   | NA    | NA    | NA    | NA    |
| 4         | tsugcana | 21    | 26    | 110   | 146   | 341   | 462   | 463   |
| 5         | acerrubr | 128   | 137   | 145   | 147   | NA    | NA    | NA    |
| 5         | acersacc | 241   | 245   | 261   | 277   | NA    | NA    | NA    |
| 5         | querrubr | 23    | 72    | 120   | 142   | 188   | NA    | NA    |
| 5         | tiliamer | 4     | 68    | 98    | 118   | 139   | 197   | NA    |
| 6         | betualle | 5     | 23    | 31    | 249   | NA    | NA    | NA    |
| 6         | pinubank | 67    | 70    | 89    | NA    | NA    | NA    | NA    |
| 6         | querelli | 194   | 217   | 257   | NA    | NA    | NA    | NA    |

: (#tab:tableLBSEtest3) Randomly generated community combination no. 1 used in the recruitment comparison runs.

| Community | Species  | Age 1 | Age 2 | Age 3 | Age 4 | Age 5 | Age 6 | Age 7 |
|:----------|:---------|:------|:------|:------|:------|:------|:------|:------|
| 0         | acerrubr | 22    | 26    | 30    | 40    | 47    | 145   | 146   |
| 0         | betualle | 23    | 41    | 43    | 120   | 209   | 227   | 270   |
| 0         | fraxamer | 25    | 90    | 119   | 173   | 185   | 282   | NA    |
| 0         | pinuresi | 48    | 53    | 70    | 121   | 157   | NA    | NA    |
| 0         | pinustro | 5     | 82    | 126   | 298   | 352   | NA    | NA    |
| 0         | querrubr | 2     | 30    | 34    | 74    | 77    | 162   | 245   |
| 1         | acerrubr | 2     | 39    | 43    | 84    | 116   | 127   | 143   |
| 1         | pinubank | 34    | 57    | 75    | NA    | NA    | NA    | NA    |
| 1         | querelli | 108   | 202   | 218   | 243   | NA    | NA    | NA    |
| 1         | querrubr | 5     | 117   | 131   | 186   | 189   | 246   | NA    |
| 1         | tiliamer | 10    | 19    | 46    | 80    | 133   | 148   | 231   |
| 1         | tsugcana | 31    | 48    | 190   | 246   | 330   | NA    | NA    |
| 2         | pinubank | 11    | 37    | 38    | 47    | 67    | 93    | NA    |
| 2         | querrubr | 11    | 48    | 57    | 177   | 180   | 228   | 236   |
| 2         | tiliamer | 28    | 42    | 78    | 79    | 223   | 250   | NA    |
| 2         | tsugcana | 140   | 202   | 372   | 381   | 451   | NA    | NA    |
| 3         | acersacc | 48    | 107   | 262   | 265   | NA    | NA    | NA    |
| 3         | betupapy | 4     | 12    | 45    | 65    | 83    | 96    | NA    |
| 3         | poputrem | 13    | 20    | 37    | 75    | 90    | NA    | NA    |
| 3         | querelli | 72    | 90    | 104   | 115   | 116   | 265   | 278   |
| 3         | tiliamer | 20    | 21    | 56    | 98    | 237   | NA    | NA    |
| 3         | tsugcana | 86    | 224   | 425   | 429   | NA    | NA    | NA    |
| 4         | fraxamer | 77    | 133   | 181   | NA    | NA    | NA    | NA    |
| 4         | pinustro | 13    | 37    | 67    | 220   | 287   | 293   | 375   |
| 4         | querrubr | 27    | 48    | 89    | 97    | NA    | NA    | NA    |
| 4         | thujocci | 91    | 244   | 305   | 390   | NA    | NA    | NA    |
| 5         | abiebals | 86    | 95    | 119   | 121   | 127   | 158   | NA    |
| 5         | betualle | 83    | 113   | 136   | 161   | 216   | 231   | NA    |
| 5         | betupapy | 10    | 38    | 64    | NA    | NA    | NA    | NA    |
| 5         | piceglau | 16    | 63    | 70    | 102   | NA    | NA    | NA    |
| 6         | acerrubr | 8     | 34    | 112   | NA    | NA    | NA    | NA    |
| 6         | betupapy | 1     | 31    | 57    | 61    | 74    | 80    | 91    |
| 6         | fraxamer | 63    | 100   | 108   | 140   | 196   | 294   | NA    |
| 6         | pinubank | 15    | 19    | 44    | 47    | 51    | 80    | NA    |
| 6         | thujocci | 78    | 146   | 163   | 213   | 214   | 228   | NA    |
| 6         | tsugcana | 47    | 108   | 387   | 389   | 449   | NA    | NA    |

: (#tab:tableLBSEtest4) Randomly generated community combination no. 2 used in the recruitment comparison runs.

| Community | Species  | Age 1 | Age 2 | Age 3 | Age 4 | Age 5 | Age 6 | Age 7 |
|:----------|:---------|:------|:------|:------|:------|:------|:------|:------|
| 0         | pinubank | 7     | 26    | 32    | 37    | 48    | 85    | 90    |
| 0         | pinuresi | 11    | 103   | 109   | 179   | 188   | 197   | NA    |
| 0         | querrubr | 89    | 139   | 180   | 206   | NA    | NA    | NA    |
| 1         | betupapy | 36    | 39    | 45    | 49    | 66    | 68    | NA    |
| 1         | piceglau | 13    | 165   | 254   | NA    | NA    | NA    | NA    |
| 1         | pinubank | 3     | 19    | 54    | 64    | 76    | NA    | NA    |
| 1         | poputrem | 22    | 59    | 93    | NA    | NA    | NA    | NA    |
| 1         | thujocci | 68    | 98    | 274   | 275   | 363   | 378   | NA    |
| 1         | tiliamer | 13    | 20    | 105   | 124   | 248   | NA    | NA    |
| 1         | tsugcana | 36    | 90    | 142   | NA    | NA    | NA    | NA    |
| 2         | fraxamer | 11    | 241   | 279   | NA    | NA    | NA    | NA    |
| 2         | piceglau | 16    | 42    | 129   | 177   | 200   | 244   | NA    |
| 2         | pinustro | 200   | 342   | 384   | NA    | NA    | NA    | NA    |
| 3         | abiebals | 31    | 57    | 61    | 92    | 108   | 162   | 183   |
| 3         | piceglau | 126   | 255   | 261   | 267   | NA    | NA    | NA    |
| 3         | poputrem | 28    | 41    | 57    | NA    | NA    | NA    | NA    |
| 3         | querrubr | 83    | 91    | 144   | 173   | 184   | 238   | NA    |
| 3         | thujocci | 6     | 66    | 68    | 204   | NA    | NA    | NA    |
| 4         | fraxamer | 12    | 110   | 266   | 270   | NA    | NA    | NA    |
| 4         | pinustro | 174   | 270   | 359   | 379   | NA    | NA    | NA    |
| 4         | poputrem | 4     | 7     | 18    | 24    | 63    | 76    | NA    |
| 4         | tiliamer | 126   | 136   | 197   | NA    | NA    | NA    | NA    |
| 4         | tsugcana | 49    | 91    | 128   | 194   | 411   | 487   | NA    |
| 5         | abiebals | 35    | 53    | 108   | 114   | 147   | 174   | 195   |
| 5         | acerrubr | 1     | 2     | 101   | 145   | NA    | NA    | NA    |
| 5         | pinubank | 14    | 15    | 38    | 40    | 59    | 69    | 83    |
| 6         | acerrubr | 4     | 46    | 117   | NA    | NA    | NA    | NA    |
| 6         | betualle | 36    | 41    | 116   | 213   | 253   | NA    | NA    |
| 6         | betupapy | 4     | 6     | 76    | NA    | NA    | NA    | NA    |
| 6         | pinuresi | 43    | 68    | 85    | 171   | NA    | NA    | NA    |
| 6         | querrubr | 84    | 86    | 113   | 185   | 193   | 223   | 228   |
| 6         | tiliamer | 13    | 106   | 181   | 199   | 246   | NA    | NA    |

: (#tab:tableLBSEtest5) Randomly generated community combination no. 3 used in the recruitment comparison runs.

+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| Species  | Longevity | Sexualmature | Shadetolerance | Seeddistance_eff | Seeddistance_max | Mortalityshape | Growthcurve |
+==========+===========+==============+================+==================+==================+================+=============+
| abiebals | 200       | 25           | 5              | 30               | 160              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| acerrubr | 150       | 10           | 4              | 100              | 200              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| acersacc | 300       | 40           | 5              | 100              | 200              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| betualle | 300       | 40           | 4              | 100              | 400              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| betupapy | 100       | 30           | 2              | 200              | 5000             | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| fraxamer | 300       | 30           | 4              | 70               | 140              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| piceglau | 300       | 25           | 3              | 30               | 200              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| pinubank | 100       | 15           | 1              | 20               | 100              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| pinuresi | 200       | 35           | 2              | 20               | 275              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| pinustro | 400       | 40           | 3              | 60               | 210              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| poputrem | 100       | 20           | 1              | 1000             | 5000             | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| querelli | 300       | 35           | 2              | 30               | 3000             | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| querrubr | 250       | 25           | 3              | 30               | 3000             | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| thujocci | 400       | 30           | 2              | 45               | 60               | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| tiliamer | 250       | 30           | 4              | 30               | 120              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+
| tsugcana | 500       | 30           | 5              | 30               | 100              | 10             | 0.25        |
+----------+-----------+--------------+----------------+------------------+------------------+----------------+-------------+

: (#tab:tableLBSEtest6) Invariant species traits table used in comparison runs.

| Ecolocation | X0  | X1   | X2   | X3  | X4  | X5   |
|:------------|:----|:-----|:-----|:----|:----|:-----|
| All         | 0   | 0.15 | 0.25 | 0.5 | 0.8 | 0.95 |

: (#tab:tableLBSEtest7) Minimum relative biomass table used in comparison runs. X0-5 represent site shade classes from no-shade (0) to maximum shade (5). All ecolocations shared the same values.

| Shadetolerance | 0   | 1   | 2   | 3   | 4   | 5   |
|:---------------|:----|:----|:----|:----|:----|:----|
| 1              | 1   | 0   | 0   | 0   | 0   | 0   |
| 2              | 1   | 1   | 0   | 0   | 0   | 0   |
| 3              | 1   | 1   | 1   | 0   | 0   | 0   |
| 4              | 1   | 1   | 1   | 1   | 0   | 0   |
| 5              | 0   | 0   | 1   | 1   | 1   | 1   |

: (#tab:tableLBSEtest8) Probability of germination for species shade tolerance and shade level combinations (called *sufficient light* table in LBSE and `sufficientLight` input `data.table` in LandR *Biomass_core*) used in comparison runs.

| Ecolocation | Species  | SEP  | maxANPP | maxB  |
|:------------|:---------|:-----|:--------|:------|
| 1           | abiebals | 0.9  | 886     | 26580 |
| 1           | acerrubr | 1    | 1175    | 35250 |
| 1           | acersacc | 0.82 | 1106    | 33180 |
| 1           | betualle | 0.64 | 1202    | 36060 |
| 1           | betupapy | 1    | 1202    | 36060 |
| 1           | fraxamer | 0.18 | 1202    | 36060 |
| 1           | piceglau | 0.58 | 969     | 29070 |
| 1           | pinubank | 1    | 1130    | 33900 |
| 1           | pinuresi | 0.56 | 1017    | 30510 |
| 1           | pinustro | 0.72 | 1090    | 38150 |
| 1           | poputrem | 1    | 1078    | 32340 |
| 1           | querelli | 0.96 | 1096    | 32880 |
| 1           | querrubr | 0.66 | 1017    | 30510 |
| 1           | thujocci | 0.76 | 1090    | 32700 |
| 1           | tiliamer | 0.54 | 1078    | 32340 |
| 1           | tsugcana | 0.22 | 1096    | 32880 |

: (#tab:tableLBSEtest9) Species ecolocation table used in comparison runs. `SEP` stands for species establishment probability, `maxB` for maximum biomass and `maxANPP` for maximum aboveground net primary productivity. Values were held constant throughout the simulation.

### Figures

<div class="figure" style="text-align: center">
<img src="figures/figLBSEtest1.png" alt="Differences in total landscape aboveground biomass when using two different input species orders for the same community. These simulations demonstrate how the sequential calculation of the competition index, combined with a lack of explicit species ordering affect the overall landscape aboveground biomass in time when using different input species orders (see Table \@ref(tab:tableLBSEtest1)). In order to prevent differences introduced by cohort recruitment, species’ ages at sexual maturity were changed to the species’ longevity values, and the simulation ran for 75 years to prevent any cohorts from reaching sexual maturity. The bottom panel shows the difference between the two simulations in percentage, calculated as $\frac{Biomass_{order2} - Biomass_{order1}}{Biomass_{order2}} * 100$" width="60%" />
<p class="caption">(\#fig:figLBSEtest1)Differences in total landscape aboveground biomass when using two different input species orders for the same community. These simulations demonstrate how the sequential calculation of the competition index, combined with a lack of explicit species ordering affect the overall landscape aboveground biomass in time when using different input species orders (see Table \@ref(tab:tableLBSEtest1)). In order to prevent differences introduced by cohort recruitment, species’ ages at sexual maturity were changed to the species’ longevity values, and the simulation ran for 75 years to prevent any cohorts from reaching sexual maturity. The bottom panel shows the difference between the two simulations in percentage, calculated as $\frac{Biomass_{order2} - Biomass_{order1}}{Biomass_{order2}} * 100$</p>
</div>

<br/><br/>

<div class="figure" style="text-align: center">
<img src="figures/figLBSEtest2.png" alt="Differences in the biomasses assigned to new cohorts, summed for each species across pixels, when using two different input species orders for the same community and when the succession time step is 1. These simulations demonstrate how the different summation of total cohort biomass for a succession time step of 1 and the lack of explicit species ordering affect simulation results when changing the species order in the input file (see Table \@ref(tab:tableLBSEtest2)). Here, initial cohort ages were also set to 1. We show the initial total biomass attributed to each species at the end of year 1." width="60%" />
<p class="caption">(\#fig:figLBSEtest2)Differences in the biomasses assigned to new cohorts, summed for each species across pixels, when using two different input species orders for the same community and when the succession time step is 1. These simulations demonstrate how the different summation of total cohort biomass for a succession time step of 1 and the lack of explicit species ordering affect simulation results when changing the species order in the input file (see Table \@ref(tab:tableLBSEtest2)). Here, initial cohort ages were also set to 1. We show the initial total biomass attributed to each species at the end of year 1.</p>
</div>

<br/><br/>

<div class="figure" style="text-align: center">
<img src="figures/figLBSEtest3.png" alt="Hashing design for (ref:moduleName). In the re-coded (ref:moduleName), the pixel group map was hashed based on the unique combination of species composition (i.e., community map) and ecolocation map, and associated with a lookup table. The subfigure in the right upper corner was the original design that linked the map to the lookup table by pixel key." width="60%" />
<p class="caption">(\#fig:figLBSEtest3)Hashing design for (ref:moduleName). In the re-coded (ref:moduleName), the pixel group map was hashed based on the unique combination of species composition (i.e., community map) and ecolocation map, and associated with a lookup table. The subfigure in the right upper corner was the original design that linked the map to the lookup table by pixel key.</p>
</div>

<br/><br/>

<div class="figure" style="text-align: center">
<img src="figures/figLBSEtest4.png" alt="Visual comparison of simulation outputs for three randomly generated initial communities (left panels) and difference between those outputs (right panels). The % difference between LBSE and (ref:moduleName) were calculated as $\frac{Biomass_{LBSE} - Biomass_{Biomass_core}}{Biomass_{LBSE}} * 100$" width="60%" />
<p class="caption">(\#fig:figLBSEtest4)Visual comparison of simulation outputs for three randomly generated initial communities (left panels) and difference between those outputs (right panels). The % difference between LBSE and (ref:moduleName) were calculated as $\frac{Biomass_{LBSE} - Biomass_{Biomass_core}}{Biomass_{LBSE}} * 100$</p>
</div>

<br/><br/>

<div class="figure" style="text-align: center">
<img src="figures/figLBSEtest5.png" alt="Simulation efficiencies of LBSE and (ref:moduleName) with increasing map size, in terms of a) mean running time across repetitions (left y-axis) and the ratio LBSE to (ref:moduleName) running times (right y-axis and blue line), and b) running time scalability as the mean running time per 1000 pixels." width="60%" />
<p class="caption">(\#fig:figLBSEtest5)Simulation efficiencies of LBSE and (ref:moduleName) with increasing map size, in terms of a) mean running time across repetitions (left y-axis) and the ratio LBSE to (ref:moduleName) running times (right y-axis and blue line), and b) running time scalability as the mean running time per 1000 pixels.</p>
</div>

## References

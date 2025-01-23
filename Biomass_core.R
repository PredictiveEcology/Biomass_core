# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "Biomass_core",
  description = paste("A fast and large landscape biomass succession model modified from",
                      "LANDIS-II Biomass Succession extension, v3.2.1"),
  keywords = c("forest succession", "LANDIS II", "Biomass"),
  authors = c(
    person("Yong", "Luo", email = "yluo1@lakeheadu.ca", role = "aut"),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Ceres", "Barros", email = "ceres.barros@ubc.ca", role = "aut"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = "aut"),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("ctb")),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "ctb")
  ),
  childModules = character(0),
  version = list(Biomass_core = numeric_version("1.4.4.9000")),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_core.Rmd"),
  loadOrder = list(after = c("Biomass_speciesParameters")),
  reqdPkgs = list("assertthat", "compiler", "crayon", "data.table",
                  "dplyr", "fpCompare", "ggplot2", "grid",
                  "parallel", "purrr", "quickPlot (>= 1.0.2.9003)", "Rcpp",
                  "R.utils", "scales", "terra", "tidyr",
                  "reproducible (>= 2.1.0)",
                  "SpaDES.core (>= 2.1.4)", "SpaDES.tools (>= 1.0.0.9001)",
                  "ianmseddy/LandR.CS@master (>= 0.0.2.0002)",
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/LandR@development (>= 1.1.5.9016)"),
  parameters = rbind(
    defineParameter("calcSummaryBGM", "character", "end", NA, NA,
                    desc = paste("A character vector describing when to calculate the summary of biomass, growth and mortality",
                                 "Currently any combination of 5 options is possible:",
                                 "'start'- as before vegetation succession events, i.e. before dispersal,",
                                 "'postDisp' - after dispersal, 'postRegen' - after post-disturbance regeneration (currently the same as 'start'),",
                                 "'postGM' - after growth and mortality, 'postAging' - after aging,",
                                 "'end' - at the end of vegetation succesion events, before plotting and saving.",
                                 "The 'end' option is always active, being also the default option.",
                                 "If NULL, then will skip all `summaryBGM` related events")),
    defineParameter("calibrate", "logical", FALSE,
                    desc = "Do calibration? Defaults to `FALSE`"),
    defineParameter("cohortDefinitionCols", "character", LandR::cohortDefinitionCols(), NA, NA,
                    desc = paste("`cohortData` columns that determine what constitutes a cohort",
                                 "This parameter should only be modified if additional modules are adding columns to cohortData")),
    defineParameter("cutpoint", "numeric", 1e10, NA, NA,
                    desc = "A numeric scalar indicating how large each chunk of an internal data.table is, when processing by chunks"),
    defineParameter("initialB", "numeric", 10, 1, NA,
                    desc = paste("Initial biomass values of new age-1 cohorts.",
                                 "If `NA` or `NULL`, initial biomass will be calculated as in LANDIS-II Biomass Suc. Extension",
                                 "(see Scheller and Miranda, 2015 or `?LandR::.initiateNewCohorts`)")),
    defineParameter("gmcsGrowthLimits", "numeric", c(2/3 * 100, 3/2 * 100), NA, NA,
                    paste("If using `LandR.CS` for climate-sensitive growth and mortality, a percentile",
                          " is used to estimate the effect of climate on growth/mortality ",
                          "(currentClimate/referenceClimate). Upper and lower limits are ",
                          "suggested to circumvent problems caused by very small denominators as well as ",
                          "predictions outside the data range used to generate the model")),
    defineParameter("gmcsMortLimits", "numeric", c(2/3 * 100, 3/2 * 100), NA, NA,
                    paste("If using `LandR.CS` for climate-sensitive growth and mortality, a percentile",
                          " is used to estimate the effect of climate on growth/mortality ",
                          "(currentClimate/referenceClimate). Upper and lower limits are ",
                          "suggested to circumvent problems caused by very small denominators as well as ",
                          "predictions outside the data range used to generate the model")),
    defineParameter("gmcsMinAge", "numeric", 21, 0, NA,
                    paste("If using `LandR.CS` for climate-sensitive growth and mortality, the minimum",
                          "age for which to predict climate-sensitive growth and mortality.",
                          "Young stands (< 30) are poorly represented by the PSP data used to parameterize the model.")),
    defineParameter("growthAndMortalityDrivers", "character", "LandR", NA, NA,
                    desc = paste("Package name where the following functions can be found:",
                                 "`calculateClimateEffect`, `assignClimateEffect`",
                                 "(see `LandR.CS` for climate sensitivity equivalent functions, or leave default if this is not desired)")),
    defineParameter("growthInitialTime", "numeric", start(sim), NA_real_, NA_real_,
                    desc = "Initial time for the growth event to occur"),
    defineParameter("initialBiomassSource", "character", "cohortData", NA, NA,
                    paste("Currently, there are three options: 'spinUp', 'cohortData', 'biomassMap'. ",
                          "If 'spinUp', it will derive biomass by running spinup derived from Landis-II.",
                          "If 'cohortData', it will be taken from the `cohortData` object, i.e., it is already correct, by cohort.",
                          "If 'biomassMap', it will be taken from `sim$biomassMap`,",
                          "divided across species using `sim$speciesLayers` percent cover values",
                          "'spinUp' uses `sim$standAgeMap` as the driver, so biomass",
                          "is an **output**. That means it will be unlikely to match any input information",
                          "about biomass, unless this is set to 'biomassMap', and a `sim$biomassMap` is supplied.",
                          "**Only the 'cohortData' option is currently active.**")),
    defineParameter("keepClimateCols", "logical", FALSE, NA, NA, "include growth and mortality predictions in `cohortData`?"),
    defineParameter("minCohortBiomass", "numeric", 0, NA, NA,
                    desc = "Cohorts with biomass below this threshold (in $g/m^2$) are removed. Not a LANDIS-II BSE parameter."),
    defineParameter("mixedType", "numeric", 2, 0, 2,
                    desc = paste("How to define mixed stands: 0 for none; 1 for any species admixture;",
                                 "2 for deciduous > conifer. See `?LandR::vegTypeMapGenerator`.")),
    defineParameter("plotOverstory", "logical", FALSE, NA, NA, desc = "swap max age plot with overstory biomass"),
    defineParameter("seedingAlgorithm", "character", "wardDispersal", NA_character_, NA_character_,
                    desc = paste("Choose which seeding algorithm will be used among",
                                 "'noSeeding' (no horizontal, nor vertical seeding - not in LANDIS-II BSE), 'noDispersal'",
                                 "(no horizontal seeding), 'universalDispersal' (seeds disperse to any pixel), and",
                                 "'wardDispersal' (default; seeds disperse according to distance and dispersal traits).",
                                 "See Scheller & Miranda (2015) - Biomass Succession extension, v3.2.1 User Guide")),
    defineParameter("spinupMortalityfraction", "numeric", 0.001,
                    desc = paste("Defines the mortality loss fraction in spin up-stage simulation.",
                                 "Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated.")),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    "The column in `sim$sppEquiv` data.table to use as a naming convention"),
    defineParameter("successionTimestep", "numeric", 10, NA, NA,
                    paste("Defines the simulation time step, default is 10 years.",
                          "Note that growth and mortality always happen on a yearly basis.",
                          "Cohorts younger than this age will not be included in competitive interactions")),
    defineParameter("vegLeadingProportion", "numeric", 0.8, 0, 1,
                    desc = "A number that defines whether a species is leading for a given pixel"),
    defineParameter(".maxMemory", "numeric", 5, NA, NA,
                    desc = "Maximum amount of memory (in GB) to use for dispersal calculations."),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    desc = paste("Vector of length = 1, describing the simulation time at which the first plot event should occur.",
                                 "To plotting off completely use `P(sim)$.plots`.")),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    desc = paste("Defines the plotting time step.",
                                 "If `NA`, the default, `.plotInterval` is set to `successionTimestep`.")),
    defineParameter(".plots", "character", default = "object",
                    desc = paste("Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set.",
                                 "Note that plots (or their data) saving will ONLY occur at `end(sim)`.",
                                 "If `NA`, plotting is turned off completely (this includes plot saving).")),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    desc = "Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA`"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = paste("Vector of length = 1, describing the simulation time at which the first save event should occur.",
                                 "Set to `NA` if no saving is desired. If not `NA`, then saving will occur at",
                                 "`P(sim)$.saveInitialTime` with a frequency equal to `P(sim)$.saveInterval`")),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    desc = paste("Defines the saving time step.",
                                 "If `NA`, the default, .saveInterval is set to `P(sim)$successionTimestep`.")),
    defineParameter(".sslVerify", "integer", as.integer(unname(curl::curl_options("^ssl_verifypeer$"))), NA_integer_, NA_integer_,
                    paste("Passed to `httr::config(ssl_verifypeer = P(sim)$.sslVerify)` when downloading KNN",
                          "(NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this",
                          "may be necessary when NFI's website SSL certificate is not correctly configured).")),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If `NA`, a hash of `studyArea` will be used."),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by `SpaDES`"),
    defineParameter(".useParallel", "ANY", 2, NA, NA,
                    desc = paste("Used only in seed dispersal.",
                                 "If numeric, it will be passed to `data.table::setDTthreads` and should be <= 2;",
                                 "If `TRUE`, it will be passed to `parallel::makeCluster`;",
                                 "and if a cluster object, it will be passed to `parallel::parClusterApplyB`."))
  ),
  inputObjects = bindrows(
    expectsInput("biomassMap", "SpatRaster",
                 desc = paste("Total biomass raster layer in study area (in $g/m^2$),",
                              "filtered for pixels covered by `cohortData.`",
                              "Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated."),
                 sourceURL = ""),
    expectsInput("cceArgs", "list",
                 desc = paste("A list of quoted objects used by the `growthAndMortalityDriver` `calculateClimateEffect` function")),
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years)")),
    expectsInput("ecoregion", "data.table",
                 desc = "Ecoregion look up table",
                 sourceURL = paste0("https://raw.githubusercontent.com/LANDIS-II-Foundation/",
                                    "Extensions-Succession/master/biomass-succession-archive/",
                                    "trunk/tests/v6.0-2.0/ecoregions.txt")),
    expectsInput("ecoregionMap", "SpatRaster",
                 desc = paste("Ecoregion map that has mapcodes match ecoregion table and `speciesEcoregion` table.",
                              "Defaults to a dummy map matching `rasterToMatch` with two regions")),
    # expectsInput("initialCommunities", "data.table",
    #              desc = "initial community table",
    #              sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.txt"),
    # expectsInput("initialCommunitiesMap", "SpatRaster",
    #              desc = "initial community map that has mapcodes match initial community table",
    #              sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.gis"),
    expectsInput("lastReg", "numeric",
                 desc = "An internal counter keeping track of when the last regeneration event occurred"),
    expectsInput("minRelativeB", "data.frame",
                 desc = "table defining the relative biomass cut points to classify stand shadeness."),
    expectsInput("pixelGroupMap", "SpatRaster",
                 desc = paste("A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped" ,
                              "based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition,",
                              "even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep`",
                              "module.")),
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "A raster of the `studyArea` in the same resolution and projection as `biomassMap`"),
    expectsInput("species", "data.table",
                 desc = paste("A table of invariant species traits with the following trait colums:",
                              "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
                              "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
                              "'mortalityshape', 'growthcurve', 'resproutage_min', 'resproutage_max',",
                              "'postfireregen', 'wooddecayrate', 'leaflongevity' 'leafLignin',",
                              "'hardsoft'. The last seven traits are not used in *Biomass_core*,",
                              "and may be ommited. However, this may result in downstream issues with",
                              "other modules. Default is from Dominic Cyr and Yan Boulanger's project"),
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = paste("Table of spatially-varying species traits (`maxB`, `maxANPP`,",
                              "`establishprob`), defined by species and `ecoregionGroup` (i.e. ecolocation).",
                              "Defaults to a dummy table based on dummy data of biomass, age, ecoregion and land cover class")),
    expectsInput("speciesLayers", "SpatRaster",
                 desc = paste("Percent cover raster layers of tree species in Canada.",
                              "Defaults to the Canadian Forestry Service, National Forest Inventory,",
                              "kNN-derived species cover maps from 2001 using a cover threshold of 10 -",
                              "see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata"),
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/")),
    expectsInput("sppColorVect", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in `sim$sppEquiv[[sim$sppEquivCol]]`,",
                              "and should also contain a color for 'Mixed'.")),
    expectsInput("sppEquiv", "data.table",
                 desc = "Table of species equivalencies. See `LandR::sppEquivalencies_CA`."),
    expectsInput("sppNameVector", "character",
                 desc = paste("An optional vector of species names to be pulled from `sppEquiv`. Species names must match",
                              "`P(sim)$sppEquivCol` column in `sppEquiv`. If not provided, then species will be taken from",
                              "the entire `P(sim)$sppEquivCol` column in `sppEquiv`.",
                              "See `LandR::sppEquivalencies_CA`.")),
    expectsInput("studyArea", "sfc",
                 desc = paste("Polygon to use as the study area. Must be supplied by the user. Can also be a SpatVector.")),
    expectsInput("studyAreaReporting", "sfc",
                 desc = paste("multipolygon (typically smaller/unbuffered than studyArea) to use for plotting/reporting.",
                              "Defaults to `studyArea`.")),
    expectsInput("sufficientLight", "data.frame",
                 desc = paste("Table defining how the species with different shade tolerance respond to stand shade.",
                              "Default is based on LANDIS-II Biomass Succession v6.2 parameters"),
                 sourceURL = paste0("https://raw.githubusercontent.com/LANDIS-II-Foundation/",
                                    "Extensions-Succession/master/biomass-succession-archive/",
                                    "trunk/tests/v6.0-2.0/biomass-succession_test.txt")),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = paste("3 columns: `pixelIndex`, `pixelGroup`, and `burnTime`.",
                              "Each row represents a forested pixel that was burned up to and including this year,",
                              "since last dispersal event, with its corresponding `pixelGroup` and time it occurred"))
    # expectsInput("spinUpCache", "logical", ""),
    # expectsInput("speciesEstablishmentProbMap", "RasterBrick", "Species establishment probability as a RasterBrick, one layer for each species")
  ),
  outputObjects = bindrows(
    createsOutput("activePixelIndex", "integer",
                  desc = "Internal use. Keeps track of which pixels are active."),
    createsOutput("activePixelIndexReporting", "integer",
                  desc = "Internal use. Keeps track of which pixels are active in the reporting study area."),
    createsOutput("ANPPMap", "SpatRaster",
                  desc = "ANPP map at each succession time step (in g /m^2)"),
    createsOutput("biomassMap", "SpatRaster",
                  desc = paste("Total biomass raster layer in study area (in $g/m^2$),",
                               "filtered for pixels covered by `cohortData`.",
                               "Only used if `P(sim)$initialBiomassSource == 'biomassMap'`, which is currently deactivated.")),
    createsOutput("cohortData", "data.table",
                  desc = paste("`data.table` with cohort-level information on age, biomass, aboveground primary",
                               "productivity (year's biomass gain) and mortality (year's biomass loss), by `pixelGroup`",
                               "and ecolocation (i.e., `ecoregionGroup`). Contains at least the following columns:",
                               "`pixelGroup` (integer), `ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$),",
                               "`age` (integer in years), `mortality` (integer in $g/m^2$), `aNPPAct` (integer in $g/m^2$).",
                               "May have other columns depending on additional simulated processes (i.e., climate sensitivity;",
                               "see, e.g., `P(sim)$keepClimateCols`).")),
    createsOutput("ecoregion", "data.table",
                  desc = "Ecoregion look up table"),
    createsOutput("ecoregionMap", "SpatRaster",
                  desc = paste("Map with mapcodes match `ecoregion` table and `speciesEcoregion` table.",
                               "Defaults to a dummy map matching rasterToMatch with two regions.")),
    createsOutput("inactivePixelIndex", "logical",
                  desc = "Internal use. Keeps track of which pixels are inactive."),
    createsOutput("inactivePixelIndexReporting", "integer",
                  desc = "Internal use. Keeps track of which pixels are inactive in the reporting study area."),
    # createsOutput("initialCommunities", "character",
    #               desc = "Because the initialCommunities object can be LARGE, it is saved to disk with this filename"),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire."),
    createsOutput("lastReg", "numeric",
                  desc = "An internal counter keeping track of when the last regeneration event occurred."),
    createsOutput("minRelativeB", "data.frame",
                  desc = "Define the *relative biomass* cut points to classify stand shade."),
    createsOutput("mortalityMap", "SpatRaster",
                  desc = "Map of biomass lost (in $g/m^2$) at each succession time step."),
    createsOutput("pixelGroupMap", "SpatRaster",
                  desc = "Updated community map at each succession time step."),
    createsOutput("regenerationOutput", "data.table",
                  desc = paste("If `P(sim)$calibrate == TRUE`, an summary of seed dispersal and germination",
                               "success (i.e., number of pixels where seeds successfully germinated) per species and year.")),
    createsOutput("reproductionMap", "SpatRaster",
                  desc = "Regeneration map (biomass gains in $g/m^2$) at each succession time step"),
    createsOutput("simulatedBiomassMap", "SpatRaster",
                  desc = "Biomass map at each succession time step (in $g/m^2$)"),
    createsOutput("simulationOutput", "data.table",
                  desc = "Contains simulation results by `ecoregionGroup` (main output)"),
    createsOutput("simulationTreeOutput", "data.table",
                  desc = "Summary of several characteristics about the stands, derived from `cohortData`"),
    createsOutput("species", "data.table",
                  desc = paste("a table that has species traits such as longevity, shade tolerance, etc.",
                               "Currently obtained from LANDIS-II Biomass Succession v.6.0-2.0 inputs")),
    createsOutput("speciesEcoregion", "data.table",
                  desc = "Define the `maxANPP`, `maxB` and `SEP` change with both ecoregion and simulation time."),
    createsOutput("speciesLayers", "SpatRaster",
                  desc = paste("Species percent cover raster layers, based on input `speciesLayers` object.",
                               "Not changed by this module.")),
    # createsOutput("spinUpCache", "logical", desc = ""),
    createsOutput("spinupOutput", "data.table",
                  desc = "Spin-up output. Currently deactivated."),
    createsOutput("sppColorVect", "character",
                  desc = paste("A named vector of colors to use for plotting.",
                               "The names must be in `sim$sppEquiv[[sim$sppEquivCol]]`,",
                               "and should also contain a color for 'Mixed'.")),
    createsOutput("summaryBySpecies", "data.table",
                  desc = paste("The total species biomass (in $g/m^2$ as in `cohortData`), average age and aNPP (in",
                               "$g/m^2$ as in `cohortData`),  across the landscape (used for plotting and reporting).")),
    createsOutput("summaryBySpecies1", "data.table",
                  desc = "Number of pixels of each leading vegetation type (used for plotting and reporting)."),
    createsOutput("summaryLandscape", "data.table",
                  desc = paste("The averages of total biomass (in *tonnes/ha*, not $g/m^2$ like in `cohortData`), age",
                               "and aNPP (also in tonnes/ha) across the landscape (used for plotting and reporting).")),
    createsOutput("treedFirePixelTableSinceLastDisp", "data.table",
                  desc = paste("3 columns: `pixelIndex`, `pixelGroup`, and `burnTime`.",
                               "Each row represents a forested pixel that was burned up to and including this year,",
                               "since last dispersal event, with its corresponding `pixelGroup` and time it occurred")),
    createsOutput("vegTypeMap", "SpatRaster",
                  desc = paste("Map of leading species in each pixel, colored according to `sim$sppColorVect`.",
                               "Species mixtures calculated according to `P(sim)$vegLeadingProportion` and `P(sim)$mixedType`."))
  )
))

doEvent.Biomass_core <- function(sim, eventTime, eventType, debug = FALSE) {
  if (is.numeric(P(sim)$.useParallel)) {
    a <- data.table::setDTthreads(P(sim)$.useParallel)
    if (getOption("LandR.verbose", TRUE) > 0) {
      if (data.table::getDTthreads() > 1L) message("Biomass_core should be using >100% CPU")
    }
    on.exit(data.table::setDTthreads(a), add = TRUE)
  }

  ## Set event priorities
  dispEvtPriority <- 5
  GMEvtPriority <- 6
  agingEvtPriotity <- 7
  summRegenPriority <- 8
  ## summary of BGM can occur several times, b4/after other events
  summBGMPriority <- list(start = dispEvtPriority - 1,
                          postDisp = dispEvtPriority + 0.25,
                          postRegen = 4,
                          postGM = GMEvtPriority + 0.25,
                          postAging = agingEvtPriotity + 0.25,
                          end = summRegenPriority + 0.25)
  ## add "end" to parameter vector if necessary
  if (!is.null(P(sim)$calcSummaryBGM))
    if (!any(P(sim)$calcSummaryBGM == "end"))
      params(sim)$Biomass_core$calcSummaryBGM <- c(P(sim)$calcSummaryBGM, "end")
  summBGMPriority <- summBGMPriority[P(sim)$calcSummaryBGM] ## filter necessary priorities

  plotPriority <- 9
  savePriority <- 10

  switch(eventType,
         init = {
           ## do stuff for this event

           ## Define .plotInterval/.saveInterval if need be
           if (is.na(P(sim)$.plotInterval))
             params(sim)$Biomass_core$.plotInterval <- P(sim)$successionTimestep

           if (is.na(P(sim)$.saveInterval))
             params(sim)$Biomass_core$.saveInterval <- P(sim)$successionTimestep

           if (anyPlotting(P(sim)$.plots)) {
             if (any(P(sim)$.plots == "screen")) {
               ## make sure plotting window is big enough
               ## if current plot dev is too small, open a new one
               if (is.null(dev.list())) {
                 dev(x = dev.cur() + 1, height = 7, width = 14)
                 clearPlot()
               } else {
                 if (dev.size()[2] < 14) {
                   dev(x = dev.cur() + 1, height = 7, width = 14)
                   clearPlot()
                 }
               }
               ## current window will be used for  summary stats
               ## a new one for maps
               mod$statsWindow <- dev.cur()
               if (P(sim)$.plotMaps) {
                 mod$mapWindow <- mod$statsWindow + 1
                 dev(x = mod$mapWindow, height = 8, width = 10)
               }
             }
           } else {
             ## if plotting is deactivated make sure maps are NOT plotted
             params(sim)[[currentModule(sim)]]$.plotMaps <- FALSE
           }

           ## if not end(sim) don't save plots and only plot to screen.
           if (time(sim) != end(sim)) {
             if (any(is.na(P(sim)$.plots))) {
               mod$plotTypes <- NA
             } else if (any(P(sim)$.plots == "screen")) {
               mod$plotTypes <- "screen"
             } else {
               mod$plotTypes <- NA
             }
           }

           ## P(sim)$.plotInitialTime == NA is no longer used to turn plotting off
           ## override if necessary
           if (is.na(P(sim)$.plotInitialTime)) {
             params(sim)[[currentModule(sim)]]$.plotInitialTime <- start(sim)
             message("Using .plotInitialTime == NA no longer turns off plotting. Please use .plots == NA instead.")
           }

           ## Run Init event
           sim <- Init(sim)

           ## schedule events
           if (!is.null(summBGMPriority$start))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "summaryBGMstart", eventPriority = summBGMPriority$start)
           sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "Dispersal", eventPriority = dispEvtPriority)
           sim <- scheduleEvent(sim, P(sim)$growthInitialTime,
                                "Biomass_core", "mortalityAndGrowth", GMEvtPriority)
           if (!is.null(summBGMPriority$postDisp))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
           if (!is.null(summBGMPriority$postRegen))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
           if (!is.null(summBGMPriority$postGM))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, "Biomass_core",
                                  "cohortAgeReclassification", eventPriority = agingEvtPriotity)
             if (!is.null(summBGMPriority$postAging))
               sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                    "Biomass_core", "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
           }

           ## note that summaryBGM and summaryBySpecies, will occur during init too
           if (!is.null(P(sim)$calcSummaryBGM)) {
             sim <- scheduleEvent(sim, start(sim),
                                  "Biomass_core", "summaryBGM", eventPriority = summBGMPriority$end)
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "summaryRegen", eventPriority = summRegenPriority)
             sim <- scheduleEvent(sim, start(sim),
                                  "Biomass_core", "plotSummaryBySpecies", eventPriority = plotPriority)   ## only occurs before summaryRegen in init.
             sim <- scheduleEvent(sim, end(sim),
                                  "Biomass_core", "plotSummaryBySpecies", eventPriority = plotPriority)  ## schedule the last plotting events (so that it doesn't depend on plot interval)
           }

           if (anyPlotting(P(sim)$.plots)) {
             if (P(sim)$.plotMaps) {
               sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                    "Biomass_core", "plotMaps", eventPriority = plotPriority + 0.25)
             }
             sim <- scheduleEvent(sim, start(sim),
                                  "Biomass_core", "plotAvgs", eventPriority = plotPriority + 0.5)
             sim <- scheduleEvent(sim, end(sim),
                                  "Biomass_core", "plotAvgs", eventPriority = plotPriority + 0.5)
           }

           if (!is.na(P(sim)$.saveInitialTime)) {
             if (P(sim)$.saveInitialTime < start(sim) + P(sim)$successionTimestep) {
               message(crayon::blue(
                 paste(".saveInitialTime should be >=",  start(sim) + P(sim)$successionTimestep,
                       ". First save changed to", start(sim) + P(sim)$successionTimestep)))
               params(sim)$Biomass_core$.saveInitialTime <- start(sim) + P(sim)$successionTimestep
             }
             sim <- scheduleEvent(sim, P(sim)$.saveInitialTime,
                                  "Biomass_core", "save", eventPriority = savePriority)
           }
         },
         summaryBGMstart = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGMstart", eventPriority = summBGMPriority$start)
         },
         Dispersal = {
           sim <- Dispersal(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "Dispersal", eventPriority = dispEvtPriority)
         },
         mortalityAndGrowth = {
           sim <- MortalityAndGrowth(sim)
           sim <- scheduleEvent(sim, time(sim) + 1,
                                "Biomass_core", "mortalityAndGrowth", eventPriority = GMEvtPriority)
         },
         summaryBGMpostDisp = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
         },
         summaryBGMpostRegen = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
         },
         summaryBGMpostGM = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
         },
         cohortAgeReclassification = {
           sim <- CohortAgeReclassification(sim)

           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                  "Biomass_core", "cohortAgeReclassification",
                                  eventPriority = agingEvtPriotity)
           }
         },
         summaryBGMpostAging = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
         },
         summaryRegen = {
           sim <- summaryRegen(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryRegen", eventPriority = summRegenPriority)
         },
         summaryBGM = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "Biomass_core", "summaryBGM", eventPriority = summBGMPriority$end)
         },
         plotSummaryBySpecies = {
           if (time(sim) == end(sim)) {
             mod$plotTypes <- P(sim)$.plots
           }
           sim <- plotSummaryBySpecies(sim)
           if (!is.na(P(sim)$.plotInterval)) {
             if (!(time(sim) + P(sim)$.plotInterval) == end(sim))
               sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                    "Biomass_core", "plotSummaryBySpecies", eventPriority = plotPriority)
           }
         },
         plotAvgs = {
           if (time(sim) == end(sim)) {
             mod$plotTypes <- P(sim)$.plots
           }
           sim <- plotAvgVegAttributes(sim)
           if (!is.na(P(sim)$.plotInterval)) {
             if (!(time(sim) + P(sim)$.plotInterval) == end(sim))
               sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                    "Biomass_core", "plotAvgs", eventPriority = plotPriority + 0.5)
           }
         },
         plotMaps = {
           sim <- plotVegAttributesMaps(sim)

           sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                "Biomass_core", "plotMaps", eventPriority = plotPriority + 0.25)
         },
         save = {
           sim <- Save(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval,
                                "Biomass_core", "save", eventPriority = savePriority)
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### EVENT FUNCTIONS
Init <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
  ## stop early if raster inputs don't match
  # Must have sim$rasterToMatch
  #if (!identical(sort(as.character(unique(sim$cohortData$speciesCode))), sort(unique(sim$species$species))))
  #  stop("the species in sim$cohortData are not the same as the species in sim$species; these must match")
  cacheTags <- c(currentModule(sim), "init")

  # Check some parameter values
  if (P(sim)$successionTimestep > 10)
    warning("successionTimestep parameter is > 10. Make sure this intended, ",
            "keeping in mind that growth in the model depends on estimating 'sumB'. ",
            "Only trees that are older than successionTimestep are included in the ",
            "calculation of sumB, i.e., trees younger than this do not contribute ",
            "to competitive interactions")

  paramCheckOtherMods(sim, "initialB", ifSetButDifferent = "warning")

  ## prepare species ------------------------------------------------
  if (is.null(sim$species))
    stop("'species' object must be provided")

  species <- as.data.table(sim$species) # The former setDT actually changed the vector
  LandR::assertSpeciesTable(species)
  set(species, NULL, "speciesCode", factor(species$species, levels = unique(species$species))) # supply levels for speed
  sim$species <- setkey(species, speciesCode)

  ## prepare dummy versions if not supplied --------------------------
  ## This next chunk is to supply defaults for the case where `cohortData`  or `pixelGroupMap` is not supplied
  ##   e.g., via a module like `Biomass_borealDataPrep`
  if (!suppliedElsewhere("cohortData", sim, where = "sim") ||
      !suppliedElsewhere("pixelGroupMap", sim, where = "sim")) {
    if (is.null(sim$rasterToMatch)) {
      stop("Must supply sim$rasterToMatch, since sim$cohortData or sim$pixelGroupMap are not supplied.")
    }

    if ((!suppliedElsewhere("cohortData", sim, where = "sim") && suppliedElsewhere("pixelGroupMap", sim, where = "sim")) ||
        (suppliedElsewhere("cohortData", sim, where = "sim") && !suppliedElsewhere("pixelGroupMap", sim, where = "sim"))) {
      stop("Either 'cohortData' or 'pixelGroupMap' are being supplied without the other.",
           "These two objects must be supplied together and conform to each other.",
           "Either supply both of them manually, or use a module like Biomass_borealDataPrep to do so.")
    }

    if (suppliedElsewhere("ecoregionMap", sim, where = "sim")) {
      message(blue("'ecoregionMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'ecoregionMap'"))
    }
    ecoregionMap <- makeDummyEcoregionMap(sim$rasterToMatch)

    if (suppliedElsewhere("biomassMap", sim, where = "sim"))
      message(blue("'biomassMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'biomassMap'"))
    ## note that to make the dummy sim$biomassMap, we need to first make a dummy rawBiomassMap
    httr::with_config(config = httr::config(ssl_verifypeer = P(sim)$.sslVerify), {
      rawBiomassMap <- makeDummyRawBiomassMap(sim$rasterToMatch)
    })

    if (suppliedElsewhere("standAgeMap", sim, where = "sim"))
      message(blue("'standAgeMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'standAgeMap'"))
    standAgeMap <- makeDummyStandAgeMap(rawBiomassMap)

    if (suppliedElsewhere("rstLCC", sim, where = "sim"))
      message(blue("'rstLCC' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'rstLCC'"))
    rstLCC <- makeDummyRstLCC(sim$rasterToMatch)

    ## make sure speciesLayers match RTM (they may not if they come from another module's init.)
    if (!.compareRas(sim$speciesLayers, sim$rasterToMatch, stopOnError = FALSE)) {
      message(blue("'speciesLayers' and 'rasterToMatch' do not match. "),
              red("'speciesLayers' will be cropped/masked/reprojected to 'rasterToMatch'. "),
              blue("If this is wrong, provide matching 'speciesLayers' and 'rasterToMatch'"))

      sim$speciesLayers <- postProcess(sim$speciesLayers,
                                       to = sim$rasterToMatch,
                                       filename1 = NULL,
                                       writeTo = NULL,
                                       userTags = c(currentModule(sim), "speciesLayers"))
    }

    ecoregionFiles <- makeDummyEcoregionFiles(ecoregionMap, rstLCC, sim$rasterToMatch)

    ## create pixelTable object ------------------------------------
    ## Round age to pixelGroupAgeClass
    ## check if all species have traits
    tempObjs <- checkSpeciesTraits(sim$speciesLayers, sim$species, sim$sppColorVect)
    sim$speciesLayers <- tempObjs$speciesLayers
    sim$sppColorVect <- tempObjs$sppColorVect
    rm(tempObjs)

    assertSppVectors(sppEquiv = sim$species, sppEquivCol = "speciesCode",
                     sppColorVect = sim$sppColorVect)

    pixelTable <- makePixelTable(speciesLayers = sim$speciesLayers, #species = sim$species,
                                 standAgeMap = standAgeMap, ecoregionFiles = ecoregionFiles,
                                 biomassMap = rawBiomassMap, rasterToMatch = sim$rasterToMatch,
                                 rstLCC = rstLCC)

    ## create initial pixelCohortData table
    ## note that pixelGroupBiomassClass here is forced to 100, to match dummy biomass units
    message(blue("Creating a", red("DUMMY"), blue("cohorData table.")))
    coverColNames <- paste0("cover.", sim$species$species)
    pixelCohortData <- Cache(makeAndCleanInitialCohortData, pixelTable,
                             sppColumns = coverColNames,
                             minCoverThreshold = 1,
                             doSubset = FALSE,
                             userTags = c(cacheTags, "pixelCohortData"),
                             omitArgs = c("userTags"))
    pixelCohortData <- partitionBiomass(x = 1, pixelCohortData)
    setnames(pixelCohortData, "initialEcoregionCode", "ecoregionGroup")


    ## When using dummy values ecoregion codes are not changed
    rmZeroBiomassQuote <- quote(B > 0)
    ## This will fail, because LandR::makeAndCleanInitialCohortData no longer returns a B column July 2020 IE
    cohortDataNoBiomass <- pixelCohortData[eval(rmZeroBiomassQuote),
                                           .(B, logAge, speciesCode, ecoregionGroup, lcc, cover)]

    ## Statistical estimation of establishprob, maxB and maxANPP
    ## only use pixels where cover > 0
    cohortDataShort <- pixelCohortData[, list(coverNum = pmax(1, .N - 1),
                                              coverPres = sum(cover > 0)),
                                       by = c("ecoregionGroup", "speciesCode")]
    cohortDataShortNoCover <- cohortDataShort[coverPres == 0]
    cohortDataShort <- cohortDataShort[coverPres > 0] # remove places where there is 0 cover

    coverModel <- quote(lme4::glmer(cbind(coverPres, coverNum) ~ speciesCode +
                                      (1 | ecoregionGroup), family = binomial))
    biomassModel <- quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                       (logAge + cover + speciesCode | ecoregionGroup)))

    ## COVER
    message(blue("Estimating Species Establishment Probability from "), red("DUMMY values of ecoregionGroup "),
            blue("using the formula:\n"), magenta(format(coverModel)))

    modelCover <- Cache(statsModel,
                        modelFn = coverModel,
                        .specialData = cohortDataShort,
                        userTags = c(cacheTags, "modelCover"),
                        omitArgs = c("userTags"))  ## DON'T IGNORE .specialData - will fail downstream due to randomness

    message(blue("  The rsquared is: "))
    print(modelCover$rsq)

    ## BIOMASS
    ## For Cache -- doesn't need to cache all columns in the data.table -- only the ones in the model
    message(blue("Estimating maxB from "), red("DUMMY values of age and ecoregionGroup "),
            blue("using the formula:\n"),
            magenta(paste0(format(biomassModel), collapse = "")))
    modelBiomass <- Cache(statsModel,
                          modelFn = biomassModel,
                          .specialData = cohortDataNoBiomass,
                          userTags = c(cacheTags, "modelBiomass"),
                          omitArgs = c("userTags"))  ## DON'T IGNORE .specialData - will fail downstream due to randomness
    message(blue("  The rsquared is: "))
    print(modelBiomass$rsq)

    ## create speciesEcoregion ---------------------------------------------
    ## a single line for each combination of ecoregionGroup & speciesCode
    ## doesn't include combinations with B = 0 because those places can't have the species/ecoregion combo
    message(blue("Create speciesEcoregion from "), red("DUMMY values"))
    speciesEcoregion <- makeSpeciesEcoregion(cohortDataBiomass = cohortDataNoBiomass,
                                             cohortDataShort = cohortDataShort,
                                             cohortDataShortNoCover = cohortDataShortNoCover,
                                             species = sim$species,
                                             modelCover = modelCover,
                                             modelBiomass = modelBiomass,
                                             successionTimestep = P(sim)$successionTimestep,
                                             currentYear = time(sim))
    if (ncell(sim$rasterToMatch) > 3e7) .gc()

    ## Create initial communities, i.e., pixelGroups -----------------------
    if (!suppliedElsewhere("columnsForPixelGroups", sim, where = "sim")) {
      columnsForPixelGroups <- LandR::columnsForPixelGroups
    } else {
      columnsForPixelGroups <- sim$columnsForPixelGroups
    }
    ## make cohortDataFiles: pixelCohortData (rm unnecessary cols, subset pixels with B>0,
    ## generate pixelGroups, add ecoregionGroup and totalBiomass) and cohortData
    cohortDataFiles <- makeCohortDataFiles(pixelCohortData, columnsForPixelGroups, speciesEcoregion,
                                           pixelGroupBiomassClass = 10,
                                           pixelGroupAgeClass = 10,
                                           minAgeForGrouping = -1)#,
    #pixelFateDT = pixelFateDT)

    sim$cohortData <- cohortDataFiles$cohortData
    pixelCohortData <- cohortDataFiles$pixelCohortData
    rm(cohortDataFiles)

    ## make a table of available active and inactive (no biomass) ecoregions
    sim$ecoregion <- makeEcoregionDT(pixelCohortData, speciesEcoregion)

    ## make biomassMap, ecoregionMap, minRelativeB, pixelGroupMap
    sim$biomassMap <- makeBiomassMap(pixelCohortData, sim$rasterToMatch)
    sim$ecoregionMap <- makeEcoregionMap(ecoregionFiles, pixelCohortData)
    sim$minRelativeB <- makeMinRelativeB(pixelCohortData)
    sim$pixelGroupMap <- makePixelGroupMap(pixelCohortData, sim$rasterToMatch)

    .compareRas(sim$biomassMap, sim$ecoregionMap, sim$pixelGroupMap, sim$rasterToMatch, res = TRUE)

    ## make ecoregionGroup a factor and export speciesEcoregion to sim
    speciesEcoregion[, ecoregionGroup := factor(as.character(ecoregionGroup))]
    sim$speciesEcoregion <- speciesEcoregion

    ## do assertions
    message(blue("Create pixelGroups based on: ", paste(columnsForPixelGroups, collapse = ", "),
                 "\n  Resulted in", magenta(length(unique(sim$cohortData$pixelGroup))),
                 "unique pixelGroup values"))
    LandR::assertERGs(sim$ecoregionMap, cohortData = sim$cohortData,
                      speciesEcoregion = speciesEcoregion,
                      minRelativeB = sim$minRelativeB)

    LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)

    LandR::assertUniqueCohortData(sim$cohortData, c("pixelGroup", "ecoregionGroup", "speciesCode"))
  }

  ## check objects
  LandR::assertColumns(sim$cohortData, c(pixelGroup = "integer",
                                         ecoregionGroup = "factor",
                                         speciesCode = "factor",
                                         age = "integer",
                                         B = "integer"))

  ## harmonize to simulated species -- we assume species is the correct set
  ## (it has been filtered by B_borealDP and B_speciesParams)
  ## leave sim$sppEquiv untouched for future reference - sim$sppColorVect can be changed
  mod$sppEquiv <- sim$sppEquiv[get(P(sim)$sppEquivCol) %in% unique(sim$species$speciesCode)]
  sppColorVect <- sim$sppColorVect[c(unique(as.character(sim$species$speciesCode)), "Mixed")]
  sppColorVect <- sppColorVect[complete.cases(sppColorVect)]

  sppOuts <- sppHarmonize(mod$sppEquiv, unique(sim$species$speciesCode), sppEquivCol = P(sim)$sppEquivCol,
                          sppColorVect = sppColorVect, vegLeadingProportion = P(sim)$vegLeadingProportion)

  ## TODO: it'd be great to functionize this:
  if (length(setdiff(sim$sppColorVect, sppOuts$sppColorVect))) {
    message(blue(
      "sim$sppColorVect will be filtered to simulated species only (sim$species$speciesCode)"
    ))
  }
  sim$sppColorVect <- sppOuts$sppColorVect

  if (length(setdiff(sim$sppNameVector, sppOuts$sppNameVector))) {
    message(blue(
      "sim$sppNameVector will be filtered to simulated species only (sim$species$speciesCode)"
    ))
  }
  sim$sppNameVector <- sppOuts$sppNameVector

  assertSppVectors(sppEquiv = sim$species, sppEquivCol = "speciesCode",
                   sppColorVect = sim$sppColorVect)
  assertSppVectors(sppEquiv = mod$sppEquiv, sppEquivCol = P(sim)$sppEquivCol, sppColorVect = sim$sppColorVect)

  rasterNamesToCompare <- c("ecoregionMap", "pixelGroupMap")
  if (!identical(P(sim)$initialBiomassSource, "cohortData")) {
    rasterNamesToCompare <- c(rasterNamesToCompare, "biomassMap")
  }
  haveAllRasters <- all(!unlist(lapply(rasterNamesToCompare, function(rn) is.null(sim[[rn]]))))

  if (haveAllRasters) {
    rastersToCompare <- mget(rasterNamesToCompare, envir(sim))
    do.call(.compareRas, append(list(x = sim$rasterToMatch, res = TRUE), rastersToCompare))
  } else {
    stop("Expecting 3 rasters at this point: sim$biomassMap, sim$ecoregionMap, ",
         "sim$pixelGroupMap and they must match sim$rasterToMatch")
  }

  LandR::assertERGs(sim$ecoregionMap, sim$cohortData, sim$speciesEcoregion, sim$minRelativeB)

  ## ecoregion -------------------------------------------------
  if (is.null(sim$ecoregion)) {
    stop("Need to supply sim$ecoregion")
  }
  setDT(sim$ecoregion)
  LandR::assertColumns(sim$ecoregion, c(active = "character", ecoregionGroup = "factor"))

  ecoregion <- sim$ecoregion
  ## speciesEcoregion - checks
  LandR::assertColumns(sim$speciesEcoregion,
                       c(ecoregionGroup = "factor", speciesCode = "factor",
                         establishprob = "numeric", maxB = "integer", maxANPP = "numeric"))
  speciesEcoregion <- sim$speciesEcoregion
  speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)

  ## minRelativeB ----------------------------------------------
  setDT(sim$minRelativeB) # make a data.table
  ## join to get ecoregionGroup column
  sim$minRelativeB <- sim$minRelativeB[unique(speciesEcoregion[, .(ecoregionGroup)]),
                                       on = "ecoregionGroup", nomatch = 0]

  ## create cohortData from communities ------------------------
  active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, ecoregionGroup)], k) # not sure what k is doing here

  pixelGroupMap <- sim$pixelGroupMap
  names(pixelGroupMap) <- "pixelGroup"

  ## Changed mechanism for active and inactive -- just use NA on ecoregionMap
  ecoregionMapNAs <- is.na(as.vector(sim$ecoregionMap[]))

  ecoregionMapReporting <- maskTo(sim$ecoregionMap, sim$studyAreaReporting)
  ecoregionMapReportingNAs <- is.na(as.vector(ecoregionMapReporting[]))

  sim$activePixelIndex <- which(!ecoregionMapNAs)                    ## store for future use
  sim$activePixelIndexReporting <- which(!ecoregionMapReportingNAs)  ## store for future use

  sim$inactivePixelIndex <- which(ecoregionMapNAs)                   ## store for future use
  sim$inactivePixelIndexReporting <- which(ecoregionMapReportingNAs) ## store for future use

  assertthat::assert_that(all(is.na(as.vector(sim$ecoregionMap[])) == is.na(as.vector(pixelGroupMap[]))))

  ## Keeps track of the length of the ecoregion
  mod$activeEcoregionLength <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                                         as.vector(values(sim$ecoregionMap)),
                                                                         att = "ecoregionGroup"),
                                          pixelIndex = 1:ncell(sim$ecoregionMap))[
                                            ecoregionGroup %in% active_ecoregion$ecoregionGroup,
                                            .(NofCell = length(pixelIndex)), by = "ecoregionGroup"]

  cohortData <- sim$cohortData[pixelGroup %in% unique(as.vector(values(pixelGroupMap))[sim$activePixelIndex]), ]
  cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = speciesEcoregion,
                                                 currentTime = round(time(sim)),
                                                 cohortData = cohortData)
  cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = cohortData)
  LandR::assertCohortData(cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)

  initialBiomassSourcePoss <- c('spinUp', 'cohortData', 'biomassMap')
  if (!any(grepl(P(sim)$initialBiomassSource, initialBiomassSourcePoss))) {
    stop("P(sim)$initialBiomassSource must be one of: ", paste(initialBiomassSourcePoss, collapse = ", "))
  }

  ## spinup ------------------------------------------------------------------
  if (grepl("spin", tolower(P(sim)$initialBiomassSource))) {
    ## negate the TRUE to allow for default to be this, even if NULL or NA
    stop("'spinUp as a value for P(sim)$initialBiomassSource is not working currently; ",
         "please use 'cohortData'")

    if (verbose > 0)
      message("Running spinup")

    spinupstage <- Cache(spinUp,
                         cohortData = cohortData,
                         calibrate = P(sim)$calibrate,
                         successionTimestep = P(sim)$successionTimestep,
                         spinupMortalityfraction = P(sim)$spinupMortalityfraction,
                         species = sim$species,
                         userTags = c(cacheTags, "spinUp"),
                         omitArgs = c("userTags"))

    cohortData <- spinupstage$cohortData
    if (P(sim)$calibrate) {
      sim$spinupOutput <- spinupstage$spinupOutput
    }
    if (P(sim)$calibrate) {
      sim$simulationTreeOutput <- data.table(Year = numeric(), siteBiomass = numeric(),
                                             Species = character(), Age = numeric(),
                                             iniBiomass = numeric(), ANPP = numeric(),
                                             Mortality = numeric(), deltaB = numeric(),
                                             finBiomass = numeric())
      sim$regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                           Year = numeric(), numberOfReg = numeric())
    }
  } else {
    if (grepl("biomassMap", tolower(P(sim)$initialBiomassSource))) {
      stop("'biomassMap as a value for P(sim)$initialBiomassSource is not working currently; ",
           "please use 'cohortData'")
      if (verbose > 0)
        message("Skipping spinup and using the sim$biomassMap * SpeciesLayers pct as initial biomass values")
      biomassTable <- data.table(biomass = as.vector(values(sim$biomassMap)),
                                 pixelGroup = as.vector(values(pixelGroupMap)))
      biomassTable <- na.omit(biomassTable)
      maxBiomass <- maxValue(sim$biomassMap)
      if (maxBiomass < 1e3) {
        if (verbose > 0) {
          message(crayon::green("  Because biomassMap values are all below 1000, assuming that these are on tonnes/ha.\n",
                                "    Converting to $g/m^2$ by multiplying by 100"))
        }
        biomassTable[, `:=`(biomass = biomass * 100)]
      }

      ## In case there are non-identical biomasses in each pixelGroup -- this should be irrelevant with
      ##   improved biomass_borealDataPrep.R (Jan 6, 2019 -- Eliot)
      biomassTable <- biomassTable[, list(Bsum = mean(biomass, na.rm = TRUE)), by = pixelGroup]
      if (!is.integer(biomassTable[["Bsum"]]))
        set(biomassTable, NULL, "Bsum", asInteger(biomassTable[["Bsum"]]))

      ## Delete the B from cohortData -- it will be joined from biomassTable
      set(cohortData, NULL, "B", NULL)
      cohortData[, totalSpeciesPresence := sum(speciesPresence), by = "pixelGroup"]
      cohortData <- cohortData[biomassTable, on = "pixelGroup"]
      cohortData[, B := Bsum * speciesPresence / totalSpeciesPresence, by = c("pixelGroup", "speciesCode")]
      if (!is.integer(cohortData[["B"]]))
        set(cohortData, NULL, "B", asInteger(cohortData[["B"]]))
    }
  }

  pixelAll <- cohortData[, .(uniqueSumB = sum(B, na.rm = TRUE)), by = pixelGroup]
  if (!is.integer(pixelAll[["uniqueSumB"]]))
    set(pixelAll, NULL, "uniqueSumB", asInteger(pixelAll[["uniqueSumB"]]))

  if (all(!is.na(P(sim)$.plots))) {
    sim$simulatedBiomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
  }

  ## 2024-08: typically, cohortDefinitionCols should not include ecoregionGroup and B,
  ## but we want to keep these columns in this case (currently never run; note 'stop()' above)
  colsToKeep <- unique(c(P(sim)$cohortDefinitionCols, "ecoregionGroup", "B"))
  sim$cohortData <- cohortData[, .SD, .SDcol = colsToKeep]
  sim$cohortData[, c("mortality", "aNPPAct") := 0L]
  # sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B, mortality = 0L, aNPPAct = 0L)]
  ## the above breaks with non-default cohortDefinitionCols
  sim$cohortData <- setcolorder(sim$cohortData, neworder = c("pixelGroup", "ecoregionGroup", "speciesCode", "age", "B",
                                                             "mortality", "aNPPAct"))
  simulationOutput <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                                as.vector(values(sim$ecoregionMap)),
                                                                att = "ecoregionGroup"),
                                 pixelGroup = as.vector(values(pixelGroupMap)),
                                 pixelIndex = 1:ncell(sim$ecoregionMap))[
                                   , .(NofPixel = length(pixelIndex)),
                                   by = c("ecoregionGroup", "pixelGroup")]

  simulationOutput <- setkey(simulationOutput, pixelGroup)[setkey(pixelAll, pixelGroup), nomatch = 0][
    , .(Biomass = sum(as.numeric(uniqueSumB) * as.numeric(NofPixel))), by = ecoregionGroup] ## NOTE:
  ## above needs to be numeric because of integer overflow -- returned to integer in 2 lines
  simulationOutput <- setkey(simulationOutput, ecoregionGroup)[
    setkey(mod$activeEcoregionLength, ecoregionGroup), nomatch = 0]
  sim$simulationOutput <- simulationOutput[, .(ecoregionGroup, NofCell, Year = asInteger(time(sim)),
                                               Biomass = asInteger(Biomass / NofCell),
                                               ANPP = 0L, Mortality = 0L, Regeneration = 0L)]

  ## make initial vegTypeMap - this is important when saving outputs at year = 1, with eventPriority = 1
  ## this vegTypeMap will be overwritten later in the same year.
  if (!is.null(P(sim)$calcSummaryBGM)) {
    sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, sim$pixelGroupMap,
                                          P(sim)$vegLeadingProportion, mixedType = P(sim)$mixedType,
                                          sppEquiv = mod$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                                          colors = sim$sppColorVect,
                                          doAssertion = getOption("LandR.assertions", TRUE))
  }

  sim$lastReg <- 0
  speciesEcoregion[, identifier := year > P(sim)$successionTimestep]
  speciesEcoregion_True <- speciesEcoregion[identifier == TRUE, ]
  speciesEcoregion_False <- speciesEcoregion[identifier == FALSE, ]
  if (NROW(speciesEcoregion_False)) {
    speciesEcoregion_True_addon <- speciesEcoregion_False[year == max(year), ]
    speciesEcoregion_True <- rbindlist(list(speciesEcoregion_True_addon, speciesEcoregion_True))
  }
  sim$speciesEcoregion <- speciesEcoregion_True[, ':='(year = year - min(year), identifier = NULL)]

  sim$lastFireYear <- "noFire"

  sim$pixelGroupMap <- pixelGroupMap
  return(invisible(sim))
}

SummaryBGM <- compiler::cmpfun(function(sim) {
  pixelGroups <- data.table(pixelGroupIndex = unique(sim$cohortData$pixelGroup),
                            temID = 1:length(unique(sim$cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = P(sim)$cutpoint),
                             max(pixelGroups$temID))))
  if (length(cutpoints) == 1)
    cutpoints <- c(cutpoints, cutpoints + 1)
  pixelGroups[, groups := cut(temID, breaks = cutpoints,
                              labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                              include.lowest = TRUE)]
  ecoPixelgroup <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                             as.vector(values(sim$ecoregionMap)),
                                                             att = "ecoregionGroup"),
                              pixelGroup = as.vector(values(sim$pixelGroupMap)),
                              pixelIndex = 1:ncell(sim$ecoregionMap))[
                                , .(NofPixelGroup = length(pixelIndex)),
                                by = c("ecoregionGroup", "pixelGroup")]

  for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
    subCohortData <- sim$cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    if (nrow(subCohortData[age == (P(sim)$successionTimestep + 1),]) > 0) {
      subCohortData[age == (P(sim)$successionTimestep + 1), reproduction := sum(B), by = pixelGroup]
    } else {
      subCohortData[, reproduction := 0]
    }
    subCohortData[is.na(reproduction), reproduction := 0L]

    ## Don't need to do asInteger within the by group calculation. Separate to next step.
    summarytable_sub <- subCohortData[, .(uniqueSumB = sum(B, na.rm = TRUE),
                                          uniqueSumANPP = sum(aNPPAct, na.rm = TRUE),
                                          uniqueSumMortality = sum(mortality, na.rm = TRUE),
                                          uniqueSumRege = mean(reproduction, na.rm = TRUE)),
                                      by = pixelGroup]
    for (column in names(summarytable_sub)) if (!is.integer(summarytable_sub[[column]]))
      set(summarytable_sub, NULL, column, asInteger(summarytable_sub[[column]]))

    tempOutput <- setkey(ecoPixelgroup[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ],
                         pixelGroup)[setkey(summarytable_sub, pixelGroup), nomatch = 0]

    if (subgroup == "Group1") {
      summaryBGMtable <- summarytable_sub
      tempOutput_All <- tempOutput
    } else {
      summaryBGMtable <- rbindlist(list(summaryBGMtable, summarytable_sub))
      tempOutput_All <- rbindlist(list(tempOutput_All, tempOutput))
    }
    rm(summarytable_sub, tempOutput, subCohortData)
  }

  ## need as.numeric below because of integer overflow -- returned to integer in 2 lines
  tempOutput_All <- tempOutput_All[, .(Biomass = sum(as.numeric(uniqueSumB) * as.numeric(NofPixelGroup)),
                                       ANPP = sum(as.numeric(uniqueSumANPP) * as.numeric(NofPixelGroup)),
                                       Mortality = sum(as.numeric(uniqueSumMortality) * as.numeric(NofPixelGroup)),
                                       Regeneration = sum(as.numeric(uniqueSumRege) * as.numeric(NofPixelGroup))),
                                   by = ecoregionGroup]
  tempOutput_All <- setkey(tempOutput_All, ecoregionGroup)[setkey(mod$activeEcoregionLength,
                                                                  ecoregionGroup), nomatch = 0]
  sim$simulationOutput <- rbindlist(
    list(sim$simulationOutput,
         tempOutput_All[, .(ecoregionGroup, NofCell, Year = as.integer(time(sim)),
                            Biomass = asInteger(Biomass / NofCell),
                            ANPP = asInteger(ANPP / NofCell),
                            Mortality = asInteger(Mortality / NofCell),
                            Regeneration = asInteger(Regeneration / NofCell))]))
  ## the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively.
  names(sim$pixelGroupMap) <- "pixelGroup"

  sim$simulatedBiomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumB")
  setColors(sim$simulatedBiomassMap) <- c("light green", "dark green")

  sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumANPP")
  setColors(sim$ANPPMap) <- c("light green", "dark green")

  sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumMortality")
  setColors(sim$mortalityMap) <- c("light green", "dark green")

  if (!is.null(P(sim)$calcSummaryBGM)) {
    sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, sim$pixelGroupMap,
                                          P(sim)$vegLeadingProportion, mixedType = P(sim)$mixedType,
                                          sppEquiv = mod$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                                          colors = sim$sppColorVect,
                                          doAssertion = getOption("LandR.assertions", TRUE))
  }

  rm(cutpoints, pixelGroups, tempOutput_All, summaryBGMtable) ## TODO: is this needed? on exit, should free the mem used for these
  return(invisible(sim))
})

MortalityAndGrowth <- compiler::cmpfun(function(sim) {
  ## If cohortData has length 0, don't do this --
  ## this can happen in more theoretical use cases where e.g., end(sim) is longer than longevity
  if (NROW(sim$cohortData)) {

    if (is.numeric(P(sim)$.useParallel)) {
      data.table::setDTthreads(P(sim)$.useParallel)
      if (data.table::getDTthreads() > 1L) message("Mortality and Growth should be using >100% CPU")
    }

    ## Install climate-sensitive functions (or not)
    # a <- try(requireNamespace(P(sim)$growthAndMortalityDrivers, quietly = TRUE)) ## Fixed (Eliot) TODO: this is not working. requireNamespace overrides try
    # if (class(a) == "try-error") {
    if (!requireNamespace(P(sim)$growthAndMortalityDrivers, quietly = TRUE)) {
      stop(paste0("The package specified for growthAndMortalityDrivers, ",
                  P(sim)$growthAndMortalityDrivers, ", must be installed"))
    }

    calculateClimateEffect <- getFromNamespace("calculateClimateEffect", P(sim)$growthAndMortalityDrivers)

    cohortData <- sim$cohortData
    pgs <- unique(cohortData$pixelGroup)

    ## This tests for available memory and tries to scale the groupSize accordingly.
    ## It is, however, a very expensive operation. It now only does it once per simulation
    groupSize <- maxRowsDT(maxLen = 1e7, maxMem = P(sim)$.maxMemory,
                           startClockTime = sim$._startClockTime, groupSize = groupSize,
                           modEnv = mod)

    numGroups <- ceiling(length(pgs) / groupSize)
    groupNames <- paste0("Group", seq(numGroups))
    if (length(pgs) > groupSize) {
      sim$cohortData <- cohortData[0, ]
      pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup),
                                temID = 1:length(unique(cohortData$pixelGroup)))
      cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = groupSize), max(pixelGroups$temID))))
      # cutpoints <- c(1,max(pixelGroups$temID))
      if (length(cutpoints) == 1)
        cutpoints <- c(cutpoints, cutpoints + 1)

      pixelGroups[, groups := rep(groupNames, each = groupSize, length.out = NROW(pixelGroups))]
    }
    verboseCSB <- 1
    for (subgroup in groupNames) {
      if (numGroups == 1) {
        subCohortData <- cohortData
      } else {
        subCohortData <- cohortData[cohortData$pixelGroup %in% pixelGroups$pixelGroupIndex[pixelGroups$groups == subgroup], ]
      }

      subCohortData[age > 1, age := age + 1L]
      subCohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                        currentTime = round(time(sim)),
                                                        cohortData = subCohortData)
      subCohortData <- updateSpeciesAttributes(species = sim$species, cohortData = subCohortData)
      setkeyv(subCohortData, "pixelGroup")
      subCohortData <- calculateSumB(cohortData = subCohortData,
                                     lastReg = sim$lastReg,
                                     currentTime = time(sim),
                                     successionTimestep = P(sim)$successionTimestep, verbose = verboseCSB)
      verboseCSB <- 0 # don't re-message in other subgroup

      ## Die from old age or low biomass -- rm from cohortData ------------------
      keep <- (subCohortData$age <= subCohortData$longevity) & (subCohortData$B >=  P(sim)$minCohortBiomass)
      if (all(keep)) {
        subCohortPostLongevity <- subCohortData
        diedCohortData <- subCohortData[0]
      } else {
        subCohortPostLongevity <- subCohortData[keep]
        diedCohortData <- subCohortData[!keep]
      }

      numCohortsDied <- NROW(diedCohortData)

      if (numCohortsDied > 0) {
        ## Identify the PGs that are totally gone, not just an individual cohort that died
        pgsToRm <- diedCohortData[!pixelGroup %in% subCohortPostLongevity$pixelGroup]

        pixelsToRm <- which(as.vector(values(sim$pixelGroupMap)) %in% unique(pgsToRm$pixelGroup))
        if (isTRUE(getOption("LandR.assertions"))) {
          a <- subCohortPostLongevity$pixelGroup %in% na.omit(as.vector(values(sim$pixelGroupMap)))
          if (!all(a)) {
            stop("Post longevity-based mortality, there is a divergence between pixelGroupMap and cohortData pixelGroups")
          }
        }
        if (length(pixelsToRm) > 0) {
          if (getOption("LandR.verbose", TRUE) > 0) {
            numPixelGrps <- sum(as.vector(sim$pixelGroupMap[]) != 0, na.rm = TRUE)
          }
          sim$pixelGroupMap[pixelsToRm] <- 0L
          if (getOption("LandR.verbose", TRUE) > 1) {
            message(blue("Death due to old age:",
                         "\n  ", numCohortsDied, "cohorts died of old age (i.e., due to passing longevity) or biomass <= 1; ",
                         sum(is.na(diedCohortData$age)), " of those because age == NA; ",
                         "\n  ", NROW(unique(pgsToRm$pixelGroup)), "pixelGroups to be removed (i.e., ",
                         "\n  ", length(pixelsToRm), "pixels; "))
          }
          if (getOption("LandR.verbose", TRUE) > 0) {
            message(blue("\n   Total number of pixelGroups -- Was:", numPixelGrps,
                         ", Now:", magenta(sum(as.vector(sim$pixelGroupMap[]) != 0, na.rm = TRUE))))
          }
        }
      }

      subCohortData <- subCohortPostLongevity

      ## Calculate age and competition effects ----------------
      subCohortData <- calculateAgeMortality(cohortData = subCohortData)

      set(subCohortData, NULL, c("longevity", "mortalityshape"), NULL)
      subCohortData <- calculateCompetition(cohortData = subCohortData)
      if (!P(sim)$calibrate) {
        set(subCohortData, NULL, "sumB", NULL)
      }

      subCohortData <- calculateANPP(cohortData = subCohortData)  ## competition effect on aNPP via bPM
      set(subCohortData, NULL, "growthcurve", NULL)

      ## This next line is step one of a double removal of mAge ... see comments a few
      ##    lines down to discuss this double counting
      set(subCohortData, NULL, "aNPPAct", pmax(1, subCohortData$aNPPAct - subCohortData$mAge))

      ## generate climate-sensitivity predictions - this will no longer run if LandR pkg is the driver
      if (!P(sim)$growthAndMortalityDrivers == "LandR") {
        ## necessary due to column joining
        if (!is.null(subCohortData$growthPred)) {
          set(subCohortData, NULL, c("growthPred", "mortPred"), NULL)
        }

        ## get arguments from sim environment --
        ## this way Biomass_core is blind to whatever is used by calculateClimateEffect fxns
        ## as long as the function is called 'calculateClimateEffect', represents a multiplier,
        ## and uses growth, mortality and age limits
        cceArgs <- lapply(sim$cceArgs, FUN = function(x) {
          arg <- eval(x, envir = sim)
        })
        names(cceArgs) <- paste(sim$cceArgs)

        predObj <- calculateClimateEffect(cceArgs = cceArgs,
                                          cohortData = subCohortData,
                                          pixelGroupMap = sim$pixelGroupMap,
                                          gmcsGrowthLimits = P(sim)$gmcsGrowthLimits,
                                          gmcsMortLimits = P(sim)$gmcsMortLimits,
                                          gmcsMinAge = P(sim)$gmcsMinAge,
                                          cohortDefinitionCols = P(sim)$cohortDefinitionCols)
        ## Join must be done this way
        commonNames <- names(predObj)[names(predObj) %in% names(subCohortData)]
        subCohortData <- subCohortData[predObj, on = commonNames]
        subCohortData[, aNPPAct := pmax(0, asInteger(aNPPAct * growthPred/100))] ## changed from ratio to pct for memory
      }
      subCohortData <- calculateGrowthMortality(cohortData = subCohortData)

      ## NOTE RE: double removal of mAge -- it is a correct implementation of the LANDIS source
      ## We already tried to purge the double removal... but reverted:
      ## https://github.com/PredictiveEcology/Biomass_core/commit/5227ae9acfe291bcb8596fd9e63c79602de5d2c6
      ## LANDIS BSExt v3.2 (implemented in LandR):
      ## https://github.com/LANDIS-II-Foundation/Extensions-Succession-Archive/blob/master/biomass-succession-archive/tags/release-3.2/src/CohortBiomass.cs
      ## LANDIS BSExt current version:
      ## https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/src/CohortBiomass.cs
      set(subCohortData, NULL, "mBio", pmax(0, subCohortData$mBio - subCohortData$mAge))
      set(subCohortData, NULL, "mBio", pmin(subCohortData$mBio, subCohortData$aNPPAct))
      set(subCohortData, NULL, "mortality", subCohortData$mBio + subCohortData$mAge)

      ## this line will return mortality unchanged unless LandR_BiomassGMCS is also run
      if (!P(sim)$growthAndMortalityDrivers == "LandR") {

        subCohortData[, mortality := pmax(0, asInteger(mortality * mortPred/100))]
        subCohortData[, mortality := pmin(mortality, B + aNPPAct)] #this prevents negative biomass, but allows B = 0 for 1 year
        if (!P(sim)$keepClimateCols) {
          set(subCohortData, NULL, c("growthPred", "mortPred"), NULL)
        }
      }

      set(subCohortData, NULL, c("mBio", "mAge", "maxANPP", "maxB", "maxB_eco", "bAP", "bPM"), NULL)
      if (P(sim)$calibrate) {
        set(subCohortData, NULL, "deltaB", asInteger(subCohortData$aNPPAct - subCohortData$mortality))
        set(subCohortData, NULL, "B", subCohortData$B + subCohortData$deltaB)
        tempcohortdata <- subCohortData[,.(pixelGroup, Year = time(sim), siteBiomass = sumB, speciesCode,
                                           Age = age, iniBiomass = B - deltaB, ANPP = round(aNPPAct, 1),
                                           Mortality = round(mortality,1), deltaB, finBiomass = B)]
        tempcohortdata <- setkey(tempcohortdata, speciesCode)[
          setkey(sim$species[, .(species, speciesCode)], speciesCode),
          nomatch = 0][, ':='(speciesCode = species, species = NULL, pixelGroup = NULL)]
        setnames(tempcohortdata, "speciesCode", "Species")
        sim$simulationTreeOutput <- rbind(sim$simulationTreeOutput, tempcohortdata)
        set(subCohortData, NULL, c("deltaB", "sumB"), NULL)
      } else {
        set(subCohortData, NULL, "B",
            subCohortData$B + asInteger(subCohortData$aNPPAct - subCohortData$mortality))
      }
      subCohortData[, `:=`(mortality = asInteger(mortality), aNPPAct = asInteger(aNPPAct))]

      if (numGroups == 1) {
        sim$cohortData <- subCohortData
      } else {
        sim$cohortData <- rbindlist(list(sim$cohortData, subCohortData), fill = TRUE)
      }
      rm(subCohortData)
    }
    rm(cohortData)
    if (ncell(sim$rasterToMatch) > 3e7) gc() ## restored this gc call 2019-08-20 (AMC)

    ## now age this year's recruits
    sim$cohortData[age == 1, age := age + 1L]

    if (isTRUE(getOption("LandR.assertions"))) {
      byCols <- P(sim)$cohortDefinitionCols
      if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = byCols)))) {
        whPGduplicated <- sim$cohortData[duplicated(sim$cohortData, by = byCols) %in% TRUE]
        cd1 <- sim$cohortData[whPGduplicated[, ..byCols], on = byCols]
        byColsNoB <- setdiff(byCols, "B")
        sumCols <- c("B", "mortality", "aNPPAct")
        cd1 <- cd1[, lapply(.SD, sum), .SDcols = sumCols,  by = byColsNoB]
        cd2 <- sim$cohortData[!whPGduplicated, on = byCols]
        sim$cohortData <- rbindlist(list(cd1, cd2), use.names = TRUE)
        message("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age.\n",
                "These identical cohorts were summed together")
      }
    }

    LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)
  }
  return(invisible(sim))
})

Dispersal <- function(sim) {
  treedFirePixelTableCurYr <- sim$treedFirePixelTableSinceLastDisp[burnTime == time(sim)]
  pixelsFromCurYrBurn <- treedFirePixelTableCurYr$pixelIndex
  tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% pixelsFromCurYrBurn)]
  # tempInactivePixel <- c(sim$inactivePixelIndex, pixelsFromCurYrBurn)

  if (P(sim)$seedingAlgorithm == "noDispersal") {
    sim <- NoDispersalSeeding(sim, tempActivePixel)
  } else if (P(sim)$seedingAlgorithm == "universalDispersal") {
    sim <- UniversalDispersalSeeding(sim, tempActivePixel)
  } else if (P(sim)$seedingAlgorithm == "wardDispersal") {
    sim <- WardDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
  } else if (!P(sim)$seedingAlgorithm == "noSeeding") {
    stop("Undefined seed dispersal type!")
  }

  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableCurYr
  return(invisible(sim))
}

NoDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel) {
  # if (sim$lastFireYear == round(time(sim))) { # if current year is both fire year and succession year
  #   # find new active pixel that remove successful postfire regeneration
  #   # since this is on site regeneration, all the burnt pixels can not seeding
  #   tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$treedFirePixelTableSinceLastDisp$pixelIndex)]
  # } else {
  #   tempActivePixel <- sim$activePixelIndex
  # }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, currentTime = time(sim),
                                  successionTimestep = P(sim)$successionTimestep)
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[
    setkey(sim$species[, .(speciesCode, sexualmature)], speciesCode), nomatch = 0]

  seedingData <- sim$cohortData[age >= sexualmature]
  set(sim$cohortData, NULL, "sexualmature", NULL)
  set(seedingData, NULL, c("sexualmature", "age", "B", "mortality", "aNPPAct"), NULL)
  siteShade <- setkey(data.table(calcSiteShade(currentTime = round(time(sim)), sim$cohortData,
                                               sim$speciesEcoregion, sim$minRelativeB)), pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[siteShade, nomatch = 0]
  seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[
    , .(speciesCode, shadetolerance)], speciesCode), nomatch = 0]
  seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, seedingData)
  seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData), 0, 1),]
  set(seedingData, NULL, c("shadetolerance", "lightProb", "siteShade", "sumB"), NULL)
  seedingData <- unique(seedingData, by = c("pixelGroup", "speciesCode"))

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = as.vector(values(sim$pixelGroupMap))[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(seedingData$pixelGroup)], pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)

  specieseco_current <- speciesEcoregionLatestYear(
    sim$speciesEcoregion[, .(year, speciesCode, establishprob, ecoregionGroup)],
    round(time(sim))
  )
  specieseco_current <- setkey(specieseco_current, ecoregionGroup, speciesCode)

  # specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  # specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
  seedingData <- seedingData[specieseco_current, nomatch = 0]
  seedingData <- seedingData[establishprob %>>% runif(nrow(seedingData), 0, 1),]
  set(seedingData, NULL, c("establishprob"), NULL)
  if (P(sim)$calibrate == TRUE && NROW(seedingData) > 0) {
    newCohortData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm,
                                          year = round(time(sim)),
                                          numberOfReg = length(pixelIndex)),
                                      by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[
      setkey(sim$species[, .(species,speciesCode)], speciesCode),
      nomatch = 0][, .(species, seedingAlgorithm, year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }

  if (nrow(seedingData) > 0) {
    outs <- updateCohortData(
      seedingData,
      cohortData = sim$cohortData,
      pixelGroupMap = sim$pixelGroupMap,
      currentTime = round(time(sim)),
      speciesEcoregion = sim$speciesEcoregion,
      cohortDefinitionCols = P(sim)$cohortDefinitionCols,
      treedFirePixelTableSinceLastDisp = NULL,
      initialB = P(sim)$initialB,
      successionTimestep = P(sim)$successionTimestep
    )
    sim$cohortData <- outs$cohortData
    sim$pixelGroupMap <- outs$pixelGroupMap
  }

  sim$lastReg <- round(time(sim))
  return(invisible(sim))
})

UniversalDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel) {
  # if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
  #   tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  # } else {
  #   tempActivePixel <- sim$activePixelIndex
  # }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, currentTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  species <- sim$species
  ## all species can provide seed source, i.e. age >= sexualmature
  speciessource <- setkey(sim$species[, .(speciesCode, k = 1)], k)
  siteShade <- data.table(calcSiteShade(currentTime = round(time(sim)), sim$cohortData,
                                        sim$speciesEcoregion, sim$minRelativeB))
  activePixelGroup <- data.table(
    pixelGroup = as.vector(values(sim$pixelGroupMap))[tempActivePixel],
    ecoregionGroup = factorValues2(sim$ecoregionMap, as.vector(values((sim$ecoregionMap))),
                                   att = "ecoregionGroup")[tempActivePixel]
  ) |>
    unique(by = "pixelGroup")
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = c("pixelGroup", "ecoregionGroup")) |>
    data.table()
  siteShade[is.na(siteShade), siteShade := 0]
  setkey(siteShade[, k := 1], k)
  ## I believe this is the latest version how the LANDIS-II folks calculate sufficient light
  ## https://github.com/LANDIS-II-Foundation/Library-Succession/blob/master/src/ReproductionDefaults.cs
  seedingData <- siteShade[speciessource, allow.cartesian = TRUE][, k := NULL]
  seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[, .(speciesCode, shadetolerance)],
                                                         speciesCode),
                                                  nomatch = 0]
  seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, seedingData)
  seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData)), ]
  set(seedingData, NULL, c("siteShade", "lightProb", "shadetolerance"), NULL)
  # pixelGroupEcoregion <- unique(sim$cohortData, by = c("pixelGroup"))[,'.'(pixelGroup, sumB)]

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = as.vector(values(sim$pixelGroupMap))[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(seedingData$pixelGroup)], pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)

  specieseco_current <- speciesEcoregionLatestYear(
    sim$speciesEcoregion[, .(year, speciesCode, establishprob, ecoregionGroup)],
    round(time(sim))
  )
  specieseco_current <- setkeyv(specieseco_current, c("ecoregionGroup", "speciesCode"))

  # specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  # specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
  #                                                 .(speciesCode, establishprob, ecoregionGroup)],
  #                              ecoregionGroup, speciesCode)
  seedingData <- seedingData[specieseco_current, nomatch = 0]
  seedingData <- seedingData[establishprob %>>% runif(nrow(seedingData)), ]
  set(seedingData, NULL, "establishprob", NULL)
  if (P(sim)$calibrate == TRUE) {
    newCohortData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm,
                                          Year = round(time(sim)),
                                          numberOfReg = length(pixelIndex)),
                                      by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[
      setkey(sim$species[, .(species, speciesCode)], speciesCode),
      nomatch = 0][, .(species, seedingAlgorithm, Year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }

  if (NROW(seedingData) > 0) {
    outs <- updateCohortData(
      seedingData,
      cohortData = sim$cohortData,
      pixelGroupMap = sim$pixelGroupMap,
      currentTime = round(time(sim)),
      speciesEcoregion = sim$speciesEcoregion,
      cohortDefinitionCols = P(sim)$cohortDefinitionCols,
      treedFirePixelTableSinceLastDisp = NULL,
      initialB = P(sim)$initialB,
      successionTimestep = P(sim)$successionTimestep
    )
    sim$cohortData <- outs$cohortData
    sim$pixelGroupMap <- outs$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
})

WardDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel, pixelsFromCurYrBurn,
                                                  verbose = getOption("LandR.verbose", TRUE)) {

  sim$cohortData <- calculateSumB(cohortData = sim$cohortData,
                                  lastReg = sim$lastReg, currentTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  siteShade <- calcSiteShade(currentTime = round(time(sim)), cohortData = sim$cohortData,
                             sim$speciesEcoregion, sim$minRelativeB)
  activePixelGroup <- data.table(pixelGroup = unique(as.vector(values(sim$pixelGroupMap))[tempActivePixel])) %>%
    na.omit()
  siteShade <- siteShade[activePixelGroup, on = "pixelGroup"]
  siteShade[is.na(siteShade), siteShade := 0]

  ## Seed source cells:
  ## 1. Select only sexually mature cohorts, then
  ## 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  sim$cohortData <- sim$species[, c("speciesCode", "sexualmature")][sim$cohortData, on = "speciesCode"]
  # sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[, .(speciesCode, sexualmature)],
  #                                                              speciesCode),
  #                                                       nomatch = 0]
  matureCohorts <- sim$cohortData[age >= sexualmature] %>%
    unique(by = c("pixelGroup", "speciesCode")) %>%
    setkey(., speciesCode)
  matureCohorts <- matureCohorts[, .(pixelGroup, speciesCode)]
  set(sim$cohortData, NULL, "sexualmature", NULL)

  if (NROW(matureCohorts) > 0) {
    seedSource <- sim$species[, list(speciesCode, seeddistance_eff, seeddistance_max)] %>%
      setkey(., speciesCode) %>%
      .[matureCohorts]
    setkey(seedSource, speciesCode)
    ##  Seed Receiving cells:
    ##  1. Must be sufficient light
    ## seed receive just for the species that are seed source
    tempspecies1 <- sim$species[speciesCode %in% unique(matureCohorts$speciesCode),][
      , .(speciesCode, shadetolerance, seeddistance_eff, seeddistance_max)]
    seedReceive <- setkey(tempspecies1[, c(k = 1, .SD)], k)[setkey(siteShade[
      , c(k = 1, .SD)], k), allow.cartesian = TRUE][, k := NULL]
    seedReceive <- assignLightProb(sufficientLight = sim$sufficientLight, seedReceive)
    set(seedReceive, NULL, "siteShade", NULL)

    ## 3. Remove any species from the seedSource that couldn't regenerate
    ##    anywhere on the map due to insufficient light
    seedReceive <- seedReceive[lightProb %>>% runif(NROW(seedReceive)), ][
      , .(pixelGroup, speciesCode, seeddistance_eff, seeddistance_max)]
    setkey(seedReceive, speciesCode)

    ## rm ones that had successful serotiny or resprouting
    seedReceive <- seedReceive[!sim$cohortData[age == 1L], on = c("pixelGroup", "speciesCode")]
    ##    (info contained within seedReceive)
    ## this is should be a inner join, needs to specify the nomatch=0, nomatch = NA is default that suggest the full join.
    seedSource <- seedSource[speciesCode %in% unique(seedReceive$speciesCode), ]

    ## it could be more efficient if sim$pixelGroupMap is reduced map by removing the pixels that
    ## have successful postdisturbance regeneration and the inactive pixels
    ## how to subset the reducedmap
    # if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
    #   inactivePixelIndex <- c(sim$inactivePixelIndex, sim$treedFirePixelTableSinceLastDisp$pixelIndex)
    # } else {
    #   inactivePixelIndex <- sim$inactivePixelIndex
    # }
    reducedPixelGroupMap <- sim$pixelGroupMap

    # Calculate the maximum size of the chunks for LANDISDisp
    if (length(pixelsFromCurYrBurn) > 0) {
      reducedPixelGroupMap[pixelsFromCurYrBurn] <- NA
    }

    seedingData <- LANDISDisp(dtRcv = seedReceive,
                              dtSrc = seedSource,
                              speciesTable = sim$species,
                              pixelGroupMap = reducedPixelGroupMap,
                              plot.it = FALSE,
                              successionTimestep = P(sim)$successionTimestep,
                              verbose = verbose > 0)

    if (verbose > 0) {
      emptyForestPixels <- sim$treedFirePixelTableSinceLastDisp[burnTime < time(sim)]
      # unique(seedingData[emptyForestPixels, on = "pixelIndex", nomatch = 0], by = "pixelIndex")
      seedsArrivedPixels <- unique(seedingData[unique(emptyForestPixels, by = "pixelIndex"),
                                               on = "pixelIndex", nomatch = 0], by = "pixelIndex")

      message(blue("Of", NROW(emptyForestPixels),
                   "burned and empty pixels: Num pixels where seeds arrived:",
                   NROW(seedsArrivedPixels)))
    }

    rm(seedReceive, seedSource)
    if (NROW(seedingData) > 0) {
      seedingData[, ecoregionGroup := factorValues2(sim$ecoregionMap, as.vector(values(sim$ecoregionMap)),
                                                    att = "ecoregionGroup")[seedingData$pixelIndex]]
      seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)

      specieseco_current <- speciesEcoregionLatestYear(
        sim$speciesEcoregion[, .(year, speciesCode, establishprob, ecoregionGroup)],
        round(time(sim)))
      specieseco_current <- setkeyv(specieseco_current, c("ecoregionGroup", "speciesCode"))

      # specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
      # specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
      #                                                 .(speciesCode, establishprob, ecoregionGroup)],
      #                              ecoregionGroup, speciesCode)
      seedingData <- seedingData[specieseco_current, nomatch = 0]

      ## Run probability of establishment --------------------------------
      LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)

      seedingData <- seedingData[runif(nrow(seedingData)) <= establishprob, ]
      if (verbose > 0) {
        # seedsArrivedPixels <- unique(seedingData[emptyForestPixels, on = "pixelIndex", nomatch = 0], by = "pixelIndex")
        seedsArrivedPixels <- unique(seedingData[unique(emptyForestPixels, by = "pixelIndex"),
                                                 on = "pixelIndex", nomatch = 0], by = "pixelIndex")
        message(blue("Of", NROW(emptyForestPixels),
                     "burned and empty pixels: Num pixels where seedlings established:",
                     NROW(seedsArrivedPixels)))
      }

      set(seedingData, NULL, "establishprob", NULL)
      if (P(sim)$calibrate == TRUE) {
        seedingData_summ <- seedingData[
          , .(seedingAlgorithm = P(sim)$seedingAlgorithm, Year = round(time(sim)),
              numberOfReg = length(pixelIndex)),
          by = speciesCode]
        seedingData_summ <- setkey(seedingData_summ, speciesCode)[
          setkey(sim$species[, .(species,speciesCode)], speciesCode), nomatch = 0][
            , .(species, seedingAlgorithm, Year, numberOfReg)]
        sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, seedingData_summ))
      }
      if (nrow(seedingData) > 0) {
        outs <- updateCohortData(
          seedingData,
          cohortData = sim$cohortData,
          pixelGroupMap = sim$pixelGroupMap,
          currentTime = round(time(sim)),
          speciesEcoregion = sim$speciesEcoregion,
          cohortDefinitionCols = P(sim)$cohortDefinitionCols,
          treedFirePixelTableSinceLastDisp = NULL,
          initialB = P(sim)$initialB,
          successionTimestep = P(sim)$successionTimestep
        )

        sim$cohortData <- outs$cohortData
        sim$pixelGroupMap <- outs$pixelGroupMap
      }
    }
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
})

summaryRegen <- compiler::cmpfun(function(sim) {
  #cohortData <- sim$cohortData
  if (all(!is.na(P(sim)$.plots))) {
    pixelGroupMap <- sim$pixelGroupMap
    names(pixelGroupMap) <- "pixelGroup"
    # please note that the calculation of reproduction is based on successioinTime step interval,
    pixelAll <- sim$cohortData[age <= P(sim)$successionTimestep + 1,
                               .(uniqueSumReproduction = sum(B, na.rm = TRUE)),
                               by = pixelGroup]
    if (!is.integer(pixelAll[["uniqueSumReproduction"]]))
      set(pixelAll, NULL, 'uniqueSumReproduction', asInteger(pixelAll[["uniqueSumReproduction"]]))

    if (NROW(pixelAll) > 0) {
      reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumReproduction")
      setColors(reproductionMap) <- c("light green", "dark green")
    } else {
      reproductionMap <- setValues(pixelGroupMap, 0L)
    }
    rm(pixelAll)
    sim$reproductionMap <- reproductionMap
    rm(pixelGroupMap)
  }
  return(invisible(sim))
})

plotSummaryBySpecies <- compiler::cmpfun(function(sim) {
  LandR::assertSpeciesPlotLabels(sim$species$species, mod$sppEquiv)
  assertSppVectors(sppEquiv = mod$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                   sppColorVect = sim$sppColorVect)

  ## BIOMASS, WEIGHTED AVERAGE AGE, AVERAGE ANPP
  ## AND AGE OF OLDEST COHORT PER SPECIES

  ## Averages are calculated across pixels
  ## don't expand table, multiply by no. pixels - faster
  thisPeriod <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap,
                                      cohortDefinitionCols = P(sim)$cohortDefinitionCols)

  for (column in names(thisPeriod)) if (is.integer(thisPeriod[[column]])) {
    set(thisPeriod, NULL, column, as.numeric(thisPeriod[[column]]))
  }

  thisPeriod <- thisPeriod[, list(year = time(sim),
                                  BiomassBySpecies = sum(B * noPixels, na.rm = TRUE),
                                  AgeBySppWeighted = sum(age * B * noPixels, na.rm = TRUE) /
                                    sum(B * noPixels, na.rm = TRUE),
                                  aNPPBySpecies = sum(aNPPAct * noPixels, na.rm = TRUE),
                                  OldestCohortBySpp = max(age, na.rm = TRUE)),
                           by = .(speciesCode)]
  ## add back in species that are not on the landscape (e.g., killed)
  thisPeriod <- thisPeriod[sim$species[, .(speciesCode)], on = "speciesCode", nomatch = NA]
  thisPeriod[is.na(year), `:=`(year = time(sim), BiomassBySpecies = 0, AgeBySppWeighted = 0,
                               aNPPBySpecies = 0, OldestCohortBySpp = 0)]

  ## overstory
  cohortData <-  addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap,
                                       cohortDefinitionCols = P(sim)$cohortDefinitionCols)
  cohortData[, bWeightedAge := floor(sum(age * B) / sum(B) / 10) * 10, .(pixelGroup)]
  ## B was set as numeric to avoid problems with big numbers being integers
  overstory <- cohortData[age >= bWeightedAge, .(overstoryBiomass = sum(as.numeric(B) * noPixels)),
                          .(speciesCode)]
  overstory <- overstory[sim$species[, .(speciesCode)], on = "speciesCode", nomatch = NA] ## add missing back
  overstory[is.na(overstoryBiomass), overstoryBiomass := 0]
  thisPeriod <- thisPeriod[overstory, on = "speciesCode"]

  assertthat::assert_that(nrow(thisPeriod) == nrow(sim$species)) ## TODO: check this if Mixed used

  if (is.null(sim$summaryBySpecies)) {
    summaryBySpecies <- thisPeriod
  } else {
    summaryBySpecies <- rbindlist(list(sim$summaryBySpecies, thisPeriod))
  }

  ## MEAN NO. PIXELS PER LEADING SPECIES
  vtm <- mask(sim$vegTypeMap, sim$studyAreaReporting)
  freqs <- table(na.omit(factorValues2(vtm, as.vector(vtm[]), att = 2)))
  tabl <- as.vector(freqs)
  summaryBySpecies1 <- data.frame(year = rep(floor(time(sim)), length(freqs)),
                                  leadingType = names(freqs),
                                  #freqs = freqs,
                                  counts = tabl,
                                  stringsAsFactors = FALSE)

  whMixedLeading <- which(summaryBySpecies1$leadingType == "Mixed")
  summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType, mod$sppEquiv,
                                                  "EN_generic_short")
  summaryBySpecies1$leadingType[whMixedLeading] <- "Mixed"

  colours <- equivalentName(names(sim$sppColorVect), mod$sppEquiv, "EN_generic_short")
  whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")
  colours[whMixedSppColors] <- "Mixed"

  colorIDs <- match(summaryBySpecies1$leadingType, colours)
  summaryBySpecies1$cols <- sim$sppColorVect[colorIDs]

  if (!is.null(sim$summaryBySpecies1)) {
    summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, summaryBySpecies1))
  }

  if (length(unique(summaryBySpecies1$year)) > 1) {
    df <- sim$species[, list(speciesCode, species)][summaryBySpecies, on = "speciesCode"]
    df$species <- equivalentName(df$species, mod$sppEquiv, "EN_generic_short")

    colorIDs <- match(df$species, colours)
    df$cols <- sim$sppColorVect[colorIDs]

    cols2 <- df$cols
    names(cols2) <- df$species

    unqdf <- unique(df[, c("cols", "species")])
    unqCols2 <- unqdf$cols
    names(unqCols2) <- unqdf$species

    assertSppVectors(sppEquiv = mod$sppEquiv, sppEquivCol = "EN_generic_short", sppColorVect = unqCols2)

    ## although Plots can deal with .plotInitialTime == NA by not plotting, we need to
    ## make sure the plotting windows are not changed/opened if  .plotInitialTime == NA
    if (!any(is.na(P(sim)$.plots))) {
      if (any(P(sim)$.plots == "screen")) {
        dev(mod$statsWindow)
      }
    }

    ## biomass by species
    Plots(df, fn = speciesBiomassPlot,
          filename = "summary_biomass_by_species",
          path = figurePath(sim),
          types = mod$plotTypes,
          ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
          y = "BiomassBySpecies",
          cols = cols2, ylab = "Biomass",
          plotTitle = paste0("Total biomass by species\n", "across pixels"))

    ## leading species
    maxNpixels <- length(sim$activePixelIndexReporting)
    cols3 <- summaryBySpecies1$cols
    names(cols3) <- summaryBySpecies1$leadingType
    Plots(summaryBySpecies1, fn = speciesLeadingPlot,
          filename = "summary_N_pixels_leading",
          path = figurePath(sim),
          types = mod$plotTypes,
          ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
          cols = cols3, maxNpixels = maxNpixels)

    ## species age
    Plots(df, fn = speciesAgeANPPPlot,
          filename = "summary_biomass-weighted_species_age",
          path = figurePath(sim),
          types = mod$plotTypes,
          ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
          y = "AgeBySppWeighted", cols = cols2,
          ylab = "Age",
          plotTitle = paste0("Biomass-weighted species age\n", "averaged across pixels"))

    ## overstory biomass by species OR oldest cohort age
    if (P(sim)$plotOverstory) {
      Plots(df, fn = speciesBiomassPlot,
            filename = "summary_overstory_biomass",
            path = figurePath(sim),
            types = mod$plotTypes,
            ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
            y = "overstoryBiomass",
            cols = cols2, ylab = "Overstory Biomass",
            plotTitle = "Overstory biomass by species")
    } else {
      Plots(df, fn = speciesAgeANPPPlot,
            filename = "summary_oldest_cohorts",
            path = figurePath(sim),
            types = mod$plotTypes,
            ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
            y = "OldestCohortBySpp", cols = cols2,
            ylab = "Age", plotTitle = paste("Oldest cohort age\n", "across pixels"))
    }
    ## aNPP by species
    Plots(df, fn = speciesAgeANPPPlot,
          filename = "summary_total_aNPP_by_species",
          path = figurePath(sim),
          types = mod$plotTypes,
          ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
          y = "aNPPBySpecies", cols = cols2,
          ylab = "aNPP",
          plotTitle = paste0("Total aNPP by species\n", "across pixels"))
  }

  ## export to sim
  sim$summaryBySpecies <- summaryBySpecies
  sim$summaryBySpecies1 <- summaryBySpecies1

  return(invisible(sim))
})

#' @param x a single layer `SpatRaster`.
#' @param title character string to use as plot title
gg_vegAttrMap <- function(x, title) {
  if (terra::is.factor(x)) {
    gg <- ggplot() +
      tidyterra::geom_spatraster(data = x) +
      tidyterra::scale_fill_coltab(data = x, na.value = "transparent")
  } else {
    ## need to convert integer rasters to float to get colours (won't use coltab b/c continuous)
    gg <- ggplot() +
      tidyterra::geom_spatraster(data = x * 10 / 10) +
      viridis::scale_fill_viridis(na.value = "transparent")
  }

  gg <- gg + ggtitle(title) + theme_bw()

  return(gg)
}

plotVegAttributesMaps <- compiler::cmpfun(function(sim) {
  LandR::assertSpeciesPlotLabels(sim$species$species, mod$sppEquiv)
  assertSppVectors(sppEquiv = mod$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                   sppColorVect = sim$sppColorVect)

  biomassMapForPlot <- mask(sim$simulatedBiomassMap, sim$studyAreaReporting)
  ANPPMapForPlot <- mask(sim$ANPPMap, sim$studyAreaReporting)
  mortalityMapForPlot <- mask(sim$mortalityMap, sim$studyAreaReporting)

  if (is.null(sim$reproductionMap)) {
    reproductionMapForPlot <- biomassMapForPlot
    reproductionMapForPlot[!is.na(as.vector(reproductionMapForPlot[]))][] <- 0
  } else {
    reproductionMapForPlot <- mask(sim$reproductionMap, sim$studyAreaReporting)
  }

  levs <- terra::cats(sim$vegTypeMap)[[1]]
  levelsID <- grep("^id$", ignore.case = TRUE, colnames(levs), value = TRUE)
  levelsName <- names(levs)[2]

  ## Doesn't change anything in the current default setting, but it does create
  ##  an NA where there is "Mixed".
  ## Other species in levs[[levelsName]] are already "Leading",
  ##  but it needs to be here in case it is not Leading in the future.
  # The ones we want
  sppEquiv <- mod$sppEquiv[!is.na(mod$sppEquiv[[P(sim)$sppEquivCol]]),]

  levsLeading <- equivalentName(levs[[levelsName]], sppEquiv, "Leading")

  if (any(grepl("Mixed", levs[[levelsName]]))) {
    hasOnlyMixedAsOther <- sum(is.na(levsLeading) == 1) &&
      levs[[levelsName]][is.na(levsLeading)] == "Mixed"
    #extraValues <- setdiff(levs[[levelsName]], levsLeading)
    if (!isTRUE(hasOnlyMixedAsOther)) {
      stop("'plotVegAttributesMaps' in Biomass_core can only deal with 'Mixed' category or the ones in sim$sppEquiv")
    }
  }

  whMixedLevs <- which(levs[[levelsName]] == "Mixed")
  whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")

  # Will return NA where there is no value, e.g., Mixed
  levsLeading[whMixedLevs] <- "Mixed"

  shortNames <- equivalentName(levsLeading, sppEquiv, "EN_generic_short")
  shortNames[whMixedLevs] <- "Mixed"
  levs[[levelsName]] <- shortNames
  levels(sim$vegTypeMap) <- levs

  colsLeading <- equivalentName(names(sim$sppColorVect), sppEquiv, "Leading")
  colsLeading[whMixedSppColors] <- "Mixed"
  sppColorVect <- sim$sppColorVect
  names(sppColorVect) <- colsLeading
  colours <- sppColorVect[na.omit(match(levsLeading, colsLeading))]
  setColors(sim$vegTypeMap, levs[[levelsID]]) <- colours

  # Mask out NAs based on rasterToMatch (for plotting only!)
  vegTypeMapForPlot <- mask(sim$vegTypeMap, sim$studyAreaReporting)

  ## Plot
  mapsToPlot <- vegTypeMapForPlot
  names(mapsToPlot) <- c("Leading vegetation")

  if (!is.null(reproductionMapForPlot)) {
    mapsToPlot <- c(reproductionMapForPlot, mapsToPlot)
    names(mapsToPlot)[1] <- "Reproduction"
  }

  if (!is.null(mortalityMapForPlot)) {
    mapsToPlot <- c(mortalityMapForPlot, mapsToPlot)
    names(mapsToPlot)[1] <- "Mortality"
  }

  if (!is.null(ANPPMapForPlot)) {
    mapsToPlot <- c(ANPPMapForPlot, mapsToPlot)
    names(mapsToPlot)[1] <- "ANPP"
  }

  if (!is.null(biomassMapForPlot)) {
    mapsToPlot <- c(biomassMapForPlot, mapsToPlot)
    names(mapsToPlot)[1] <- "Biomass"
  }

  if (any(P(sim)$.plots == "screen")) {
    dev(mod$mapWindow)
    ## for some reason not clearing the plot at the start was causing issues.
    ## even when graphics were turned off, there seemed to be some inheritance of previous plots
    clearPlot()
  }

  lapply(names(mapsToPlot), function(lyr) {
    Plots(terra::subset(mapsToPlot, lyr),
          fn = gg_vegAttrMap,
          types = P(sim)$.plots,
          filename = paste0("vegAttr_", lyr, "_year_", round(time(sim))),
          title = paste(lyr, "year", round(time(sim))))
  })

  return(invisible(sim))
})

plotAvgVegAttributes <- compiler::cmpfun(function(sim) {
  LandR::assertSpeciesPlotLabels(sim$species$species, mod$sppEquiv)

  ## AVERAGE STAND BIOMASS/AGE/ANPP
  ## calculate across pixels
  ## don't expand table, multiply by no. pixels - faster
  pixelCohortData <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)
  thisPeriod <- pixelCohortData[, list(year = time(sim),
                                       sumB = sum(B*noPixels, na.rm = TRUE),
                                       maxAge = asInteger(max(age, na.rm = TRUE)),
                                       sumANPP = asInteger(sum(aNPPAct*noPixels, na.rm = TRUE)))]
  denominator <- length(sim$pixelGroupMap[!is.na(sim$pixelGroupMap)]) * 100 # to get tonnes/ha below
  thisPeriod[, sumB := asInteger(sumB/denominator)]
  thisPeriod[, sumANPP := asInteger(sumANPP/denominator)]

  if (is.null(sim$summaryLandscape)) {
    summaryLandscape <- thisPeriod
  } else {
    summaryLandscape <- rbindlist(list(sim$summaryLandscape, thisPeriod))
  }

  if (length(unique(summaryLandscape$year)) > 1) {
    df2 <- melt(summaryLandscape, id.vars = "year")

    varLabels <- c(sumB = "Biomass", maxAge = "Age", sumANPP = "aNPP")

    if (any(P(sim)$.plots == "screen")) {
      dev(mod$statsWindow)
    }
    Plots(df2, fn = landscapeAttributesPlot,
          types = mod$plotTypes,
          filename = "landscape_biomass_aNPP_max_age",
          path = figurePath(sim),
          ggsaveArgs = list(width = 10, height = 5, units = "in", dpi = 300),
          varLabels = varLabels)
  }

  ## export to sim
  sim$summaryLandscape <- summaryLandscape

  return(invisible(sim))
})

Save <- compiler::cmpfun(function(sim) {
  crs(sim$simulatedBiomassMap) <- crs(sim$ecoregionMap)
  crs(sim$ANPPMap) <- crs(sim$ecoregionMap)
  crs(sim$mortalityMap) <- crs(sim$ecoregionMap)
  crs(sim$reproductionMap) <- crs(sim$ecoregionMap)
  writeRaster(sim$simulatedBiomassMap,
              file.path(outputPath(sim),
                        paste0("simulatedBiomassMap_Year", round(time(sim)), ".tif")),
              datatype = "INT4S", overwrite = TRUE)
  writeRaster(sim$ANPPMap,
              file.path(outputPath(sim),
                        paste0("ANPP_Year", round(time(sim)), ".tif")),
              datatype = "INT4S", overwrite = TRUE)
  writeRaster(sim$mortalityMap,
              file.path(outputPath(sim),
                        paste0("mortalityMap_Year", round(time(sim)), ".tif")),
              datatype = "INT4S", overwrite = TRUE)
  writeRaster(sim$reproductionMap,
              file.path(outputPath(sim),
                        paste0("reproductionMap_Year", round(time(sim)), ".tif")),
              datatype = "INT4S", overwrite = TRUE)

  return(invisible(sim))
})

CohortAgeReclassification <- function(sim) {
  if (time(sim) != start(sim)) {
    sim$cohortData <- ageReclassification(cohortData = sim$cohortData,
                                          successionTimestep = P(sim)$successionTimestep,
                                          stage = "mainSimulation",
                                          byGroups = P(sim)$cohortDefinitionCols)
  }
  return(invisible(sim))
}


## .inputObjects ------------------------------------------------------------------------------
.inputObjects <- compiler::cmpfun(function(sim) {
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("studyArea", sim)) {
    stop("Please provide a 'studyArea' polygon") ## Jan 2021 we agreed user must provide SA/SAL
  }

  if (is.na(P(sim)$.studyAreaName)) {
    params(sim)[[currentModule(sim)]][[".studyAreaName"]] <- reproducible::studyAreaName(sim$studyArea)
    message("The .studyAreaName is not supplied; derived name from sim$studyArea: ",
            params(sim)[[currentModule(sim)]][[".studyAreaName"]])
  }

  needRTM <- FALSE
  if (is.null(sim$rasterToMatch)) {
    if (!suppliedElsewhere("rasterToMatch", sim)) {
      needRTM <- TRUE
      message("There is no rasterToMatch supplied; will attempt to use rawBiomassMap")
    } else {
      stop("rasterToMatch is going to be supplied, but ", currentModule(sim), " requires it ",
           "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
           " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)",
           " or in a module that gets loaded prior to ", currentModule(sim))
    }
  }

  if (needRTM) {
    if (is.null(sim$rawBiomassMap)) {
      rawBiomassMapURL <- paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                 "canada-forests-attributes_attributs-forests-canada/",
                                 "2001-attributes_attributs-2001/",
                                 "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif")

      httr::with_config(config = httr::config(ssl_verifypeer = P(sim)$.sslVerify), {
        rawBiomassMap <- prepRawBiomassMap(url = rawBiomassMapURL,
                                           studyAreaName = P(sim)$.studyAreaName,
                                           cacheTags = cacheTags,
                                           to = sim$studyArea,
                                           projectTo = NA,  ## don't project to SA
                                           destinationPath = dPath)
      })
    } else {
      rawBiomassMap <- sim$rawBiomassMap
      if (!.compareCRS(sim$rawBiomassMap, sim$studyArea)) {
        ## note that extents may never align if the resolution and projection do not allow for it
        rawBiomassMap <- Cache(postProcess,
                               rawBiomassMap,
                               method = "bilinear",
                               to = sim$studyAreaLarge,
                               projectTo = NA,  ## don't project to SA
                               overwrite = TRUE)
      }
    }

    RTMs <- prepRasterToMatch(studyArea = sim$studyArea,
                              studyAreaLarge = sim$studyArea,
                              rasterToMatch = NULL,
                              rasterToMatchLarge = NULL,
                              destinationPath = dPath,
                              templateRas = rawBiomassMap,
                              studyAreaName = P(sim)$.studyAreaName,
                              cacheTags = cacheTags)
    sim$rasterToMatch <- RTMs$rasterToMatch
    rm(RTMs)
  }

  if (!.compareCRS(sim$studyArea, sim$rasterToMatch)) {
    warning(paste0("studyArea and rasterToMatch projections differ.\n",
                   "studyArea will be projected to match rasterToMatch"))
    sim$studyArea <- projectInputs(sim$studyArea, crs(sim$rasterToMatch))
    sim$studyArea <- fixErrors(sim$studyArea)
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    if (getOption("LandR.verbose", TRUE) > 0) {
      message("'studyAreaReporting' was not provided by user. Using the same as 'studyArea'.")
    }
    sim$studyAreaReporting <- sim$studyArea
  }

  if (!.compareCRS(sim$studyArea, sim$sim$studyAreaReporting)) {
    warning(paste("studyArea and studyAreaReporting projections differ.\n",
                  "studyAreaReporting will be projected to match studyArea."))
    sim$studyAreaReporting <- projectInputs(sim$studyAreaReporting, crs(sim$studyArea))
    sim$studyAreaReporting <- fixErrors(sim$studyAreaReporting)
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    ## load the biomass_succession.txt to get shade tolerance parameters
    mainInput <- prepInputsMainInput(url = extractURL("sufficientLight"),
                                     dPath,
                                     cacheTags = c(cacheTags, "mainInput")) ## uses default URL

    sufficientLight <- data.frame(mainInput, stringsAsFactors = FALSE)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight, stringsAsFactors = FALSE)
  }

  ## check parameter consistency across modules
  paramCheckOtherMods(sim, "cohortDefinitionCols", ifSetButDifferent = "error")
  paramCheckOtherMods(sim, "initialB", ifSetButDifferent = "error")
  paramCheckOtherMods(sim, "sppEquivCol", ifSetButDifferent = "error")
  paramCheckOtherMods(sim, "vegLeadingProportion", ifSetButDifferent = "error")

  ## make sppEquiv table and associated columns, vectors
  ## do not use suppliedElsewhere here as we need the tables to exist (or not)
  ## already (rather than potentially being supplied by a downstream module)
  ## the function checks whether the tables exist internally.
  sppOuts <- sppHarmonize(sim$sppEquiv, sim$sppNameVector, P(sim)$sppEquivCol,
                          sim$sppColorVect, P(sim)$vegLeadingProportion, sim$studyArea)

  ## the following may, or may not change inputs
  sim$sppEquiv <- sppOuts$sppEquiv
  sim$sppNameVector <- sppOuts$sppNameVector
  P(sim, module = currentModule(sim))$sppEquivCol <- sppOuts$sppEquivCol
  sim$sppColorVect <- sppOuts$sppColorVect

  ## make empty treedFirePixelTableSinceLastDisp
  if (!suppliedElsewhere("treedFirePixelTableSinceLastDisp", sim)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(0),
                                                       pixelGroup = integer(0),
                                                       burnTime = numeric(0))
  }

  ## get default species layers
  if (!suppliedElsewhere("speciesLayers", sim)) {
    message("No SpatRaster map of biomass X species is provided; using KNN to",
            "create sim$speciesLayers")
    url <- paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                  "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/")
    sim$speciesLayers <- Cache(loadkNNSpeciesLayers,
                               dPath = dPath,
                               rasterToMatch = sim$rasterToMatch,
                               studyArea = sim$studyArea,
                               sppEquiv = sim$sppEquiv,
                               knnNamesCol = "KNN",
                               sppEquivCol = P(sim)$sppEquivCol,
                               thresh = 10,
                               url = url,
                               userTags = c(cacheTags, "speciesLayers"),
                               omitArgs = c("userTags"))
  }

  ## additional species traits
  if (!suppliedElsewhere("species", sim)) {
    speciesTable <- getSpeciesTable(dPath = dPath, url = extractURL("species"),
                                    cacheTags = c(cacheTags, "speciesTable"))
    tempSppEquiv <- if (!is.null(sim$speciesLayers)) {   ## may be supplied *after* and not exist yet
      sim$sppEquiv[get(P(sim)$sppEquivCol) %in% names(sim$speciesLayers)]
    } else {
      copy(sim$sppEquiv)
    }
    sim$species <- prepSpeciesTable(speciesTable = speciesTable,
                                    # speciesLayers = sim$speciesLayers,
                                    sppEquiv = tempSppEquiv,
                                    sppEquivCol = P(sim)$sppEquivCol)
    rm(tempSppEquiv)
  }

  ## if not using LandR growth/mortality drivers... (assumes LandR.CS)
  if (P(sim)$growthAndMortalityDrivers != "LandR") {
    if (!suppliedElsewhere("cceArgs", sim)) {
      sim$cceArgs <- list(quote(CMI),
                          quote(ATA),
                          quote(CMInormal),
                          quote(mcsModel),
                          quote(gcsModel))
      names(sim$cceArgs) <- paste(sim$cceArgs)
    }

    ## check for climate args
    # if (!all(unlist(lapply(names(sim$cceArgs), suppliedElsewhere, sim = sim)))) {
    #   stop("Some or all of sim$cceArgs are not supplied")
    # }
  }

  gc() ## AMC added this 2019-08-20

  return(invisible(sim))
})

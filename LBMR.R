# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LBMR",
  description = "A fast and large landscape biomass succession model modified from LANDIS II",
  keywords = c("forest succession", "LANDIS II", "Biomass"),
  authors = c(
    person("Yong", "Luo", email = "yluo1@lakeheadu.ca", role = "aut"),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "ctb"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@friresearch.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009", LBMR = numeric_version("1.3.1.9000")),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LBMR.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "fpCompare", "ggplot2", "grid",
                  "purrr", "quickPlot", "raster", "Rcpp", "scales", "sp", "tidyr",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/reproducible@development",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development"),
  parameters = rbind(
    defineParameter("calcSummaryBGM", "character", "end", NA, NA,
                    desc = paste("A character vector describing when to calculate the summary of biomass, growth and mortality",
                                 "Currently any combination of 5 options is possible:",
                                 "'start'- as before vegetation succession events, i.e. before dispersal,",
                                 "'postDisp' - after dispersal, 'postRegen' - after post-disturbance regeneration (currently the same as 'start'),",
                                 "'postGM' - after growth and mortality, 'postAging' - after aging,",
                                 "'end' - at the end of vegetation succesion events, before plotting and saving.",
                                 "The 'end' option is always active, being also the default option.")),
    defineParameter("calibrate", "logical", FALSE,
                    desc = "Do calibration? Defaults to FALSE"),
    defineParameter("growthAndMortalityDrivers", "character", "LandR", NA, NA,
                    desc = "package name where the following functions can be found: calculateClimateEffect,
                    assignClimateEffect"),
    defineParameter("growthInitialTime", "numeric", start(sim), NA_real_, NA_real_,
                    desc = "Initial time for the growth event to occur"),
    defineParameter("initialBiomassSource", "character", "cohortData", NA, NA,
                    paste("Currently, there are three options: 'spinUp', 'cohortData', 'biomassMap'. ",
                          "If 'spinUp', it will derive biomass by running spinup derived from Landis-II.",
                          "If 'cohortData', it will be taken from the 'cohortData' object, i.e., it is already correct, by cohort.",
                          "If 'biomassMap', it will be taken from sim$biomassMap, divided across species using sim$speciesLayers percent cover values",
                          "`spinUp`` uses sim$ageMap as the driver, so biomass",
                          "is an output. That means it will be unlikely to match any input information",
                          "about biomass, unless this is set to TRUE, and a sim$biomassMap is supplied")),
    defineParameter("seedingAlgorithm", "character", "wardDispersal", NA_character_, NA_character_,
                    desc = paste("choose which seeding algorithm will be used among",
                                 "noDispersal, universalDispersal, and wardDispersal (default).")),
    defineParameter("spinupMortalityfraction", "numeric", 0.001,
                    desc = "defines the mortality loss fraction in spin up-stage simulation"),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter("successionTimestep", "numeric", 10, NA, NA,
                    paste("defines the simulation time step, default is 10 years.",
                          "Note that growth and mortality always happen on a yearly basis.")),
    defineParameter("vegLeadingProportion", "numeric", 0.8, 0, 1,
                    desc = "a number that define whether a species is leading for a given pixel"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA,
                    desc = paste("Vector of length = 1, describing the simulation time at which the first plot event should occur.",
                                 "Set to NA to turn plotting off.")),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    desc = paste("defines the plotting time step.",
                                 "If NA, the default, .plotInterval is set to successionTimestep.")),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    desc = "Controls whether maps should be plotted or not"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = paste("Vector of length = 1, describing the simulation time at which the first save event should occur.",
                                 "Set to NA if no saving is desired. If not NA, then saving will occur at",
                                 ".saveInitialTime with a frequency equal to .saveInterval")),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    desc = paste("defines the saving time step.",
                                 "If NA, the default, .saveInterval is set to successionTimestep.")),
    defineParameter(".useCache", "logical", "init",
                    desc = "use caching for the spinup simulation?"),
    defineParameter(".useParallel", "ANY", 2,
                    desc = paste("Used only in seed dispersal.",
                                 "If numeric, it will be passed to data.table::setDTthreads and should be <= 2;",
                                 "If TRUE, it will be passed to parallel:makeCluster;",
                                 "and if a cluster object, it will be passed to parallel::parClusterApplyB."))
  ),
  inputObjects = bind_rows(
    expectsInput("cohortData", "data.table",
                 desc = "Columns: B, pixelGroup, speciesCode, Indicating several features about ages and current vegetation of stand"),
    expectsInput("ecoregion", "data.table",
                 desc = "ecoregion look up table",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt"),
    expectsInput("ecoregionMap", "RasterLayer",
                 desc = paste("ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table.",
                              "Defaults to a dummy map matching rasterToMatch with two regions")),
    # expectsInput("initialCommunities", "data.table",
    #              desc = "initial community table",
    #              sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.txt"),
    # expectsInput("initialCommunitiesMap", "RasterLayer",
    #              desc = "initial community map that has mapcodes match initial community table",
    #              sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.gis"),
    expectsInput("minRelativeB", "data.frame",
                 desc = "table defining the cut points to classify stand shadeness"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "initial community map that has mapcodes match initial community table"),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = paste("Raster layer of buffered study area used for cropping, masking and projecting.",
                              "Defaults to the kNN biomass map masked with `studyArea`"),
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput("rasterToMatchReporting", "RasterLayer",
                 desc = paste("Raster layer of study area used for plotting and reporting only.",
                              "Defaults to the kNN biomass map masked with `studyArea`"),
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput("species", "data.table",
                 desc = paste("a table that has species traits such as longevity, shade tolerance, etc.",
                              "Default is partially based on Dominic Cir and Yan's project"),
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = paste("table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time.",
                              "Defaults to a dummy table based on dummy data os biomass, age, ecoregion and land cover class")),
    expectsInput("sppColorVect", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in sim$speciesEquivalency[[sim$sppEquivCol]],",
                              "and should also contain a color for 'Mixed'"),
                 sourceURL = NA),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.",
                 sourceURL = ""),
    expectsInput("studyArea", "SpatialPolygonsDataFrame",
                 desc = paste("multipolygon to use as the study area,",
                              "with attribute LTHFC describing the fire return interval.",
                              "Defaults to a square shapefile in Southwestern Alberta, Canada."),
                 sourceURL = ""),
    expectsInput("studyAreaReporting", "SpatialPolygonsDataFrame",
                 desc = paste("multipolygon (typically smaller/unbuffered than studyArea) to use for plotting/reporting.",
                              "Defaults to an area in Southwestern Alberta, Canada."),
                 sourceURL = NA),
    expectsInput("sufficientLight", "data.frame",
                 desc = paste("table defining how the species with different shade tolerance respond to stand shadeness.",
                              "Default is based on LANDIS-II Biomass Succession v6.2 parameters"),
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred",
                 sourceURL = "")
    # expectsInput("spinUpCache", "logical", ""),
    # expectsInput("speciesEstablishmentProbMap", "RasterBrick", "Species establishment probability as a RasterBrick, one layer for each species")
  ),
  outputObjects = bind_rows(
    createsOutput("activePixelIndex", "logical",
                  desc = "internal use. Keeps track of which pixels are active"),
    createsOutput("ANPPMap", "RasterLayer",
                  desc = "ANPP map at each succession time step"),
    createsOutput("cohortData", "data.table",
                  desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at succession time step"),
    createsOutput(objectName = "simulatedBiomassMap", objectClass = "RasterLayer",
                  desc = "Biomass map at each succession time step"),
    createsOutput("inactivePixelIndex", "logical",
                  desc = "internal use. Keeps track of which pixels are inactive"),
    createsOutput("initialCommunities", "character",
                  desc = "Because the initialCommunities object can be LARGE, it is saved to disk with this filename"),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("lastReg", "numeric",
                  desc = "an internal counter keeping track of when the last regeneration event occurred"),
    createsOutput("minRelativeB", "data.frame",
                  desc = "define the cut points to classify stand shadeness"),
    createsOutput("mortalityMap", "RasterLayer",
                  desc = "Mortality map at each succession time step"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("regenerationOutput", "data.table", desc = ""),
    createsOutput("reproductionMap", "RasterLayer",
                  desc = "Regeneration map at each succession time step"),
    createsOutput("simulationOutput", "data.table",
                  desc = "contains simulation results by ecoregion (main output)"),
    createsOutput("simulationTreeOutput", "data.table",
                  desc = "Summary of several characteristics about the stands, derived from cohortData"),
    createsOutput("species", "data.table",
                  desc = paste("a table that has species traits such as longevity, shade tolerance, etc.",
                               "Currently obtained from LANDIS-II Biomass Succession v.6.0-2.0 inputs")),
    createsOutput("speciesEcoregion", "data.table",
                  desc = "define the maxANPP, maxB and SEP change with both ecoregion and simulation time"),
    # createsOutput("spinUpCache", "logical", desc = ""),
    createsOutput("spinupOutput", "data.table", desc = "Spin-up output"),
    createsOutput("summaryBySpecies", "data.table",
                  desc = "The total species biomass, average age and aNPP across the landscape (used for plotting and reporting)."),
    createsOutput("summaryBySpecies1", "data.table",
                  desc = "No. pixels of each leading vegetation type (used for plotting and reporting)."),
    createsOutput("summaryLandscape", "data.table",
                  desc = "The averages of total biomass, age and aNPP across the landscape (used for plotting and reporting).")
  )
))

doEvent.LBMR <- function(sim, eventTime, eventType, debug = FALSE) {
  if (is.numeric(P(sim)$.useParallel)) {
    a <- data.table::setDTthreads(P(sim)$.useParallel)
    if (getOption("LandR.verbose", TRUE) > 0) {
      message("LBMR should be using >100% CPU")
      if (data.table::getDTthreads() == 1L) crayon::red(message("Only using 1 thread."))
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
  if (!any(P(sim)$calcSummaryBGM == "end"))
    params(sim)$LBMR$calcSummaryBGM <- c(P(sim)$calcSummaryBGM, "end")
  summBGMPriority <- summBGMPriority[P(sim)$calcSummaryBGM] ## filter necessary priorities

  plotPriority <- 9
  savePriority <- 10

  switch(eventType,
         init = {
           ## do stuff for this event

           ## Define .plotInterval/.saveInterval if need be
           if (is.na(P(sim)$.plotInterval))
             params(sim)$LBMR$.plotInterval <- P(sim)$successionTimestep

           if (is.na(P(sim)$.saveInterval))
             params(sim)$LBMR$.saveInterval <- P(sim)$successionTimestep

           ## make sure plotting window is big enough
           if (!is.na(P(sim)$.plotInitialTime) &&
               tryCatch(dev.size()[2] < 14, error = function(e) FALSE)) {
             dev.off()
             dev(height = 10, width = 14)
             clearPlot()

             ## current window will be used for maps
             ## a new one for summary stats
             if (P(sim)$.plotMaps) {
               mod$mapWindow <- dev.cur()
               mod$statsWindow <- mod$mapWindow + 1
             } else
               mod$statsWindow <- dev.cur()
           }

           ## Run Init event
           sim <- Init(sim)

           ## schedule events
           if (!is.null(summBGMPriority$start))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "LBMR", "summaryBGMstart", eventPriority = summBGMPriority$start)
           sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                "LBMR", "Dispersal", eventPriority = dispEvtPriority)
           sim <- scheduleEvent(sim, P(sim)$growthInitialTime,
                                "LBMR", "mortalityAndGrowth", GMEvtPriority)
           if (!is.null(summBGMPriority$postDisp))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "LBMR", "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
           if (!is.null(summBGMPriority$postRegen))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "LBMR", "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
           if (!is.null(summBGMPriority$postGM))
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                  "LBMR", "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, "LBMR",
                                  "cohortAgeReclassification", eventPriority = agingEvtPriotity)
             if (!is.null(summBGMPriority$postAging))
               sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                    "LBMR", "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
           }

           ## note that summaryBGM and summaryBySpecies, will occur during init too
           sim <- scheduleEvent(sim, start(sim),
                                "LBMR", "summaryBGM", eventPriority = summBGMPriority$end)
           sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryRegen", eventPriority = summRegenPriority)
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                "LBMR", "plotSummaryBySpecies", eventPriority = plotPriority)   ## only occurs before summaryRegen in init.
           if (P(sim)$.plotMaps)
             sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                  "LBMR", "plotMaps", eventPriority = plotPriority + 0.25)
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                "LBMR", "plotAvgs", eventPriority = plotPriority + 0.5)

           if (!is.na(P(sim)$.saveInitialTime)) {
             if (P(sim)$.saveInitialTime < start(sim) + P(sim)$successionTimestep) {
               message(crayon::blue(
                 paste(".saveInitialTime should be >=",  start(sim) + P(sim)$successionTimestep,
                       ". First save changed to", start(sim) + P(sim)$successionTimestep)))
               params(sim)$LBMR$.saveInitialTime <- start(sim) + P(sim)$successionTimestep
             }
             sim <- scheduleEvent(sim, P(sim)$.saveInitialTime,
                                  "LBMR", "save", eventPriority = savePriority)
           }
         },
         summaryBGMstart = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGMstart", eventPriority = summBGMPriority$start)
         },
         Dispersal = {
           sim <- Dispersal(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "Dispersal", eventPriority = dispEvtPriority)
         },
         mortalityAndGrowth = {
           sim <- MortalityAndGrowth(sim)
           sim <- scheduleEvent(sim, time(sim) + 1,
                                "LBMR", "mortalityAndGrowth", eventPriority = GMEvtPriority)
         },
         summaryBGMpostDisp = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
         },
         summaryBGMpostRegen = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
         },
         summaryBGMpostGM = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
         },
         cohortAgeReclassification = {
           sim <- CohortAgeReclassification(sim)

           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                  "LBMR", "cohortAgeReclassification",
                                  eventPriority = agingEvtPriotity)
           }
         },
         summaryBGMpostAging = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
         },
         summaryRegen = {
           sim <- summaryRegen(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryRegen", eventPriority = summRegenPriority)
         },
         summaryBGM = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGM", eventPriority = summBGMPriority$end)
         },
         plotSummaryBySpecies = {
           sim <- plotSummaryBySpecies(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                "LBMR", "plotSummaryBySpecies", eventPriority = plotPriority)
         },
         plotMaps = {
           sim <- plotVegAttributesMaps(sim)
           if (P(sim)$.plotMaps)
             sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                  "LBMR", "plotMaps", eventPriority = plotPriority + 0.25)
         },
         save = {
           sim <- Save(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval,
                                "LBMR", "save", eventPriority = savePriority)
         },
         plotAvgs = {
           sim <- plotAvgVegAttributes(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                                "LBMR", "plotAvgs", eventPriority = plotPriority + 0.5)
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### EVENT FUNCTIONS
Init <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
  ## A numeric scalar indicating how large each chunk of an internal data.table with processing by chunks
  mod$cutpoint <- 1e10

  ##############################################
  ## Prepare individual objects
  ##############################################

  ##############################################
  ## species
  ##############################################
  species <- setDT(sim$species)[, speciesCode := as.factor(species)]
  LandR::assertColumns(species,
                       c(species = "character", Area = "factor", longevity = "integer",
                         sexualmature = "integer", shadetolerance = "integer",
                         firetolerance = "integer", seeddistance_eff = "integer",
                         seeddistance_max = "integer", resproutprob = "numeric",
                         resproutage_min = "integer", resproutage_max = "integer",
                         postfireregen = "factor", leaflongevity = "integer",
                         wooddecayrate = "numeric", mortalityshape = "integer",
                         growthcurve = "numeric", leafLignin = "numeric",
                         hardsoft = "factor", speciesCode = "factor"))
  sim$species <- setkey(species, speciesCode)

  if (!suppliedElsewhere("cohortData", sim) |
      !suppliedElsewhere("pixelGroupMap")) {

    if ((!suppliedElsewhere("cohortData", sim) &&
         suppliedElsewhere("pixelGroupMap")) ||
        (suppliedElsewhere("cohortData", sim) &&
         !suppliedElsewhere("pixelGroupMap"))) {
      stop("Either 'cohortData' or 'pixelGroupMap' are being supplied without the other.",
           "These two objects must be supplied together and conform to each other.",
           "Either supply both of them manually, or use a module like Biomass_BorealDataPrep to do so.")
    }

    if (suppliedElsewhere("ecoregionMap", sim))
      message(blue("'ecoregionMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'ecoregionMap'"))
    ecoregionMap <- randomPolygons(ras = sim$rasterToMatch,
                                   res = res(sim$rasterToMatch),
                                   numTypes = 2)
    ecoregionMap <- mask(ecoregionMap, sim$rasterToMatch)

    if (suppliedElsewhere("biomassMap", sim))
      message(blue("'biomassMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'biomassMap'"))
    biomassMap <- gaussMap(sim$rasterToMatch)
    biomassMap <- setValues(biomassMap, rescale(getValues(biomassMap), c(100, 20000)))
    biomassMap <- mask(biomassMap, sim$rasterToMatch)

    if (suppliedElsewhere("standAgeMap", sim))
      message(blue("'standAgeMap' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'standAgeMap'"))
    standAgeMap <- setValues(biomassMap, asInteger(rescale(getValues(biomassMap), c(1, 300))))

    if (suppliedElsewhere("LCC2005", sim))
      message(blue("'LCC2005' was supplied, but "),
              red("will be replaced by a dummy version to make "),
              blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ",
                   "'cohortData', 'pixelGroupMap' and 'LCC2005'"))
    LCC2005 <- randomPolygons(ras = sim$rasterToMatch,
                              res = res(sim$rasterToMatch),
                              numTypes = 5)
    LCC2005 <- mask(LCC2005, sim$rasterToMatch)

    ecoregionstatus <- data.table(active = "yes",
                                  ecoregion = unique(ecoregionMap[]))
    ecoregionstatus <- ecoregionstatus[complete.cases(ecoregionstatus)]

    ecoregionFiles <- Cache(ecoregionProducer,
                            ecoregionMaps = list(ecoregionMap, LCC2005),
                            ecoregionName = "ECODISTRIC",
                            ecoregionActiveStatus = ecoregionstatus,
                            rasterToMatch = sim$rasterToMatch,
                            userTags = "ecoregionFiles")

    ################################################################
    ## put together pixelTable object
    ################################################################
    #  Round age to pixelGroupAgeClass
    ## check if all species have traits
    tempObjs <- checkSpeciesTraits(sim$speciesLayers, sim$species, sim$sppColorVect)
    sim$speciesLayers <- tempObjs$speciesLayers
    sim$sppColorVect <- tempObjs$sppColorVect
    rm(tempObjs)

    pixelTable <- makePixelTable(speciesLayers = sim$speciesLayers, species = sim$species,
                                 standAgeMap = standAgeMap, ecoregionFiles = ecoregionFiles,
                                 biomassMap = biomassMap, rasterToMatch = sim$rasterToMatch,
                                 LCC2005 = LCC2005, pixelGroupAgeClass = 10)

    #######################################################
    # Make the initial pixelCohortData table
    #######################################################
    ## note that pixelGroupBiomassClass here is forced to 100, to match dummy biomass units
    message(blue("Creating a", red("DUMMY"), blue("cohorData table.")))
    coverColNames <- paste0("cover.", sim$species$species)
    pixelCohortData <- Cache(makeAndCleanInitialCohortData, pixelTable,
                             sppColumns = coverColNames,
                             pixelGroupBiomassClass = 100,
                             doSubset = FALSE,
                             userTags = "stable")
    setnames(pixelCohortData, "initialEcoregionCode", "ecoregionGroup")

    ## When using dummy values ecoregion codes are not changed
    rmZeroBiomassQuote <- quote(B > 0)
    cohortDataNoBiomass <- pixelCohortData[eval(rmZeroBiomassQuote),
                                           .(B, logAge, speciesCode, ecoregionGroup, lcc, cover)]

    ##############################################################
    # Statistical estimation of establishprob, maxB and maxANPP
    ##############################################################
    ## only use pixels where cover > 0
    cohortDataShort <- pixelCohortData[, list(coverNum = .N,
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
                        uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(cohortDataShort$ecoregionGroup)),
                        .specialData = cohortDataShort,
                        omitArgs = c(".specialData"))

    message(blue("  The rsquared is: "))
    print(modelCover$rsq)

    ## BIOMASS
    # For Cache -- doesn't need to cache all columns in the data.table -- only the ones in the model
    message(blue("Estimating maxB from "), red("DUMMY values of age and ecoregionGroup "),
            blue("using the formula:\n"),
            magenta(paste0(format(biomassModel), collapse = "")))
    modelBiomass <- Cache(statsModel,
                          modelFn = biomassModel,
                          uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(pixelCohortData$ecoregionGroup)),
                          .specialData = cohortDataNoBiomass,
                          omitArgs = c(".specialData"))
    message(blue("  The rsquared is: "))
    print(modelBiomass$rsq)

    ########################################################################
    # create speciesEcoregion -- a single line for each combination of ecoregionGroup & speciesCode
    #   doesn't include combinations with B = 0 because those places can't have the species/ecoregion combo
    ########################################################################
    message(blue("Create speciesEcoregion from "), red("DUMMY values"))
    speciesEcoregion <- makeSpeciesEcoregion(cohortDataNoBiomass = cohortDataNoBiomass,
                                             cohortDataShort = cohortDataShort,
                                             cohortDataShortNoCover = cohortDataShortNoCover,
                                             species = sim$species,
                                             modelCover = modelCover,
                                             modelBiomass = modelBiomass,
                                             successionTimestep = P(sim)$successionTimestep,
                                             currentYear = time(sim))
    if (ncell(sim$rasterToMatch) > 3e6) .gc()

    ########################################################################
    # Create initial communities, i.e., pixelGroups
    ########################################################################

    ## TODO: code below needs to be converted to funtions to avoid repeating code across modules (Boreal_LBMRDataPrep)
    pixelCohortData[, ecoregionGroup := factor(as.character(ecoregionGroup))]
    pixelCohortData[ , `:=`(logAge = NULL, coverOrig = NULL, totalBiomass = NULL, #pixelIndex = NULL,
                            cover = NULL, lcc = NULL)]
    pixelCohortData <- pixelCohortData[B > 0]

    if (!suppliedElsewhere("columnsForPixelGroups", sim)) {
      columnsForPixelGroups <- LandR::columnsForPixelGroups
    } else {
      columnsForPixelGroups <- sim$columnsForPixelGroups
    }

    cd <- pixelCohortData[, .SD, .SDcols = c("pixelIndex", columnsForPixelGroups)]
    pixelCohortData[, pixelGroup := Cache(generatePixelGroups, cd, maxPixelGroup = 0,
                                          columns = columnsForPixelGroups)]

    ########################################################################
    ## build ecoregion, ecoregionMap objects
    ## build biomassMap object -- biomasses have been adjusted
    ########################################################################
    ecoregionsWeHaveParametersFor <- levels(speciesEcoregion$ecoregionGroup)

    pixelCohortData <- pixelCohortData[ecoregionGroup %in% ecoregionsWeHaveParametersFor] # keep only ones we have params for
    pixelCohortData[ , ecoregionGroup := factor(as.character(ecoregionGroup))]
    pixelCohortData[, totalBiomass := sum(B), by = "pixelIndex"]

    sim$ecoregion <- data.table(active = "yes",
                                ecoregionGroup = factor(as.character(unique(pixelCohortData$ecoregionGroup))))

    # Some ecoregions have NO BIOMASS -- so they are not active
    sim$ecoregion[!ecoregionGroup %in% unique(speciesEcoregion$ecoregionGroup), active := "no"]

    pixelData <- unique(pixelCohortData, by = "pixelIndex")
    pixelData[, ecoregionGroup := factor(as.character(ecoregionGroup))] # resorts them in order

    ## re-do biomassMap and ecoregionMap
    sim$biomassMap <- raster(sim$rasterToMatch)
    sim$biomassMap[pixelData$pixelIndex] <- pixelData$totalBiomass

    sim$ecoregionMap <-  raster(ecoregionFiles$ecoregionMap)
    sim$ecoregionMap[pixelData$pixelIndex] <- as.integer(pixelData$ecoregionGroup)
    levels(sim$ecoregionMap) <- data.frame(ID = seq(levels(pixelData$ecoregionGroup)),
                                           ecoregion = gsub("_.*", "", levels(pixelData$ecoregionGroup)),
                                           ecoregionGroup = levels(pixelData$ecoregionGroup),
                                           stringsAsFactors = TRUE)

    sim$minRelativeB <- data.frame(ecoregionGroup = levels(pixelData$ecoregionGroup),
                                   X1 = 0.2, X2 = 0.4, X3 = 0.5,
                                   X4 = 0.7, X5 = 0.9)

    speciesEcoregion[, ecoregionGroup := factor(as.character(ecoregionGroup))]

    sim$speciesEcoregion <- speciesEcoregion

    ##############################################################################
    ##  Collapse pixelCohortData to its cohortData : need pixelGroupMap
    sim$pixelGroupMap <- raster(sim$rasterToMatch)
    sim$pixelGroupMap[pixelData$pixelIndex] <- as.integer(pixelData$pixelGroup)

    sim$cohortData <- unique(pixelCohortData, by = c("pixelGroup", columnsForPixelGroups))
    sim$cohortData[ , `:=`(pixelIndex = NULL)]

    message(blue("Create pixelGroups based on: ", paste(columnsForPixelGroups, collapse = ", "),
                 "\n  Resulted in", magenta(length(unique(sim$cohortData$pixelGroup))),
                 "unique pixelGroup values"))
    LandR::assertERGs(sim$ecoregionMap, cohortData = sim$cohortData,
                      speciesEcoregion = speciesEcoregion,
                      minRelativeB = sim$minRelativeB)

    assertCohortData(sim$cohortData, sim$pixelGroupMap)

    LandR::assertUniqueCohortData(sim$cohortData, c("pixelGroup", "ecoregionGroup", "speciesCode"))
  }

  ## check objects
  LandR::assertERGs(sim$ecoregionMap, sim$cohortData, sim$speciesEcoregion, sim$minRelativeB)

  ##############################################
  # ecoregion
  ##############################################
  setDT(sim$ecoregion)
  LandR::assertColumns(sim$ecoregion, c(active = "character", ecoregionGroup = "factor"))

  ecoregion <- sim$ecoregion#[, ecoregionGroup := as.factor(ecoregion)]
  #ecoregion_temp <- setkey(ecoregion[, .(ecoregion, ecoregionGroup)], ecoregion)

  ##############################################
  # speciesEcoregion - checks
  ##############################################
  LandR::assertColumns(sim$speciesEcoregion,
                       c(ecoregionGroup = "factor", speciesCode = "factor",
                         establishprob = "numeric", maxB = "integer", maxANPP = "numeric"))
  #speciesEcoregion[, ecoregionGroup := as.factor(ecoregion)]
  speciesEcoregion <- sim$speciesEcoregion#[sim$species[, .(species, speciesCode)],
  speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)

  ##############################################
  # minRelativeB
  ##############################################
  setDT(sim$minRelativeB) # make a data.table
  # join to get ecoregionGroup column
  sim$minRelativeB <- sim$minRelativeB[unique(speciesEcoregion[, .(ecoregionGroup)]),
                                       on = "ecoregionGroup", nomatch = 0]

  #############################################
  # Create cohortData from communities
  #############################################
  active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, ecoregionGroup)], k) # not sure what k is doing here

  pixelGroupMap <- sim$pixelGroupMap
  names(pixelGroupMap) <- "pixelGroup"

  # Changed mechanism for active and inactive -- just use NA on ecoregionMap
  ecoregionMapNAs <- is.na(sim$ecoregionMap[])
  sim$activePixelIndex <- which(!ecoregionMapNAs) # store this for future use
  sim$inactivePixelIndex <- which(ecoregionMapNAs) # store this for future use

  # Keeps track of the length of the ecoregion
  mod$activeEcoregionLength <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                                         getValues(sim$ecoregionMap),
                                                                         att = "ecoregionGroup"),
                                          pixelIndex = 1:ncell(sim$ecoregionMap))[
                                            ecoregionGroup %in% active_ecoregion$ecoregionGroup,
                                            .(NofCell = length(pixelIndex)), by = "ecoregionGroup"]

  cohortData <- sim$cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[sim$activePixelIndex]), ]
  cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = speciesEcoregion,
                                                 time = round(time(sim)),
                                                 cohortData = cohortData)
  cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = cohortData)

  #############################################
  initialBiomassSourcePoss <- c('spinUp', 'cohortData', 'biomassMap')
  if (!any(grepl(P(sim)$initialBiomassSource, initialBiomassSourcePoss))) {
    stop("P(sim)$initialBiomassSource must be one of: ", paste(initialBiomassSourcePoss, collapse = ", "))
  }

  ## spinup
  if (grepl("spin", tolower(P(sim)$initialBiomassSource))) { # negate the TRUE to allow for default to be this, even if NULL or NA
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
                         userTags = c("LBMR", "spinUp"))

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
  } else if (grepl("biomassmap", tolower(P(sim)$initialBiomassSource))) {
    stop("'biomassMap as a value for P(sim)$initialBiomassSource is not working currently; ",
         "please use 'cohortData'")
    if (verbose > 0)
      message("Skipping spinup and using the sim$biomassMap * SpeciesLayers pct as initial biomass values")
    biomassTable <- data.table(biomass = getValues(sim$biomassMap),
                               pixelGroup = getValues(pixelGroupMap))
    biomassTable <- na.omit(biomassTable)
    maxBiomass <- maxValue(sim$biomassMap)
    if (maxBiomass < 1e3) {
      if (verbose > 0) {
        message(crayon::green("  Because biomassMap values are all below 1000, assuming that these should be\n",
                              "    converted to tonnes/ha by multiplying by 100"))
      }
      biomassTable[, `:=`(biomass = biomass * 100)]
    }

    # In case there are non-identical biomasses in each pixelGroup -- this should be irrelevant with
    #   improved Boreal_LBMRDataPrep.R (Jan 6, 2019 -- Eliot)
    biomassTable <- biomassTable[, list(Bsum = mean(biomass, na.rm = TRUE)), by = pixelGroup]
    if (!is.integer(biomassTable[["Bsum"]]))
      set(biomassTable, NULL, "Bsum", asInteger(biomassTable[["Bsum"]]))

    # Delete the B from cohortData -- it will be joined from biomassTable
    set(cohortData, NULL, "B", NULL)
    cohortData[, totalSpeciesPresence := sum(speciesPresence), by = "pixelGroup"]
    cohortData <- cohortData[biomassTable, on = "pixelGroup"]
    cohortData[, B := Bsum * speciesPresence / totalSpeciesPresence, by = c("pixelGroup", "speciesCode")]
    if (!is.integer(cohortData[["B"]]))
      set(cohortData, NULL, "B", asInteger(cohortData[["B"]]))
  }

  pixelAll <- cohortData[, .(uniqueSumB = sum(B, na.rm = TRUE)), by = pixelGroup]
  if (!is.integer(pixelAll[["uniqueSumB"]]))
    set(pixelAll, NULL, "uniqueSumB", asInteger(pixelAll[["uniqueSumB"]]))

  if (!any(is.na(P(sim)$.plotInitialTime)) | !any(is.na(P(sim)$.saveInitialTime))) {
    simulatedBiomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
  }

  sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B, mortality = 0L, aNPPAct = 0L)]
  simulationOutput <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                                getValues(sim$ecoregionMap),
                                                                att = "ecoregionGroup"),
                                 pixelGroup = getValues(pixelGroupMap),
                                 pixelIndex = 1:ncell(sim$ecoregionMap))[
                                   , .(NofPixel = length(pixelIndex)),
                                   by = c("ecoregionGroup", "pixelGroup")]

  simulationOutput <- setkey(simulationOutput, pixelGroup)[setkey(pixelAll, pixelGroup), nomatch = 0][
    , .(Biomass = sum(as.numeric(uniqueSumB*NofPixel))), by = ecoregionGroup] ## NOTE:
  ## above needs to be numeric because of integer overflow -- returned to integer in 2 lines
  simulationOutput <- setkey(simulationOutput, ecoregionGroup)[
    setkey(mod$activeEcoregionLength, ecoregionGroup), nomatch = 0]
  sim$simulationOutput <- simulationOutput[, .(ecoregionGroup, NofCell, Year = asInteger(time(sim)),
                                               Biomass = asInteger(Biomass / NofCell),
                                               ANPP = 0L, Mortality = 0L, Regeneration = 0L)]
  sim$lastReg <- 0
  speciesEcoregion[, identifier := year > P(sim)$successionTimestep]
  speciesEcoregion_True <- speciesEcoregion[identifier == TRUE, ]
  speciesEcoregion_False <- speciesEcoregion[identifier == FALSE, ]
  speciesEcoregion_True_addon <- speciesEcoregion_False[year == max(year), ]
  sim$speciesEcoregion <- rbindlist(list(speciesEcoregion_True_addon, speciesEcoregion_True))[
    , ':='(year = year - min(year), identifier = NULL)]
  sim$lastFireYear <- "noFire"

  sim$pixelGroupMap <- pixelGroupMap
  return(invisible(sim))
}

SummaryBGM <- function(sim) {
  pixelGroups <- data.table(pixelGroupIndex = unique(sim$cohortData$pixelGroup),
                            temID = 1:length(unique(sim$cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = mod$cutpoint),
                             max(pixelGroups$temID))))
  if (length(cutpoints) == 1)
    cutpoints <- c(cutpoints, cutpoints + 1)
  pixelGroups[, groups := cut(temID, breaks = cutpoints,
                              labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                              include.lowest = TRUE)]
  ecoPixelgroup <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap,
                                                             getValues(sim$ecoregionMap),
                                                             att = "ecoregionGroup"),
                              pixelGroup = getValues(sim$pixelGroupMap),
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

    # Don't need to do asInteger within the by group calculation. Separate to next step.
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

  # need as.numeric below because of integer overflow -- returned to integer in 2 lines
  tempOutput_All <- tempOutput_All[, .(Biomass = sum(as.numeric(uniqueSumB * NofPixelGroup)),
                                       ANPP = sum(uniqueSumANPP * NofPixelGroup),
                                       Mortality = sum(uniqueSumMortality * NofPixelGroup),
                                       Regeneration = sum(uniqueSumRege * NofPixelGroup)),
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
  # the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively.
  names(sim$pixelGroupMap) <- "pixelGroup"

  sim$simulatedBiomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumB")
  setColors(sim$simulatedBiomassMap) <- c("light green", "dark green")

  sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumANPP")
  setColors(sim$ANPPMap) <- c("light green", "dark green")

  sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumMortality")
  setColors(sim$mortalityMap) <- c("light green", "dark green")

  sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, sim$pixelGroupMap,
                                        P(sim)$vegLeadingProportion,
                                        colors = sim$sppColorVect,
                                        unitTest = getOption("LandR.assertions", TRUE))

  # the following codes for preparing the data table for saving
  rm(cutpoints, pixelGroups, tempOutput_All, summaryBGMtable) ## TODO: is this needed? on exit, should free the mem used for these
  return(invisible(sim))
}

MortalityAndGrowth <- function(sim) {
  if (is.numeric(P(sim)$.useParallel)) {
    data.table::setDTthreads(P(sim)$.useParallel)
    message("Mortality and Growth should be using >100% CPU")
  }
  if (!all(colnames(sim$cohortData) %in% c("pixelGroup", "ecoregionGroup",
                                           "speciesCode", "age", "B", "mortality", "aNPPAct")))
    sim$cohortData <- sim$cohortData[, .(pixelGroup, ecoregionGroup,
                                         speciesCode, age, B, mortality, aNPPAct)]

  #Install climate-sensitive functions (or not)
  a <- try(requireNamespace(P(sim)$growthAndMortalityDrivers)) ## TODO: this is not working. requireNamespace overrides try
  if (class(a) == "try-error") {
    stop("The package you specified for P(sim)$growthAndMortalityDrivers must be installed.")
  }
  calculateClimateEffect <- getFromNamespace("calculateClimateEffect", P(sim)$growthAndMortalityDrivers)
  assignClimateEffect <- getFromNamespace("assignClimateEffect", P(sim)$growthAndMortalityDrivers)

  cohortData <- sim$cohortData
  pgs <- unique(cohortData$pixelGroup)
  groupSize <- 1e7 ## This should be large because this function is not the current RAM limitation
  numGroups <- ceiling(length(pgs) / groupSize)
  groupNames <- paste0("Group", seq(numGroups))
  if (length(pgs) > groupSize) {
    sim$cohortData <- cohortData[0, ]
    pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup),
                              temID = 1:length(unique(cohortData$pixelGroup)))
    cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = groupSize), max(pixelGroups$temID))))
    #cutpoints <- c(1,max(pixelGroups$temID))
    if (length(cutpoints) == 1) cutpoints <- c(cutpoints, cutpoints + 1)
    pixelGroups[, groups := rep(groupNames,
                                each = groupSize, length.out = NROW(pixelGroups))]
  }
  for (subgroup in groupNames) {
    if (numGroups == 1) {
      subCohortData <- cohortData
    } else {
      subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    }

    subCohortData[age > 1, age := age + 1L]
    subCohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                      time = round(time(sim)),
                                                      cohortData = subCohortData)
    subCohortData <- updateSpeciesAttributes(species = sim$species, cohortData = subCohortData)
    subCohortData <- calculateSumB(cohortData = subCohortData,
                                   lastReg = sim$lastReg,
                                   simuTime = time(sim),
                                   successionTimestep = P(sim)$successionTimestep)
    startNumCohorts <- NROW(subCohortData)

    #########################################################
    # Die from old age -- rm from cohortData
    #########################################################
    subCohortPostLongevity <- subCohortData[age <= longevity, ]
    diedCohortData <- subCohortData[age > longevity, ]
    numCohortsDiedOldAge <- NROW(diedCohortData)

    if (numCohortsDiedOldAge > 0) {
      # Identify the PGs that are totally gone, not just an individual cohort that died
      pgsToRm <- diedCohortData[!pixelGroup %in% subCohortPostLongevity$pixelGroup]

      pixelsToRm <- which(getValues(sim$pixelGroupMap) %in% unique(pgsToRm$pixelGroup))
      # RM from the pixelGroupMap -- since it is a whole pixelGroup that is gone, not just a cohort, this is necessary
      if (isTRUE(getOption("LandR.assertions"))) {
        a <- subCohortPostLongevity$pixelGroup %in% na.omit(getValues(sim$pixelGroupMap))
        if (!all(a)) {
          stop("Post longevity-based mortality, there is a divergence between pixelGroupMap and cohortData pixelGroups")
        }
      }
      if (length(pixelsToRm) > 0) {
        if (getOption("LandR.verbose", TRUE) > 0) {
          numPixelGrps <- sum(sim$pixelGroupMap[] != 0, na.rm = TRUE)
        }
        sim$pixelGroupMap[pixelsToRm] <- 0L
        if (getOption("LandR.verbose", TRUE) > 1) {
          message(blue("Death due to old age:",
                       "\n  ", numCohortsDiedOldAge, "cohorts died of old age (i.e., due to passing longevity); ",
                       sum(is.na(diedCohortData$age)), " of those because age == NA; ",
                       "\n  ", NROW(unique(pgsToRm$pixelGroup)), "pixelGroups to be removed (i.e., ",
                       "\n  ", length(pixelsToRm), "pixels; "))
        }
        if (getOption("LandR.verbose", TRUE) > 0) {
          message(blue("\n   Total number of pixelGroups -- Was:", numPixelGrps,
                       ", Now:", magenta(sum(sim$pixelGroupMap[] != 0, na.rm = TRUE))))
        }
      }
    }

    subCohortData <- subCohortPostLongevity

    #########################################################
    # Calculate age and competition effects
    #########################################################
    subCohortData <- calculateAgeMortality(cohortData = subCohortData)

    set(subCohortData, NULL, c("longevity", "mortalityshape"), NULL)
    subCohortData <- calculateCompetition(cohortData = subCohortData)
    if (!P(sim)$calibrate) {
      set(subCohortData, NULL, "sumB", NULL)
    }

    subCohortData <- calculateANPP(cohortData = subCohortData)  ## competion effect on aNPP via bPM
    set(subCohortData, NULL, "growthcurve", NULL)
    set(subCohortData, NULL, "aNPPAct", pmax(1, subCohortData$aNPPAct - subCohortData$mAge))

    ## generate climate-sensitivity predictions
    ## - NULL w/o module biomassGMCS. age-related mortality is included in this model
    ## - 20/03/2019 IE: after discussion we determined it is acceptable to include age
    ##   because 1) the Landis age-related mortality fxn is very different from this model,
    ##   and 2) because it would be difficult to separate the climate/age interaction
    predObj <- calculateClimateEffect(gcsModel = sim$gcsModel,
                                      mcsModel = sim$mcsModel,
                                      CMI = sim$CMI,
                                      ATA = sim$ATA,
                                      cohortData = subCohortData,
                                      pixelGroupMap = sim$pixelGroupMap,
                                      centeringVec = sim$centeringVec)

    #This line will return aNPPAct unchanged unless LandR_BiomassGMCS is also run
    subCohortData$climGrowth <- assignClimateEffect(predObj, subCohortData = subCohortData, type = 'growthPred')
    subCohortData$aNPPAct <- pmax(0, subCohortData$aNPPAct + subCohortData$climGrowth)

    subCohortData <- calculateGrowthMortality(cohortData = subCohortData)
    set(subCohortData, NULL, "mBio", pmax(0, subCohortData$mBio - subCohortData$mAge))
    set(subCohortData, NULL, "mBio", pmin(subCohortData$mBio, subCohortData$aNPPAct))
    set(subCohortData, NULL, "mortality", subCohortData$mBio + subCohortData$mAge)

    #This line will return mortality unchanged unless LandR_BiomassGMCS is also run
    subCohortData$climMort <- assignClimateEffect(predObj, subCohortData = subCohortData, type = "mortPred")
    #Total mortality can't be negative
    subCohortData$mortality <- pmax(0, subCohortData$mortality + subCohortData$climMort)
    #Ian added this check 04/04/2019 - without climate-sensitivity, mortality never exceeds biomass
    subCohortData$mortality <- pmin(subCohortData$mortality, subCohortData$B)

    set(subCohortData, NULL, c("mBio", "mAge", "maxANPP", "maxB", "maxB_eco", "bAP", "bPM", "climGrowth", "climMort"), NULL)
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

    #=#
    if (numGroups == 1) {
      sim$cohortData <- subCohortData
    } else {
      sim$cohortData <- rbindlist(list(sim$cohortData, subCohortData), fill = TRUE)
    }
    rm(subCohortData)
    #.gc()
  }
  rm(cohortData)

  if (isTRUE(getOption("LandR.assertions"))) {
    if (NROW(unique(sim$cohortData[pixelGroup == 67724]$ecoregionGroup)) > 1)
      stop()

    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }
  }
  assertCohortData(sim$cohortData, sim$pixelGroupMap)
  return(invisible(sim))
}

Dispersal <- function(sim) {
  treedFirePixelTableCurYr <- sim$treedFirePixelTableSinceLastDisp[burnTime == time(sim)]
  pixelsFromCurYrBurn <- treedFirePixelTableCurYr$pixelIndex
  tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% pixelsFromCurYrBurn)]
  # tempInactivePixel <- c(sim$inactivePixelIndex, pixelsFromCurYrBurn)

  if (P(sim)$seedingAlgorithm == "noDispersal") {
    sim <- NoDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
  } else if (P(sim)$seedingAlgorithm == "universalDispersal") {
    sim <- UniversalDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
  } else if (P(sim)$seedingAlgorithm == "wardDispersal") {
    sim <- WardDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
  } else stop("Undefined seed dispersal type!")

  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableCurYr
  return(invisible(sim))
}

NoDispersalSeeding <- function(sim, tempActivePixel, pixelsFromCurYrBurn) {
  # if (sim$lastFireYear == round(time(sim))) { # if current year is both fire year and succession year
  #   # find new active pixel that remove successful postfire regeneration
  #   # since this is on site regeneration, all the burnt pixels can not seeding
  #   tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$treedFirePixelTableSinceLastDisp$pixelIndex)]
  # } else {
  #   tempActivePixel <- sim$activePixelIndex
  # }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = time(sim),
                                  successionTimestep = P(sim)$successionTimestep)
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[
    setkey(sim$species[, .(speciesCode, sexualmature)], speciesCode), nomatch = 0]

  seedingData <- sim$cohortData[age >= sexualmature]
  set(sim$cohortData, NULL, "sexualmature", NULL)
  set(seedingData, NULL, c("sexualmature", "age", "B", "mortality", "aNPPAct"), NULL)
  siteShade <- setkey(data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                               sim$speciesEcoregion, sim$minRelativeB)), pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[siteShade, nomatch = 0]
  seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[
    , .(speciesCode, shadetolerance)], speciesCode), nomatch = 0]
  seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, seedingData)
  seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData), 0, 1),]
  set(seedingData, NULL, c("shadetolerance", "lightProb", "siteShade", "sumB"), NULL)
  seedingData <- unique(seedingData, by = c("pixelGroup", "speciesCode"))

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(sim$pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(seedingData$pixelGroup)], pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)

  specieseco_current <- speciesEcoregionLatestYear(
    sim$speciesEcoregion[, .(year, speciesCode, establishprob, ecoregionGroup)],
    round(time(sim))
  )
  specieseco_current <- setkey(specieseco_current, ecoregionGroup, speciesCode)

  #specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
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
    outs <- updateCohortData(seedingData, cohortData = sim$cohortData, sim$pixelGroupMap,
                             time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion,
                             treedFirePixelTableSinceLastDisp = NULL,
                             successionTimestep = P(sim)$successionTimestep)
    sim$cohortData <- outs$cohortData
    sim$pixelGroupMap <- outs$pixelGroupMap
  }

  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

UniversalDispersalSeeding <- function(sim, tempActivePixel) {
  # if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
  #   tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  # } else {
  #   tempActivePixel <- sim$activePixelIndex
  # }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  species <- sim$species
  # all species can provide seed source, i.e. age>=sexualmature
  speciessource <- setkey(sim$species[, .(speciesCode, k = 1)], k)
  siteShade <- data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                        sim$speciesEcoregion, sim$minRelativeB))
  activePixelGroup <- unique(data.table(pixelGroup = getValues(sim$pixelGroupMap)[tempActivePixel],
                                        ecoregionGroup = factorValues2(sim$ecoregionMap, getValues(sim$ecoregionMap),
                                                                       att = "ecoregionGroup")[tempActivePixel]),
                             by = "pixelGroup")
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table()
  siteShade[is.na(siteShade), siteShade := 0]
  setkey(siteShade[, k := 1], k)
  # i believe this is the latest version how the landis guys calculate sufficient light
  # http://landis-extensions.googlecode.com/svn/trunk/succession-library/trunk/src/ReproductionDefaults.cs
  seedingData <- siteShade[speciessource, allow.cartesian = TRUE][, k := NULL]
  seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[, .(speciesCode, shadetolerance)],
                                                         speciesCode),
                                                  nomatch = 0]
  seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, seedingData)
  seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData), 0 , 1),]
  set(seedingData, NULL, c("siteShade", "lightProb", "shadetolerance"), NULL)
  #   pixelGroupEcoregion <- unique(sim$cohortData, by = c("pixelGroup"))[,'.'(pixelGroup, sumB)]

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(sim$pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(seedingData$pixelGroup)], pixelGroup)
  seedingData <- setkey(seedingData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)

  specieseco_current <- speciesEcoregionLatestYear(
    sim$speciesEcoregion[,.(year, speciesCode, establishprob, ecoregionGroup)],
    round(time(sim)))
  specieseco_current <- setkeyv(specieseco_current, c("ecoregionGroup", "speciesCode"))

  #specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  #specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
  #                                                .(speciesCode, establishprob, ecoregionGroup)],
  #                             ecoregionGroup, speciesCode)
  seedingData <- seedingData[specieseco_current, nomatch = 0]
  seedingData <- seedingData[establishprob %>>% runif(nrow(seedingData), 0, 1),]
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
  if (nrow(seedingData) > 0) {
    outs <- updateCohortData(seedingData, cohortData = sim$cohortData, sim$pixelGroupMap,
                             time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion,
                             treedFirePixelTableSinceLastDisp = NULL,
                             successionTimestep = P(sim)$successionTimestep)
    sim$cohortData <- outs$cohortData
    sim$pixelGroupMap <- outs$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

WardDispersalSeeding <- function(sim, tempActivePixel, pixelsFromCurYrBurn,
                                 verbose = getOption("LandR.verbose", TRUE)) {
  sim$cohortData <- calculateSumB(cohortData = sim$cohortData,
                                  lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  siteShade <- calcSiteShade(time = round(time(sim)), cohortData = sim$cohortData,
                             sim$speciesEcoregion, sim$minRelativeB)
  activePixelGroup <- data.table(pixelGroup = unique(getValues(sim$pixelGroupMap)[tempActivePixel])) %>%
    na.omit()
  siteShade <- siteShade[activePixelGroup, on = "pixelGroup"]
  siteShade[is.na(siteShade),siteShade := 0]

  ## Seed source cells:
  ## 1. Select only sexually mature cohorts, then
  ## 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  sim$cohortData <- sim$species[, c("speciesCode", "sexualmature")][sim$cohortData,
                                                                    on = "speciesCode"]
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
    #  Seed Receiving cells:
    #  1. Must be sufficient light
    # seed receive just for the species that are seed source
    tempspecies1 <- sim$species[speciesCode %in% unique(matureCohorts$speciesCode),][
      , .(speciesCode, shadetolerance, seeddistance_eff, seeddistance_max)]
    seedReceive <- setkey(tempspecies1[, c(k = 1, .SD)], k)[setkey(siteShade[
      , c(k = 1, .SD)], k), allow.cartesian = TRUE][, k := NULL]
    seedReceive <- assignLightProb(sufficientLight = sim$sufficientLight, seedReceive)
    set(seedReceive, NULL, "siteShade", NULL)
    seedReceive <- seedReceive[lightProb %>>% runif(NROW(seedReceive), 0, 1), ][
      , .(pixelGroup, speciesCode, seeddistance_eff, seeddistance_max)]
    setkey(seedReceive, speciesCode)

    # rm ones that had successful serotiny or resprouting
    seedReceive <- seedReceive[!sim$cohortData[age == 1L], on = c("pixelGroup", "speciesCode")]

    # 3. Remove any species from the seedSource that couldn't regeneration anywhere on the map due to insufficient light
    #    (info contained within seedReceive)
    # this is should be a inner join, needs to specify the nomatch=0, nomatch = NA is default that sugest the full joint.
    seedSource <- seedSource[speciesCode %in% unique(seedReceive$speciesCode),]

    # Add inSituReceived data.table from the inSitu seeding function or event
    inSituReceived <- data.table(fromInit = integer(), species = character())

    # it could be more effecient if sim$pixelGroupMap is reduced map by removing the pixels that have
    # successful postdisturbance regeneration and the inactive pixels
    # how to subset the reducedmap
    # if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
    #   inactivePixelIndex <- c(sim$inactivePixelIndex, sim$treedFirePixelTableSinceLastDisp$pixelIndex)
    # } else {
    #   inactivePixelIndex <- sim$inactivePixelIndex
    # }
    reducedPixelGroupMap <- sim$pixelGroupMap
    if (length(pixelsFromCurYrBurn) > 0) {
      reducedPixelGroupMap[pixelsFromCurYrBurn] <- NA
    }

    seedingData <- LANDISDisp(sim, dtRcv = seedReceive, plot.it = FALSE,
                              dtSrc = seedSource, inSituReceived = inSituReceived,
                              species = sim$species,
                              reducedPixelGroupMap,
                              maxPotentialsLength = 1e5,
                              successionTimestep = P(sim)$successionTimestep,
                              verbose = FALSE,
                              useParallel = P(sim)$.useParallel)

    if (getOption("LandR.verbose", TRUE) > 0) {
      emptyForestPixels <- sim$treedFirePixelTableSinceLastDisp[burnTime < time(sim)]
      seedsArrivedPixels <- unique(seedingData[emptyForestPixels, on = "pixelIndex", nomatch = 0], by = "pixelIndex")
      message(blue("Of", NROW(emptyForestPixels),
                   "burned and empty pixels: Num pixels where seeds arrived:",
                   NROW(seedsArrivedPixels)))
    }

    rm(seedReceive, seedSource)
    if (NROW(seedingData) > 0) {
      seedingData[, ecoregionGroup := factorValues2(sim$ecoregionMap, getValues(sim$ecoregionMap),
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

      ##############################################
      # Run probability of establishment
      ##############################################
      assertCohortData(sim$cohortData, sim$pixelGroupMap)

      seedingData <- seedingData[runif(nrow(seedingData)) <= establishprob, ]
      if (getOption("LandR.verbose", TRUE) > 0) {
        seedsArrivedPixels <- unique(seedingData[emptyForestPixels, on = "pixelIndex", nomatch = 0],
                                     by = "pixelIndex")
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
        outs <- updateCohortData(seedingData, cohortData = sim$cohortData,
                                 pixelGroupMap = sim$pixelGroupMap,
                                 time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion,
                                 treedFirePixelTableSinceLastDisp = NULL,
                                 successionTimestep = P(sim)$successionTimestep)

        sim$cohortData <- outs$cohortData
        sim$pixelGroupMap <- outs$pixelGroupMap
      }
    }
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

summaryRegen <- function(sim) {
  #cohortData <- sim$cohortData
  if (!is.na(P(sim)$.plotInitialTime) | !is.na(P(sim)$.saveInitialTime)) {
    pixelGroupMap <- sim$pixelGroupMap
    names(pixelGroupMap) <- "pixelGroup"
    # please note that the calculation of reproduction is based on successioinTime step interval,
    pixelAll <- sim$cohortData[age <= P(sim)$successionTimestep + 1,
                               .(uniqueSumReproduction = sum(B, na.rm = TRUE)),
                               by = pixelGroup]
    if (!is.integer(pixelAll[["uniqueSumReproduction"]]))
        set(pixelAll, NULL, uniqueSumReproduction, asInteger(pixelAll[["uniqueSumReproduction"]]))

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
}

plotSummaryBySpecies <- function(sim) {
  ## BIOMASS, WEIGHTED AVERAGE AGE, AVERAGE ANPP
  ## AND AGE OF OLDEST COHORT PER SPECIES

  ## Averages are calculated across pixels
  ## don't expand table, multiply by no. pixels - faster
  thisPeriod <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap)

  for (column in names(thisPeriod)) if (is.integer(thisPeriod[[column]]))
    set(thisPeriod, NULL, column, as.numeric(thisPeriod[[column]]))
  thisPeriod <- thisPeriod[, list(year = time(sim),
                                  BiomassBySpecies = sum(B * noPixels, na.rm = TRUE),
                                  AgeBySppWeighted = sum(age * B * noPixels, na.rm = TRUE) /
                                    sum(B * noPixels, na.rm = TRUE),
                                  aNPPBySpecies = sum(aNPPAct * noPixels, na.rm = TRUE),
                                  OldestCohortBySpp = max(age, na.rm = TRUE)),
                           by = .(speciesCode)]

  if (is.null(sim$summaryBySpecies)) {
    sim$summaryBySpecies <- thisPeriod
  } else {
    sim$summaryBySpecies <- rbindlist(list(sim$summaryBySpecies, thisPeriod))
  }

  ## MEAN NO. PIXELS PER LEADING SPECIES
  vtm <- raster::mask(sim$vegTypeMap, sim$studyAreaReporting)
  freqs <- table(na.omit(factorValues2(vtm, vtm[], att = 2)))
  tabl <- as.vector(freqs)
  summaryBySpecies1 <- data.frame(year = rep(floor(time(sim)), length(freqs)),
                                  leadingType = names(freqs),
                                  #freqs = freqs,
                                  counts = tabl,
                                  stringsAsFactors = FALSE)

  whMixedLeading <- which(summaryBySpecies1$leadingType == "Mixed")
  summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType,
                                                  sim$sppEquiv,
                                                  "EN_generic_short")
  summaryBySpecies1$leadingType[whMixedLeading] <- "Mixed"

  colours <- equivalentName(names(sim$sppColorVect), sim$sppEquiv, "EN_generic_short")
  whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")
  colours[whMixedSppColors] <- "Mixed"

  colorIDs <- match(summaryBySpecies1$leadingType, colours)
  summaryBySpecies1$cols <- sim$sppColorVect[colorIDs]

  if (is.null(sim$summaryBySpecies1)) {
    sim$summaryBySpecies1 <- summaryBySpecies1
  } else {
    sim$summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, summaryBySpecies1))
  }

  if (length(unique(sim$summaryBySpecies1$year)) > 1) {
    df <- sim$species[, list(speciesCode, species)][sim$summaryBySpecies, on = "speciesCode"]
    df$species <- equivalentName(df$species, sim$sppEquiv, "EN_generic_short")

    colorIDs <- match(df$species, colours)
    df$cols <- sim$sppColorVect[colorIDs]

    cols2 <- df$cols
    names(cols2) <- df$species

    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)
      plot2 <- ggplot(data = df, aes(x = year, y = BiomassBySpecies,
                                     fill = species, group = species)) +
        geom_area(position = "stack") +
        scale_fill_manual(values = cols2) +
        labs(x = "Year", y = "Biomass") +
        theme(legend.text = element_text(size = 6), legend.title = element_blank())

      Plot(plot2, title = paste0("Total biomass by species\n",
                                 "across pixels"), new = TRUE)
    }

    maxNpixels <- sum(!is.na(sim$rasterToMatchReporting[]))
    cols3 <- sim$summaryBySpecies1$cols
    names(cols3) <- sim$summaryBySpecies1$leadingType

    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)
      plot3 <- ggplot(data = sim$summaryBySpecies1, aes(x = year, y = counts, fill = leadingType)) +
        scale_fill_manual(values = cols3) +
        labs(x = "Year", y = "Count") +
        geom_area() +
        theme(legend.text = element_text(size = 6), legend.title = element_blank()) +
        geom_hline(yintercept = maxNpixels, linetype = "dashed", color = "darkgrey", size = 1)

      Plot(plot3, title = "Number of pixels, by leading type", new = TRUE)
    }

    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)
      plot4 <- ggplot(data = df, aes(x = year, y = AgeBySppWeighted,
                                     colour = species, group = species)) +
        geom_line(size = 1) +
        scale_colour_manual(values = cols2) +
        labs(x = "Year", y = "Age") +
        theme(legend.text = element_text(size = 6), legend.title = element_blank())

      Plot(plot4, title = paste0("Biomass-weighted species age\n",
                                 "(averaged across pixels)"), new = TRUE)
    }

    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)
      plot5 <- ggplot(data = df, aes(x = year, y = OldestCohortBySpp,
                                     colour = species, group = species)) +
        geom_line(size = 1) +
        scale_colour_manual(values = cols2) +
        labs(x = "Year", y = "Age") +
        theme(legend.text = element_text(size = 6), legend.title = element_blank())

      Plot(plot5, title = paste("Oldest cohort age\n",
                                "by species (across pixels)"), new = TRUE)
    }

    ## test
    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)
      plot6 <- ggplot(data = df, aes(x = year, y = aNPPBySpecies,
                                     colour = species, group = species)) +
        geom_line(size = 1) +
        scale_color_manual(values = cols2) +
        labs(x = "Year", y = "aNPP") +
        theme(legend.text = element_text(size = 6), legend.title = element_blank())

      Plot(plot6, title = paste0("Total aNPP by species\n",
                                 "across pixels"), new = TRUE)
    }
    ## end test
  }

  return(invisible(sim))
}

plotVegAttributesMaps <- function(sim) {
  if (!is.na(P(sim)$.plotInitialTime)) {
    biomassMapForPlot <- raster::mask(sim$simulatedBiomassMap, sim$studyAreaReporting)
    ANPPMapForPlot <- raster::mask(sim$ANPPMap, sim$studyAreaReporting)
    mortalityMapForPlot <- raster::mask(sim$mortalityMap, sim$studyAreaReporting)
    if (is.null(sim$reproductionMap)) {
      reproductionMapForPlot <- biomassMapForPlot
      reproductionMapForPlot[!is.na(reproductionMapForPlot)][] <- 0
    } else {
      reproductionMapForPlot <-  raster::mask(sim$reproductionMap, sim$studyAreaReporting)
    }

    levs <- raster::levels(sim$vegTypeMap)[[1]]
    levelsName <- names(levs)[2]
    # facVals <- pemisc::factorValues2(sim$vegTypeMap, sim$vegTypeMap[],
    #                                  att = levelsName,
    #                                  na.rm = TRUE)

    ## Doesn't change anything in the current default setting, but it does create
    ##  an NA where there is "Mixed".
    ## Other species in levs[[levelsName]] are already "Leading",
    ##  but it needs to be here in case it is not Leading in the future.
    # The ones we want
    sppEquiv <- sim$sppEquiv[!is.na(sim$sppEquiv[[P(sim)$sppEquivCol]]),]

    levsLeading <- equivalentName(levs[[levelsName]], sppEquiv, "Leading")

    if (any(grepl("Mixed", levs[[levelsName]]))) {
      hasOnlyMixedAsOther <- sum(is.na(levsLeading) == 1) &&
        levs[[levelsName]][is.na(levsLeading)] == "Mixed"
      #extraValues <- setdiff(levs[[levelsName]], levsLeading)
      if (!isTRUE(hasOnlyMixedAsOther)) {
        stop("'plotVegAttributesMaps' in LBMR can only deal with 'Mixed' category or the ones in sim$sppEquiv")
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
    setColors(sim$vegTypeMap, levs$ID) <- colours

    # Mask out NAs based on rasterToMatch (for plotting only!)
    vegTypeMapForPlot <- raster::mask(sim$vegTypeMap, sim$studyAreaReporting)
    #vegTypeMapForPlot[is.na(sim$rasterToMatchReporting[])] <- NA ## faster than raster::mask

    ## Plot
    dev(mod$mapWindow)
    if (!is.null(biomassMapForPlot))
      Plot(biomassMapForPlot, title = "Biomass", new = TRUE)
    if (!is.null(ANPPMapForPlot))
      Plot(ANPPMapForPlot, title = "ANPP", new = TRUE)
    if (!is.null(mortalityMapForPlot))
      Plot(mortalityMapForPlot, title = "Mortality", new = TRUE)
    Plot(vegTypeMapForPlot, new = TRUE, title = "Leading vegetation")
    grid.rect(0.93, 0.97, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
    grid.text(label = paste0("Year = ", round(time(sim))), x = 0.93, y = 0.97)

    #if (!is.null(reproductionMapForPlot))
    #  Plot(reproductionMapForPlot, title = "Reproduction", new = TRUE)
  }

  return(invisible(sim))
}

plotAvgVegAttributes <- function(sim) {
  ## AVERAGE STAND BIOMASS/AGE/ANPP
  ## calculate acrosS pixels
  ## don't expand table, multiply by no. pixels - faster
  pixelCohortData <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap)
  thisPeriod <- pixelCohortData[, list(year = time(sim),
                                       sumB = asInteger(sum(B*noPixels, na.rm = TRUE)),
                                       maxAge = asInteger(max(age, na.rm = TRUE)),
                                       sumANPP = asInteger(sum(aNPPAct*noPixels, na.rm = TRUE)))]
  if (is.null(sim$summaryLandscape)) {
    sim$summaryLandscape <- thisPeriod
  } else {
    sim$summaryLandscape <- rbindlist(list(sim$summaryLandscape, thisPeriod))
  }

  if (length(unique(sim$summaryLandscape$year)) > 1) {
    df2 <- melt(sim$summaryLandscape, id.vars = "year")
    if (!is.na(P(sim)$.plotInitialTime)) {
      dev(mod$statsWindow)

      varLabels <- c(sumB = "Biomass", maxAge = "Age", sumANPP = "aNPP")

      plot1 <- ggplot(data = df2, aes(x = year, y = value, colour = variable)) +
        geom_line(size = 1) +
        scale_colour_brewer(labels = varLabels, type = "qual", palette = "Dark2") +
        theme_bw() +
        theme(legend.text = element_text(size = 6), legend.title = element_blank(),
              legend.position = "bottom") +
        facet_wrap(~ variable, scales = "free_y",
                   labeller = labeller(variable = varLabels)) +
        labs(x = "Year", y = "Value")

      Plot(plot1, title = "Total landscape biomass and aNPP and max stand age", new = TRUE)
    }
  }
  return(invisible(sim))
}

Save <- function(sim) {
  raster::projection(sim$simulatedBiomassMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$ANPPMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$mortalityMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$reproductionMap) <- raster::projection(sim$ecoregionMap)
  writeRaster(sim$simulatedBiomassMap,
              file.path(outputPath(sim), paste("simulatedBiomassMap_Year", round(time(sim)), ".tif", sep = "")),
              datatype = 'INT4S', overwrite = TRUE)
  writeRaster(sim$ANPPMap,
              file.path(outputPath(sim), paste("ANPP_Year", round(time(sim)), ".tif", sep = "")),
              datatype = 'INT4S', overwrite = TRUE)
  writeRaster(sim$mortalityMap,
              file.path(outputPath(sim), paste("mortalityMap_Year", round(time(sim)), ".tif", sep = "")),
              datatype = 'INT4S', overwrite = TRUE)
  writeRaster(sim$reproductionMap,
              file.path(outputPath(sim), paste("reproductionMap_Year", round(time(sim)), ".tif", sep = "")),
              datatype = 'INT4S', overwrite = TRUE)
  return(invisible(sim))
}

CohortAgeReclassification <- function(sim) {
  if (time(sim) != 0) {
    sim$cohortData <- ageReclassification(cohortData = sim$cohortData,
                                          successionTimestep = P(sim)$successionTimestep,
                                          stage = "mainSimulation")
    return(invisible(sim))
  } else {
    return(invisible(sim))
  }
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #######################################################

  if (!suppliedElsewhere("studyArea", sim)) {
    if (getOption("LandR.verbose", TRUE) > 0)
      message("'studyArea' was not provided by user. Using a polygon (6250000 m^2) in southwestern Alberta, Canada")
    sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    if (getOption("LandR.verbose", TRUE) > 0)
      message("'studyAreaReporting' was not provided by user. Using the same as 'studyArea'.")
    sim$studyAreaReporting <- sim$studyArea
  }

  needRTM <- FALSE
  if (is.null(sim$rasterToMatch)) {
    if (!suppliedElsewhere("rasterToMatch", sim)) {
      needRTM <- TRUE
      message("There is no rasterToMatch supplied; will attempt to use biomassMap")
    } else {
      stop("rasterToMatch is going to be supplied, but ", currentModule(sim), " requires it ",
           "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
           " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)",
           " or in a module that gets loaded prior to ", currentModule(sim))
    }
  }

  if (needRTM) {
    if (!suppliedElsewhere("biomassMap", sim)) {
      biomassMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
      biomassMapURL <- "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"
      biomassMap <- Cache(prepInputs,
                          targetFile = asPath(basename(biomassMapFilename)),
                          archive = asPath(c("kNN-StructureBiomass.tar",
                                             "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip")),
                          url = biomassMapURL,
                          destinationPath = dPath,
                          studyArea = sim$studyArea,
                          rasterToMatch = NULL,
                          maskWithRTM = FALSE,
                          useSAcrs = TRUE,
                          method = "bilinear",
                          datatype = "INT2U",
                          filename2 = TRUE, overwrite = TRUE,
                          omitArgs = c("destinationPath", "targetFile", cacheTags, "stable"))
    } else {
      biomassMap <- sim$biomassMap
    }

    # if we need rasterToMatch, that means a) we don't have it, but b) we will have biomassMap
    sim$rasterToMatch <- biomassMap
    studyArea <- sim$studyArea # temporary copy because it will be overwritten if it is suppliedElsewhere
    message("  Rasterizing the studyArea polygon map")
    if (!is(studyArea, "SpatialPolygonsDataFrame")) {
      dfData <- if (is.null(rownames(studyArea))) {
        polyID <- sapply(slot(studyArea, "polygons"), function(x) slot(x, "ID"))
        data.frame("field" = as.character(seq_along(length(studyArea))), row.names = polyID)
      } else {
        polyID <- sapply(slot(studyArea, "polygons"), function(x) slot(x, "ID"))
        data.frame("field" = rownames(studyArea), row.names = polyID)
      }
      studyArea <- SpatialPolygonsDataFrame(studyArea, data = dfData)
    }
    if (!identical(crs(studyArea), crs(sim$rasterToMatch))) {
      studyArea <- spTransform(studyArea, crs(sim$rasterToMatch))
      studyArea <- fixErrors(studyArea)
    }
    ## TODO: review whether this is necessary (or will break LandWeb if removed) see Git Issue #22
    ## layers provided by David Andison sometimes have LTHRC, sometimes LTHFC ... chose whichever
    LTHxC <- grep("(LTH.+C)", names(studyArea), value = TRUE)
    fieldName <- if (length(LTHxC)) {
      LTHxC
    } else {
      if (length(names(studyArea)) > 1) {
        ## study region may be a simple polygon
        names(studyArea)[1]
      } else NULL
    }

    sim$rasterToMatch <- crop(fasterizeFromSp(studyArea, sim$rasterToMatch, fieldName),
                              studyArea)
    sim$rasterToMatch <- Cache(writeRaster, sim$rasterToMatch,
                               filename = file.path(dataPath(sim), "rasterToMatch.tif"),
                               datatype = "INT2U", overwrite = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatchReporting")) {
    sim$rasterToMatchReporting <- sim$rasterToMatch
  }

  if (FALSE) { # not using this -- Eliot Jan 22, 2019 -- use pixelGroupMap and pixelGroups instead
    if (!suppliedElsewhere("initialCommunities", sim)) {
      maxcol <- 7 #max(count.fields(file.path(dPath, "initial-communities.txt"), sep = ""))
      initialCommunities <- Cache(prepInputs,
                                  url = extractURL("initialCommunities"),
                                  targetFile = "initial-communities.txt",
                                  destinationPath = dPath,
                                  fun = "utils::read.table", #purge = 7,
                                  fill = TRUE, row.names = NULL,
                                  sep = "",
                                  blank.lines.skip = TRUE,
                                  col.names = c("species", paste("age", 1:(maxcol - 1), sep = "")),
                                  stringsAsFactors = FALSE,
                                  overwrite = TRUE)
      # correct the typo in the original txt
      initialCommunities[14, 1:4] <- initialCommunities[14, 2:5]

      initialCommunities <- data.table(initialCommunities)
      initialCommunities <- cbind(data.table(mapcode = 1:nrow(initialCommunities),
                                             description = NA), initialCommunities)
      initialCommunities <- initialCommunities[species != "LandisData",]
      cutRows <- grep(">>", initialCommunities$species)
      for (i in cutRows) {
        initialCommunities[i, desc := paste(initialCommunities[i, 3:maxcol, with = FALSE], collapse = " ")]
      }

      initialCommunities[, rowN := 1:nrow(initialCommunities)]
      initialCommunities[, ':='(mapcode = cut(rowN, breaks = c(cutRows, max(rowN)),
                                              labels = initialCommunities[cutRows + 1,]$age1),
                                description = cut(rowN, breaks = c(cutRows, max(rowN)),
                                                  labels = initialCommunities[cutRows,]$desc))]
      initialCommunities <- initialCommunities[!c(cutRows, cutRows + 1), ][, ':='(desc = NULL, rowN = NULL)]
      initialCommunities[, ':='(description = gsub(">>", "", description),
                                mapcode = as.integer(as.character(mapcode)))]

      initialCommunities <- data.table(initialCommunities[, 1:3, with = FALSE],
                                       initialCommunities[, lapply(.SD, as.integer), .SDcols = age1:age6])

      ## rename species for compatibility across modules (Genu_spe)
      initialCommunities$species1 <- as.character(substring(initialCommunities$species, 1, 4))
      initialCommunities$species2 <- as.character(substring(initialCommunities$species, 5, 7))
      initialCommunities[, ':='(species = paste0(toupper(substring(species1, 1, 1)),
                                                 substring(species1, 2, 4), "_", species2))]

      initialCommunities[, ':='(species1 = NULL, species2 = NULL)]

      sim$initialCommunities <- initialCommunities
      rm(cutRows, i, maxcol)
    }
  }

  ## load the initial community map
  if (FALSE) { # No longer using this
    if (!suppliedElsewhere("initialCommunitiesMap", sim)) {
      ## LANDIS-II demo data:
      # sim$initialCommunitiesMap <- Cache(prepInputs,
      #                                    targetFile = "initial-communities.gis",
      #                                    url = extractURL("initialCommunitiesMap"),
      #                                    destinationPath = dPath,
      #                                    fun = "raster::raster")

      ## Dummy version with spatial location in Canada
      ras <- projectExtent(sim$studyArea, crs = sim$studyArea)
      res(ras) <- 250 ## TODO: this shouldn't be hardcoded; get this from rasterToMatch?
      initialCommunitiesMap <- rasterize(sim$studyArea, ras)

      ## make uniform communities (well structured in space)
      mapvals <- rep(unique(initialCommunities$mapcode),
                     each = ceiling(sum(!is.na(getValues(initialCommunitiesMap))) /
                                      length(unique(initialCommunities$mapcode))))
      mapvals <- mapvals[1:sum(!is.na(getValues(initialCommunitiesMap)))]   ## remove any extra values

      ## assign communities to map and export to sim
      initialCommunitiesMap[!is.na(getValues(initialCommunitiesMap))][] <- mapvals
      sim$initialCommunitiesMap <- initialCommunitiesMap
    }
  }

  ######################################################
  ## load ecoregion map
  if (FALSE) {   ## Ceres: March 25th no longer using this, default ecoregionMap is a dummy.
    if (!suppliedElsewhere("ecoregionMap", sim)) {
      ## LANDIS-II demo data:

      ## TODO: restore the demo data version with prepInputs:
      # sim$ecoregionMap <- Cache(prepInputs,
      #                           url = extractURL("ecoregionMap"),
      #                           destinationPath = dPath,
      #                           targetFile = "ecoregions.gis",
      #                           fun = "raster::raster")

      ecoregionMap <- sim$rasterToMatch

      ## make uniform communities (well structured in space)
      mapvals <- rep(unique(sim$ecoregion$mapcode),
                     each = ceiling(sum(!is.na(getValues(ecoregionMap))) /
                                      length(unique(sim$ecoregion$mapcode))))
      mapvals <- mapvals[1:sum(!is.na(getValues(ecoregionMap)))] ## remove any extra values

      ## assign communities to map and export to sim
      ecoregionMap[!is.na(getValues(ecoregionMap))][] <- mapvals

      sim$ecoregionMap <- ecoregionMap
    }
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    ## load the biomass_succession.txt to get shade tolerance parameters
    mainInput <- prepInputsMainInput(url = extractURL("sufficientLight"), dPath, cacheTags) ## uses default URL

    sufficientLight <- data.frame(mainInput)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight)
  }

  if (!suppliedElsewhere("sppEquiv", sim)) {
    if (!is.null(sim$sppColorVect))
      stop("If you provide sppColorVect, you MUST also provide sppEquiv")

    data("sppEquivalencies_CA", package = "LandR", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA)
    ## By default, Abies_las is renamed to Abies_sp
    sim$sppEquiv[KNN == "Abie_Las", LandR := "Abie_sp"]

    ## check spp column to use
    if (P(sim)$sppEquivCol == "Boreal") {
      message(paste("There is no 'sppEquiv' table supplied;",
                    "will attempt to use species listed under 'Boreal'",
                    "in the 'LandR::sppEquivalencies_CA' table"))
    } else {
      if (grepl(P(sim)$sppEquivCol, names(sim$sppEquiv))) {
        message(paste("There is no 'sppEquiv' table supplied,",
                      "will attempt to use species listed under", P(sim)$sppEquivCol,
                      "in the 'LandR::sppEquivalencies_CA' table"))
      } else {
        stop("You changed 'sppEquivCol' without providing 'sppEquiv',",
             "and the column name can't be found in the default table ('LandR::sppEquivalencies_CA').",
             "Please provide conforming 'sppEquivCol', 'sppEquiv' and 'sppColorVect'")
      }
    }

    ## remove empty lines/NAs
    sim$sppEquiv <- sim$sppEquiv[!"", on = P(sim)$sppEquivCol]
    sim$sppEquiv <- na.omit(sim$sppEquiv, P(sim)$sppEquivCol)

    ## add default colors for species used in model
    sim$sppColorVect <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol,
                                  newVals = "Mixed", palette = "Accent")
  } else {
    if (is.null(sim$sppColorVect))
      stop("If you provide 'sppEquiv' you MUST also provide 'sppColorVect'")
  }

  if (!suppliedElsewhere("treedFirePixelTableSinceLastDisp", sim)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(),
                                                       pixelGroup = integer(),
                                                       burnTime = numeric())
  }

  if (!suppliedElsewhere("speciesLayers", sim)) {
    #opts <- options(reproducible.useCache = "overwrite")
    if (!suppliedElsewhere("studyAreaLarge", sim)) {
      message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
      sim <- objectSynonyms(sim, list(c("studyAreaLarge", "studyArea")))
    }

    sim$speciesLayers <- Cache(loadkNNSpeciesLayers,
                               dPath = dPath,
                               rasterToMatch = sim$rasterToMatch,
                               studyArea = sim$studyAreaLarge,
                               sppEquiv = sim$sppEquiv,
                               knnNamesCol = "KNN",
                               sppEquivCol = P(sim)$sppEquivCol,
                               thresh = 5,
                               url = "http://tree.pfc.forestry.ca/kNN-Species.tar",
                               userTags = c(cacheTags, "speciesLayers"))
  }

  ## additional species traits
  if(!suppliedElsewhere("species", sim)) {
    speciesTable <- getSpeciesTable(dPath = dPath, cacheTags = cacheTags)
    sim$species <- prepSpeciesTable(speciesTable = speciesTable,
                                    speciesLayers = sim$speciesLayers,
                                    sppEquiv = sim$sppEquiv[get(P(sim)$sppEquivCol) %in%
                                                              names(sim$speciesLayers)],
                                    sppEquivCol = P(sim)$sppEquivCol)
  }

  return(invisible(sim))
}

# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LBMR",
  description = "A fast and large landscape biomass succession model modified from LANDIS II",
  keywords = c("forest succession", "LANDIS II", "Biomass"),
  authors = c(person(c("Yong"), "Luo", email = "Yong.Luo@canada.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre")),
              person(c("Jean"), "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("1.3.0.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LBMR.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "fpCompare", "ggplot2", "grid",
                  "purrr", "quickPlot", "raster", "Rcpp", "scales", "sp", "tidyr",
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/reproducible@development",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE,
                    desc = "Do calibration? Defaults to FALSE"),
    defineParameter("growthInitialTime", "numeric", 0, NA_real_, NA_real_,
                    desc = "Initial time for the growth event to occur"),
    defineParameter("overrideSpinup", "logical", FALSE, NA, NA,
                    paste("Should biomass be inserted from sim$biomassMap (an input), rather than",
                          "derived using spinUp procedures. spinUp uses Age as the driver, so biomass",
                          "is an output. That means it will be unlikely to match any input information",
                          "about biomass, unless this is set to TRUE, and a sim$biomassMap is supplied")),
    defineParameter("seedingAlgorithm", "character", "wardDispersal", NA_character_, NA_character_,
                    desc = paste("choose which seeding algorithm will be used among",
                                 "noDispersal, universalDispersal, and wardDispersal (default).")),
    defineParameter("spinupMortalityfraction", "numeric", 0.001,
                    desc = "defines the mortality loss fraction in spin up-stage simulation"),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter("speciesEstablishmentProbAsMap", "logical", FALSE,
                    desc = paste("Should species establishment probability be represented at the pixel level,",
                                 "as a rescaled map of original species percent cover")),
    defineParameter("successionTimestep", "numeric", 10, NA, NA, "defines the simulation time step, default is 10 years"),
    defineParameter("vegLeadingProportion", "numeric", 0.8, 0, 1,
                    desc = "a number that define whether a species is leading for a given pixel"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA,
                    desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = paste("This describes the simulation time at which the first save event should occur.",
                                 "Set to NA if no saving is desired.")),
    # defineParameter(".useCache", "logical", TRUE,
    #                 desc = "use caching for the spinup simulation?"),
    defineParameter(".useParallel", "ANY", parallel::detectCores(),
                    desc = paste("Used only in seed dispersal.",
                                 "If numeric, it will be passed to data.table::setDTthreads",
                                 "If TRUE, it will be passed to parallell:makeCluster,",
                                 "and if a cluster object, it will be passed to parallel::parClusterApplyB"))
  ),
  inputObjects = bind_rows(
    expectsInput("calculateAgeMortality", "function",
                  desc = "function to calculate aging and mortality"),
    expectsInput("calculateANPP", "function",
                  desc = "function to calculate ANPP"),
    expectsInput("calculateCompetition", "function",
                  desc = "function to calculate competition for light"),
    expectsInput("calculateGrowthMortality", "function",
                  desc = "function to calculate growth and mortality"),
    expectsInput("calculateSumB", "function",
                  desc = "function to sum biomass"),
    expectsInput("ecoregion", "data.table",
                 desc = "ecoregion look up table",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt"),
    expectsInput("ecoregionMap", "RasterLayer",
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.gis"),
    expectsInput("initialCommunities", "data.table",
                 desc = "initial community table",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.txt"),
    expectsInput("initialCommunitiesMap", "RasterLayer",
                 desc = "initial community map that has mapcodes match initial community table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.gis"),
    expectsInput("minRelativeB", "data.frame",
                 desc = "table defining the cut points to classify stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = paste("Raster layer of buffered study area used for cropping, masking and projecting.",
                              "Defaults to the kNN biomass map masked with `studyArea`"),
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput("rasterToMatchReporting", "RasterLayer",
                 desc = paste("Raster layer of study area used for plotting and reporting only.",
                              "Defaults to the kNN biomass map masked with `studyArea`"),
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("sppColors", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in sim$speciesEquivalency[[sim$sppEquivCol]],",
                              "and should also contain a color for 'Mixed'"),
                 sourceURL = NA),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See pemisc::sppEquivalencies_CA.",
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
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("updateSpeciesAttributes", "function",
                 desc = "function to add/update species attributes in species cohort table"),
    expectsInput("updateSpeciesEcoregionAttributes", "function",
                 desc = "function to add/update species ecoregion attributes in species cohort table"),
    ## for inputs from optional fire module:
    expectsInput("spinUpCache", "logical", ""),
    expectsInput("speciesEstablishmentProbMap", "RasterBrick", "Species establishment probability as a RasterBrick, one layer for each species")
  ),
  outputObjects = bind_rows(
    createsOutput("activePixelIndex", "logical",
                  desc = "internal use. Keeps track of which pixels are active"),
    createsOutput("ANPPMap", "RasterLayer",
                  desc = "ANPP map at each succession time step"),
    createsOutput("burnLoci", "numeric", desc = "Fire pixel IDs"),
    createsOutput("cohortData", "data.table",
                  desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                  succession time step"),
    createsOutput(objectName = "simulatedBiomassMap", objectClass = "RasterLayer",
                  desc = "Biomass map at each succession time step"),
    createsOutput("inactivePixelIndex", "logical",
                  desc = "internal use. Keeps track of which pixels are inactive"),
    createsOutput("initialCommunitiesMap", "RasterLayer",
                  desc = "initial community map that has mapcodes match initial community table"),
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
                  desc = "a table that has species traits such as longevity..."),
    createsOutput("speciesEcoregion", "data.table",
                  desc = "define the maxANPP, maxB and SEP change with both ecoregion and simulation time"),
    createsOutput("spinUpCache", "logical", desc = ""),
    createsOutput("spinupOutput", "data.table", desc = ""),
    createsOutput("summaryBySpecies", "data.table", desc = "The average biomass in a pixel, by species")
  )
))

doEvent.LBMR <- function(sim, eventTime, eventType, debug = FALSE) {
  if (is.numeric(P(sim)$.useParallel)) {
    a <- data.table::setDTthreads(P(sim)$.useParallel)
    message("Mortality and Growth should be using >100% CPU")
    if (data.table::getDTthreads() == 1L) crayon::red(message("Only using 1 thread."))
    on.exit(setDTthreads(a))
  }
  switch(eventType,
         init = {
           ## do stuff for this event
           sim <- Init(sim)

           ## schedule events
           sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                                "LBMR", "Dispersal", eventPriority = 5)
           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, "LBMR",
                                  "cohortAgeReclassification", eventPriority = 6.25)
           }
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime + P(sim)$successionTimestep,
                                "LBMR", "summaryRegen", eventPriority = 5.5)
           sim <- scheduleEvent(sim, start(sim),
                                "LBMR", "summaryBGM", eventPriority = 5.75)
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                "LBMR", "summaryBySpecies", eventPriority = 6)
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                                "LBMR", "plotMaps", eventPriority = 7)

           if (!any(is.na(P(sim)$.saveInitialTime))) {
             sim <- scheduleEvent(sim, P(sim)$.saveInitialTime + P(sim)$successionTimestep,
                                  "LBMR", "save", eventPriority = 8.5)
             ## stats plot is retrieving saved rasters so needs data to be saved
             # start on second time around b/c ggplot doesn't like 1 data point
             tPlotInit <- P(sim)$.plotInitialTime + 2*P(sim)$successionTimestep
             sim <- scheduleEvent(sim, tPlotInit, "LBMR", "plotAvgs", eventPriority = 7.75)
           }
         },
         Dispersal = {
           if (P(sim)$seedingAlgorithm == "noDispersal") {
             sim <- NoDispersalSeeding(sim)
           } else if (P(sim)$seedingAlgorithm == "universalDispersal") {
             sim <- UniversalDispersalSeeding(sim)
           } else if (P(sim)$seedingAlgorithm == "wardDispersal") {
             sim <- WardDispersalSeeding(sim)
           } else stop("Undefined seed dispersal type!")

           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "Dispersal", eventPriority = 4)
         },
         cohortAgeReclassification = {
           sim <- CohortAgeReclassification(sim)

           if (P(sim)$successionTimestep != 1) {
             sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                  "LBMR", "cohortAgeReclassification",
                                  eventPriority = 5.25)
           }
         },
         summaryRegen = {
           sim <- summaryRegen(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryRegen", eventPriority = 5.5)
         },
         summaryBySpecies = {
           sim <- summaryBySpecies(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBySpecies", eventPriority = 5.75)
         },
         summaryBGM = {
           sim <- SummaryBGM(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "summaryBGM",
                                eventPriority = 6)
         },
         plotMaps = {
           sim <- plotVegAttributesMaps(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "plotMaps", eventPriority = 8)
         },
         save = {
           sim <- Save(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "save", eventPriority = 8.5)
         },
         plotAvgs = {
           ## only occurs once at the end of the simulation
           sim <- plotAvgVegAttributes(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "plotAvgs", eventPriority = 8.75)
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  # A numeric scalar indicating how large each chunk of an internal data.table with processing by chuncks
  mod$cutpoint <- 1e10
  ##############################################
  # Prepare individual objects
  ##############################################

  ##############################################
  # ecoregion
  ##############################################
  setDT(sim$ecoregion)
  ecoregion <- sim$ecoregion[, ecoregionGroup := as.factor(ecoregion)]
  ecoregion_temp <- setkey(ecoregion[, .(ecoregion, ecoregionGroup)], ecoregion)

  ##############################################
  # species
  ##############################################
  species <- data.table(sim$species)[, speciesCode := as.factor(species)]
  sim$species <- setkey(species, speciesCode)

  ##############################################
  # speciesEcoregion
  ##############################################
  speciesEcoregion <- sim$speciesEcoregion[sim$species[, .(species, speciesCode)],
                                                on = "species", nomatch = 0]
  speciesEcoregion[, ecoregionGroup := as.factor(ecoregion)]
  speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)

  ##############################################
  # minRelativeB
  ##############################################
  setDT(sim$minRelativeB) # make a data.table
  # join to get ecoregionGroup column
  sim$minRelativeB <- sim$minRelativeB[unique(speciesEcoregion[, .(ecoregion, ecoregionGroup)]),
                                         on = "ecoregion", nomatch = 0]

  #############################################
  # Create cohortData from communities
  #############################################
  active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, ecoregionGroup)], k) # not sure what k is doing here

  sim$initialCommunities[, communityGroup := as.integer(mapcode)]
  cohortData <- unique(
    sim$initialCommunities[, .(
      speciesCode = as.factor(species),
      age = as.integer(ceiling(as.numeric(age) / P(sim)$successionTimestep) *
                         P(sim)$successionTimestep),
      communityGroup,
      mapcode,
      speciesPresence,
      ecoregionGroup = as.factor(gsub(".*_.*_", "", mapcode)))]
    , by = c("communityGroup", "speciesCode", "age", "mapcode"))

  set(cohortData, NULL, "pixelGroup", cohortData$communityGroup)
  set(cohortData, NULL, "B", as.integer(0L))
  cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B, speciesPresence)] # removed communityGroup column

  pixelGroupMap <- sim$initialCommunitiesMap
  names(pixelGroupMap) <- "pixelGroup"

  # Changed mechanism for active and inactive -- just use NA on ecoregionMap
  ecoregionMapNAs <- is.na(sim$ecoregionMap[])
  sim$activePixelIndex <- which(!ecoregionMapNAs) # store this for future use
  sim$inactivePixelIndex <- which(ecoregionMapNAs) # store this for future use

  # Keeps track of the length of the ecoregion
  mod$activeEcoregionLength <- data.table(Ecoregion = getValues(sim$ecoregionMap),
                                          pixelIndex = 1:ncell(sim$ecoregionMap))[
                                            Ecoregion %in% active_ecoregion$ecoregionGroup,
                                            .(NofCell = length(pixelIndex)), by = Ecoregion]
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[sim$activePixelIndex]),]
  cohortData <- sim$updateSpeciesEcoregionAttributes(speciesEcoregion = speciesEcoregion,
                                                     time = round(time(sim)),
                                                     cohortData = cohortData)
  cohortData <- sim$updateSpeciesAttributes(species = sim$species, cohortData = cohortData)

  ################
  # if (FALSE) { # OLD STUFF
  #
  #   communities <- sim$initialCommunities %>%
  #     gather(key = cohort, value = age, -mapcode, -description, -species, na.rm = TRUE) %>%
  #     data.table() %>% ## this needs to be here to appease the data.table gods
  #     .[, ':='(age = as.integer(ceiling(as.numeric(age) / P(sim)$successionTimestep) *
  #                                 P(sim)$successionTimestep),
  #              communityGroup = as.integer(mapcode),
  #              mapcode = NULL)] %>%
  #     unique(., by = c("communityGroup", "species", "age"))
  #   species <- data.table(sim$species)[, speciesCode := as.integer(as.factor(species))]
  #   tempspecies <- setkey(species[, .(species, speciesCode)], species)
  #   communities <- setkey(communities, species)[tempspecies, nomatch = 0]
  #   speciesEcoregion <- setkey(data.table(sim$speciesEcoregion), species)[tempspecies, nomatch = 0]
  #   sim$species <- setkey(species, speciesCode)
  #   ecoregion <- data.table(sim$ecoregion)[, ecoregionGroup := as.integer(mapcode)]
  #   ecoregion_temp <- setkey(ecoregion[, .(ecoregion, ecoregionGroup)], ecoregion)
  #   sim$minRelativeB <- data.table(sim$minRelativeB, key = "ecoregion")[ecoregion_temp, nomatch = 0]
  #   speciesEcoregion <- setkey(speciesEcoregion, ecoregion)[ecoregion_temp, nomatch = 0]
  #   sim$speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)
  #   nrowCommunities <- nrow(communities) #line 197 in Yong code
  #   initialCommunitiesMap <- setValues(sim$initialCommunitiesMap, as.integer(sim$initialCommunitiesMap[]))
  #   napixels <- which(is.na(getValues(initialCommunitiesMap)))
  #   initialCommunitiesMap[napixels] <- as.integer(maxValue(initialCommunitiesMap) + 1)
  #   pixelGroupFactor <- as.integer(10^ceiling(log10((maxValue(initialCommunitiesMap) + 1))))
  #   #ecoregionMap <- sim$ecoregionMap
  #   pixelGroupMap <- setValues(initialCommunitiesMap, as.integer((initialCommunitiesMap +
  #                                                                   sim$ecoregionMap*pixelGroupFactor)[]))
  #   sim$initialCommunitiesMap <- NULL
  #   active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, ecoregionGroup)], k)
  #   cohortData <- setkey(communities[, k := 1], k)[active_ecoregion, allow.cartesian = TRUE][, k := NULL]
  #   set(cohortData, NULL, "pixelGroup", cohortData$communityGroup + cohortData$ecoregionGroup*pixelGroupFactor)
  #   set(cohortData, NULL, "B", as.integer(0L))
  #   cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B)] # removed communityGroup column
  #   # the cohortData here is a full joint table of community Group and ecoregion Group
  #   # some redundant pixelGroups are removed, because they are not present on the pixelGroupMap
  #   # we are dealing with the case that all the ecoregion is active, how about some ecoregion is not active
  #
  # }
  # # # pixels with -1 in the pixelGroupMap are inactive #  ELIOT -- WHY NOT JUST LEAVE AS NA? TRY RM -1
  # # if (length(inactivePixelIndex) > 0) {
  # #   pixelGroupMap[inactivePixelIndex] <- -1L
  # # }

  #sim <- cacheSpinUpFunction(sim, cachePath = outputPath(sim))
  if (!isTRUE(P(sim)$overrideSpinup)) { # negate the TRUE to allow for default to be this, even if NULL or NA
    message("Running spinup")
    spinupstage <- Cache(spinUp,
                         fnList = list(
                           calculateAgeMortality = sim$calculateAgeMortality,
                           calculateANPP = sim$calculateANPP,
                           calculateCompetition = sim$calculateCompetition,
                           calculateGrowthMortality = sim$calculateGrowthMortality,
                           calculateSumB = sim$calculateSumB,
                           updateSpeciesEcoregionAttributes = sim$updateSpeciesEcoregionAttributes,
                           updateSpeciesAttributes = sim$updateSpeciesAttributes
                         ),
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
    pixelAll <- cohortData[, .(uniqueSumB = as.integer(sum(B, na.rm = TRUE))), by = pixelGroup]
    if (!any(is.na(P(sim)$.plotInitialTime)) | !any(is.na(P(sim)$.saveInitialTime))) {
      simulatedBiomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
      #ANPPMap <- setValues(simulatedBiomassMap, 0L)
      #mortalityMap <- setValues(simulatedBiomassMap, 0L)
      #reproductionMap <- setValues(pixelGroupMap, 0L)
    }
    #}

    sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                     B, mortality = 0, aNPPAct = 0)]
  } else {
    message("Skipping spinup and using the sim$biomassMap as initial biomass values")
    biomassTable <- data.table(biomass = getValues(sim$biomassMap),
                          pixelGroup = getValues(pixelGroupMap))
    biomassTable <- na.omit(biomassTable)
    maxBiomass <- maxValue(sim$biomassMap)
    if (maxBiomass < 1e3) {
      message(crayon::red("  Because biomassMap values are all below 1000, assuming that these should be\n",
              "    converted to tonnes/ha by multiplying by 100"))
      biomassTable[, `:=`(biomass = biomass * 100)]
    }
    biomassTable <- biomassTable[, list(Bsum = as.integer(mean(biomass, na.rm = TRUE))),
                                 by = pixelGroup]
    # Delete the B from cohortData -- it will be joined from biomassTable
    set(cohortData, NULL, "B", NULL)
    cohortData[, totalSpeciesPresence := sum(speciesPresence), by = "pixelGroup"]
    cohortData <- cohortData[biomassTable, on = "pixelGroup"]
    cohortData[, B := as.integer(Bsum * speciesPresence / totalSpeciesPresence),
               by = c("pixelGroup", "speciesCode")]
    sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                     B, mortality = 0, aNPPAct = 0)]
    pixelAll <- cohortData[, .(uniqueSumB = as.integer(sum(B, na.rm = TRUE))), by = pixelGroup]
  }
  sim$pixelGroupMap <- pixelGroupMap

  simulationOutput <- data.table(Ecoregion = getValues(sim$ecoregionMap),
                                 pixelGroup = getValues(pixelGroupMap),
                                 pixelIndex = 1:ncell(sim$ecoregionMap))[
                                   , .(NofPixel = length(pixelIndex)), by = c("Ecoregion", "pixelGroup")]

  simulationOutput <- setkey(simulationOutput, pixelGroup)[
    setkey(pixelAll, pixelGroup), nomatch = 0][
      , .(Biomass = sum(as.numeric(uniqueSumB*NofPixel))), by = Ecoregion] ## NOTE:
  ## above needs to be numeric because of integer overflow -- returned to integer in 2 lines
  simulationOutput <- setkey(simulationOutput, Ecoregion)[
    setkey(mod$activeEcoregionLength, Ecoregion), nomatch = 0]
  sim$simulationOutput <- simulationOutput[, .(Ecoregion, NofCell, Year = as.integer(time(sim)),
                                               Biomass = as.integer(Biomass/NofCell),
                                               ANPP = 0L, Mortality = 0L, Regeneration = 0L)]
  sim$lastReg <- 0
  speciesEcoregion[, identifier := year > P(sim)$successionTimestep]
  speciesEcoregion_True <- speciesEcoregion[identifier == "TRUE",]
  speciesEcoregion_False <- speciesEcoregion[identifier == "FALSE",]
  speciesEcoregion_True_addon <- speciesEcoregion_False[year == max(speciesEcoregion_False$year),]
  sim$speciesEcoregion <- rbindlist(list(speciesEcoregion_True_addon, speciesEcoregion_True))[
    , ':='(year = year - min(year), identifier = NULL)]
  sim$lastFireYear <- "noFire"
  return(invisible(sim))
}

### EVENT FUNCTIONS
SummaryBGM <- function(sim) {
  pixelGroups <- data.table(pixelGroupIndex = unique(sim$cohortData$pixelGroup),
                            temID = 1:length(unique(sim$cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = mod$cutpoint), max(pixelGroups$temID))))
  if (length(cutpoints) == 1) {cutpoints <- c(cutpoints, cutpoints + 1)}
  pixelGroups[, groups := cut(temID, breaks = cutpoints,
                              labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                              include.lowest = TRUE)]
  ecoPixelgroup <- data.table(Ecoregion = getValues(sim$ecoregionMap),
                              pixelGroup = getValues(sim$pixelGroupMap),
                              pixelIndex = 1:ncell(sim$ecoregionMap))[
                                , .(NofPixelGroup = length(pixelIndex)),
                                by = c("Ecoregion", "pixelGroup")]

  for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
    subCohortData <- sim$cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    if (nrow(subCohortData[age == (P(sim)$successionTimestep + 1),]) > 0) {
      subCohortData[age == (P(sim)$successionTimestep + 1),reproduction := sum(B), by = pixelGroup]
    } else {
      subCohortData[, reproduction := 0]
    }
    subCohortData[is.na(reproduction), reproduction := 0L]
    summarytable_sub <- subCohortData[, .(uniqueSumB = as.integer(sum(B, na.rm=TRUE)),
                                          uniqueSumANPP = as.integer(sum(aNPPAct, na.rm=TRUE)),
                                          uniqueSumMortality = as.integer(sum(mortality, na.rm=TRUE)),
                                          uniqueSumRege = as.integer(mean(reproduction, na.rm = TRUE))),
                                      by = pixelGroup]

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
                                   by = Ecoregion]
  tempOutput_All <- setkey(tempOutput_All, Ecoregion)[setkey(mod$activeEcoregionLength,
                                                             Ecoregion), nomatch = 0]
  sim$simulationOutput <- rbindlist(list(sim$simulationOutput,
                                         tempOutput_All[, .(Ecoregion, NofCell, Year = as.integer(time(sim)),
                                                            Biomass = as.integer(Biomass / NofCell),
                                                            ANPP = as.integer(ANPP / NofCell),
                                                            Mortality = as.integer(Mortality / NofCell),
                                                            Regeneration = as.integer(Regeneration / NofCell))]))
  # the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively.
  names(sim$pixelGroupMap) <- "pixelGroup"
  sim$biomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumB")
  setColors(sim$biomassMap) <- c("light green", "dark green")

  if (!any(is.na(P(sim)$.plotInitialTime)) | !any(is.na(P(sim)$.saveInitialTime))) {
    sim$simulatedBiomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                                "uniqueSumB")
    setColors(sim$simulatedBiomassMap) <- c("light green", "dark green")

    sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                    "uniqueSumANPP")
    setColors(sim$ANPPMap) <- c("light green", "dark green")

    sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                         "uniqueSumMortality")
    setColors(sim$mortalityMap) <- c("light green", "dark green")

    sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, sim$pixelGroupMap,
                                          P(sim)$vegLeadingProportion,
                                          colors = sim$sppColors,
                                          unitTest = TRUE)


  }
  # the following codes for preparing the data table for saving
  rm(cutpoints, pixelGroups, tempOutput_All, summaryBGMtable)
  return(invisible(sim))
}

NoDispersalSeeding <- function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  if (sim$lastFireYear == round(time(sim))) { # if current year is both fire year and succession year
    # find new active pixel that remove successful postfire regeneration
    # since this is on site regeneration, all the burnt pixels can not seeding
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$firePixelTable$pixelIndex)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- sim$calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = time(sim),
                                      successionTimestep = P(sim)$successionTimestep)
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[
    setkey(sim$species[, .(speciesCode, sexualmature)], speciesCode), nomatch = 0]

  newCohortData <- sim$cohortData[age >= sexualmature]
  set(sim$cohortData, NULL, "sexualmature", NULL)
  set(newCohortData, NULL, c("sexualmature", "age", "B", "mortality", "aNPPAct"), NULL)
  siteShade <- setkey(data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                               sim$speciesEcoregion, sim$minRelativeB)), pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[siteShade, nomatch = 0]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[
    , .(speciesCode, shadetolerance)], speciesCode), nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, NULL, c("shadetolerance", "lightProb", "siteShade", "sumB"), NULL)
  newCohortData <- unique(newCohortData, by = c("pixelGroup", "speciesCode"))

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(newCohortData$pixelGroup)], pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)
  specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, establishprob, ecoregionGroup)],
                               ecoregionGroup, speciesCode)
  newCohortData <- newCohortData[specieseco_current, nomatch = 0]
  newCohortData <- newCohortData[establishprob %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, NULL, c("establishprob"), NULL)
  if (P(sim)$calibrate == TRUE & NROW(newCohortData) > 0) {
    newCohortData_summ <- newCohortData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm, Year = round(time(sim)),
                                            numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[
      setkey(sim$species[, .(species,speciesCode)], speciesCode),
      nomatch = 0][, .(species, seedingAlgorithm, Year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }
  if (NROW(newCohortData) > 0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

UniversalDispersalSeeding <- function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- which(getValues(pixelGroupMap) == 0)
  if (length(fire_nonRegPixels) > 0) {
    pixelGroupMap[fire_nonRegPixels] <- getValues(sim$ecoregionMap)[fire_nonRegPixels] %>%
      as.factor() %>%
      as.integer() %>%
      `+`(maxValue(pixelGroupMap))
  }
  if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- sim$calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  species <- sim$species
  # all species can provide seed source, i.e. age>=sexualmature
  speciessource <- setkey(sim$species[, .(speciesCode, k = 1)], k)
  siteShade <- data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                        sim$speciesEcoregion, sim$minRelativeB))
  activePixelGroup <- unique(data.table(pixelGroup = getValues(pixelGroupMap)[tempActivePixel],
                                        ecoregionGroup = sim$ecoregionMap[tempActivePixel]),
                             by = "pixelGroup")
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table()
  siteShade[is.na(siteShade), siteShade := 0]
  setkey(siteShade[, k := 1], k)
  # i believe this is the latest version how the landis guys calculate sufficient light
  # http://landis-extensions.googlecode.com/svn/trunk/succession-library/trunk/src/ReproductionDefaults.cs
  newCohortData <- siteShade[speciessource, allow.cartesian = TRUE][, k := NULL]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[, .(speciesCode, shadetolerance)],
                                                             speciesCode),
                                                      nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0 , 1),]
  set(newCohortData, NULL, c("siteShade", "lightProb", "shadetolerance"), NULL)
  #   pixelGroupEcoregion <- unique(sim$cohortData, by = c("pixelGroup"))[,'.'(pixelGroup, sumB)]

  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(newCohortData$pixelGroup)], pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)
  specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, establishprob, ecoregionGroup)],
                               ecoregionGroup, speciesCode)
  newCohortData <- newCohortData[specieseco_current, nomatch = 0]
  newCohortData <- newCohortData[establishprob %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, NULL, "establishprob", NULL)
  if (P(sim)$calibrate == TRUE) {
    newCohortData_summ <- newCohortData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm,
                                            Year = round(time(sim)),
                                            numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[
      setkey(sim$species[, .(species, speciesCode)], speciesCode),
      nomatch = 0][, .(species, seedingAlgorithm, Year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }
  if (NROW(newCohortData) > 0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

WardDispersalSeeding <- function(sim) {
  #   cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- which(getValues(pixelGroupMap) == 0)
  if (length(fire_nonRegPixels) > 0) {
    pixelGroupMap[fire_nonRegPixels] <- getValues(sim$ecoregionMap)[fire_nonRegPixels] %>%
      as.factor() %>%
      as.integer() %>%
      `+`(max(sim$cohortData$pixelGroup))
  }
  if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- sim$calculateSumB(cohortData = sim$cohortData,
                                  lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = P(sim)$successionTimestep)
  siteShade <- calcSiteShade(time = round(time(sim)), cohortData = sim$cohortData,
                             sim$speciesEcoregion, sim$minRelativeB)
  activePixelGroup <- data.table(pixelGroup = unique(getValues(pixelGroupMap)[tempActivePixel]))
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table()
  siteShade[is.na(siteShade),siteShade := 0]
  # Seed source cells:
  # 1. Select only sexually mature cohorts, then
  # 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[, .(speciesCode, sexualmature)],
                                                               speciesCode),
                                                        nomatch = 0]
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
    seedReceive <- seedReceive[lightProb %>>% runif(nrow(seedReceive), 0, 1), ][
      , .(pixelGroup, speciesCode, seeddistance_eff, seeddistance_max)]
    setkey(seedReceive, speciesCode)

    # 3. Remove any species from the seedSource that couldn't regeneration anywhere on the map due to insufficient light
    #    (info contained within seedReceive)
    # this is should be a inner join, needs to specify the nomatch=0, nomatch = NA is default that sugest the full joint.
    seedSource <- seedSource[speciesCode %in% unique(seedReceive$speciesCode),]

    # Add inSituReceived data.table from the inSitu seeding function or event
    inSituReceived <- data.table(fromInit = numeric(), species = character())

    # it could be more effecient if pixelGroupMap is reduced map by removing the pixels that have successful postdisturbance regeneration
    # and the inactive pixels
    # how to subset the reducedmap
    if (sim$lastFireYear == round(time(sim))) { # the current year is both fire year and succession year
      inactivePixelIndex <- c(sim$inactivePixelIndex, sim$postFirePixel)
    } else {
      inactivePixelIndex <- sim$inactivePixelIndex
    }
    if (length(inactivePixelIndex) > 0) {
      reducedPixelGroupMap <- pixelGroupMap
      reducedPixelGroupMap[inactivePixelIndex] <- NA
    } else {
      reducedPixelGroupMap <- pixelGroupMap
    }

    seedingData <- LANDISDisp(sim, dtRcv = seedReceive, plot.it = FALSE,
                              dtSrc = seedSource, inSituReceived = inSituReceived,
                              species = sim$species,
                              reducedPixelGroupMap,
                              maxPotentialsLength = 1e5,
                              verbose = FALSE,
                              useParallel = P(sim)$.useParallel)

    rm(seedReceive, seedSource)
    if (NROW(seedingData) > 0) {
      seedingData[, ecoregionGroup := as.factor(getValues(sim$ecoregionMap)[seedingData$pixelIndex])]
      seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)
      specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
      specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                      .(speciesCode, establishprob, ecoregionGroup)],
                                   ecoregionGroup, speciesCode)
      seedingData <- seedingData[specieseco_current, nomatch = 0]

      seedingData <- seedingData[establishprob >= runif(nrow(seedingData), 0, 1), ]
      set(seedingData, NULL, "establishprob", NULL)
      if (P(sim)$calibrate == TRUE) {
        seedingData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm, Year = round(time(sim)),
                                            numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
        seedingData_summ <- setkey(seedingData_summ, speciesCode)[setkey(sim$species[, .(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][, .(species, seedingAlgorithm,
                                                                                   Year, numberOfReg)]
        sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, seedingData_summ))
      }
      if (nrow(seedingData) > 0) {
        addnewcohort <- addNewCohorts(seedingData, cohortData = sim$cohortData, pixelGroupMap,
                                      time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
        sim$cohortData <- addnewcohort$cohortData
        sim$pixelGroupMap <- addnewcohort$pixelGroupMap
      }
    }
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

summaryRegen <- function(sim) {
  #cohortData <- sim$cohortData
  if (!any(is.na(P(sim)$.plotInitialTime)) | !any(is.na(P(sim)$.saveInitialTime))) {
    pixelGroupMap <- sim$pixelGroupMap
    names(pixelGroupMap) <- "pixelGroup"
    # please note that the calculation of reproduction is based on successioinTime step interval,
    pixelAll <- sim$cohortData[age <= P(sim)$successionTimestep + 1,
                               .(uniqueSumReproduction = as.integer(sum(B, na.rm = TRUE))),
                               by = pixelGroup]
    if (NROW(pixelAll) > 0) {
      reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumReproduction")
      setColors(reproductionMap) <- c("light green", "dark green")
    } else {
      reproductionMap <- setValues(pixelGroupMap, 0L)
    }
    rm(pixelAll)
    sim$reproductionMap <- reproductionMap
    #rm(cohortData, pixelGroupMap)
    rm(pixelGroupMap)
  }
  return(invisible(sim))
}

summaryBySpecies <- function(sim) {
  thisPeriod <- sim$cohortData[, list(year = time(sim), BiomassBySpecies = mean(B)), by = speciesCode]
  if (is.null(sim$summaryBySpecies)) {
    sim$summaryBySpecies <- thisPeriod
  } else {
    sim$summaryBySpecies <- rbindlist(list(sim$summaryBySpecies, thisPeriod))
  }

  freqs <- table(na.omit(factorValues2(sim$vegTypeMap, sim$vegTypeMap[], att = 2)))
  tabl <- as.vector(freqs)
  summaryBySpecies1 <- data.frame(year = rep(floor(time(sim)), length(freqs)),
                                  leadingType = names(freqs),
                                  #freqs = freqs,
                                  counts = tabl, stringsAsFactors = FALSE)

  whMixedLeading <- which(summaryBySpecies1$leadingType == "Mixed")
  summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType,
                                                  sim$sppEquiv,
                                                  "EN_generic_short")
  summaryBySpecies1$leadingType[whMixedLeading] <- "Mixed"

  colours <- equivalentName(names(sim$sppColors), sim$sppEquiv, "EN_generic_short")
  whMixedSppColors <- which(names(sim$sppColors) == "Mixed")
  colours[whMixedSppColors] <- "Mixed"

  colorIDs <- match(summaryBySpecies1$leadingType, colours)
  summaryBySpecies1$cols <- sim$sppColors[colorIDs]

  if (is.null(sim$summaryBySpecies1)) {
    sim$summaryBySpecies1 <- summaryBySpecies1
  } else {
    sim$summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, summaryBySpecies1))
  }

  if (length(unique(sim$summaryBySpecies1$year)) > 1) {
    df <- sim$species[, list(speciesCode, species)][sim$summaryBySpecies, on = "speciesCode"]
    df$species <- equivalentName(df$species, sim$sppEquiv, "EN_generic_short")

    colorIDs <- match(df$species, colours)
    df$cols <- sim$sppColors[colorIDs]

    cols2 <- df$cols
    names(cols2) <- df$species
    plot2 <- ggplot(data = df, aes(x = year, y = BiomassBySpecies, fill = species)) +
      scale_fill_manual(values = cols2) +
      geom_area(position = "stack") +
      labs(x = "Year", y = "Biomass by species") +
      theme(legend.text = element_text(size = 6), legend.title = element_blank())

    title2 <- if (identical(time(sim), P(sim)$.plotInitialTime))
      "Average biomass by species" else ""
    Plot(plot2, title = title2, new = TRUE)
browser()
    maxNpixels <- sum(!is.na(rasterToMatchReporting))
    cols3 <- sim$summaryBySpecies1$cols
    names(cols3) <- sim$summaryBySpecies1$leadingType
    plot3 <- ggplot(data = sim$summaryBySpecies1, aes(x = year, y = counts, fill = leadingType)) +
      scale_fill_manual(values = cols3) +
      labs(x = "Year", y = "Count") +
      geom_area() +
      theme(legend.text = element_text(size = 6), legend.title = element_blank()) +
      geom_hline(yintercept = maxNpixels, linetype = "dashed", color = "darkgrey")

    title3 <- if (identical(time(sim), P(sim)$.plotInitialTime))
      "Number of pixels, by leading type" else ""
    Plot(plot3, title = title3, new = TRUE)
  }

  # means <- cbind(meanBiomass, meanANPP)
  # means <- melt(means)
  #
  # plot1 <- ggplot(data = means, aes(x = Var1, y = value, colour = Var2)) +
  #   geom_line(size = 1, show.legend = FALSE) + theme_bw() +
  #   facet_wrap(~ Var2, scales = "free_y") +
  #   labs(x = "Year", y = "Average value")
  #
  # Plot(plot1, title = c("Average biomass/ANPP"))

  return(invisible(sim))
}

plotVegAttributesMaps <- function(sim) {
  biomassMapForPlot <- raster::mask(sim$simulatedBiomassMap, sim$studyAreaReporting)
  ANPPMapForPlot <- raster::mask(sim$ANPPMap, sim$studyAreaReporting)
  mortalityMapForPlot <- raster::mask(sim$mortalityMap, sim$studyAreaReporting)
  reproductionMapForPlot <- if (is.null(sim$reproductionMap)) {
    NULL
  } else {
    raster::mask(sim$reproductionMap, sim$studyAreaReporting)
  }

  objsToPlot <- list(Biomass = biomassMapForPlot,
                     ANPP = ANPPMapForPlot,
                     mortality = mortalityMapForPlot,
                     reproduction = reproductionMapForPlot)

  # The ones we want
  sppEquiv <- sim$sppEquiv[!is.na(sim$sppEquiv[[P(sim)$sppEquivCol]]),]

  objsToPlot <- objsToPlot[!sapply(objsToPlot, is.null)]
  Plot(objsToPlot, new = TRUE) # not sure why, but errors if all 5 are put into one command

  levs <- raster::levels(sim$vegTypeMap)[[1]]
  levelsName <- names(levs)[2]
  # facVals <- pemisc::factorValues2(sim$vegTypeMap, sim$vegTypeMap[],
  #                                  att = levelsName,
  #                                  na.rm = TRUE)

  ## Doesn't change anything in the current default setting, but it does create
  ##  an NA where there is "Mixed".
  ## Other species in levs[[levelsName]] are already "Leading",
  ##  but it needs to be here in case it is not Leading in the future.
  levsLeading <- equivalentName(levs[[levelsName]], sppEquiv, "Leading")
  hasOnlyMixedAsOther <- sum(is.na(levsLeading) == 1) &&
    levs[[levelsName]][is.na(levsLeading)] == "Mixed"
  #extraValues <- setdiff(levs[[levelsName]], levsLeading)
  if (!isTRUE(hasOnlyMixedAsOther)) {
    stop("'plotVegAttributesMaps' in LBMR can only deal with 'Mixed' category or the ones in sim$sppEquiv")
  }

  whMixedLevs <- which(levs[[levelsName]] == "Mixed")
  whMixedSppColors <- which(names(sim$sppColors) == "Mixed")

  # Will return NA where there is no value, e.g., Mixed
  levsLeading[whMixedLevs] <- "Mixed"

  shortNames <- equivalentName(levsLeading, sppEquiv, "EN_generic_short")
  shortNames[whMixedLevs] <- "Mixed"
  levs[[levelsName]] <- shortNames
  levels(sim$vegTypeMap) <- levs

  colsLeading <- equivalentName(names(sim$sppColors), sppEquiv, "Leading")
  colsLeading[whMixedSppColors] <- "Mixed"
  sppColors <- sim$sppColors
  names(sppColors) <- colsLeading
  colours <- sppColors[na.omit(match(levsLeading, colsLeading))]
  setColors(sim$vegTypeMap, levs$ID) <- colours

  # Mask out NAs based on rasterToMatch (for plotting only!)
  vegTypeMapForPlot <- sim$vegTypeMap
  vegTypeMapForPlot[is.na(sim$rasterToMatchReporting[])] <- NA ## faster than raster::mask

  # Plot
  Plot(vegTypeMapForPlot, new = TRUE, title = "Leading vegetation")
  grid.rect(0.93, 0.97, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid.text(label = paste0("Year = ", round(time(sim))), x = 0.93, y = 0.97)
  return(invisible(sim))
}

plotAvgVegAttributes <- function(sim) {
  # only take the files in outputPath(sim) that were new since the startClockTime of the spades call
  biomassFiles <- list.files(outputPath(sim), pattern = "simulatedBiomassMap", full.names = TRUE)
  biomassKeepers <- file.info(biomassFiles)$atime > sim@.envir$._startClockTime

  biomass.stk <- lapply(biomassFiles[biomassKeepers], raster)

  ANPPFiles <- list.files(outputPath(sim), pattern = "ANPP", full.names = TRUE)
  ANPPKeepers <- file.info(ANPPFiles)$atime > sim@.envir$._startClockTime

  ANPP.stk <- lapply(ANPPFiles[ANPPKeepers], raster)
  meanBiomass <- sapply(biomass.stk, FUN <- function(x) mean(x[], na.rm = TRUE))
  names(meanBiomass) = sub(".tif", "",  sub(".*simulatedBiomass_Year", "",
                                            basename(biomassFiles[biomassKeepers])))

  meanANPP <- sapply(ANPP.stk, FUN <- function(x) mean(x[], na.rm = TRUE))
  names(meanANPP) = sub(".tif", "", sub(".*ANPP_Year", "", basename(ANPPFiles[ANPPKeepers])))

  means <- cbind(meanBiomass, meanANPP)
  means <- melt(means)

  plot1 <- ggplot(data = means, aes(x = Var1, y = value, colour = Var2)) +
    geom_line(size = 1, show.legend = FALSE) + theme_bw() +
    facet_wrap(~ Var2, scales = "free_y") +
    labs(x = "Year", y = "Average value")

  Plot(plot1, title = c("Average biomass/ANPP"))
  return(invisible(sim))
}

Save <- function(sim) {
  raster::projection(sim$simulatedBiomassMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$ANPPMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$mortalityMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$reproductionMap) <- raster::projection(sim$ecoregionMap)
  writeRaster(sim$simulatedBiomassMap,
              file.path(outputPath(sim), paste("biomassMap_Year", round(time(sim)), ".tif", sep = "")),
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

## DEFAULT INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  ######################################################
  ## Check GM functions have been supplied
  if (!suppliedElsewhere("calculateAgeMortality", sim) |
      !suppliedElsewhere("calculateANPP", sim) |
      !suppliedElsewhere("calculateCompetition", sim) |
      !suppliedElsewhere("calculateGrowthMortality", sim) |
      !suppliedElsewhere("calculateSumB", sim) |
      !suppliedElsewhere("updateSpeciesAttributes", sim) |
      !suppliedElsewhere("updateSpeciesEcoregionAttributes", sim)) {
    stop("Growth and mortality (GM) function(s) missing.\n
         Make sure you are using LandR_BiomassGMOrig, or another GM module")
  }

  #######################################################

  if (is.null(sim$rasterToMatch)) {
    if (!suppliedElsewhere("rasterToMatch", sim)) {
      stop("There is no 'rasterToMatch' supplied")
    }
  }

  if (!suppliedElsewhere("rasterToMatchReporting")) {
    sim$rasterToMatchReporting <- sim$rasterToMatch
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon in southwestern Alberta, Canada,")

    sim$studyArea <- randomStudyArea(seed = 1234)
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    message("'studyAreaReporting' was not provided by user. Using the same as 'studyArea'.")
    sim$studyAreaReporting <- sim$studyArea
  }

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
      initialCommunities[i,
                         desc := paste(initialCommunities[i, 3:maxcol, with = FALSE],
                                       collapse = " ")]
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

  # load the initial community map
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

  ## load the biomass_succession.txt and obtain:
  ##    1) minRelativeB;
  ##    2) sufficientLight; and
  ##    3) additional species traits.
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim)) |
      (!suppliedElsewhere("minRelativeB", sim))) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL

    sim$species <- prepInputsSpecies(url = extractURL("species"), dPath, cacheTags)
  }

  if (!suppliedElsewhere("ecoregion", sim)) {
    sim$ecoregion <- prepInputsEcoregion(url = extractURL("ecoregion"),
                                         dPath = dPath, cacheTags = cacheTags)
  }

  ######################################################
  ## load ecoregion map
  if (!suppliedElsewhere("ecoregionMap", sim )) {
    ## LANDIS-II demo data:

    ## TODO: restore the demo data version with prepInputs:
    # sim$ecoregionMap <- Cache(prepInputs,
    #                           url = extractURL("ecoregionMap"),
    #                           destinationPath = dPath,
    #                           targetFile = "ecoregions.gis",
    #                           fun = "raster::raster")

    ## Dummy version with spatial location in Canada
    ras <- projectExtent(sim$studyArea, crs = sim$studyArea)
    res(ras) <- 250 ## TODO: don't hardcode this; get from rasterToMatch?
    ecoregionMap <- rasterize(sim$studyArea, ras) ## TODO: use fasterize

    ## make uniform communities (well structured in space)
    mapvals <- rep(unique(ecoregion$mapcode),
                   each = ceiling(sum(!is.na(getValues(ecoregionMap))) /
                                    length(unique(ecoregion$mapcode))))
    mapvals <- mapvals[1:sum(!is.na(getValues(ecoregionMap)))] ## remove any extra values

    ## assign communities to map and export to sim
    ecoregionMap[!is.na(getValues(ecoregionMap))][] <- mapvals

    sim$ecoregionMap <- ecoregionMap
  }

  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }

  if (!suppliedElsewhere("minRelativeB", sim)) {
    minRelativeB <- data.frame(mainInput)
    startRow <- which(minRelativeB$col1 == "MinRelativeBiomass")
    minRelativeB <- minRelativeB[(startRow + 1):(startRow + 6),]
    minRelativeB[1, 2:ncol(minRelativeB)] <- minRelativeB[1, 1:(ncol(minRelativeB) - 1)]
    names(minRelativeB) <- NULL
    minRelativeB <- minRelativeB[, apply(minRelativeB, 2, function(x) all(nzchar(x)))]
    minRelativeB <- minRelativeB[, -1] %>%
      t(.) %>%
      gsub(pattern = "%", replacement = "") %>%
      data.table()

    colNames <- c("ecoregion", "X1", "X2", "X3", "X4", "X5")
    names(minRelativeB) <- colNames
    minRelativeB[, (colNames[-1]) := lapply(.SD, function(x)
      as.numeric(as.character(x))), .SDcols = colNames[-1]]
    # minRelativeB <- minRelativeB %>%
    #   mutate_at(funs(as.numeric(as.character(.))/100), .vars=-ecoregion)
    sim$minRelativeB <- minRelativeB
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
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
    data("sppEquivalencies_CA", package = "pemisc", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA)

    ## By default, Abies_las is renamed to Abies_sp
    sim$sppEquiv[KNN == "Abie_Las", LandR := "Abie_sp"]

    ## add default colors for species used in model
    if (!is.null(sim$sppColors))
      stop("If you provide sppColors, you MUST also provide sppEquiv")
    sim$sppColors <- pemisc::sppColors(sim$sppEquiv, P(sim)$sppEquivCol,
                                       newVals = "Mixed", palette = "Accent")
  }

  return(invisible(sim))
}

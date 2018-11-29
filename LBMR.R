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
                  "PredictiveEcology/pemisc",
                  "PredictiveEcology/reproducible@development",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development"),
  parameters = rbind(
    defineParameter("growthInitialTime", "numeric", 0, NA_real_, NA_real_,
                    desc = "Initial time for the growth event to occur"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA,
                    desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = paste("This describes the simulation time at which the first save event should occur.",
                                 "Set to NA if no saving is desired.")),
    defineParameter("calibrate", "logical", FALSE,
                    desc = "Do calibration? Defaults to FALSE"),
    defineParameter("growthInitialTime", "numeric", 0, NA_real_, NA_real_,
                    desc = "Initial time for the growth event to occur"),
    defineParameter("seedingAlgorithm", "character", "wardDispersal",
                    desc = paste("choose which seeding algorithm will be used among",
                                 "noDispersal, universalDispersal, and wardDispersal (default).")),
    defineParameter("spinupMortalityfraction", "numeric", 0.001,
                    desc = "defines the mortality loss fraction in spin up-stage simulation"),
    defineParameter("speciesEstablishmentProbAsMap", "logical", FALSE,
                    desc = paste("Should species establishment probability be represented at the pixel level,",
                                 "as a rescaled map of original species percent cover")),
    defineParameter("speciesEquivalency", "data.frame", NA,
                    desc = "species equivalency table as in pemisc::sppEquivalencies_CA TODO: descitpion needed"),
    defineParameter("successionTimestep", "numeric", 10, NA, NA, "defines the simulation time step, default is 10 years"),
    defineParameter("useCache", "logic", TRUE,
                    desc = "use caching for the spinup simulation?"),
    defineParameter("useParallel", "ANY", parallel::detectCores(),
                    desc = paste("Used only in seed dispersal.",
                                 "If numeric, it will be passed to data.table::setDTthreads, ",
                                 "if logical and TRUE, it will be passed to parallel::makeCluster, ",
                                 "and if cluster object it will be passed to parallel::parClusterApplyLB."))
  ),
  inputObjects = bind_rows(
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
    expectsInput("shpStudyArea", "SpatialPolygonsDataFrame",
                 desc = "Study area used to source any objects that are not supplied",
                 sourceURL = NA),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),

    ## for inputs from optional fire module:
    expectsInput("spinUpCache", "logical", ""),
    expectsInput("speciesEstablishmentProbMap", "RasterBrick", "Species establishment probability as a RasterBrick, one layer for each species"),
    expectsInput("speciesEquivalency", "data.frame", "")
  ),
  outputObjects = bind_rows(
    createsOutput("activeEcoregionLength", "data.table",
                  desc = "internal use. Keeps track of the length of the ecoregion"),
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
    createsOutput(objectName = "cutpoint", objectClass = "numeric",
                  desc = "A numeric scalar indicating how large each chunk of an internal data.table with processing by chuncks"),
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
  if (is.numeric(P(sim)$useParallel)) {
    a <- data.table::setDTthreads(P(sim)$useParallel)
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
                                "LBMR", "plot", eventPriority = 7)

           if (!any(is.na(P(sim)$.saveInitialTime))) {
             sim <- scheduleEvent(sim, P(sim)$.saveInitialTime + P(sim)$successionTimestep,
                                  "LBMR", "save", eventPriority = 8.5)
             ## stats plot is retrieving saved rasters so needs data to be saved
             # start on second time around b/c ggplot doesn't like 1 data point
             tPlotInit <- P(sim)$.plotInitialTime + 2*P(sim)$successionTimestep
             sim <- scheduleEvent(sim, tPlotInit, "LBMR", "statsPlot", eventPriority = 7.75)
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
         plot = {
           sim <- plotFn(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "plot", eventPriority = 8)
         },
         save = {
           sim <- Save(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "save", eventPriority = 8.5)
         },
         statsPlot = {
           ## only occurs once at the end of the simulation
           sim <- statsPlotFn(sim)
           sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                                "LBMR", "statsPlot", eventPriority = 8.75)
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  sim$cutpoint <- 1e10
  communities <- sim$initialCommunities %>%
    gather(key = cohort, value = age, -mapcode, -description, -species, na.rm = TRUE) %>%
    data.table() %>% ## this needs to be here to appease the data.table gods
    .[, ':='(age = as.integer(ceiling(as.numeric(age) / P(sim)$successionTimestep) *
                                P(sim)$successionTimestep),
             communityGroup = as.integer(mapcode),
             mapcode = NULL)] %>%
    unique(., by = c("communityGroup", "species", "age"))
  species <- data.table(sim$species)[, speciesCode := as.integer(as.factor(species))]
  tempspecies <- setkey(species[, .(species, speciesCode)], species)
  communities <- setkey(communities, species)[tempspecies, nomatch = 0]
  speciesEcoregion <- setkey(data.table(sim$speciesEcoregion), species)[tempspecies, nomatch = 0]
  sim$species <- setkey(species, speciesCode)
  ecoregion <- data.table(sim$ecoregion)[, ecoregionGroup := as.integer(mapcode)]
  ecoregion_temp <- setkey(ecoregion[, .(ecoregion, ecoregionGroup)], ecoregion)
  sim$minRelativeB <- data.table(sim$minRelativeB, key = "ecoregion")[ecoregion_temp, nomatch = 0]
  speciesEcoregion <- setkey(speciesEcoregion, ecoregion)[ecoregion_temp, nomatch = 0]
  sim$speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)
  nrowCommunities <- nrow(communities) #line 197 in Yong code
  initialCommunitiesMap <- setValues(sim$initialCommunitiesMap, as.integer(sim$initialCommunitiesMap[]))
  napixels <- which(is.na(getValues(initialCommunitiesMap)))
  initialCommunitiesMap[napixels] <- as.integer(maxValue(initialCommunitiesMap) + 1)
  pixelGroupFactor <- as.integer(10^ceiling(log10((maxValue(initialCommunitiesMap) + 1))))
  #ecoregionMap <- sim$ecoregionMap
  pixelGroupMap <- setValues(initialCommunitiesMap, as.integer((initialCommunitiesMap +
                                                                  sim$ecoregionMap*pixelGroupFactor)[]))
  sim$initialCommunitiesMap <- NULL
  active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, ecoregionGroup)], k)
  cohortData <- setkey(communities[, k := 1], k)[active_ecoregion, allow.cartesian = TRUE][, k := NULL]
  set(cohortData, NULL, "pixelGroup", cohortData$communityGroup + cohortData$ecoregionGroup*pixelGroupFactor)
  set(cohortData, NULL, "B", as.integer(0L))
  cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B)] # removed communityGroup column
  # the cohortData here is a full joint table of community Group and ecoregion Group
  # some redundant pixelGroups are removed, because they are not present on the pixelGroupMap
  # we are dealing with the case that all the ecoregion is active, how about some ecoregion is not active
  activePixelIndex <- which(getValues(sim$ecoregionMap) %in% active_ecoregion$ecoregionGroup)
  inactivePixelIndex <- seq(from = 1, to = ncell(sim$ecoregionMap))[
    (seq(from = 1, to = ncell(sim$ecoregionMap)) %in% activePixelIndex) == FALSE]
  sim$activeEcoregionLength <- data.table(Ecoregion = getValues(sim$ecoregionMap),
                                          pixelIndex = 1:ncell(sim$ecoregionMap))[
    Ecoregion %in% active_ecoregion$ecoregionGroup, .(NofCell = length(pixelIndex)), by = Ecoregion]
  sim$activePixelIndex <- activePixelIndex # store this for future use
  sim$inactivePixelIndex <- inactivePixelIndex # store this for future use
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[activePixelIndex]),]
  rm(nrowCommunities, pixelGroupFactor)
  # pixels with -1 in the pixelGroupMap are inactive
  if (length(inactivePixelIndex) > 0) {
    pixelGroupMap[inactivePixelIndex] <- -1L
  }
  cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                 time = round(time(sim)), cohortData = cohortData)
  cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = cohortData)

  #sim <- cacheSpinUpFunction(sim, cachePath = outputPath(sim))
  message("Running spinup")
  spinupstage <- Cache(spinUp, cohortData = cohortData, calibrate = P(sim)$calibrate,
                       successionTimestep = P(sim)$successionTimestep,
                       spinupMortalityfraction = P(sim)$spinupMortalityfraction,
                       species = sim$species, userTags = c("LBMR", "spinUp"))

  cohortData <- spinupstage$cohortData
  if (P(sim)$calibrate) {
    sim$spinupOutput <- spinupstage$spinupOutput
  }
  if (P(sim)$calibrate) {
    sim$simulationTreeOutput <- data.table(Year = numeric(), siteBiomass = numeric(), Species = character(),
                                           Age = numeric(), iniBiomass = numeric(), ANPP = numeric(),
                                           Mortality = numeric(), deltaB = numeric(), finBiomass = numeric())
    sim$regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                         Year = numeric(), numberOfReg = numeric())
  }
  names(pixelGroupMap) <- "pixelGroup"
  pixelAll <- cohortData[, .(uniqueSumB = as.integer(sum(B, na.rm = TRUE))), by = pixelGroup]
  if (!any(is.na(P(sim)$.plotInitialTime)) | !any(is.na(P(sim)$.saveInitialTime))) {
    simulatedBiomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
    #ANPPMap <- setValues(simulatedBiomassMap, 0L)
    #mortalityMap <- setValues(simulatedBiomassMap, 0L)
    #reproductionMap <- setValues(pixelGroupMap, 0L)
  }

  #}

  sim$pixelGroupMap <- pixelGroupMap
  sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                   B, mortality = 0, aNPPAct = 0)]
  simulationOutput <- data.table(Ecoregion = getValues(sim$ecoregionMap),
                                 pixelGroup = getValues(pixelGroupMap),
                                 pixelIndex = 1:ncell(sim$ecoregionMap))[
                                   , .(NofPixel = length(pixelIndex)), by = c("Ecoregion", "pixelGroup")]
  simulationOutput <- setkey(simulationOutput, pixelGroup)[
    setkey(pixelAll, pixelGroup), nomatch = 0][
      , .(Biomass = sum(as.numeric(uniqueSumB*NofPixel))), by = Ecoregion] ## NOTE:
  ## above needs to be numeric because of integer overflow -- returned to integer in 2 lines
  simulationOutput <- setkey(simulationOutput, Ecoregion)[
    setkey(sim$activeEcoregionLength, Ecoregion), nomatch = 0]
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
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = sim$cutpoint), max(pixelGroups$temID))))
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
  tempOutput_All <- setkey(tempOutput_All, Ecoregion)[setkey(sim$activeEcoregionLength,
                                                             Ecoregion), nomatch = 0]
  sim$simulationOutput <- rbindlist(list(sim$simulationOutput,
                                         tempOutput_All[, .(Ecoregion, NofCell, Year = as.integer(time(sim)),
                                                            Biomass = as.integer(Biomass / NofCell),
                                                            ANPP = as.integer(ANPP / NofCell),
                                                            Mortality = as.integer(Mortality / NofCell),
                                                            Regeneration = as.integer(Regeneration / NofCell))]))
  # the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively.
  names(sim$pixelGroupMap) <- "pixelGroup"
  sim$biomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                     "uniqueSumB")
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

    sim$vegTypeMap <- vegTypeMapGenerator(sim$species, sim$cohortData, sim$pixelGroupMap,
                                          sim$vegLeadingProportion)
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
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = time(sim),
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
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = round(time(sim)),
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
  sim$cohortData <- calculateSumB(cohortData = sim$cohortData,
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
                              useParallel = P(sim)$useParallel)

    rm(seedReceive, seedSource)
    if (NROW(seedingData) > 0) {
      seedingData$ecoregionGroup <- getValues(sim$ecoregionMap)[seedingData$pixelIndex]
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
                               .(uniqueSumReproduction = as.integer(sum(B, na.rm=TRUE))),
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

  freqs <- table(na.omit(factorValues(sim$vegTypeMap, sim$vegTypeMap[], att = "Factor")[[1]]))
  tabl <- as.vector(freqs)
  summaryBySpecies1 <- data.frame(year = rep(floor(time(sim)), length(freqs)), leadingType = names(freqs),
                                  #freqs = freqs,
                                  counts = tabl, stringsAsFactors = FALSE)
  summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType, sim$speciesEquivalency, "shortNames")
  summaryBySpecies1$cols <- equivalentName(summaryBySpecies1$leadingType, sim$speciesEquivalency, "cols")

  if (is.null(sim$summaryBySpecies1)) {
    sim$summaryBySpecies1 <- summaryBySpecies1
  } else {
    sim$summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, summaryBySpecies1))
  }

  if (length(unique(sim$summaryBySpecies1$year)) > 1) {
    df <- sim$species[,list(speciesCode, species)][sim$summaryBySpecies, on = "speciesCode"]
    df$species <- equivalentName(df$species, sim$speciesEquivalency, "shortNames")
    df$cols <- equivalentName(df$species, sim$speciesEquivalency, "cols")

    cols2 <- df$cols
    names(cols2) <- df$species
    plot2 <- ggplot(data = df, aes(x = year, y = BiomassBySpecies, fill = species)) +
      scale_fill_manual(values = cols2) +
      geom_area(position = 'stack') +
      labs(x = "Year", y = "Biomass by species") +
      theme(legend.text = element_text(size = 6), legend.title = element_blank())

    Plot(plot2, title = c("Average biomass by species"))

    cols3 <- sim$summaryBySpecies1$cols
    names(cols3) <- sim$summaryBySpecies1$leadingType
    plot3 <- ggplot(data = sim$summaryBySpecies1, aes(x = year, y = counts, fill = leadingType)) +
      scale_fill_manual(values = cols3) +
      labs(x = "Year", y = "Count") +
      geom_area() +
      theme(legend.text = element_text(size = 6), legend.title = element_blank())

    Plot(plot3, title = c("Number of pixels, by leading type"), new = TRUE)
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

plotFn <- function(sim) {
  objsToPlot <- list(Biomass = sim$simulatedBiomassMap,
                     ANPP = sim$ANPPMap,
                     mortality = sim$mortalityMap,
                     reproduction = sim$reproducitionMap)
  objsToPlot <- objsToPlot[!sapply(objsToPlot, is.null)]
  Plot(objsToPlot, new = TRUE)
  # not sure why, but errors if all 5 are put into one command
  facVals <- pemisc::factorValues2(sim$vegTypeMap, sim$vegTypeMap[], att = "Factor",
                                   na.rm = TRUE)
  levs <- raster::levels(sim$vegTypeMap)[[1]]
  setColors(sim$vegTypeMap, levs) <- equivalentName(levs$Factor, sim$speciesEquivalency, "cols")
  levs$Factor <- equivalentName(levs$Factor, sim$speciesEquivalency, "shortNames")
  levels(sim$vegTypeMap) <- levs
  Plot(sim$vegTypeMap, new = TRUE, title = "Leading vegetation")
  grid.rect(0.93, 0.97, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid.text(label = paste0("Year = ",round(time(sim))), x = 0.93, y = 0.97)
  return(invisible(sim))
}

statsPlotFn <- function(sim) {
  # only take the files in outputPath(sim) that were new since the startClockTime of the spades call
  biomassFiles <- list.files(outputPath(sim), pattern = "simulatedBiomassMap", full.names = TRUE)
  biomassKeepers <- file.info(biomassFiles)$atime > sim@.envir$._startClockTime

  biomass.stk <- lapply(biomassFiles[biomassKeepers], raster)

  ANPPFiles <- list.files(outputPath(sim), pattern = "ANPP", full.names = TRUE)
  ANPPKeepers <- file.info(ANPPFiles)$atime > sim@.envir$._startClockTime

  ANPP.stk <- lapply(ANPPFiles[ANPPKeepers],
                     raster)
  meanBiomass <- sapply(biomass.stk, FUN <- function(x) mean(x[], na.rm = TRUE))
  names(meanBiomass) = sub(".tif", "",  sub(".*simulatedBiomass_Year", "",
                                            basename(biomassFiles[biomassKeepers])))

  meanANPP <- sapply(ANPP.stk, FUN <- function(x) mean(x[], na.rm = TRUE))
  names(meanANPP) = sub(".tif", "",
                        sub(".*ANPP_Year", "", basename(ANPPFiles[ANPPKeepers])))

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
              file.path(outputPath(sim), paste("ANPP_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  writeRaster(sim$mortalityMap,
              file.path(outputPath(sim), paste("mortalityMap_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  writeRaster(sim$reproductionMap,
              file.path(outputPath(sim), paste("reproductionMap_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  return(invisible(sim))
}

CohortAgeReclassification <- function(sim) {
  if (time(sim) != 0) {
    #cohortData <- sim$cohortData
    sim$cohortData <- ageReclassification(cohortData = sim$cohortData,
                                          successionTimestep = P(sim)$successionTimestep,
                                          stage = "mainSimulation")
    #sim$cohortData <- cohortData
    return(invisible(sim))
  } else {
    return(invisible(sim))
  }
}

### OTHER FUNCTIONS
spinUp <- function(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species) {
  maxAge <- max(cohortData$age) # determine the pre-simulation length
  set(cohortData, NULL, "origAge", cohortData$age)
  set(cohortData, NULL, c("age", "sumB"), as.integer(0L))
  set(cohortData, NULL, c("mortality", "aNPPAct"), as.numeric(0))
  if (calibrate) {
    spinupOutput <- data.table(pixelGroup = integer(), species = character(), age = integer(),
                               iniBiomass = integer(), ANPP = numeric(), Mortality = numeric(),
                               finBiomass = integer())
  }
  k <- 0
  if (successionTimestep == 1 & maxAge != 1) {
    presimuT_end <- 2
  } else {
    presimuT_end <- 1
  }

  for (presimuT in (maxAge):presimuT_end) {
    message("Spin up time: year ", -presimuT)
    k <- k + 1
    cohortData[origAge == presimuT, age := 1L]
    cohortData[origAge >= presimuT, age := age + 1L]

    if (successionTimestep != 1 &
        as.integer(k/successionTimestep) == k/successionTimestep) {
      cohortData <- ageReclassification(cohortData = cohortData,
                                        successionTimestep = successionTimestep,
                                        stage = "spinup")
    }
    # 1. assign the biomass for the first cohort
    if (nrow(cohortData[age == 2, ]) > 0) {
      lastReg <- k - 1
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k,
                                  successionTimestep = successionTimestep)
      cohortData[age == 2, B := as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[age == 2, B := as.integer(pmin(maxANPP, B))]
    }
    if (maxAge != 1) {
      # 2. calculate age-related mortality
      cohortData <- calculateAgeMortality(cohortData, stage = "spinup",
                                          spinupMortalityfraction = spinupMortalityfraction)
      # 3. calculate the actual ANPP
      # calculate biomass Potential, for each cohort
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k - 1,
                                  successionTimestep = successionTimestep)
      cohortData <- calculateCompetition(cohortData, stage = "spinup")
      # calculate ANPP
      cohortData <- calculateANPP(cohortData, stage = "spinup")
      cohortData[age > 0, aNPPAct := pmax(1, aNPPAct - mAge)]
      # calculate growth related mortality
      cohortData <- calculateGrowthMortality(cohortData, stage = "spinup")
      cohortData[age > 0, mBio := pmax(0, mBio - mAge)]
      cohortData[age > 0, mBio := pmin(mBio, aNPPAct)]
      cohortData[age > 0, mortality := mBio + mAge]
      cohortData[age > 0, B := as.integer(B + as.integer(aNPPAct - mortality))]
      set(cohortData, NULL, c("bPM", "mBio"), NULL)
    }
    if (calibrate) {
      if (maxAge != 1) {
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = B + as.integer(mortality - aNPPAct),
                                                      ANPP = round(aNPPAct, 1),
                                                      Mortality = round(mortality, 1),finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[, .(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      , species := NULL]

        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
        cohortData[,':='(bAP = NULL)]
      } else {
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = 0, ANPP = 0,
                                                      Mortality = 0, finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[, .(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      , species := NULL]

        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
      }
    }
    lastnewcohorts <- which(cohortData$origAge == 1)
    if (presimuT == presimuT_end & length(lastnewcohorts) > 0 & maxAge != 1) {
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k,
                                  successionTimestep = successionTimestep)
      cohortData[origAge == 1,B := as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[origAge == 1,B := as.integer(pmin(maxANPP, B))]
    }
  }
  cohortData[, ':='(age = origAge, origAge = NULL)]
  if (calibrate) {
    all <- list(cohortData = cohortData, spinupOutput = spinupOutput)
  } else {
    all <- list(cohortData = cohortData)
  }
  return(all)
}

# cacheSpinUpFunction <- function(sim, cachePath) {
#   # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
#   if (P(sim)$useCache) {
#     sim$spinUpCache <- function(...) {
#       reproducible::Cache(FUN = spinUp, ...)
#     }
#   } else {
#     # Step 3 - create a non-caching version in case caching is not desired
#     #  sim$spinUp <- sim$spinUpRaw
#     sim$spinUpCache <- spinUp
#   }
#   return(invisible(sim))
# }

updateSpeciesEcoregionAttributes <- function(speciesEcoregion, time, cohortData) {
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]

  cohortData <- setkey(cohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch = 0]
  return(cohortData)
}

updateSpeciesAttributes <- function(species, cohortData) {
  # to assign longevity, mortalityshape, growthcurve to cohortData
  species_temp <- setkey(species[, .(speciesCode, longevity, mortalityshape,
                                     growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch = 0]
  return(cohortData)
}

ageReclassification <- function(cohortData, successionTimestep, stage) {
  if (stage == "spinup") {
    # for spin up stage
    cohortData[age == successionTimestep + 1, age := successionTimestep]
  } else {
    # non- spinup stage
    targetData <- cohortData[age <= (successionTimestep + 1), ]
    targetData <- targetData[, .(ecoregionGroup = mean(ecoregionGroup),
                                 age = successionTimestep + 1,
                                 B = sum(B, na.rm = TRUE),
                                 mortality = sum(mortality, na.rm = TRUE),
                                 aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                             by = .(pixelGroup, speciesCode)]
    targetData <- targetData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                 B, mortality, aNPPAct)]
    cohortData <- cohortData[age >= successionTimestep + 2]
    cohortData <- rbindlist(list(cohortData, targetData))
  }
  return(cohortData)
}

calculateAgeMortality <- function(cohortData, stage, spinupMortalityfraction) {
  # for age-related mortality calculation
  if (stage == "spinup") {
    cohortData[age > 0, mAge := B*(exp((age)/longevity*mortalityshape)/exp(mortalityshape))]
    cohortData[age > 0, mAge := mAge+B*spinupMortalityfraction]
    cohortData[age > 0, mAge := pmin(B, mAge)]
  } else {
    set(cohortData, NULL, "mAge",
        cohortData$B*(exp((cohortData$age)/cohortData$longevity*cohortData$mortalityshape)/exp(cohortData$mortalityshape)))
    set(cohortData, NULL, "mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

calculateANPP <- function(cohortData, stage) {
  if (stage == "spinup") {
    cohortData[age > 0, aNPPAct := maxANPP*exp(1)*(bAP^growthcurve)*exp(-(bAP^growthcurve))*bPM]
    cohortData[age > 0, aNPPAct := pmin(maxANPP*bPM,aNPPAct)]
  } else {
    set(cohortData, NULL, "aNPPAct",
        cohortData$maxANPP*exp(1)*(cohortData$bAP^cohortData$growthcurve)*exp(-(cohortData$bAP^cohortData$growthcurve))*cohortData$bPM)
    set(cohortData, NULL, "aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM,cohortData$aNPPAct))
  }
  return(cohortData)
}

calculateGrowthMortality <- function(cohortData, stage) {
  if (stage == "spinup") {
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP) / (1 + bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, NULL, "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, NULL, "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
}

calculateSumB <- function(cohortData, lastReg, simuTime, successionTimestep) {
  # this function is used to calculate total stand biomass that does not include the new cohorts
  # the new cohorts are defined as the age younger than simulation time step
  # reset sumB
  pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup),
                            temID = 1:length(unique(cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
  if (length(cutpoints) == 1) {cutpoints <- c(cutpoints, cutpoints + 1)}
  pixelGroups[, groups := cut(temID, breaks = cutpoints,
                              labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                              include.lowest = TRUE)]
  for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
    subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    set(subCohortData, NULL, "sumB", 0L)
    if (simuTime == lastReg + successionTimestep - 2) {
      sumBtable <- subCohortData[age > successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    } else {
      sumBtable <- subCohortData[age >= successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    }
    subCohortData <- merge(subCohortData, sumBtable, by = "pixelGroup", all.x = TRUE)
    subCohortData[is.na(tempsumB), tempsumB := as.integer(0L)][, ':='(sumB = tempsumB, tempsumB = NULL)]
    if (subgroup == "Group1") {
      newcohortData <- subCohortData
    } else {
      newcohortData <- rbindlist(list(newcohortData, subCohortData))
    }
    rm(subCohortData, sumBtable)
  }
  rm(cohortData, pixelGroups, cutpoints)
  return(newcohortData)
}

calculateCompetition <- function(cohortData,stage) {
  # two competition indics are calculated bAP and bPM
  if (stage == "spinup") {
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, NULL, "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier), by = pixelGroup]
    cohortData[age > 0, bPM := cMultiplier / cMultTotal]
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, NULL, "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))
    set(cohortData, NULL, "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, NULL, "bPot", NULL)
    set(cohortData, NULL, "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))
    cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
    set(cohortData, NULL, "bPM", cohortData$cMultiplier/cohortData$cMultTotal)
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  }
  return(cohortData)
}

calcSiteShade <- function(time, cohortData, speciesEcoregion, minRelativeB) {
  # the siteshade was calculated based on the codes:
  # https://github.com/LANDIS-II-Foundation/Extensions-Succession/blob/master/biomass-succession/trunk/src/PlugIn.cs
  if (nrow(cohortData[age > 5,]) > 0) {
    bAMterm1 <- cohortData[age > 5, ':='(prevMortality = sum(mortality, na.rm = TRUE),
                                         sumB = sum(B, na.rm = TRUE)),
                            by = .(pixelGroup, ecoregionGroup)]
    bAMterm1[is.na(sumB), sumB := 0]
    bAMterm1[is.na(prevMortality), prevMortality := 0]
    bAMterm1 <- unique(bAMterm1, by = c("pixelGroup", "ecoregionGroup"))
    set(cohortData, NULL, "prevMortality", NULL)
  } else {
    bAMterm1 <- unique(cohortData, by = c("pixelGroup", "ecoregionGroup"))[
      , .(pixelGroup, ecoregionGroup)][
        , ':='(prevMortality = 0, sumB = 0)]
  }
  #bAM <- data.table(speciesEcoregion)[year <= time(sim) & (year > (time(sim)-P(sim)$successionTimestep))]
  bAM <- speciesEcoregion[year <= time]
  bAM <- bAM[year == max(bAM$year)]
  bAM <- bAM[, .(maxMaxB = max(maxB)), by = ecoregionGroup]
  setkey(bAM, ecoregionGroup)
  setkey(bAMterm1, ecoregionGroup)
  bAMterm1 <- bAMterm1[bAM, nomatch = 0]
  bAMterm1[, sumB := pmin((maxMaxB - prevMortality), sumB)]
  bAMterm1[, bAM := sumB/maxMaxB]
  minRelativeB <- data.table(minRelativeB)
  setkey(minRelativeB, ecoregionGroup)
  bAMterm1 <- bAMterm1[minRelativeB, nomatch = 0]
  bAMterm1$bAM <- round(bAMterm1$bAM, 3)
  bAMterm1[,siteShade := cut(bAM,sort(unique(c(0, X1, X2, X3, X4, X5, 1))),
                             labels = FALSE, right = FALSE, include.lowest = TRUE) - 1, by = pixelGroup]
  bAMterm1 <- bAMterm1[, .(pixelGroup, siteShade)]
  return(bAMterm1)
}

assignLightProb <- function(sufficientLight, newCohortData) {
  ## for each line, get the survival probability from sufficientLight table
  ## note that sufficentLight is a table of survival probs for each tolerance level (row) by and shade level (column)
  ## siteShade + 2 is necessary to skip the first column
  newCohortData[ , lightProb := sufficientLight[cbind(shadetolerance, siteShade + 2)]]
}

addNewCohorts <- function(newCohortData, cohortData, pixelGroupMap, time, speciesEcoregion) {
  # this function is for 1) adding new cohort data into cohortdata
  # 2) assign initial biomass and age for new cohort
  # 3) assign the new pixelgroup to the pixels that have new cohort
  # 4) update the pixelgroup map
  # newCohortData must have the original pixelgroup, regenerated species and pixelindex
  # it also would be better if it has the columns of cohortData plus pixelIndex
  newCohortData$pixelGroup <- getValues(pixelGroupMap)[newCohortData$pixelIndex]
  set(newCohortData, NULL, "temppixelGroup", as.integer(as.factor(newCohortData$pixelGroup)))
  set(newCohortData, NULL, "speciesposition", 2^(newCohortData$speciesCode))
  # newCohortDataExtra is used to connect the original pixelGroup to the newPixelGroup
  # communities are any unique combinations of species
  newCohortDataExtra <- newCohortData[, .(community = sum(speciesposition),
                                          pixelGroup = mean(pixelGroup),
                                          temppixelGroup = mean(temppixelGroup)), by = pixelIndex]
  set(newCohortData, NULL, c("temppixelGroup", "speciesposition"), NULL)
  set(newCohortDataExtra, NULL, "community",
      as.integer(as.factor(newCohortDataExtra$community)))
  ## make unique ids for the combination of communities and pix groups
  ## if there more communities, start IDs >max(communities)
  ## if there more/= pix groups, start IDs >max(pix groups)
  if (max(newCohortDataExtra$community) > max(newCohortDataExtra$temppixelGroup)) {
    set(newCohortDataExtra, NULL,  "community",
        newCohortDataExtra$community + max(newCohortDataExtra$community)*newCohortDataExtra$temppixelGroup)
  } else {
    set(newCohortDataExtra, NULL, "community",
        newCohortDataExtra$temppixelGroup + max(newCohortDataExtra$temppixelGroup)*newCohortDataExtra$community)
  }

  ## make new pixel groups by adding community IDs to previous max pix group ID
  maxPixelGroup <- max(max(cohortData$pixelGroup), maxValue(pixelGroupMap))
  set(newCohortDataExtra, NULL,  "newpixelGroup",
      as.integer(as.factor(newCohortDataExtra$community)) + maxPixelGroup)
  set(newCohortDataExtra, NULL, c("community", "temppixelGroup"), NULL)
  setkey(newCohortData, pixelIndex)
  setkey(newCohortDataExtra, pixelIndex)
  newCohortData <- newCohortData[,pixelGroup := NULL][newCohortDataExtra][,pixelIndex := NULL]
  newCohortData <- unique(newCohortData, by = c("newpixelGroup", "speciesCode"))

  ## extract total pix group biomass, and join to new data - WHY there are no common pixIDs?
  sumTable <- cohortData[, .(pixelGroup,sumB)] %>%
    unique(, by = c("pixelGroup"))
  newCohortData <- dplyr::left_join(newCohortData, sumTable, by = "pixelGroup") %>% data.table()
  newCohortData[is.na(sumB),sumB := 0]
  set(cohortData, NULL, "sumB", NULL)
  set(newCohortData, NULL, "pixelGroup", newCohortData$newpixelGroup)
  set(newCohortData, NULL, c("newpixelGroup"), NULL)

  ## get spp "productivity traits" per ecoregion/present year
  ## calculate maximum biomass per ecoregion, join to new cohort data
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP, maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]
  newCohortData <- setkey(newCohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch = 0]
  set(newCohortData, NULL, "age", 1)  ## set age to 1
  ## set biomass - if B=0, it's getting maxANPP ???
  set(newCohortData, NULL, "B",
      as.integer(pmax(1, newCohortData$maxANPP*exp(-1.6*newCohortData$sumB/newCohortData$maxB_eco))))
  set(newCohortData, NULL, "B", as.integer(pmin(newCohortData$maxANPP, newCohortData$B)))

  newCohortData <- newCohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B,
                                     mortality = 0, aNPPAct = 0)]
  newCohortDataExtra2 <- unique(newCohortDataExtra, by = c("pixelGroup", "newpixelGroup"))
  # newCohortDataExtra2 is further simplified form
  # identify which pixelGroups in cohortData have new regeneration ???
  ## Ceres: its seems to me like this is actually checking for overlapping data
  existingData <- cohortData[pixelGroup %in% unique(newCohortDataExtra2$pixelGroup)]
  setkey(newCohortDataExtra2, pixelGroup)
  setkey(existingData, pixelGroup)
  existingData <- existingData[newCohortDataExtra2, allow.cartesian = TRUE]
  existingData <- existingData[!is.na(ecoregionGroup)]
  set(existingData, NULL, "pixelGroup", existingData$newpixelGroup)
  set(existingData, NULL, c("pixelIndex", "newpixelGroup"), NULL)
  existingData <- unique(existingData, by = c("pixelGroup", "speciesCode", "age"))
  rm(newCohortDataExtra2)
  cohortData <- setkey(rbindlist(list(cohortData, newCohortData, existingData)),
                       pixelGroup, speciesCode, age)
  pixelGroupMap[as.integer(newCohortDataExtra$pixelIndex)] <- newCohortDataExtra$newpixelGroup

  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)),]
  pixelGroupMap_new <- pixelGroupMap

  temppixelIndex11 <- which(!(getValues(pixelGroupMap) %in% c(0, -1)))
  pgmTemp <- getValues(pixelGroupMap)[temppixelIndex11]
  pixelGroupMap_new[temppixelIndex11] <- as.integer(as.factor(pgmTemp))
  pixelGroupConnection <- data.table(pixelGroup = pgmTemp,
                                     newPixelGroup = getValues(pixelGroupMap_new)[temppixelIndex11]) %>%
    unique(by = "pixelGroup")
  setkey(pixelGroupConnection, pixelGroup)
  setkey(cohortData, pixelGroup)
  cohortData <- cohortData[pixelGroupConnection, nomatch = 0]
  set(cohortData, NULL, "pixelGroup", cohortData$newPixelGroup)
  set(cohortData, NULL, "newPixelGroup", NULL)
  pixelGroupMap <- setValues(pixelGroupMap_new, as.integer(pixelGroupMap_new[]))
  return(list(cohortData = cohortData,pixelGroupMap = pixelGroupMap))
}

.inputObjects <- function(sim) {
  dPath <- dataPath(sim) #file.path(modulePath(sim), "LBMR", "data")
  cacheTags <- c(currentModule(sim), "function:.inputObjects", "function:spades")
  if (!suppliedElsewhere("shpStudyArea", sim)) {

    message("'shpStudyArea' was not provided by user. Using a polygon in southwestern Alberta, Canada,")

    polyCenter <- SpatialPoints(coords = data.frame(x = c(-1349980), y = c(6986895)),
                                proj4string = CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
            "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")))

    seedToKeep <- .GlobalEnv$.Random.seed
    set.seed(1234)
    sim$shpStudyArea <- SpaDES.tools::randomPolygon(x = polyCenter, hectares = 10000)
    .GlobalEnv$.Random.seed <- seedToKeep
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

    ## rename species for compatibility across modules (Xxxx_xxx)
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
    ras <- projectExtent(sim$shpStudyArea, crs = sim$shpStudyArea)
    res(ras) = 250
    initialCommunitiesMap <- rasterize(sim$shpStudyArea, ras)

    ## make uniform communities (well structured in space)
    mapvals <- rep(unique(initialCommunities$mapcode),
                   each = ceiling(sum(!is.na(getValues(initialCommunitiesMap)))/length(unique(initialCommunities$mapcode))))
    mapvals <- mapvals[1:sum(!is.na(getValues(initialCommunitiesMap)))]   ## remove any extra values

    ## assign communities to map and export to sim
    initialCommunitiesMap[!is.na(getValues(initialCommunitiesMap))][] <- mapvals
    sim$initialCommunitiesMap <- initialCommunitiesMap
  }

  ######################################################
  #   # load the biomass succession txt and obtain 1) minRelativeB,
  #                                                2) sufficientLight, and
  #                                                3) additional species traits
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim)) |
      (!suppliedElsewhere("minRelativeB", sim))) {
    maxcol <- 7L
    for (i in 1:2) {
      mainInput <- Cache(prepInputs,
                         extractURL("sufficientLight"),
                         targetFile = "biomass-succession_test.txt",
                         destinationPath = dPath,
                         fun = "utils::read.table",
                         fill = TRUE, #purge = 7,
                         sep = "",
                         header = FALSE,
                         col.names = c(paste("col",1:maxcol, sep = "")),
                         blank.lines.skip = TRUE,
                         stringsAsFactors = FALSE,
                         overwrite = TRUE)
      maxcol1 <- max(count.fields(file.path(dPath, "biomass-succession_test.txt"), sep = ""))
      if (identical(maxcol1,maxcol)) break
    }

    mainInput <- data.table(mainInput)
    mainInput <- mainInput[col1 != ">>",]

    # read species.txt and convert it to data.table
    maxcol <- 13 #max(count.fields(file.path(dPath, "species.txt"), sep = ""))
    url <- paste0("https://raw.githubusercontent.com/LANDIS-II-Foundation/",
                  "Extensions-Succession/master/biomass-succession-archive/",
                  "trunk/tests/v6.0-2.0/species.txt")
    species <- Cache(prepInputs,
                     url = url,
                     targetFile = "species.txt",
                     destinationPath = asPath(dPath),
                     fun = "utils::read.table",
                     fill = TRUE, row.names = NULL, #purge = 7,
                     sep = "",
                     header = FALSE,
                     blank.lines.skip = TRUE,
                     col.names = c(paste("col", 1:maxcol, sep = "")),
                     stringsAsFactors = FALSE,
                     userTags = c(cacheTags, "species")) %>%
      data.table()
    species <- species[, 1:11]
    species <- species[col1 != "LandisData",]
    species <- species[col1 != ">>",]
    
    colNames <- c("species", "longevity", "sexualmature", "shadetolerance",
                  "firetolerance", "seeddistance_eff", "seeddistance_max",
                  "resproutprob", "resproutage_min", "resproutage_max",
                  "postfireregen")
    names(species) <- colNames
    species[, ':='(seeddistance_eff = gsub(",", "", seeddistance_eff),
                   seeddistance_max = gsub(",", "", seeddistance_max))]
    # change all columns to integer
    species <- species[, lapply(.SD, as.integer), .SDcols = names(species)[-c(1,NCOL(species))],
                       by = "species,postfireregen"]
    setcolorder(species, colNames)

    # get additional species traits
    speciesAddon <- mainInput
    startRow <- which(speciesAddon$col1 == "SpeciesParameters")
    speciesAddon <- speciesAddon[(startRow + 1):(startRow + nrow(species)), 1:6, with = FALSE]
    names(speciesAddon) <- c("species", "leaflongevity", "wooddecayrate",
                             "mortalityshape", "growthcurve", "leafLignin")
    speciesAddon[, ':='(leaflongevity = as.numeric(leaflongevity),
                        wooddecayrate = as.numeric(wooddecayrate),
                        mortalityshape = as.numeric(mortalityshape),
                        growthcurve = as.numeric(growthcurve),
                        leafLignin = as.numeric(leafLignin))]

    species <- setkey(species, species)[setkey(speciesAddon, species), nomatch = 0]

    ## rename species for compatibility across modules (Genu_spe)
    species$species1 <- as.character(substring(species$species, 1, 4))
    species$species2 <- as.character(substring(species$species, 5, 7))
    species[, ':='(species = paste0(toupper(substring(species1, 1, 1)),
                                    substring(species1, 2, 4), "_",
                                    species2))]

    species[, ':='(species1 = NULL, species2 = NULL)]

    sim$species <- species
    rm(maxcol)
  }

  if (!suppliedElsewhere("ecoregion", sim)) {
    ## Get the dummy ecoregion table from LANDIS-II examples.
    maxcol <- 5 #max(count.fields(file.path(dPath, "ecoregions.txt"), sep = ""))
    ecoregion <- Cache(prepInputs,
                       url = extractURL("ecoregion"),
                       targetFile = "ecoregions.txt",
                       destinationPath = dPath,
                       fun = "utils::read.table",
                       fill = TRUE,
                       sep = "",
                       # purge = 7,
                       header = FALSE,
                       blank.lines.skip = TRUE,
                       stringsAsFactors = FALSE)
    maxcol <- max(count.fields(file.path(dPath, "ecoregions.txt"), sep = ""))
    colnames(ecoregion) <- c(paste("col", 1:maxcol, sep = ""))
    ecoregion <- data.table(ecoregion)
    ecoregion <- ecoregion[col1 != "LandisData",]
    ecoregion <- ecoregion[col1 != ">>",]
    names(ecoregion)[1:4] <- c("active", "mapcode", "ecoregion", "description")
    ecoregion$mapcode <- as.integer(ecoregion$mapcode)
    sim$ecoregion <- ecoregion
    rm(maxcol)
  }

  ######################################################
  ## load ecoregion map
  if (!suppliedElsewhere("ecoregionMap", sim )) {
    ## LANDIS-II demo data:

    # sim$ecoregionMap <- Cache(prepInputs,
    #                           url = extractURL("ecoregionMap"),
    #                           destinationPath = dPath,
    #                           targetFile = "ecoregions.gis",
    #                           fun = "raster::raster")

    ## Dummy version with spatial location in Canada
    ras <- projectExtent(sim$shpStudyArea, crs = sim$shpStudyArea)
    res(ras) = 250
    ecoregionMap <- rasterize(sim$shpStudyArea, ras)

    ## make uniform communities (well structured in space)
    mapvals <- rep(unique(ecoregion$mapcode),
                   each = ceiling(sum(!is.na(getValues(ecoregionMap)))/length(unique(ecoregion$mapcode))))
    mapvals <- mapvals[1:sum(!is.na(getValues(ecoregionMap)))]   ## remove any extra values

    ## assign communities to map and export to sim
    ecoregionMap[!is.na(getValues(ecoregionMap))][] <- mapvals
    sim$ecoregionMap <- ecoregionMap
  }

  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    speciesEcoregion <- Cache(prepInputs,
                              url = extractURL("speciesEcoregion"),
                              fun = "utils::read.table",
                              destinationPath = dPath,
                              targetFile = "biomass-succession-dynamic-inputs_test.txt",
                              fill = TRUE,
                              sep = "",
                              header = FALSE,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)
    maxcol <- max(count.fields(file.path(dPath, "biomass-succession-dynamic-inputs_test.txt"),
                               sep = ""))
    colnames(speciesEcoregion) <- paste("col",1:maxcol, sep = "")
    speciesEcoregion <- data.table(speciesEcoregion)
    speciesEcoregion <- speciesEcoregion[col1 != "LandisData",]
    speciesEcoregion <- speciesEcoregion[col1 != ">>",]
    keepColNames <- c("year", "ecoregion", "species", "establishprob", "maxANPP", "maxB")
    names(speciesEcoregion)[1:6] <- keepColNames
    speciesEcoregion <- speciesEcoregion[, keepColNames, with = FALSE]
    integerCols <- c("year", "establishprob", "maxANPP", "maxB")
    speciesEcoregion[, (integerCols) := lapply(.SD, as.integer), .SDcols = integerCols]

    ## rename species for compatibility across modules (Xxxx_xxx)
    speciesEcoregion$species1 <- as.character(substring(speciesEcoregion$species, 1, 4))
    speciesEcoregion$species2 <- as.character(substring(speciesEcoregion$species, 5, 7))
    speciesEcoregion[, ':='(species = paste0(toupper(substring(species1, 1, 1)),
                                             substring(species1, 2, 4), "_", species2))]

    speciesEcoregion[, ':='(species1 = NULL, species2 = NULL)]

    sim$speciesEcoregion <- speciesEcoregion
    rm(maxcol)
  }

  if (!suppliedElsewhere("minRelativeB", sim)) {
    minRelativeB <- mainInput %>%
      data.frame()
    startRow <- which(minRelativeB$col1 == "MinRelativeBiomass")
    minRelativeB <- minRelativeB[(startRow + 1):(startRow + 6),]
    minRelativeB[1,2:ncol(minRelativeB)] <- minRelativeB[1,1:(ncol(minRelativeB)-1)]
    names(minRelativeB) <- NULL
    minRelativeB <- minRelativeB[,apply(minRelativeB, 2, function(x) all(nzchar(x)))]
    minRelativeB <- minRelativeB[,-1] %>%
      t(.) %>%
      gsub(pattern="%",replacement="") %>%
      data.table()

    colNames <- c("ecoregion", "X1", "X2", "X3", "X4", "X5")
    names(minRelativeB) <- colNames
    minRelativeB[, (colNames[-1]) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = colNames[-1]]
    # minRelativeB <- minRelativeB %>%
    #   mutate_at(funs(as.numeric(as.character(.))/100), .vars=-ecoregion)
    sim$minRelativeB <- minRelativeB
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    sufficientLight <- mainInput %>%
      data.frame
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight)
  }

  if (!suppliedElsewhere("speciesEquivalency")) {
    ## TODO: this is in pemisc@development
    leadingNames <- c("Black spruce leading", "White spruce leading", "Deciduous leading",
                      "Mixed", "Pine leading", "Fir leading")
    latinNames <- c("Pice_mar", "Pice_gla", "Popu_tre", "Mixed", "Pinu_sp", "Abie_sp")
    shortNames <- c("Bl spruce", "Wh spruce", "Decid", "Mixed", "Pine", "Fir")
    fullNames <- c("Black.Spruce", "White.Spruce", "Deciduous", "Mixed", "Pine", "Fir")
    cols <- RColorBrewer::brewer.pal(6, "Accent")

    sim$speciesEquivalency <- data.frame(leadingNames, latinNames, shortNames, fullNames, cols, stringsAsFactors = FALSE)
  }
  return(invisible(sim))
}

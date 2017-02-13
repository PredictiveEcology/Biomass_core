# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LBMR",
  description = "A fast and large landscape biomass succession model modified from LANDIS II",
  keywords = c("forest succession", "LANDIS II", "Biomass"),
  authors = c(person(c("Yong"), "Luo", email="Yong.Luo@canada.ca", role=c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@canada.ca", role=c("aut", "cre")),
              person(c("Jean"), "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("1.2.0.9011"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LBMR.Rmd"),
  reqdPkgs = list("raster", "sp", "data.table", "dplyr", "ggplot2",
                  "fpCompare", "grid", "archivist", "tidyr", "Rcpp", "scales"),
  parameters = rbind(
    defineParameter("growthInitialTime", "numeric", 0, NA_real_, NA_real_, "Initial time for the growth event to occur"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", 0, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("fireDisturbanceInitialTime", "numeric", 1, NA_real_, NA_real_, "Initial time for the post fire reproduction event to occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "initialCommunities", objectClass = "data.table",
                 desc = "initial community table", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.txt"),
    expectsInput(objectName = "species", objectClass = "data.table", 
                 desc = "a table that has species traits such as longevity...", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput(objectName = "ecoregionMap", objectClass = "RasterLayer", 
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.gis"),
    expectsInput(objectName = "initialCommunitiesMap", objectClass = "RasterLayer", 
                 desc = "initial community map that has mapcodes match initial community table", 
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.gis"),
    expectsInput(objectName = "ecoregion", objectClass = "data.table", 
                 desc = "ecoregion look up table", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt"),
    expectsInput(objectName = "speciesEcoregion", objectClass = "data.table", 
                 desc = "define the maxANPP, maxB and SEP change with both ecoregion and simulation time", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput(objectName = "minRelativeB", objectClass = "data.frame", 
                 desc = "define the cut points to classify stand shadeness", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput(objectName = "sufficientLight", objectClass = "data.frame", 
                 desc = "define how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput(objectName = "spinupMortalityfraction", objectClass = "numeric", 
                 desc = "define the mortality loss fraction in spin up-stage simulation, default is 0.001", 
                 sourceURL = "NA"),
    expectsInput(objectName = "successionTimestep", objectClass = "numeric", 
                 desc = "define the simulation time step, default is 10 years", sourceURL = "NA"),
    expectsInput(objectName = "cellSize", objectClass = "numeric", 
                 desc = "define the cell size", sourceURL = "NA"),
    expectsInput(objectName = "seedingAlgorithm", objectClass = "character", 
                 desc = "choose which seeding algorithm will be used among noDispersal, universalDispersal,
                 and wardDispersal, default is wardDispersal", sourceURL = "NA"),
    expectsInput(objectName = "useCache", objectClass = "logic", 
                 desc = "define which the caching for spinup simulation should be used, default is TRUE",
                 sourceURL = "NA"),
    expectsInput(objectName = "calibrate", objectClass = "logic", 
                 desc = "should the detailed simulation information be outputed in 
                 spinupOutput and simulationTreeOutput, default is FALSE", 
                 sourceURL = "NA"),
    # For inputs from optional fire module
    expectsInput(objectName = "fireTimestep", objectClass = "numeric", 
                 desc = "The number of time units between successive fire events in a fire module", 
                 sourceURL = "NA"),
    expectsInput(objectName = "rstCurrentBurn", objectClass = "RasterLayer", 
                 desc = "a fire burn raster", 
                 sourceURL = "NA")
    ),
  outputObjects = bind_rows(
    createsOutput(objectName = "simulationOutput", objectClass = "data.table", 
                  desc = "contains simulation results by ecoregion", 
                  other = "this is main output"),
    createsOutput(objectName = "cohortData", objectClass = "data.table", 
                  desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                  succession time step"),
    createsOutput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                  desc = "updated community map at each succession time step"),
    createsOutput(objectName = "BiomassMap", objectClass = "RasterLayer", 
                  desc = "Bioimass map at each succession time step"),
    createsOutput(objectName = "ANPPMap", objectClass = "RasterLayer", 
                  desc = "ANPP map at each succession time step"),
    createsOutput(objectName = "MortalityMap", objectClass = "RasterLayer", 
                  desc = "Mortality map at each succession time step"),
    createsOutput(objectName = "RegenerationMap", objectClass = "RasterLayer", 
                  desc = "Regeneration map at each succession time step")
    )
))

doEvent.LBMR = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    sim <- sim$LBMRInit(sim)
    if(sim$successionTimestep != 1){
      sim <- scheduleEvent(sim, start(sim) + 2*sim$successionTimestep - 1, "LBMR",
                           "cohortAgeReclassification", eventPriority = 0.5)
    }
    sim <- scheduleEvent(sim, start(sim) + params(sim)$LBMR$growthInitialTime,
                         "LBMR", "mortalityAndGrowth", eventPriority = 5)
    sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                         "LBMR", "summaryBGM", eventPriority = 6)
    if(!is.null(sim$rstCurrentBurn)){ # anything related to fire disturbance
      sim <- scheduleEvent(sim, start(sim) + params(sim)$LBMR$fireDisturbanceInitialTime,
                           "LBMR", "fireDisturbance", eventPriority = 3)
    }
    if(sim$seedingAlgorithm == "noDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                           "LBMR", "noDispersalSeeding", eventPriority = 4)
    } else if(sim$seedingAlgorithm == "universalDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                           "LBMR", "universalDispersalSeeding", eventPriority = 4)
    } else if(sim$seedingAlgorithm == "wardDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                           "LBMR", "wardDispersalSeeding", eventPriority = 4)
    } else {
      stop("Undefined seed dispersal type!")
    }
    sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                         "LBMR", "summaryRegen", eventPriority = 5.5)
    sim <- scheduleEvent(sim, params(sim)$LBMR$.plotInitialTime + sim$successionTimestep,
                         "LBMR", "plot", eventPriority = 7)
    sim <- scheduleEvent(sim, params(sim)$LBMR$.saveInitialTime + sim$successionTimestep,
                         "LBMR", "save", eventPriority = 7.5)
  } else if (eventType == "mortalityAndGrowth") {
    sim <- LBMRMortalityAndGrowth(sim)
    sim <- scheduleEvent(sim, time(sim) + 1, "LBMR", "mortalityAndGrowth", 
                         eventPriority = 5)
  } else if (eventType == "summaryBGM"){
    sim <- LBMRSummaryBGM(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "LBMR", "summaryBGM",
                         eventPriority = 6)
  } else if (eventType == "fireDisturbance" & !is.null(sim$rstCurrentBurn)) {
    sim <- LBMRFireDisturbance(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$fireTimestep,
                         "LBMR", "fireDisturbance", 
                         eventPriority = 3)
  } else if (eventType == "noDispersalSeeding" | eventType=="universalDispersalSeeding" | eventType=="wardDispersalSeeding") {
    if(sim$seedingAlgorithm=="noDispersal"){
      sim <- LBMRNoDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep, 
                           "LBMR", "noDispersalSeeding", eventPriority = 4)
    }
    if(sim$seedingAlgorithm == "universalDispersal"){
      sim <- LBMRUniversalDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep, 
                           "LBMR", "universalDispersalSeeding", eventPriority = 4)
    }
    if(sim$seedingAlgorithm == "wardDispersal"){
      sim <- LBMRWardDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                           "LBMR", "wardDispersalSeeding", eventPriority = 4)
    }
  } else if (eventType == "summaryRegen"){
    sim <- LBMRSummaryRegen(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "LBMR", "summaryRegen", eventPriority = 5.5)
  } else if (eventType == "plot") {
    sim <- LBMRPlot(sim)
    sim <- scheduleEvent(sim, time(sim) + pmin(sim$successionTimestep, sim$fireTimestep),
                         "LBMR", "plot", eventPriority = 7)
  } else if (eventType == "save") {
    sim <- LBMRSave(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "LBMR", "save", eventPriority = 7.5)
  } else if (eventType == "cohortAgeReclassification" & sim$successionTimestep != 1) {
    sim <- LBMRCohortAgeReclassification(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "LBMR", "cohortAgeReclassification",
                         eventPriority = 0.5)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

LBMRInit <- function(sim) {
  communities <- sim$initialCommunities %>%
    gather(key=cohort, value=age, -mapcode,-description,-species,na.rm=TRUE) %>%
    data.table %>%
    .[,`:=`(age = as.integer(ceiling(as.numeric(age)/sim$successionTimestep) * sim$successionTimestep),
            communityGroup = as.integer(mapcode),
            mapcode = NULL)] %>%
    unique(., by = c("communityGroup", "species", "age"))
  species <- data.table(sim$species)[,speciesCode:=as.integer(as.factor(species))]
  tempspecies <- setkey(species[,.(species,speciesCode)], species)
  communities <- setkey(communities, species)[tempspecies, nomatch=0]
  speciesEcoregion <- setkey(data.table(sim$speciesEcoregion), species)[tempspecies, nomatch=0]
  sim$species <- setkey(species, speciesCode)
  ecoregion <- data.table(sim$ecoregion)[, ecoregionGroup := as.integer(mapcode)]
  ecoregion_temp <- setkey(ecoregion[, .(ecoregion,ecoregionGroup)], ecoregion)
  sim$minRelativeB <- data.table(sim$minRelativeB, key = "ecoregion")[ecoregion_temp, nomatch = 0]
  speciesEcoregion <- setkey(speciesEcoregion, ecoregion)[ecoregion_temp, nomatch=0]
  sim$speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)
  nrowCommunities <- nrow(communities)
  initialCommunitiesMap <- sim$initialCommunitiesMap
  napixels <- which(is.na(getValues(initialCommunitiesMap)))
  initialCommunitiesMap[napixels] <- max(getValues(initialCommunitiesMap), na.rm=TRUE) + 1
  pixelGroupFactor <- 10^ceiling(log10((max(getValues(initialCommunitiesMap), na.rm=TRUE) + 1)))
  ecoregionMap <- sim$ecoregionMap
  pixelGroupMap <- initialCommunitiesMap + ecoregionMap*pixelGroupFactor
  active_ecoregion <- setkey(ecoregion[active=="yes" ,.(k=1,ecoregionGroup)], k)
  cohortData <- setkey(communities[, k:=1], k)[active_ecoregion, allow.cartesian=TRUE][, k:=NULL]
  set(cohortData, ,"pixelGroup", cohortData$communityGroup + cohortData$ecoregionGroup*pixelGroupFactor)
  set(cohortData, , "B", as.integer(0L))
  cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B)] # removed the collumn cummunityGroup
  # the cohortData here is a full joint table of community Group and ecoregion Group
  # some redundant pixelGroups are removed, because they are not present on the pixelGroupMap
  # we are dealing with the case that all the ecoregion is active, how about some ecoregion is not active
  activePixelIndex <- which(getValues(ecoregionMap) %in% active_ecoregion$ecoregionGroup)
  inactivePixelIndex <- seq(from=1, to=ncell(ecoregionMap))[(seq(from=1, to=ncell(ecoregionMap)) %in% activePixelIndex) == FALSE]
  sim$activeEcoregionLength <- data.table(Ecoregion = getValues(ecoregionMap), pixelIndex = 1:ncell(ecoregionMap))[
    Ecoregion %in% active_ecoregion$ecoregionGroup, .(NofCell = length(pixelIndex)), by = Ecoregion]
  sim$activePixelIndex <- activePixelIndex # store this for future use
  sim$inactivePixelIndex <- inactivePixelIndex # store this for future use
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[activePixelIndex]),]
  rm(nrowCommunities, pixelGroupFactor)
  # pixels with -1 in the pixelGroupMap are inactive
  if(length(inactivePixelIndex) > 0){
    pixelGroupMap[inactivePixelIndex] <- -1
  }
  cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                 time = round(time(sim)), cohortData = cohortData)
  cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = cohortData)
  
  if(is.null(sim$calibrate)){
    sim$calibrate <- FALSE
  }
  sim <- cacheSpinUpFunction(sim, cachePath = outputPath(sim))
  spinupstage <- sim$spinUpCache(cohortData = cohortData, calibrate = sim$calibrate,
                                 successionTimestep = sim$successionTimestep,
                                 spinupMortalityfraction = sim$spinupMortalityfraction,
                                 species = sim$species)
  cohortData <- spinupstage$cohortData
  if(sim$calibrate){
    sim$spinupOutput <- spinupstage$spinupOutput
  }
  if(sim$calibrate){
    sim$simulationTreeOutput <- data.table(Year = numeric(), siteBiomass = numeric(), Species = character(),
                                           Age = numeric(), iniBiomass = numeric(), ANPP = numeric(),
                                           Mortality = numeric(), deltaB = numeric(), finBiomass = numeric())
    sim$regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                         Year = numeric(), numberOfReg=  numeric())
  }
  names(pixelGroupMap) <- "pixelGroup"
  pixelAll <- cohortData[,.(uniqueSumB = as.integer(sum(B, na.rm=TRUE))), by=pixelGroup]
  biomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
  ANPPMap <- setValues(biomassMap, 0)
  mortalityMap <- setValues(biomassMap, 0)
  reproductionMap <- setValues(biomassMap, 0)
  sim$pixelGroupMap <- pixelGroupMap
  sim$cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age,
                                  B, mortality = 0, aNPPAct = 0)]
  simulationOutput <- data.table(Ecoregion = getValues(ecoregionMap), 
                                 pixelGroup = getValues(pixelGroupMap),
                                 pixelIndex = 1:ncell(ecoregionMap))[
                                   ,.(NofPixel = length(pixelIndex)), by = c("Ecoregion", "pixelGroup")]
  simulationOutput <- setkey(simulationOutput, pixelGroup)[setkey(pixelAll, pixelGroup), nomatch = 0][
    ,.(Biomass = sum(uniqueSumB*NofPixel)), by = Ecoregion]
  simulationOutput <- setkey(simulationOutput, Ecoregion)[setkey(sim$activeEcoregionLength, Ecoregion),
                                                          nomatch = 0]
  sim$simulationOutput <- simulationOutput[,.(Ecoregion, NofCell, Year = time(sim), Biomass = round(Biomass/NofCell),
                                              ANPP = 0, Mortality = 0, Regeneration = 0)]
  sim$lastReg <- 0
  speciesEcoregion[, identifier:=year>sim$successionTimestep]
  speciesEcoregion_True <- speciesEcoregion[identifier == "TRUE",]
  speciesEcoregion_False <- speciesEcoregion[identifier == "FALSE",]
  speciesEcoregion_True_addon <- speciesEcoregion_False[year == max(speciesEcoregion_False$year),]
  sim$speciesEcoregion <- rbindlist(list(speciesEcoregion_True_addon, speciesEcoregion_True))[
    ,':='(year = year - min(year), identifier = NULL)]
  sim$lastFireYear <- "noFire"
  return(invisible(sim))
}

cacheSpinUpFunction <- function(sim, cachePath) {
  # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
  if(sim$useCache) {
    sim$spinUpCache <- function(...) {
      SpaDES::Cache(FUN = spinUp, ...)
    }
  } else {
    # Step 3 - create a non-caching version in case caching is not desired
    #  sim$spinUp <- sim$spinUpRaw
    sim$spinUpCache <- spinUp
  }
  return(invisible(sim))
}

spinUp <- function(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species){
  maxAge <- max(cohortData$age) # determine the pre-simulation length
  set(cohortData, ,"origAge", cohortData$age)
  set(cohortData, ,c("age","sumB"), as.integer(0L))
  set(cohortData, ,c("mortality","aNPPAct"), as.numeric(0))
  if(calibrate){
    spinupOutput <- data.table(pixelGroup = integer(), species = character(), age = integer(), 
                               iniBiomass = integer(), ANPP = numeric(), Mortality=numeric(),
                               finBiomass = integer())
  }
  k <- 0
  if(successionTimestep == 1 & maxAge!=1){
    presimuT_end <- 2
  } else {
    presimuT_end <- 1
  }
  
  for(presimuT in (maxAge):presimuT_end) {
    k <- k+1
    cohortData[origAge == presimuT,     age:=1L]
    cohortData[origAge >= presimuT,     age:=age+1L]
    
    if(successionTimestep !=1 &
       as.integer(k/successionTimestep) == k/successionTimestep){
      cohortData <- ageReclassification(cohortData = cohortData, successionTimestep = successionTimestep,
                                        stage = "spinup")
    }
    # 1. assign the biomass for the first cohort
    if(nrow(cohortData[age == 2,])>0){
      lastReg <- k-1
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k, 
                                  successionTimestep = successionTimestep)
      cohortData[age == 2, B:=as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[age == 2, B:=as.integer(pmin(maxANPP, B))]
    }
    if(maxAge!=1){
      # 2. calculate age-related mortality
      cohortData <- calculateAgeMortality(cohortData, stage="spinup",
                                          spinupMortalityfraction = spinupMortalityfraction)
      # 3. calculate the actual ANPP
      # calculate biomass Potential, for each cohort
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k-1,
                                  successionTimestep = successionTimestep)
      cohortData <- calculateCompetition(cohortData, stage = "spinup")
      # calculate ANPP
      cohortData <- calculateANPP(cohortData, stage = "spinup")
      cohortData[age > 0, aNPPAct:=pmax(1, aNPPAct - mAge)]
      # calculate growth related mortality
      cohortData <- calculateGrowthMortality(cohortData, stage = "spinup")
      cohortData[age > 0, mBio:=pmax(0,mBio - mAge)]
      cohortData[age > 0, mBio:=pmin(mBio, aNPPAct)]
      cohortData[age > 0, mortality:=mBio + mAge]
      cohortData[age > 0, B:=as.integer(B + as.integer(aNPPAct - mortality))]
      set(cohortData, ,c("bPM", "mBio"), NULL)
    }
    if(calibrate){
      if(maxAge != 1){
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = B + as.integer(mortality - aNPPAct),
                                                      ANPP = round(aNPPAct, 1),
                                                      Mortality = round(mortality, 1),finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[,.(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      ,species := NULL]
        
        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
        cohortData[,':='(bAP = NULL)]
      } else {
        spoutput <- cohortData[origAge >= presimuT,.(pixelGroup, speciesCode, age,
                                                     iniBiomass = 0, ANPP = 0,
                                                     Mortality = 0, finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[,.(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      ,species := NULL]
        
        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
      }
    }
    lastnewcohorts <- which(cohortData$origAge == 1)
    if(presimuT == presimuT_end & length(lastnewcohorts) > 0 & maxAge != 1){
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k,
                                  successionTimestep = successionTimestep)
      cohortData[origAge == 1,B:=as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[origAge == 1,B:=as.integer(pmin(maxANPP, B))]
    }
  }
  cohortData[,':='(age = origAge, origAge = NULL)]
  if(calibrate){
    all <- list(cohortData = cohortData, spinupOutput = spinupOutput)
  } else {
    all <- list(cohortData = cohortData)
  }
  return(all)
}
### template for your event1
LBMRMortalityAndGrowth = function(sim) {
  cohortData <- sim$cohortData
  sim$cohortData <- cohortData[0,]
  pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup), 
                            temID = 1:length(unique(cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
  if(length(cutpoints) == 1){cutpoints <- c(cutpoints, cutpoints+1)}
  pixelGroups[, groups:=cut(temID, breaks = cutpoints,
                            labels = paste("Group", 1:(length(cutpoints)-1),
                                           sep = ""),
                            include.lowest = T)]
  for(subgroup in paste("Group",  1:(length(cutpoints)-1), sep = "")){
    subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    #   cohortData <- sim$cohortData
    set(subCohortData, ,"age", subCohortData$age + 1)
    subCohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                      time = round(time(sim)), cohortData = subCohortData)
    subCohortData <- updateSpeciesAttributes(species = sim$species, cohortData = subCohortData)
    
    #   if(as.integer(time(sim)/sim$successionTimestep) == time(sim)/sim$successionTimestep){
    #     cohortData <- 
    #     cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, species, speciesCode, age,
    #                                 B, maxANPP, maxB,  establishprob, maxB_eco,longevity, mortalityshape,
    #                                 growthcurve, sexualmature, shadetolerance,
    #                                 mortality, prevMortality = 0, sumB = as.integer(0L), aNPPAct = 0)]
    #   }
    subCohortData <- calculateSumB(cohortData = subCohortData, 
                                   lastReg = sim$lastReg, 
                                   simuTime = time(sim),
                                   successionTimestep = sim$successionTimestep)
    subCohortData <- subCohortData[age <= longevity,]
    subCohortData <- calculateAgeMortality(cohortData = subCohortData,
                                           stage = "mainsimulation", 
                                           spinupMortalityfraction = 0)
    set(subCohortData, , c("longevity", "mortalityshape"), NULL)
    subCohortData <- calculateCompetition(cohortData = subCohortData,
                                          stage = "mainsimulation")
    if(!sim$calibrate){
      set(subCohortData, , "sumB", NULL)
    }
    #### the below two lines of codes are to calculate actual ANPP
    subCohortData <- calculateANPP(cohortData = subCohortData, 
                                   stage = "mainsimulation")
    set(subCohortData, , "growthcurve", NULL)
    set(subCohortData, ,"aNPPAct",
        pmax(1, subCohortData$aNPPAct - subCohortData$mAge))
    subCohortData <- calculateGrowthMortality(cohortData = subCohortData,
                                              stage = "mainsimulation")
    set(subCohortData, ,"mBio",
        pmax(0, subCohortData$mBio - subCohortData$mAge))
    set(subCohortData, ,"mBio",
        pmin(subCohortData$mBio, subCohortData$aNPPAct))
    set(subCohortData, ,"mortality",
        subCohortData$mBio + subCohortData$mAge)
    set(subCohortData, ,c("mBio", "mAge", "maxANPP",
                          "maxB", "maxB_eco", "bAP", "bPM"),
        NULL)
    if(sim$calibrate){
      set(subCohortData, ,"deltaB",
          as.integer(subCohortData$aNPPAct - subCohortData$mortality))
      set(subCohortData, ,"B",
          subCohortData$B + subCohortData$deltaB)
      tempcohortdata <- subCohortData[,.(pixelGroup, Year = time(sim), siteBiomass = sumB, speciesCode,
                                         Age = age, iniBiomass = B - deltaB, ANPP = round(aNPPAct, 1),
                                         Mortality = round(mortality,1), deltaB, finBiomass = B)]
      
      tempcohortdata <- setkey(tempcohortdata, speciesCode)[setkey(sim$species[,.(species, speciesCode)],
                                                                   speciesCode),
                                                            nomatch = 0][, ':='(speciesCode = species,
                                                                                species = NULL,
                                                                                pixelGroup = NULL)]
      setnames(tempcohortdata, "speciesCode", "Species")
      sim$simulationTreeOutput <- rbind(sim$simulationTreeOutput, tempcohortdata)
      set(subCohortData, ,c("deltaB", "sumB"), NULL)
    } else {
      set(subCohortData, ,"B",
          subCohortData$B + as.integer(subCohortData$aNPPAct - subCohortData$mortality))
    }
    sim$cohortData <- rbindlist(list(sim$cohortData, subCohortData))
    rm(subCohortData)
    gc()
  }
  rm(cohortData, cutpoints, pixelGroups)
  gc()
  return(invisible(sim))
}

LBMRSummaryBGM = function(sim) {
  pixelGroups <- data.table(pixelGroupIndex = unique(sim$cohortData$pixelGroup), 
                            temID = 1:length(unique(sim$cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
  pixelGroups[, groups:=cut(temID, breaks = cutpoints,
                            labels = paste("Group", 1:(length(cutpoints)-1),
                                           sep = ""),
                            include.lowest = T)]
  ecoPixelgroup <- data.table(Ecoregion = getValues(sim$ecoregionMap), 
                              pixelGroup = getValues(sim$pixelGroupMap),
                              pixelIndex = 1:ncell(sim$ecoregionMap))[
                                ,.(NofPixelGroup = length(pixelIndex)),
                                by = c("Ecoregion", "pixelGroup")]
  
  for(subgroup in paste("Group",  1:(length(cutpoints)-1), sep = "")){
    subCohortData <- sim$cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    if(nrow(subCohortData[age == 1,])>0){
      subCohortData[age == 1,reproduction:=sum(B), by = pixelGroup]
    } else {
      subCohortData[, reproduction:=0]
    }
    
    summarytable_sub <- subCohortData[,.(uniqueSumB = as.numeric(sum(as.numeric(B), na.rm=TRUE)),
                                         uniqueSumANPP = as.numeric(sum(as.numeric(aNPPAct), na.rm=TRUE)),
                                         uniqueSumMortality = as.numeric(sum(as.numeric(mortality), na.rm=TRUE)),
                                         uniqueSumRege = as.numeric(sum(as.numeric(reproduction), na.rm = TRUE))),
                                      by = pixelGroup]
    tempOutput <- setkey(ecoPixelgroup[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ],
                         pixelGroup)[setkey(summarytable_sub, pixelGroup), nomatch = 0]
    
    if(subgroup == "Group1"){
      summaryBGMtable <- summarytable_sub
      tempOutput_All <- tempOutput
    } else {
      summaryBGMtable <- rbindlist(list(summaryBGMtable, summarytable_sub))
      tempOutput_All <- rbindlist(list(tempOutput_All, tempOutput))
    }
    rm(summarytable_sub, tempOutput, subCohortData)
    gc()
  }
  tempOutput_All <- tempOutput_All[,.(Biomass = sum(uniqueSumB*NofPixelGroup),
                 ANPP = sum(uniqueSumANPP*NofPixelGroup),
                 Mortality = sum(uniqueSumMortality*NofPixelGroup),
                 Regeneration = sum(uniqueSumRege*NofPixelGroup)),
                 by = Ecoregion]
  tempOutput_All <- setkey(tempOutput_All, Ecoregion)[setkey(sim$activeEcoregionLength, 
                                                             Ecoregion), nomatch = 0]
  sim$simulationOutput <- rbindlist(list(sim$simulationOutput, 
                                         tempOutput_All[,.(Ecoregion, NofCell, Year = round(time(sim)),
                                                           Biomass = round(Biomass/NofCell),
                                                           ANPP = round(ANPP/NofCell),
                                                           Mortality = round(Mortality/NofCell),
                                                           Regeneration = round(Regeneration/NofCell))]))
  # the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively. 
  names(sim$pixelGroupMap) <- "pixelGroup"
  sim$biomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                     "uniqueSumB")
  setColors(sim$biomassMap) <- c("light green", "dark green")
  
  sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumANPP")
  setColors(sim$ANPPMap) <- c("light green", "dark green")
  
  sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumMortality")
  setColors(sim$mortalityMap) <- c("light green", "dark green")
  
  # the following codes for preparing the data table for saving
  
  rm(cutpoints, pixelGroups, tempOutput_All, summaryBGMtable)
  gc()
  return(invisible(sim))
}

LBMRFireDisturbance = function(sim) {
  # the presence of valid fire can cause three processes:
  # 1. remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration
  # 3. change of cohortdata and pixelgroup map
  # may be a supplemenatary function is needed to convert non-logical map
  # to a logical map
  postFireReproData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(), 
                                  speciesCode = numeric(), pixelIndex = numeric())
  if(sim$calibrate){
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(), 
                                           species = character(), 
                                           numberOfRegen = numeric())
  }
  if(extent(sim$rstCurrentBurn) != extent(sim$pixelGroupMap)){
   sim$rstCurrentBurn <- raster::crop(sim$rstCurrentBurn, extent(sim$pixelGroupMap))
  }
  sim$burnLoci <- Which(sim$rstCurrentBurn == 1, cell = TRUE)
  if(length(sim$inactivePixelIndex) > 0){
    sim$burnLoci <- sim$burnLoci[!(sim$burnLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  }
  firePixelTable <- data.table(cbind(pixelIndex = sim$burnLoci,
                                     pixelGroup = sim$pixelGroupMap[sim$burnLoci]))
  burnPixelGroup <- unique(firePixelTable$pixelGroup)
  sim$pixelGroupMap[sim$burnLoci] <- 0 # 0 is the fire burnt pixels without regenerations
  burnedcohortData <- sim$cohortData[pixelGroup %in% burnPixelGroup]
  set(burnedcohortData, ,c("B", "mortality", "aNPPAct"), NULL)
  #   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)
  tempspecies <- sim$species[postfireregen == "serotiny",
                             .(speciesCode, postfireregen)]
  serotinyAssessCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]
  
  rm(tempspecies)
  if(NROW(serotinyAssessCohortData) > 0){
    # assess potential serotiny reg
    serotinyAssessCohortData <- setkey(serotinyAssessCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
                                                                              nomatch = 0]
    newCohortData <- serotinyAssessCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode")) 
    set(newCohortData, ,"sexualmature", NULL)
    # this is amazing!!!!
    # select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- firePixelTable[pixelGroup %in% unique(newCohortData$pixelGroup)]
    
    # from now on the regeneration process is assessed for each potential pixel
    setkey(serotinyPixelTable, pixelGroup)
    setkey(newCohortData, pixelGroup)
    newCohortData <- serotinyPixelTable[newCohortData, nomatch = 0, allow.cartesian = TRUE]
    
    # light check
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][,siteShade := 0]
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    set(newCohortData, ,c("shadetolerance", "siteShade", "lightProb"), NULL)
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]
    newCohortData <- newCohortData[(runif(nrow(newCohortData), 0, 1)) %<<% establishprob][, establishprob := NULL]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    if(NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)] #
      if(sim$calibrate){
        serotinyRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode] 
        serotinyRegenSummary <- serotinyRegenSummary[,.(year = time(sim), regenMode = "Serotiny",
                                                        speciesCode, numberOfRegen)]
        serotinyRegenSummary <- setkey(serotinyRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        serotinyRegenSummary[, ':='(speciesCode = species, species = NULL)]
        setnames(serotinyRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, serotinyRegenSummary))
      }
      serotinyPixel <- unique(newCohortData$pixelIndex) # save the pixel index for resprouting assessment use,
      # i.e., removing these pixel from assessing resprouting
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
    } else {
      serotinyPixel <- NULL
    }
    rm(newCohortData)
  } else {
    serotinyPixel <- NULL
  }
  
  #############################################################
  #############################################################
  # from now on, starting assessing resprouting reproduction:
  # basically same thing as serotiny
  # remove the pixels that had successful serotiny regeneration
  if(is.null(serotinyPixel)){
    resproutingPixelTable <- setkey(firePixelTable, pixelGroup)
  } else {
    resproutingPixelTable <- setkey(data.table(dplyr::anti_join(firePixelTable, 
                                                                data.table(cbind(pixelIndex = serotinyPixel)),
                                                                by = "pixelIndex")),
                                    pixelGroup)
  }
  setkey(burnedcohortData, speciesCode)
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]
  resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingAssessCohortData, ,c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)
  if(NROW(resproutingAssessCohortData) > 0){
    resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    setkey(resproutingAssessCohortData, pixelGroup)
    newCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][, siteShade := 0]
    
    # Light check
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    newCohortData <- newCohortData[runif(nrow(newCohortData), 0, 1) %<<% newCohortData$resproutprob]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    set(newCohortData, ,c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)
    # remove all columns that were used temporarily here
    if(NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)]#
      if(sim$calibrate){
        resproutRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode] 
        resproutRegenSummary <- resproutRegenSummary[,.(year = time(sim), regenMode = "Resprout",
                                                        speciesCode, numberOfRegen)]
        resproutRegenSummary <- setkey(resproutRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        resproutRegenSummary[,':='(speciesCode = species, species = NULL)]
        setnames(resproutRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, resproutRegenSummary))
      }
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
      postFirePixel <- c(serotinyPixel, unique(newCohortData$pixelIndex))
      sim$postFirePixel <- postFirePixel # send it to a sim object
      rm(newCohortData)
    } else{
      sim$postFirePixel <- serotinyPixel
      postFireReproData <- postFireReproData
    }
  } else {
    postFireReproData <- postFireReproData
    sim$postFirePixel <- serotinyPixel
  }
  if(NROW(postFireReproData) > 0) {
    maxPixelGroup <- max(getValues(sim$pixelGroupMap))
    if(!is.null(sim$postFirePixel)){
      sim$pixelGroupMap[sim$postFirePixel] <- maxPixelGroup +
        as.numeric(as.factor(sim$ecoregionMap[sim$postFirePixel]))
      postFireReproData[, pixelGroup := maxPixelGroup + 
                          as.numeric(as.factor(postFireReproData$ecoregionGroup))]
    }
    sim$cohortData[,sumB := sum(B, na.rm = TRUE), by = pixelGroup]
    addnewcohort <- addNewCohorts(postFireReproData, sim$cohortData, sim$pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastFireYear <- time(sim)
  sim$firePixelTable <- firePixelTable
  return(invisible(sim))
}

LBMRNoDispersalSeeding = function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  if(sim$lastFireYear == round(time(sim))){ # if current year is both fire year and succession year
    # find new active pixel that remove successful postfire regeneration
    # since this is on site regeneration, all the burnt pixels can not seeding
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$firePixelTable$pixelIndex)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = time(sim),
                                  successionTimestep = sim$successionTimestep) 
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[,.(speciesCode, sexualmature)],
                                                               speciesCode),
                                                        nomatch = 0]
  
  newCohortData <- sim$cohortData[age >= sexualmature]
  set(sim$cohortData, ,"sexualmature", NULL)
  set(newCohortData, ,c("sexualmature", "age", "B", "mortality", "aNPPAct"), NULL)
  siteShade <- setkey(data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                               sim$speciesEcoregion, sim$minRelativeB)), pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[siteShade, nomatch=0]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[,.(speciesCode, shadetolerance)],
                                                             speciesCode),
                                                      nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, ,c("shadetolerance", "lightProb", "siteShade", "sumB"), NULL)
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
  set(newCohortData, ,c("establishprob"), NULL)
  if(sim$calibrate == TRUE & NROW(newCohortData) > 0){
    newCohortData_summ <- newCohortData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                           numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][,.(species, seedingAlgorithm,
                                                                                  Year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }
  if(NROW(newCohortData) > 0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

LBMRUniversalDispersalSeeding = function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- Which(pixelGroupMap == 0, cell = TRUE)
  if(length(fire_nonRegPixels) > 0){
    pixelGroupMap[fire_nonRegPixels] <- as.numeric(as.factor(getValues(sim$ecoregionMap)[fire_nonRegPixels])) + max(getValues(pixelGroupMap))
  }
  if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = sim$successionTimestep) 
  species <- sim$species
  # all species can provide seed source, i.e. age>=sexualmature
  speciessource <- setkey(sim$species[,.(speciesCode, k = 1)], k)
  siteShade <- data.table(calcSiteShade(time = round(time(sim)), sim$cohortData, 
                                        sim$speciesEcoregion, sim$minRelativeB))
  activePixelGroup <- unique(data.table(pixelGroup = pixelGroupMap[tempActivePixel],
                                        ecoregionGroup = sim$ecoregionMap[tempActivePixel]),
                             by = "pixelGroup")
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table
  siteShade[is.na(siteShade), siteShade := 0]
  setkey(siteShade[, k := 1], k)
  # i believe this is the latest version how the landis guys calculate sufficient light
  # http://landis-extensions.googlecode.com/svn/trunk/succession-library/trunk/src/ReproductionDefaults.cs
  newCohortData <- siteShade[speciessource, allow.cartesian = TRUE][, k := NULL]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[,.(speciesCode, shadetolerance)],
                                                             speciesCode),
                                                      nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0 , 1),]
  set(newCohortData, , c("siteShade", "lightProb", "shadetolerance"), NULL)
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
  set(newCohortData, ,"establishprob", NULL)
  if(sim$calibrate == TRUE){
    newCohortData_summ <- newCohortData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                           numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][,.(species, seedingAlgorithm,
                                                                                  Year, numberOfReg)]
    sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, newCohortData_summ))
  }
  if(NROW(newCohortData)>0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

LBMRWardDispersalSeeding = function(sim) {
  #   cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- Which(pixelGroupMap == 0, cell = TRUE)
  if(length(fire_nonRegPixels) > 0){
    pixelGroupMap[fire_nonRegPixels] <- as.numeric(as.factor(getValues(sim$ecoregionMap)[fire_nonRegPixels])) + max(sim$cohortData$pixelGroup)
  }
  if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(cohortData = sim$cohortData,
                                  lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = sim$successionTimestep) 
  siteShade <- calcSiteShade(time = round(time(sim)), cohortData = sim$cohortData, 
                             sim$speciesEcoregion, sim$minRelativeB)
  activePixelGroup <- data.table(pixelGroup = unique(pixelGroupMap[tempActivePixel]))
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table
  siteShade[is.na(siteShade),siteShade := 0]
  # Seed source cells:
  # 1. Select only sexually mature cohorts, then
  # 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[,.(speciesCode, sexualmature)],
                                                               speciesCode),
                                                        nomatch = 0]
  matureCohorts <- setkey(sim$cohortData[age >= sexualmature] %>% unique(by = c("pixelGroup", "speciesCode")),
                          speciesCode)
  matureCohorts <- matureCohorts[,.(pixelGroup, speciesCode)]
  set(sim$cohortData, ,"sexualmature", NULL)
  
  if(NROW(matureCohorts)>0){
    seedSource <- setkey(sim$species[,list(speciesCode, seeddistance_eff, seeddistance_max)], speciesCode) %>%
      .[matureCohorts]
    setkey(seedSource, speciesCode)
    #  Seed Receiving cells:
    #  1. Must be sufficient light 
    # seed receive just for the species that are seed source
    tempspecies1 <- sim$species[speciesCode %in% unique(matureCohorts$speciesCode),][
      ,.(speciesCode, shadetolerance, seeddistance_eff, seeddistance_max)]
    seedReceive = setkey(tempspecies1[,c(k = 1, .SD)], k)[setkey(siteShade[,c(k = 1, .SD)], k), allow.cartesian=TRUE][
      ,k:=NULL]
    seedReceive <- assignLightProb(sufficientLight = sim$sufficientLight, seedReceive)
    set(seedReceive, ,"siteShade", NULL)
    seedReceive <- seedReceive[lightProb %>>% runif(nrow(seedReceive), 0, 1), ][
      ,.(pixelGroup, speciesCode, seeddistance_eff, seeddistance_max)]
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
    if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
      inactivePixelIndex <- c(sim$inactivePixelIndex, sim$postFirePixel)
    } else {
      inactivePixelIndex <- sim$inactivePixelIndex
    }
    if(length(inactivePixelIndex) > 0){
      reducedPixelGroupMap <- pixelGroupMap
      reducedPixelGroupMap[inactivePixelIndex] <- NA
    } else {
      reducedPixelGroupMap <- pixelGroupMap
    }
    source(file.path(modulePath(sim), "LBMR", "R", "seedDispersalLANDIS.R"))
    seedingData <- LANDISDisp(sim, dtRcv=seedReceive, plot.it = FALSE,
                              dtSrc = seedSource, inSituReceived = inSituReceived,
                              species = sim$species,
                              reducedPixelGroupMap,
                              maxPotentialsLength = 3e5,
                              verbose = FALSE)
                              # verbose = globals(sim)$verbose)
    rm(seedReceive, seedSource)
    if(NROW(seedingData) > 0) {
      seedingData$ecoregionGroup <- getValues(sim$ecoregionMap)[seedingData$pixelIndex]
      seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)
      specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
      specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                      .(speciesCode, establishprob, ecoregionGroup)],
                                   ecoregionGroup, speciesCode)
      seedingData <- seedingData[specieseco_current, nomatch = 0]
      
      seedingData <- seedingData[establishprob >= runif(nrow(seedingData), 0, 1), ]
      set(seedingData, ,"establishprob", NULL)
      if(sim$calibrate == TRUE){
        seedingData_summ <- seedingData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                           numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
        seedingData_summ <- setkey(seedingData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][,.(species, seedingAlgorithm,
                                                                                  Year, numberOfReg)]
        sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, seedingData_summ))
      }
      addnewcohort <- addNewCohorts(seedingData, cohortData = sim$cohortData, pixelGroupMap,
                                    time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
      sim$cohortData <- addnewcohort$cohortData
      sim$pixelGroupMap <- addnewcohort$pixelGroupMap
    }
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}


LBMRSummaryRegen = function(sim){
  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  names(pixelGroupMap) <- "pixelGroup"
  # please note that the calculation of reproduction is based on successioinTime step interval,
  pixelAll <- cohortData[age <= sim$successionTimestep,
                         .(uniqueSumReproduction = sum(B, na.rm=TRUE)),
                         by = pixelGroup]
  if(NROW(pixelAll)>0){
    reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumReproduction")
    setColors(reproductionMap) <- c("light green", "dark green")
  } else {
    reproductionMap <- setValues(pixelGroupMap, 0)
  }
  rm(pixelAll)
  sim$reproductionMap <- reproductionMap
  rm(cohortData, pixelGroupMap)
  gc()
  return(invisible(sim))
}

LBMRPlot = function(sim) {
  # if(time(sim) == sim$successionTimestep){
  #   clearPlot()
  # }
  Plot(sim$biomassMap, sim$ANPPMap, sim$mortalityMap, sim$reproductionMap, 
       title = c("Biomass", "ANPP", "mortality", "reproduction"), new = TRUE, speedup = 1)
  grid.rect(0.93, 0.97, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid.text(label = paste0("Year = ",round(time(sim))), x = 0.93, y = 0.97)
  #rm(biomassMap, ANPPMap, mortalityMap, reproductionMap)
  #gc()
  return(invisible(sim))
}

LBMRSave = function(sim) {
  raster::projection(sim$biomassMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$ANPPMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$mortalityMap) <- raster::projection(sim$ecoregionMap)
  raster::projection(sim$reproductionMap) <- raster::projection(sim$ecoregionMap)
  writeRaster(sim$biomassMap, 
              file.path(outputPath(sim), paste("biomassMap_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  writeRaster(sim$ANPPMap,
              file.path(outputPath(sim), paste("ANPP_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  writeRaster(sim$mortalityMap, 
              file.path(outputPath(sim), paste("mortalityMap_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  writeRaster(sim$reproductionMap, 
              file.path(outputPath(sim), paste("regenerationMap_Year", round(time(sim)), ".tif",sep="")), datatype='INT4S',
              overwrite = TRUE)
  return(invisible(sim))
}

LBMRCohortAgeReclassification = function(sim) {
  if(time(sim) != 0){
    cohortData <- sim$cohortData
    cohortData <- ageReclassification(cohortData = cohortData, successionTimestep = successionTimestep,
                                      stage = "mainSimulation")
    sim$cohortData <- cohortData
    return(invisible(sim))
  } else {
    return(invisible(sim))
  }
  
}

updateSpeciesEcoregionAttributes <- function(speciesEcoregion, time, cohortData){
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco:=max(maxB), by = ecoregionGroup]
  
  cohortData <- setkey(cohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch=0]
  return(cohortData)
}

updateSpeciesAttributes <- function(species, cohortData){
  # to assign longevity, mortalityshape, growthcurve to cohortData
  species_temp <- setkey(species[,.(speciesCode, longevity, mortalityshape,
                                    growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch=0]
  return(cohortData)
}

ageReclassification <- function(cohortData, successionTimestep, stage){
  if(stage == "spinup"){
    # for spin up stage
    cohortData[age == successionTimestep+1, age:=successionTimestep]
  } else {
    # non- spinup stage
    targetData <- cohortData[age <= successionTimestep, ]
    targetData <- targetData[,.(ecoregionGroup = mean(ecoregionGroup),
                                age = successionTimestep - 1,
                                B = sum(B, na.rm = TRUE), 
                                mortality = sum(mortality, na.rm = TRUE), 
                                aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                             by = .(pixelGroup, speciesCode)]
    targetData <- targetData[,.(pixelGroup, ecoregionGroup, speciesCode, age,
                                B, mortality, aNPPAct)]
    cohortData <- cohortData[age >= successionTimestep + 1]
    cohortData <- rbindlist(list(cohortData, targetData))
  }
  return(cohortData)
}

calculateAgeMortality <- function(cohortData, stage, spinupMortalityfraction){
  # for age-related mortality calculation
  if(stage == "spinup"){
    cohortData[age > 0, mAge:=B*(exp((age)/longevity*mortalityshape)/exp(mortalityshape))]
    cohortData[age > 0, mAge:=mAge+B*spinupMortalityfraction]
    cohortData[age > 0, mAge:=pmin(B,mAge)]
  } else {
    set(cohortData, ,"mAge",
        cohortData$B*(exp((cohortData$age)/cohortData$longevity*cohortData$mortalityshape)/exp(cohortData$mortalityshape)))
    set(cohortData, ,"mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

calculateANPP <- function(cohortData,stage){
  if(stage=="spinup"){
    cohortData[age>0,aNPPAct:=maxANPP*exp(1)*(bAP^growthcurve)*exp(-(bAP^growthcurve))*bPM]
    cohortData[age>0,aNPPAct:=pmin(maxANPP*bPM,aNPPAct)]
  } else {
    set(cohortData, ,"aNPPAct",
        cohortData$maxANPP*exp(1)*(cohortData$bAP^cohortData$growthcurve)*exp(-(cohortData$bAP^cohortData$growthcurve))*cohortData$bPM)
    set(cohortData, ,"aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM,cohortData$aNPPAct))
  }
  return(cohortData)
}

calculateGrowthMortality <- function(cohortData, stage){
  if(stage == "spinup"){
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1+bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, , "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, , "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
}

calculateSumB <- function(cohortData, lastReg, simuTime, successionTimestep){
  # this function is used to calculate total stand biomass that does not include the new cohorts
  # the new cohorts are defined as the age younger than simulation time step
  # reset sumB
  pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup), 
                            temID = 1:length(unique(cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
  pixelGroups[, groups:=cut(temID, breaks = cutpoints,
                            labels = paste("Group", 1:(length(cutpoints)-1),
                                           sep = ""),
                            include.lowest = T)]
  for(subgroup in paste("Group",  1:(length(cutpoints)-1), sep = "")){
    subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    set(subCohortData, ,"sumB", 0L)
    if(simuTime == lastReg + successionTimestep - 2){
      sumBtable <- subCohortData[age > successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    } else {
      sumBtable <- subCohortData[age >= successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    }
    subCohortData <- merge(subCohortData, sumBtable, by = "pixelGroup", all.x = TRUE)
    subCohortData[is.na(tempsumB), tempsumB:=as.integer(0L)][,':='(sumB = tempsumB, tempsumB = NULL)]
    if(subgroup == "Group1"){
      newcohortData <- subCohortData
    } else {
      newcohortData <- rbindlist(list(newcohortData, subCohortData))
    }
    rm(subCohortData, sumBtable)
  }
  rm(cohortData, pixelGroups, cutpoints)
  gc()
  return(newcohortData)
}


calculateCompetition <- function(cohortData,stage){
  # two competition indics are calculated bAP and bPM
  if(stage=="spinup"){
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, , "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier) ,by=pixelGroup]
    cohortData[age > 0, bPM := cMultiplier/cMultTotal]
    set(cohortData, , c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, , "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))
    set(cohortData, , "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, , "bPot", NULL)
    set(cohortData, , "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))
    cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
    set(cohortData, , "bPM", cohortData$cMultiplier/cohortData$cMultTotal)
    set(cohortData, , c("cMultiplier", "cMultTotal"), NULL)
  }
  return(cohortData)
}

calcSiteShade <- function(time, cohortData, speciesEcoregion, minRelativeB) {
  # the siteshade was calculated based on the codes:
  # https://github.com/LANDIS-II-Foundation/Extensions-Succession/blob/master/biomass-succession/trunk/src/PlugIn.cs
  if(nrow(cohortData[age > 5,]) > 0){
    bAMterm1 <- cohortData [age > 5, ':='(prevMortality = sum(mortality, na.rm = TRUE), 
                                          sumB = sum(B, na.rm = TRUE)),
                            by = .(pixelGroup, ecoregionGroup)]
    bAMterm1[is.na(sumB), sumB := 0]
    bAMterm1[is.na(prevMortality), prevMortality := 0]
    bAMterm1 <- unique(bAMterm1, by = c("pixelGroup", "ecoregionGroup"))
    set(cohortData, ,"prevMortality", NULL)
  } else {
    bAMterm1 <- unique(cohortData, by=c("pixelGroup", "ecoregionGroup"))[
      ,.(pixelGroup, ecoregionGroup)][
        ,':='(prevMortality = 0, sumB = 0)]
  }
  #bAM <- data.table(speciesEcoregion)[year <= time(sim) & (year > (time(sim)-sim$successionTimestep))]
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
  bAMterm1 <- bAMterm1[,.(pixelGroup, siteShade)]
  return(bAMterm1)
}

assignLightProb <- function(sufficientLight, newCohortData){
  newCohortData[ , lightProb := sufficientLight[cbind(shadetolerance, siteShade + 2)]]
}

addNewCohorts <- function(newCohortData, cohortData, pixelGroupMap, time, speciesEcoregion){
  # this function is for 1) adding new cohort data into cohortdata
  # 2) assign initial biomass and age for new cohort
  # 3) assign the new pixelgroup to the pixels that have new cohort
  # 4) update the pixelgroup map
  # newCohortData must have the original pixelgroup, regenerated species and pixelindex
  # it also would be better if it has the collums of cohortData plus pixelindex
  newCohortData$pixelGroup <- getValues(pixelGroupMap)[newCohortData$pixelIndex]
  set(newCohortData, , "temppixelGroup", as.integer(as.factor(newCohortData$pixelGroup)))
  set(newCohortData, , "speciesposition", 2^(newCohortData$speciesCode))
  # newCohortDataExtra is used to connect the original pixelGroup to the newPixelGroup
  newCohortDataExtra <- newCohortData[, .(community = sum(speciesposition),
                                          pixelGroup = mean(pixelGroup),
                                          temppixelGroup = mean(temppixelGroup)), by = pixelIndex]
  set(newCohortData, , c("temppixelGroup", "speciesposition"), NULL)
  set(newCohortDataExtra, , "community",
      as.numeric(as.factor(newCohortDataExtra$community)))
  if(max(newCohortDataExtra$community) > max(newCohortDataExtra$temppixelGroup)){
    set(newCohortDataExtra, ,  "community",
        newCohortDataExtra$community + max(newCohortDataExtra$community)*newCohortDataExtra$temppixelGroup)
  } else {
    set(newCohortDataExtra, , "community",
        newCohortDataExtra$temppixelGroup + max(newCohortDataExtra$temppixelGroup)*newCohortDataExtra$community)
  }
  maxPixelGroup <- max(max(cohortData$pixelGroup), max(getValues(pixelGroupMap)))
  set(newCohortDataExtra, ,  "newpixelGroup",
      as.numeric(as.factor(newCohortDataExtra$community)) + maxPixelGroup)
  set(newCohortDataExtra, , c("community", "temppixelGroup"), NULL)
  setkey(newCohortData, pixelIndex)
  setkey(newCohortDataExtra, pixelIndex)
  newCohortData <- newCohortData[,pixelGroup:=NULL][newCohortDataExtra][,pixelIndex := NULL]
  newCohortData <- unique(newCohortData, by = c("newpixelGroup", "speciesCode"))
  sumTable <- cohortData[, .(pixelGroup,sumB)] %>%
    unique(, by = c("pixelGroup"))
  newCohortData <- dplyr::left_join(newCohortData, sumTable, by = "pixelGroup") %>% data.table
  newCohortData[is.na(sumB),sumB:=0]
  set(cohortData, ,"sumB", NULL)
  set(newCohortData, , "pixelGroup", newCohortData$newpixelGroup)
  set(newCohortData, , c("newpixelGroup"), NULL)
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco:=max(maxB), by = ecoregionGroup]
  newCohortData <- setkey(newCohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch=0]
  set(newCohortData, , "age", 1)
  set(newCohortData, ,"B",
      as.integer(pmax(1, newCohortData$maxANPP*exp(-1.6*newCohortData$sumB/newCohortData$maxB_eco))))
  set(newCohortData, ,"B", as.integer(pmin(newCohortData$maxANPP, newCohortData$B)))
  
  newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age, B,
                                    mortality = 0, aNPPAct = 0)]
  newCohortDataExtra2 <- unique(newCohortDataExtra, by = c("pixelGroup", "newpixelGroup"))
  # newCohortDataExtra2 is further simplified form
  # identify which pixelGroups in cohortData have new regeneration
  existingData <- cohortData[pixelGroup %in% unique(newCohortDataExtra2$pixelGroup)]
  setkey(newCohortDataExtra2, pixelGroup)
  setkey(existingData, pixelGroup)
  existingData <- existingData[newCohortDataExtra2, allow.cartesian = TRUE]
  existingData <- existingData[!is.na(ecoregionGroup)]
  set(existingData, ,"pixelGroup", existingData$newpixelGroup)
  set(existingData, ,c("pixelIndex", "newpixelGroup"), NULL)
  existingData <- unique(existingData, by = c("pixelGroup", "speciesCode", "age"))
  rm(newCohortDataExtra2)
  cohortData <- setkey(rbindlist(list(cohortData, newCohortData, existingData)),
                       pixelGroup, speciesCode, age)
  pixelGroupMap[as.integer(newCohortDataExtra$pixelIndex)] <- newCohortDataExtra$newpixelGroup
  
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)),]
  pixelGroupMap_new <- pixelGroupMap
  
  temppixelIndex11 <- which(!(getValues(pixelGroupMap) %in% c(0, -1)))
  pixelGroupMap_new[temppixelIndex11] <- as.numeric(as.factor(pixelGroupMap[temppixelIndex11]))
  pixelGroupConnection <- data.table(pixelGroup = pixelGroupMap[temppixelIndex11],
                                     newPixelGroup = pixelGroupMap_new[temppixelIndex11]) %>%
    unique(by = "pixelGroup")
  setkey(pixelGroupConnection, pixelGroup)
  setkey(cohortData, pixelGroup)
  cohortData <- cohortData[pixelGroupConnection, nomatch = 0]
  set(cohortData, , "pixelGroup", cohortData$newPixelGroup)
  set(cohortData, , "newPixelGroup", NULL)
  pixelGroupMap <- pixelGroupMap_new
  return(list(cohortData = cohortData,pixelGroupMap = pixelGroupMap))
}


.inputObjects = function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # and objects required by this module and identified in the inputObjects element of defineModule,
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that is not part of inputObjects. Any other initiation
  # procedures should be put in "init" eventType of the doEvent function.
  # ! ----- EDIT BELOW ----- ! #
  # check local existence
  dataPath <- file.path(modulePath(sim), "LBMR", "data")
  fileNames <- c("biomass-succession_test.txt", "biomass-succession-dynamic-inputs_test.txt",
                 "ecoregions.gis", "ecoregions.txt", "initial-communities.gis", "initial-communities.txt",
                 "species.txt")
  fileNames <- lapply(fileNames, function(x){file.path(dataPath, x)})
  allFiles <- lapply(fileNames, function(x) {
    file.info(x)[,"size"]}
  )
  names(allFiles) <- unlist(lapply(fileNames, basename))
  needDownload <- digest::digest(allFiles) != "6e31b97b0fc075f46c44f07d1c94a71f"
  if(needDownload){
   checkTable <- data.table(downloadData(module = "LBMR", path = modulePath(sim))) 
  } else {
    message("  Download data step skipped for module LBMR. Local copy exists")
  }
  # convert inititial communities txt to data table
  # input initial communities
  maxcol <- max(count.fields(file.path(dataPath, "initial-communities.txt"), sep = ""))
  initialCommunities <- read.table(file.path(dataPath, "initial-communities.txt"),
                                   fill = TRUE,
                                   sep = "",
                                   blank.lines.skip = TRUE,
                                   col.names = c("species", paste("age",1:(maxcol-1), sep = "")),
                                   stringsAsFactor = FALSE)
  # correct the typo in the original txt
  initialCommunities[14,1:4] <- initialCommunities[14,2:5]
  
  initialCommunities <- data.table(initialCommunities)
  initialCommunities <- cbind(data.table(mapcode = 1:nrow(initialCommunities),
                                         description = NA), initialCommunities)
  initialCommunities <- initialCommunities[species != "LandisData",]
  cutRows <- grep(">>", initialCommunities$species)
  for(i in cutRows){
  initialCommunities[i, 
                     desc:=paste(initialCommunities[i, 3:maxcol, with = F],
                                 collapse = " ")]
  }
  initialCommunities[, rowN := 1:nrow(initialCommunities)]
  initialCommunities[, ':='(mapcode = cut(rowN, breaks = c(cutRows, max(rowN)),
                                          labels = initialCommunities[cutRows+1,]$age1),
                            description = cut(rowN, breaks = c(cutRows, max(rowN)),
                                              labels = initialCommunities[cutRows,]$desc))]
  initialCommunities <- initialCommunities[!c(cutRows, cutRows+1),][,':='(desc = NULL, rowN = NULL)]
  initialCommunities[, ':='(description = gsub(">>", "", description), 
                            mapcode = as.numeric(as.character(mapcode)))]
  for(i in 4:ncol(initialCommunities)){
    initialCommunities[,i] <- as.integer(unlist(initialCommunities[,i, with = FALSE]))
  }
  sim$initialCommunities <- data.table(initialCommunities)
  rm(cutRows, i, maxcol)
  # load the initial community map
  sim$initialCommunitiesMap <- raster(file.path(dataPath, "initial-communities.gis"))
  
  # read species txt and convert it to data table
  maxcol <- max(count.fields(file.path(dataPath, "species.txt"), sep = ""))
  species <- read.table(file.path(dataPath, "species.txt"),
                        fill = TRUE,
                        sep = "",
                        header = FALSE,
                        blank.lines.skip = TRUE,
                        col.names = c(paste("col",1:maxcol, sep = "")),
                        stringsAsFactor = FALSE)
  
  species <- data.table(species[,1:11])
  species <- species[col1!= "LandisData",]
  species <- species[col1!= ">>",]
  names(species) <- c("species", "longevity", "sexualmature", "shadetolerance", 
                      "firetolerance", "seeddistance_eff", "seeddistance_max", 
                      "resproutprob", "resproutage_min", "resproutage_max",
                      "postfireregen")
  species[,':='(seeddistance_eff = gsub(",", "", seeddistance_eff),
                seeddistance_max = gsub(",", "", seeddistance_max))]
  species$longevity <- as.integer(species$longevity)
  species$sexualmature <- as.integer(species$sexualmature)
  species$shadetolerance <- as.integer(species$shadetolerance)
  species$firetolerance <- as.integer(species$firetolerance)
  species$seeddistance_eff <- as.integer(species$seeddistance_eff)
  species$seeddistance_max <- as.integer(species$seeddistance_max)
  species$resproutprob <- as.numeric(species$resproutprob)
  species$resproutage_min <- as.integer(species$resproutage_min)
  species$resproutage_max <- as.integer(species$resproutage_max)
  rm(maxcol)
  # read ecoregion and ecoregioin map
  maxcol <- max(count.fields(file.path(dataPath, "ecoregions.txt"), sep = ""))
  ecoregion <- read.table(file.path(dataPath, "ecoregions.txt"),
                          fill = TRUE,
                          sep = "",
                          header = FALSE,
                          blank.lines.skip = TRUE,
                          col.names = c(paste("col",1:maxcol, sep = "")),
                          stringsAsFactor = FALSE)
  ecoregion <- data.table(ecoregion)
  ecoregion <- ecoregion[col1 != "LandisData",]
  ecoregion <- ecoregion[col1 != ">>",]
  names(ecoregion)[1:4] <- c("active", "mapcode", "ecoregion", "description")
  ecoregion$mapcode <- as.integer(ecoregion$mapcode)
  sim$ecoregion <- ecoregion
  rm(maxcol)
  # load ecoregion map
  sim$ecoregionMap <- raster(file.path(dataPath, "ecoregions.gis"))
  
  # input species ecoregion dynamics table
  maxcol <- max(count.fields(file.path(dataPath, "biomass-succession-dynamic-inputs_test.txt"), 
                             sep = ""))
  speciesEcoregion <- read.table(file.path(dataPath, "biomass-succession-dynamic-inputs_test.txt"),
                                 fill = TRUE,
                                 sep = "",
                                 header = FALSE,
                                 blank.lines.skip = TRUE,
                                 col.names = c(paste("col",1:maxcol, sep = "")),
                                 stringsAsFactor = FALSE)
  speciesEcoregion <- data.table(speciesEcoregion)
  speciesEcoregion <- speciesEcoregion[col1 != "LandisData",]
  speciesEcoregion <- speciesEcoregion[col1 != ">>",]
  names(speciesEcoregion)[1:6] <- c("year", "ecoregion", "species",
                                    "establishprob", "maxANPP", "maxB")
  speciesEcoregion <- speciesEcoregion[,.(year, ecoregion, species,
                                          establishprob, maxANPP, maxB)]
  speciesEcoregion$year <- as.integer(speciesEcoregion$year)
  speciesEcoregion$establishprob <- as.numeric(speciesEcoregion$establishprob)
  speciesEcoregion$maxANPP <- as.integer(speciesEcoregion$maxANPP)
  speciesEcoregion$maxB <- as.integer(speciesEcoregion$maxB)
  sim$speciesEcoregion <- speciesEcoregion
  rm(maxcol)
  
#   # load the biomass succession txt and obtain 1) minRelativeB, 
#                                                2) sufficientLight, and 
#                                                3) additional species traits
  maxcol <- max(count.fields(file.path(dataPath, "biomass-succession_test.txt"), sep = ""))
  mainInput <- read.table(file.path(dataPath, "biomass-succession_test.txt"),
                          fill = TRUE,
                          sep = "",
                          header = FALSE,
                          blank.lines.skip = TRUE,
                          col.names = c(paste("col",1:maxcol, sep = "")),
                          stringsAsFactor = FALSE)
  mainInput <- data.table(mainInput)
  mainInput <- mainInput[col1 != ">>",]
  
  
  # get additional species traits
  speciesAddon <- mainInput
  startRow <- which(speciesAddon$col1 == "SpeciesParameters")
  speciesAddon <- speciesAddon[(startRow+1):(startRow+nrow(species)),1:6, with = FALSE]
  names(speciesAddon) <- c("species", "leaflongevity", "wooddecayrate",
                           "mortalityshape", "growthcurve", "leafLignin")
  speciesAddon[, ':='(leaflongevity = as.numeric(leaflongevity),
                      wooddecayrate = as.numeric(wooddecayrate),
                      mortalityshape = as.numeric(mortalityshape),
                      growthcurve = as.numeric(growthcurve),
                      leafLignin = as.numeric(leafLignin))]
  sim$species <- setkey(species, species)[setkey(speciesAddon, species), nomatch = 0]
  rm(maxcol)
  
  minRelativeB <- mainInput %>%
    data.frame
  startRow <- which(minRelativeB$col1 == "MinRelativeBiomass")
  minRelativeB <- minRelativeB[(startRow+1):(startRow+6),]
  minRelativeB[1,2:ncol(minRelativeB)] <- minRelativeB[1,1:(ncol(minRelativeB)-1)]
  names(minRelativeB) <- NULL
  minRelativeB <- minRelativeB[,-1] %>%
    t(.) %>%
    gsub(pattern="%",replacement="") %>%
    data.table
  names(minRelativeB) <- c("ecoregion", "X1", "X2", "X3", "X4", "X5")
  minRelativeB <- minRelativeB %>%
    mutate_each(funs(as.numeric(as.character(.))/100), vars=-ecoregion)
  sim$minRelativeB <- minRelativeB
  
  sufficientLight <- mainInput %>%
    data.frame
  startRow <- which(sufficientLight$col1 == "SufficientLight")
  sufficientLight <- sufficientLight[(startRow+1):(startRow+5), 1:7]
  for(i in 1:ncol(sufficientLight)){
    sufficientLight[,i] <- as.numeric(sufficientLight[,i])
  }
  names(sufficientLight) <- c("speciesshadetolerance",
                              "X0", "X1", "X2", "X3", "X4", "X5")
  
  sim$sufficientLight <- sufficientLight
  sim$spinupMortalityfraction <- 0.001
  sim$successionTimestep <- 10
  sim$seedingAlgorithm <- "wardDispersal"
  sim$useCache <- TRUE
  sim$cellSize <- res(ecoregionMap)[1]
  sim$calibrate <- FALSE
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above


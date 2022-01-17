Init <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
    cacheTags <- c(currentModule(sim), "init")
    if (P(sim)$successionTimestep > 10) 
        warning("successionTimestep parameter is > 10. Make sure this intended, ", 
            "keeping in mind that growth in the model depends on estimating 'sumB'. ", 
            "Only trees that are older than successionTimestep are included in the ", 
            "calculation of sumB, i.e., trees younger than this do not contribute ", 
            "to competitive interactions")
    if (is.null(sim$species)) 
        stop("'species' object must be provided")
    species <- as.data.table(sim$species)
    set(species, NULL, "speciesCode", factor(species$species, 
        levels = unique(species$species)))
    LandR::assertColumns(species, c(species = "character", Area = "factor", 
        longevity = "integer", sexualmature = "integer", shadetolerance = "numeric", 
        firetolerance = "integer", seeddistance_eff = "integer", 
        seeddistance_max = "integer", resproutprob = "numeric", 
        resproutage_min = "integer", resproutage_max = "integer", 
        postfireregen = "factor", leaflongevity = "integer", 
        wooddecayrate = "numeric", mortalityshape = "integer", 
        growthcurve = "numeric", leafLignin = "numeric", hardsoft = "factor", 
        speciesCode = "factor"))
    sim$species <- setkey(species, speciesCode)
    if (!suppliedElsewhere("cohortData", sim, where = "sim") | 
        !suppliedElsewhere("pixelGroupMap", sim, where = "sim")) {
        if (is.null(sim$rasterToMatch)) 
            stop("Must supply sim$rasterToMatch, since sim$cohortData or sim$pixelGroupMap are not supplied")
        if ((!suppliedElsewhere("cohortData", sim, where = "sim") && 
            suppliedElsewhere("pixelGroupMap", sim, where = "sim")) || 
            (suppliedElsewhere("cohortData", sim, where = "sim") && 
                !suppliedElsewhere("pixelGroupMap", sim, where = "sim"))) {
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
        rawBiomassMap <- makeDummyRawBiomassMap(sim$rasterToMatch)
        if (suppliedElsewhere("standAgeMap", sim, where = "sim")) 
            message(blue("'standAgeMap' was supplied, but "), 
                red("will be replaced by a dummy version to make "), 
                blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ", 
                  "'cohortData', 'pixelGroupMap' and 'standAgeMap'"))
        standAgeMap <- makeDummyStandAgeMap(rawBiomassMap)
        if (suppliedElsewhere("rstLCC", sim, where = "sim")) 
            message(blue("'rstLCC' was supplied, but "), red("will be replaced by a dummy version to make "), 
                blue("'cohortData' or 'pixelGroupMap'.\n If this is wrong, provide matching ", 
                  "'cohortData', 'pixelGroupMap' and 'rstLCC'"))
        rstLCC <- makeDummyRstLCC(sim$rasterToMatch)
        if (!compareRaster(sim$speciesLayers, sim$rasterToMatch, 
            stopiffalse = FALSE)) {
            message(blue("'speciesLayers' and 'rasterToMatch' do not match. "), 
                red("'speciesLayers' will be cropped/masked/reprojected to 'rasterToMatch'. "), 
                blue("If this is wrong, provide matching 'speciesLayers' and 'rasterToMatch'"))
            sim$speciesLayers <- postProcess(sim$speciesLayers, 
                rasterToMatch = sim$rasterToMatch, maskWithRTM = TRUE, 
                filename1 = NULL, filename2 = NULL, userTags = c(currentModule(sim), 
                  "speciesLayers"))
        }
        ecoregionFiles <- makeDummyEcoregionFiles(ecoregionMap, 
            rstLCC, sim$rasterToMatch)
        tempObjs <- checkSpeciesTraits(sim$speciesLayers, sim$species, 
            sim$sppColorVect)
        sim$speciesLayers <- tempObjs$speciesLayers
        sim$sppColorVect <- tempObjs$sppColorVect
        rm(tempObjs)
        pixelTable <- makePixelTable(speciesLayers = sim$speciesLayers, 
            standAgeMap = standAgeMap, ecoregionFiles = ecoregionFiles, 
            biomassMap = rawBiomassMap, rasterToMatch = sim$rasterToMatch, 
            rstLCC = rstLCC)
        message(blue("Creating a", red("DUMMY"), blue("cohorData table.")))
        coverColNames <- paste0("cover.", sim$species$species)
        pixelCohortData <- Cache(makeAndCleanInitialCohortData, 
            pixelTable, sppColumns = coverColNames, minCoverThreshold = 1, 
            doSubset = FALSE, userTags = c(cacheTags, "pixelCohortData"), 
            omitArgs = c("userTags"))
        pixelCohortData <- partitionBiomass(x = 1, pixelCohortData)
        setnames(pixelCohortData, "initialEcoregionCode", "ecoregionGroup")
        rmZeroBiomassQuote <- quote(B > 0)
        cohortDataNoBiomass <- pixelCohortData[eval(rmZeroBiomassQuote), 
            .(B, logAge, speciesCode, ecoregionGroup, lcc, cover)]
        cohortDataShort <- pixelCohortData[, list(coverNum = pmax(1, 
            .N - 1), coverPres = sum(cover > 0)), by = c("ecoregionGroup", 
            "speciesCode")]
        cohortDataShortNoCover <- cohortDataShort[coverPres == 
            0]
        cohortDataShort <- cohortDataShort[coverPres > 0]
        coverModel <- quote(lme4::glmer(cbind(coverPres, coverNum) ~ 
            speciesCode + (1 | ecoregionGroup), family = binomial))
        biomassModel <- quote(lme4::lmer(B ~ logAge * speciesCode + 
            cover * speciesCode + (logAge + cover + speciesCode | 
            ecoregionGroup)))
        message(blue("Estimating Species Establishment Probability from "), 
            red("DUMMY values of ecoregionGroup "), blue("using the formula:\n"), 
            magenta(format(coverModel)))
        modelCover <- Cache(statsModel, modelFn = coverModel, 
            .specialData = cohortDataShort, userTags = c(cacheTags, 
                "modelCover"), omitArgs = c("userTags"))
        message(blue("  The rsquared is: "))
        print(modelCover$rsq)
        message(blue("Estimating maxB from "), red("DUMMY values of age and ecoregionGroup "), 
            blue("using the formula:\n"), magenta(paste0(format(biomassModel), 
                collapse = "")))
        modelBiomass <- Cache(statsModel, modelFn = biomassModel, 
            .specialData = cohortDataNoBiomass, userTags = c(cacheTags, 
                "modelBiomass"), omitArgs = c("userTags"))
        message(blue("  The rsquared is: "))
        print(modelBiomass$rsq)
        message(blue("Create speciesEcoregion from "), red("DUMMY values"))
        speciesEcoregion <- makeSpeciesEcoregion(cohortDataBiomass = cohortDataNoBiomass, 
            cohortDataShort = cohortDataShort, cohortDataShortNoCover = cohortDataShortNoCover, 
            species = sim$species, modelCover = modelCover, modelBiomass = modelBiomass, 
            successionTimestep = P(sim)$successionTimestep, currentYear = time(sim))
        if (ncell(sim$rasterToMatch) > 3e+07) 
            .gc()
        if (!suppliedElsewhere("columnsForPixelGroups", sim, 
            where = "sim")) {
            columnsForPixelGroups <- LandR::columnsForPixelGroups
        }
        else {
            columnsForPixelGroups <- sim$columnsForPixelGroups
        }
        cohortDataFiles <- makeCohortDataFiles(pixelCohortData, 
            columnsForPixelGroups, speciesEcoregion, pixelGroupBiomassClass = 10, 
            pixelGroupAgeClass = 10, minAgeForGrouping = -1)
        sim$cohortData <- cohortDataFiles$cohortData
        pixelCohortData <- cohortDataFiles$pixelCohortData
        rm(cohortDataFiles)
        sim$ecoregion <- makeEcoregionDT(pixelCohortData, speciesEcoregion)
        sim$biomassMap <- makeBiomassMap(pixelCohortData, sim$rasterToMatch)
        sim$ecoregionMap <- makeEcoregionMap(ecoregionFiles, 
            pixelCohortData)
        sim$minRelativeB <- makeMinRelativeB(pixelCohortData)
        sim$pixelGroupMap <- makePixelGroupMap(pixelCohortData, 
            sim$rasterToMatch)
        compareRaster(sim$biomassMap, sim$ecoregionMap, sim$pixelGroupMap, 
            sim$rasterToMatch, orig = TRUE)
        speciesEcoregion[, `:=`(ecoregionGroup, factor(as.character(ecoregionGroup)))]
        sim$speciesEcoregion <- speciesEcoregion
        message(blue("Create pixelGroups based on: ", paste(columnsForPixelGroups, 
            collapse = ", "), "\n  Resulted in", magenta(length(unique(sim$cohortData$pixelGroup))), 
            "unique pixelGroup values"))
        LandR::assertERGs(sim$ecoregionMap, cohortData = sim$cohortData, 
            speciesEcoregion = speciesEcoregion, minRelativeB = sim$minRelativeB)
        LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, 
            cohortDefinitionCols = P(sim)$cohortDefinitionCols)
        LandR::assertUniqueCohortData(sim$cohortData, c("pixelGroup", 
            "ecoregionGroup", "speciesCode"))
    }
    rasterNamesToCompare <- c("ecoregionMap", "pixelGroupMap")
    if (!identical(P(sim)$initialBiomassSource, "cohortData")) {
        rasterNamesToCompare <- c(rasterNamesToCompare, "biomassMap")
    }
    haveAllRasters <- all(!unlist(lapply(rasterNamesToCompare, 
        function(rn) is.null(sim[[rn]]))))
    if (haveAllRasters) {
        rastersToCompare <- mget(rasterNamesToCompare, envir(sim))
        do.call(compareRaster, append(list(x = sim$rasterToMatch, 
            orig = TRUE), rastersToCompare))
    }
    else {
        stop("Expecting 3 rasters at this point: sim$biomassMap, sim$ecoregionMap, ", 
            "sim$pixelGroupMap and they must match sim$rasterToMatch")
    }
    LandR::assertERGs(sim$ecoregionMap, sim$cohortData, sim$speciesEcoregion, 
        sim$minRelativeB)
    if (is.null(sim$ecoregion)) 
        stop("Need to supply sim$ecoregion")
    setDT(sim$ecoregion)
    LandR::assertColumns(sim$ecoregion, c(active = "character", 
        ecoregionGroup = "factor"))
    ecoregion <- sim$ecoregion
    LandR::assertColumns(sim$speciesEcoregion, c(ecoregionGroup = "factor", 
        speciesCode = "factor", establishprob = "numeric", maxB = "integer", 
        maxANPP = "numeric"))
    speciesEcoregion <- sim$speciesEcoregion
    speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, 
        speciesCode)
    setDT(sim$minRelativeB)
    sim$minRelativeB <- sim$minRelativeB[unique(speciesEcoregion[, 
        .(ecoregionGroup)]), on = "ecoregionGroup", nomatch = 0]
    active_ecoregion <- setkey(ecoregion[active == "yes", .(k = 1, 
        ecoregionGroup)], k)
    pixelGroupMap <- sim$pixelGroupMap
    names(pixelGroupMap) <- "pixelGroup"
    ecoregionMapNAs <- is.na(sim$ecoregionMap[])
    ecoregionMapReporting <- mask(sim$ecoregionMap, sim$studyAreaReporting)
    ecoregionMapReportingNAs <- is.na(ecoregionMapReporting[])
    sim$activePixelIndex <- which(!ecoregionMapNAs)
    sim$activePixelIndexReporting <- which(!ecoregionMapReportingNAs)
    sim$inactivePixelIndex <- which(ecoregionMapNAs)
    sim$inactivePixelIndexReporting <- which(ecoregionMapReportingNAs)
    assertthat::assert_that(all(is.na(sim$ecoregionMap[]) == 
        is.na(pixelGroupMap[])))
    mod$activeEcoregionLength <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap, 
        getValues(sim$ecoregionMap), att = "ecoregionGroup"), 
        pixelIndex = 1:ncell(sim$ecoregionMap))[ecoregionGroup %in% 
        active_ecoregion$ecoregionGroup, .(NofCell = length(pixelIndex)), 
        by = "ecoregionGroup"]
    cohortData <- sim$cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[sim$activePixelIndex]), 
        ]
    cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = speciesEcoregion, 
        currentTime = round(time(sim)), cohortData = cohortData)
    cohortData <- updateSpeciesAttributes(species = sim$species, 
        cohortData = cohortData)
    LandR::assertCohortData(cohortData, sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)
    initialBiomassSourcePoss <- c("spinUp", "cohortData", "biomassMap")
    if (!any(grepl(P(sim)$initialBiomassSource, initialBiomassSourcePoss))) {
        stop("P(sim)$initialBiomassSource must be one of: ", 
            paste(initialBiomassSourcePoss, collapse = ", "))
    }
    if (grepl("spin", tolower(P(sim)$initialBiomassSource))) {
        stop("'spinUp as a value for P(sim)$initialBiomassSource is not working currently; ", 
            "please use 'cohortData'")
        if (verbose > 0) 
            message("Running spinup")
        spinupstage <- Cache(spinUp, cohortData = cohortData, 
            calibrate = P(sim)$calibrate, successionTimestep = P(sim)$successionTimestep, 
            spinupMortalityfraction = P(sim)$spinupMortalityfraction, 
            species = sim$species, userTags = c(cacheTags, "spinUp"), 
            omitArgs = c("userTags"))
        cohortData <- spinupstage$cohortData
        if (P(sim)$calibrate) {
            sim$spinupOutput <- spinupstage$spinupOutput
        }
        if (P(sim)$calibrate) {
            sim$simulationTreeOutput <- data.table(Year = numeric(), 
                siteBiomass = numeric(), Species = character(), 
                Age = numeric(), iniBiomass = numeric(), ANPP = numeric(), 
                Mortality = numeric(), deltaB = numeric(), finBiomass = numeric())
            sim$regenerationOutput <- data.table(seedingAlgorithm = character(), 
                species = character(), Year = numeric(), numberOfReg = numeric())
        }
    }
    else if (grepl("biomassMap", tolower(P(sim)$initialBiomassSource))) {
        stop("'biomassMap as a value for P(sim)$initialBiomassSource is not working currently; ", 
            "please use 'cohortData'")
        if (verbose > 0) 
            message("Skipping spinup and using the sim$biomassMap * SpeciesLayers pct as initial biomass values")
        biomassTable <- data.table(biomass = getValues(sim$biomassMap), 
            pixelGroup = getValues(pixelGroupMap))
        biomassTable <- na.omit(biomassTable)
        maxBiomass <- maxValue(sim$biomassMap)
        if (maxBiomass < 1000) {
            if (verbose > 0) {
                message(crayon::green("  Because biomassMap values are all below 1000, assuming that these should be\n", 
                  "    converted to tonnes/ha by multiplying by 100"))
            }
            biomassTable[, `:=`(biomass = biomass * 100)]
        }
        biomassTable <- biomassTable[, list(Bsum = mean(biomass, 
            na.rm = TRUE)), by = pixelGroup]
        if (!is.integer(biomassTable[["Bsum"]])) 
            set(biomassTable, NULL, "Bsum", asInteger(biomassTable[["Bsum"]]))
        set(cohortData, NULL, "B", NULL)
        cohortData[, `:=`(totalSpeciesPresence, sum(speciesPresence)), 
            by = "pixelGroup"]
        cohortData <- cohortData[biomassTable, on = "pixelGroup"]
        cohortData[, `:=`(B, Bsum * speciesPresence/totalSpeciesPresence), 
            by = c("pixelGroup", "speciesCode")]
        if (!is.integer(cohortData[["B"]])) 
            set(cohortData, NULL, "B", asInteger(cohortData[["B"]]))
    }
    pixelAll <- cohortData[, .(uniqueSumB = sum(B, na.rm = TRUE)), 
        by = pixelGroup]
    if (!is.integer(pixelAll[["uniqueSumB"]])) 
        set(pixelAll, NULL, "uniqueSumB", asInteger(pixelAll[["uniqueSumB"]]))
    if (all(!is.na(P(sim)$.plots))) {
        simulatedBiomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, 
            "uniqueSumB")
    }
    sim$cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, 
        speciesCode, age, B, mortality = 0L, aNPPAct = 0L)]
    simulationOutput <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap, 
        getValues(sim$ecoregionMap), att = "ecoregionGroup"), 
        pixelGroup = getValues(pixelGroupMap), pixelIndex = 1:ncell(sim$ecoregionMap))[, 
        .(NofPixel = length(pixelIndex)), by = c("ecoregionGroup", 
            "pixelGroup")]
    simulationOutput <- setkey(simulationOutput, pixelGroup)[setkey(pixelAll, 
        pixelGroup), nomatch = 0][, .(Biomass = sum(as.numeric(uniqueSumB) * 
        as.numeric(NofPixel))), by = ecoregionGroup]
    simulationOutput <- setkey(simulationOutput, ecoregionGroup)[setkey(mod$activeEcoregionLength, 
        ecoregionGroup), nomatch = 0]
    sim$simulationOutput <- simulationOutput[, .(ecoregionGroup, 
        NofCell, Year = asInteger(time(sim)), Biomass = asInteger(Biomass/NofCell), 
        ANPP = 0L, Mortality = 0L, Regeneration = 0L)]
    if (!is.null(P(sim)$calcSummaryBGM)) 
        sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, 
            sim$pixelGroupMap, P(sim)$vegLeadingProportion, mixedType = P(sim)$mixedType, 
            sppEquiv = sim$sppEquiv, sppEquivCol = P(sim)$sppEquivCol, 
            colors = sim$sppColorVect, doAssertion = getOption("LandR.assertions", 
                TRUE))
    sim$lastReg <- 0
    speciesEcoregion[, `:=`(identifier, year > P(sim)$successionTimestep)]
    speciesEcoregion_True <- speciesEcoregion[identifier == TRUE, 
        ]
    speciesEcoregion_False <- speciesEcoregion[identifier == 
        FALSE, ]
    if (NROW(speciesEcoregion_False)) {
        speciesEcoregion_True_addon <- speciesEcoregion_False[year == 
            max(year), ]
        speciesEcoregion_True <- rbindlist(list(speciesEcoregion_True_addon, 
            speciesEcoregion_True))
    }
    sim$speciesEcoregion <- speciesEcoregion_True[, `:=`(year = year - 
        min(year), identifier = NULL)]
    sim$lastFireYear <- "noFire"
    sim$pixelGroupMap <- pixelGroupMap
    return(invisible(sim))
}

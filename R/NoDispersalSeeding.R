NoDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel, 
    pixelsFromCurYrBurn) {
    sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, 
        currentTime = time(sim), successionTimestep = P(sim)$successionTimestep)
    sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[, 
        .(speciesCode, sexualmature)], speciesCode), nomatch = 0]
    seedingData <- sim$cohortData[age >= sexualmature]
    set(sim$cohortData, NULL, "sexualmature", NULL)
    set(seedingData, NULL, c("sexualmature", "age", "B", "mortality", 
        "aNPPAct"), NULL)
    siteShade <- setkey(data.table(calcSiteShade(currentTime = round(time(sim)), 
        sim$cohortData, sim$speciesEcoregion, sim$minRelativeB)), 
        pixelGroup)
    seedingData <- setkey(seedingData, pixelGroup)[siteShade, 
        nomatch = 0]
    seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[, 
        .(speciesCode, shadetolerance)], speciesCode), nomatch = 0]
    seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, 
        seedingData)
    seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData), 
        0, 1), ]
    set(seedingData, NULL, c("shadetolerance", "lightProb", "siteShade", 
        "sumB"), NULL)
    seedingData <- unique(seedingData, by = c("pixelGroup", "speciesCode"))
    pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel, 
        pixelGroup = getValues(sim$pixelGroupMap)[tempActivePixel]), 
        pixelGroup)
    pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(seedingData$pixelGroup)], 
        pixelGroup)
    seedingData <- setkey(seedingData, pixelGroup)[pixelsInfor, 
        allow.cartesian = TRUE]
    seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)
    specieseco_current <- speciesEcoregionLatestYear(sim$speciesEcoregion[, 
        .(year, speciesCode, establishprob, ecoregionGroup)], 
        round(time(sim)))
    specieseco_current <- setkey(specieseco_current, ecoregionGroup, 
        speciesCode)
    seedingData <- seedingData[specieseco_current, nomatch = 0]
    seedingData <- seedingData[establishprob %>>% runif(nrow(seedingData), 
        0, 1), ]
    set(seedingData, NULL, c("establishprob"), NULL)
    if (P(sim)$calibrate == TRUE && NROW(seedingData) > 0) {
        newCohortData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm, 
            year = round(time(sim)), numberOfReg = length(pixelIndex)), 
            by = speciesCode]
        newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[, 
            .(species, speciesCode)], speciesCode), nomatch = 0][, 
            .(species, seedingAlgorithm, year, numberOfReg)]
        sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, 
            newCohortData_summ))
    }
    if (nrow(seedingData) > 0) {
        outs <- updateCohortData(seedingData, cohortData = sim$cohortData, 
            sim$pixelGroupMap, currentTime = round(time(sim)), 
            speciesEcoregion = sim$speciesEcoregion, cohortDefinitionCols = P(sim)$cohortDefinitionCols, 
            treedFirePixelTableSinceLastDisp = NULL, successionTimestep = P(sim)$successionTimestep)
        sim$cohortData <- outs$cohortData
        sim$pixelGroupMap <- outs$pixelGroupMap
    }
    sim$lastReg <- round(time(sim))
    return(invisible(sim))
})

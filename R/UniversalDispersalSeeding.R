UniversalDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel) {
    sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, 
        currentTime = round(time(sim)), successionTimestep = P(sim)$successionTimestep)
    species <- sim$species
    speciessource <- setkey(sim$species[, .(speciesCode, k = 1)], 
        k)
    siteShade <- data.table(calcSiteShade(currentTime = round(time(sim)), 
        sim$cohortData, sim$speciesEcoregion, sim$minRelativeB))
    activePixelGroup <- unique(data.table(pixelGroup = getValues(sim$pixelGroupMap)[tempActivePixel], 
        ecoregionGroup = factorValues2(sim$ecoregionMap, getValues(sim$ecoregionMap), 
            att = "ecoregionGroup")[tempActivePixel]), by = "pixelGroup")
    siteShade <- dplyr::left_join(activePixelGroup, siteShade, 
        by = "pixelGroup") %>% data.table()
    siteShade[is.na(siteShade), `:=`(siteShade, 0)]
    setkey(siteShade[, `:=`(k, 1)], k)
    seedingData <- siteShade[speciessource, allow.cartesian = TRUE][, 
        `:=`(k, NULL)]
    seedingData <- setkey(seedingData, speciesCode)[setkey(sim$species[, 
        .(speciesCode, shadetolerance)], speciesCode), nomatch = 0]
    seedingData <- assignLightProb(sufficientLight = sim$sufficientLight, 
        seedingData)
    seedingData <- seedingData[lightProb %>>% runif(nrow(seedingData), 
        0, 1), ]
    set(seedingData, NULL, c("siteShade", "lightProb", "shadetolerance"), 
        NULL)
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
    specieseco_current <- setkeyv(specieseco_current, c("ecoregionGroup", 
        "speciesCode"))
    seedingData <- seedingData[specieseco_current, nomatch = 0]
    seedingData <- seedingData[establishprob %>>% runif(nrow(seedingData), 
        0, 1), ]
    set(seedingData, NULL, "establishprob", NULL)
    if (P(sim)$calibrate == TRUE) {
        newCohortData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm, 
            Year = round(time(sim)), numberOfReg = length(pixelIndex)), 
            by = speciesCode]
        newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[, 
            .(species, speciesCode)], speciesCode), nomatch = 0][, 
            .(species, seedingAlgorithm, Year, numberOfReg)]
        sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, 
            newCohortData_summ))
    }
    if (nrow(seedingData) > 0) {
        outs <- updateCohortData(seedingData, cohortData = sim$cohortData, 
            sim$pixelGroupMap, currentTime = round(time(sim)), 
            speciesEcoregion = sim$speciesEcoregion, treedFirePixelTableSinceLastDisp = NULL, 
            successionTimestep = P(sim)$successionTimestep)
        sim$cohortData <- outs$cohortData
        sim$pixelGroupMap <- outs$pixelGroupMap
    }
    sim$lastReg <- round(time(sim))
    return(invisible(sim))
})

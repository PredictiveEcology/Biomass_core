WardDispersalSeeding <- compiler::cmpfun(function(sim, tempActivePixel, 
    pixelsFromCurYrBurn, verbose = getOption("LandR.verbose", 
        TRUE)) {
    sim$cohortData <- calculateSumB(cohortData = sim$cohortData, 
        lastReg = sim$lastReg, currentTime = round(time(sim)), 
        successionTimestep = P(sim)$successionTimestep)
    siteShade <- calcSiteShade(currentTime = round(time(sim)), 
        cohortData = sim$cohortData, sim$speciesEcoregion, sim$minRelativeB)
    activePixelGroup <- data.table(pixelGroup = unique(getValues(sim$pixelGroupMap)[tempActivePixel])) %>% 
        na.omit()
    siteShade <- siteShade[activePixelGroup, on = "pixelGroup"]
    siteShade[is.na(siteShade), `:=`(siteShade, 0)]
    sim$cohortData <- sim$species[, c("speciesCode", "sexualmature")][sim$cohortData, 
        on = "speciesCode"]
    matureCohorts <- sim$cohortData[age >= sexualmature] %>% 
        unique(by = c("pixelGroup", "speciesCode")) %>% setkey(., 
        speciesCode)
    matureCohorts <- matureCohorts[, .(pixelGroup, speciesCode)]
    set(sim$cohortData, NULL, "sexualmature", NULL)
    if (NROW(matureCohorts) > 0) {
        seedSource <- sim$species[, list(speciesCode, seeddistance_eff, 
            seeddistance_max)] %>% setkey(., speciesCode) %>% 
            .[matureCohorts]
        setkey(seedSource, speciesCode)
        tempspecies1 <- sim$species[speciesCode %in% unique(matureCohorts$speciesCode), 
            ][, .(speciesCode, shadetolerance, seeddistance_eff, 
            seeddistance_max)]
        seedReceive <- setkey(tempspecies1[, c(k = 1, .SD)], 
            k)[setkey(siteShade[, c(k = 1, .SD)], k), allow.cartesian = TRUE][, 
            `:=`(k, NULL)]
        seedReceive <- assignLightProb(sufficientLight = sim$sufficientLight, 
            seedReceive)
        set(seedReceive, NULL, "siteShade", NULL)
        seedReceive <- seedReceive[lightProb %>>% runif(NROW(seedReceive), 
            0, 1), ][, .(pixelGroup, speciesCode, seeddistance_eff, 
            seeddistance_max)]
        setkey(seedReceive, speciesCode)
        seedReceive <- seedReceive[!sim$cohortData[age == 1L], 
            on = c("pixelGroup", "speciesCode")]
        seedSource <- seedSource[speciesCode %in% unique(seedReceive$speciesCode), 
            ]
        reducedPixelGroupMap <- sim$pixelGroupMap
        if (length(pixelsFromCurYrBurn) > 0) {
            reducedPixelGroupMap[pixelsFromCurYrBurn] <- NA
        }
        seedingData <- LANDISDisp(dtRcv = seedReceive, plot.it = FALSE, 
            dtSrc = seedSource, speciesTable = sim$species, pixelGroupMap = reducedPixelGroupMap, 
            successionTimestep = P(sim)$successionTimestep, verbose = getOption("LandR.verbose", 
                TRUE) > 0)
        if (getOption("LandR.verbose", TRUE) > 0) {
            emptyForestPixels <- sim$treedFirePixelTableSinceLastDisp[burnTime < 
                time(sim)]
            seedsArrivedPixels <- unique(seedingData[emptyForestPixels, 
                on = "pixelIndex", nomatch = 0], by = "pixelIndex")
            message(blue("Of", NROW(emptyForestPixels), "burned and empty pixels: Num pixels where seeds arrived:", 
                NROW(seedsArrivedPixels)))
        }
        rm(seedReceive, seedSource)
        if (NROW(seedingData) > 0) {
            seedingData[, `:=`(ecoregionGroup, factorValues2(sim$ecoregionMap, 
                getValues(sim$ecoregionMap), att = "ecoregionGroup")[seedingData$pixelIndex])]
            seedingData <- setkey(seedingData, ecoregionGroup, 
                speciesCode)
            specieseco_current <- speciesEcoregionLatestYear(sim$speciesEcoregion[, 
                .(year, speciesCode, establishprob, ecoregionGroup)], 
                round(time(sim)))
            specieseco_current <- setkeyv(specieseco_current, 
                c("ecoregionGroup", "speciesCode"))
            seedingData <- seedingData[specieseco_current, nomatch = 0]
            LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, 
                cohortDefinitionCols = P(sim)$cohortDefinitionCols)
            seedingData <- seedingData[runif(nrow(seedingData)) <= 
                establishprob, ]
            if (getOption("LandR.verbose", TRUE) > 0) {
                seedsArrivedPixels <- unique(seedingData[emptyForestPixels, 
                  on = "pixelIndex", nomatch = 0], by = "pixelIndex")
                message(blue("Of", NROW(emptyForestPixels), "burned and empty pixels: Num pixels where seedlings established:", 
                  NROW(seedsArrivedPixels)))
            }
            set(seedingData, NULL, "establishprob", NULL)
            if (P(sim)$calibrate == TRUE) {
                seedingData_summ <- seedingData[, .(seedingAlgorithm = P(sim)$seedingAlgorithm, 
                  Year = round(time(sim)), numberOfReg = length(pixelIndex)), 
                  by = speciesCode]
                seedingData_summ <- setkey(seedingData_summ, 
                  speciesCode)[setkey(sim$species[, .(species, 
                  speciesCode)], speciesCode), nomatch = 0][, 
                  .(species, seedingAlgorithm, Year, numberOfReg)]
                sim$regenerationOutput <- rbindlist(list(sim$regenerationOutput, 
                  seedingData_summ))
            }
            if (nrow(seedingData) > 0) {
                outs <- updateCohortData(seedingData, cohortData = sim$cohortData, 
                  pixelGroupMap = sim$pixelGroupMap, currentTime = round(time(sim)), 
                  speciesEcoregion = sim$speciesEcoregion, treedFirePixelTableSinceLastDisp = NULL, 
                  successionTimestep = P(sim)$successionTimestep)
                sim$cohortData <- outs$cohortData
                sim$pixelGroupMap <- outs$pixelGroupMap
            }
        }
    }
    sim$lastReg <- round(time(sim))
    return(invisible(sim))
})

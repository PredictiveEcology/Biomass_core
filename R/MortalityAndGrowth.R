MortalityAndGrowth <- compiler::cmpfun(function(sim) {
    if (NROW(sim$cohortData)) {
        if (is.numeric(P(sim)$.useParallel)) {
            data.table::setDTthreads(P(sim)$.useParallel)
            if (data.table::getDTthreads() > 1L) 
                message("Mortality and Growth should be using >100% CPU")
        }
        if (!requireNamespace(P(sim)$growthAndMortalityDrivers, 
            quietly = TRUE)) {
            stop(paste0("The package specified for growthAndMortalityDrivers, ", 
                P(sim)$growthAndMortalityDrivers, ", must be installed"))
        }
        calculateClimateEffect <- getFromNamespace("calculateClimateEffect", 
            P(sim)$growthAndMortalityDrivers)
        cohortData <- sim$cohortData
        pgs <- unique(cohortData$pixelGroup)
        mod$groupSize <- maxRowsDT(maxLen = 1e+07, maxMem = P(sim)$.maxMemory, 
            startClockTime = sim$._startClockTime, groupSize = mod$groupSize, 
            modEnv = mod)
        numGroups <- ceiling(length(pgs)/mod$groupSize)
        groupNames <- paste0("Group", seq(numGroups))
        if (length(pgs) > mod$groupSize) {
            sim$cohortData <- cohortData[0, ]
            pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup), 
                temID = 1:length(unique(cohortData$pixelGroup)))
            cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), 
                by = mod$groupSize), max(pixelGroups$temID))))
            if (length(cutpoints) == 1) 
                cutpoints <- c(cutpoints, cutpoints + 1)
            pixelGroups[, `:=`(groups, rep(groupNames, each = mod$groupSize, 
                length.out = NROW(pixelGroups)))]
        }
        for (subgroup in groupNames) {
            if (numGroups == 1) {
                subCohortData <- cohortData
            }
            else {
                subCohortData <- cohortData[cohortData$pixelGroup %in% 
                  pixelGroups$pixelGroupIndex[pixelGroups$groups == 
                    subgroup], ]
            }
            subCohortData[age > 1, `:=`(age, age + 1L)]
            subCohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion, 
                currentTime = round(time(sim)), cohortData = subCohortData)
            subCohortData <- updateSpeciesAttributes(species = sim$species, 
                cohortData = subCohortData)
            setkeyv(subCohortData, "pixelGroup")
            subCohortData <- calculateSumB(cohortData = subCohortData, 
                lastReg = sim$lastReg, currentTime = time(sim), 
                successionTimestep = P(sim)$successionTimestep)
            startNumCohorts <- NROW(subCohortData)
            keep <- (subCohortData$age <= subCohortData$longevity) & 
                (subCohortData$B >= P(sim)$minCohortBiomass)
            if (all(keep)) {
                subCohortPostLongevity <- subCohortData
                diedCohortData <- subCohortData[0]
            }
            else {
                subCohortPostLongevity <- subCohortData[keep]
                diedCohortData <- subCohortData[!keep]
            }
            numCohortsDied <- NROW(diedCohortData)
            if (numCohortsDied > 0) {
                pgsToRm <- diedCohortData[!pixelGroup %in% subCohortPostLongevity$pixelGroup]
                pixelsToRm <- which(getValues(sim$pixelGroupMap) %in% 
                  unique(pgsToRm$pixelGroup))
                if (isTRUE(getOption("LandR.assertions"))) {
                  a <- subCohortPostLongevity$pixelGroup %in% 
                    na.omit(getValues(sim$pixelGroupMap))
                  if (!all(a)) {
                    stop("Post longevity-based mortality, there is a divergence between pixelGroupMap and cohortData pixelGroups")
                  }
                }
                if (length(pixelsToRm) > 0) {
                  if (getOption("LandR.verbose", TRUE) > 0) {
                    numPixelGrps <- sum(sim$pixelGroupMap[] != 
                      0, na.rm = TRUE)
                  }
                  sim$pixelGroupMap[pixelsToRm] <- 0L
                  if (getOption("LandR.verbose", TRUE) > 1) {
                    message(blue("Death due to old age:", "\n  ", 
                      numCohortsDied, "cohorts died of old age (i.e., due to passing longevity) or biomass <= 1; ", 
                      sum(is.na(diedCohortData$age)), " of those because age == NA; ", 
                      "\n  ", NROW(unique(pgsToRm$pixelGroup)), 
                      "pixelGroups to be removed (i.e., ", "\n  ", 
                      length(pixelsToRm), "pixels; "))
                  }
                  if (getOption("LandR.verbose", TRUE) > 0) {
                    message(blue("\n   Total number of pixelGroups -- Was:", 
                      numPixelGrps, ", Now:", magenta(sum(sim$pixelGroupMap[] != 
                        0, na.rm = TRUE))))
                  }
                }
            }
            subCohortData <- subCohortPostLongevity
            subCohortData <- calculateAgeMortality(cohortData = subCohortData)
            set(subCohortData, NULL, c("longevity", "mortalityshape"), 
                NULL)
            subCohortData <- calculateCompetition(cohortData = subCohortData)
            if (!P(sim)$calibrate) {
                set(subCohortData, NULL, "sumB", NULL)
            }
            subCohortData <- calculateANPP(cohortData = subCohortData)
            set(subCohortData, NULL, "growthcurve", NULL)
            set(subCohortData, NULL, "aNPPAct", pmax(1, subCohortData$aNPPAct - 
                subCohortData$mAge))
            if (!P(sim)$growthAndMortalityDrivers == "LandR") {
                if (!is.null(subCohortData$growthPred)) {
                  set(subCohortData, NULL, c("growthPred", "mortPred"), 
                    NULL)
                }
                cceArgs <- lapply(sim$cceArgs, FUN = function(x) {
                  arg <- eval(x, envir = sim)
                })
                names(cceArgs) <- paste(sim$cceArgs)
                predObj <- calculateClimateEffect(cceArgs = cceArgs, 
                  cohortData = subCohortData, pixelGroupMap = sim$pixelGroupMap, 
                  gmcsGrowthLimits = P(sim)$gmcsGrowthLimits, 
                  gmcsMortLimits = P(sim)$gmcsMortLimits, gmcsMinAge = P(sim)$gmcsMinAge, 
                  cohortDefinitionCols = P(sim)$cohortDefinitionCols)
                commonNames <- names(predObj)[names(predObj) %in% 
                  names(subCohortData)]
                subCohortData <- subCohortData[predObj, on = commonNames]
                subCohortData[, `:=`(aNPPAct, pmax(0, asInteger(aNPPAct * 
                  growthPred/100)))]
            }
            subCohortData <- calculateGrowthMortality(cohortData = subCohortData)
            set(subCohortData, NULL, "mBio", pmax(0, subCohortData$mBio - 
                subCohortData$mAge))
            set(subCohortData, NULL, "mBio", pmin(subCohortData$mBio, 
                subCohortData$aNPPAct))
            set(subCohortData, NULL, "mortality", subCohortData$mBio + 
                subCohortData$mAge)
            if (!P(sim)$growthAndMortalityDrivers == "LandR") {
                subCohortData[, `:=`(mortality, pmax(0, asInteger(mortality * 
                  mortPred/100)))]
                subCohortData[, `:=`(mortality, pmin(mortality, 
                  B + aNPPAct))]
                if (!P(sim)$keepClimateCols) {
                  set(subCohortData, NULL, c("growthPred", "mortPred"), 
                    NULL)
                }
            }
            set(subCohortData, NULL, c("mBio", "mAge", "maxANPP", 
                "maxB", "maxB_eco", "bAP", "bPM"), NULL)
            if (P(sim)$calibrate) {
                set(subCohortData, NULL, "deltaB", asInteger(subCohortData$aNPPAct - 
                  subCohortData$mortality))
                set(subCohortData, NULL, "B", subCohortData$B + 
                  subCohortData$deltaB)
                tempcohortdata <- subCohortData[, .(pixelGroup, 
                  Year = time(sim), siteBiomass = sumB, speciesCode, 
                  Age = age, iniBiomass = B - deltaB, ANPP = round(aNPPAct, 
                    1), Mortality = round(mortality, 1), deltaB, 
                  finBiomass = B)]
                tempcohortdata <- setkey(tempcohortdata, speciesCode)[setkey(sim$species[, 
                  .(species, speciesCode)], speciesCode), nomatch = 0][, 
                  `:=`(speciesCode = species, species = NULL, 
                    pixelGroup = NULL)]
                setnames(tempcohortdata, "speciesCode", "Species")
                sim$simulationTreeOutput <- rbind(sim$simulationTreeOutput, 
                  tempcohortdata)
                set(subCohortData, NULL, c("deltaB", "sumB"), 
                  NULL)
            }
            else {
                set(subCohortData, NULL, "B", subCohortData$B + 
                  asInteger(subCohortData$aNPPAct - subCohortData$mortality))
            }
            subCohortData[, `:=`(mortality = asInteger(mortality), 
                aNPPAct = asInteger(aNPPAct))]
            if (numGroups == 1) {
                sim$cohortData <- subCohortData
            }
            else {
                sim$cohortData <- rbindlist(list(sim$cohortData, 
                  subCohortData), fill = TRUE)
            }
            rm(subCohortData)
        }
        rm(cohortData)
        if (ncell(sim$rasterToMatch) > 3e+07) 
            gc()
        sim$cohortData[age == 1, `:=`(age, age + 1L)]
        if (isTRUE(getOption("LandR.assertions"))) {
            if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, 
                by = P(sim)$cohortDefinitionCols)))) {
                stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
            }
        }
        LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap, 
            cohortDefinitionCols = P(sim)$cohortDefinitionCols)
    }
    return(invisible(sim))
})

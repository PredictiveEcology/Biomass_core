SummaryBGM <- compiler::cmpfun(function(sim) {
    pixelGroups <- data.table(pixelGroupIndex = unique(sim$cohortData$pixelGroup), 
        temID = 1:length(unique(sim$cohortData$pixelGroup)))
    cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), 
        by = P(sim)$cutpoint), max(pixelGroups$temID))))
    if (length(cutpoints) == 1) 
        cutpoints <- c(cutpoints, cutpoints + 1)
    pixelGroups[, `:=`(groups, cut(temID, breaks = cutpoints, 
        labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""), 
        include.lowest = TRUE))]
    ecoPixelgroup <- data.table(ecoregionGroup = factorValues2(sim$ecoregionMap, 
        getValues(sim$ecoregionMap), att = "ecoregionGroup"), 
        pixelGroup = getValues(sim$pixelGroupMap), pixelIndex = 1:ncell(sim$ecoregionMap))[, 
        .(NofPixelGroup = length(pixelIndex)), by = c("ecoregionGroup", 
            "pixelGroup")]
    for (subgroup in paste("Group", 1:(length(cutpoints) - 1), 
        sep = "")) {
        subCohortData <- sim$cohortData[pixelGroup %in% pixelGroups[groups == 
            subgroup, ]$pixelGroupIndex, ]
        if (nrow(subCohortData[age == (P(sim)$successionTimestep + 
            1), ]) > 0) {
            subCohortData[age == (P(sim)$successionTimestep + 
                1), `:=`(reproduction, sum(B)), by = pixelGroup]
        }
        else {
            subCohortData[, `:=`(reproduction, 0)]
        }
        subCohortData[is.na(reproduction), `:=`(reproduction, 
            0L)]
        summarytable_sub <- subCohortData[, .(uniqueSumB = sum(B, 
            na.rm = TRUE), uniqueSumANPP = sum(aNPPAct, na.rm = TRUE), 
            uniqueSumMortality = sum(mortality, na.rm = TRUE), 
            uniqueSumRege = mean(reproduction, na.rm = TRUE)), 
            by = pixelGroup]
        for (column in names(summarytable_sub)) if (!is.integer(summarytable_sub[[column]])) 
            set(summarytable_sub, NULL, column, asInteger(summarytable_sub[[column]]))
        tempOutput <- setkey(ecoPixelgroup[pixelGroup %in% pixelGroups[groups == 
            subgroup, ]$pixelGroupIndex, ], pixelGroup)[setkey(summarytable_sub, 
            pixelGroup), nomatch = 0]
        if (subgroup == "Group1") {
            summaryBGMtable <- summarytable_sub
            tempOutput_All <- tempOutput
        }
        else {
            summaryBGMtable <- rbindlist(list(summaryBGMtable, 
                summarytable_sub))
            tempOutput_All <- rbindlist(list(tempOutput_All, 
                tempOutput))
        }
        rm(summarytable_sub, tempOutput, subCohortData)
    }
    tempOutput_All <- tempOutput_All[, .(Biomass = sum(as.numeric(uniqueSumB) * 
        as.numeric(NofPixelGroup)), ANPP = sum(as.numeric(uniqueSumANPP) * 
        as.numeric(NofPixelGroup)), Mortality = sum(as.numeric(uniqueSumMortality) * 
        as.numeric(NofPixelGroup)), Regeneration = sum(as.numeric(uniqueSumRege) * 
        as.numeric(NofPixelGroup))), by = ecoregionGroup]
    tempOutput_All <- setkey(tempOutput_All, ecoregionGroup)[setkey(mod$activeEcoregionLength, 
        ecoregionGroup), nomatch = 0]
    sim$simulationOutput <- rbindlist(list(sim$simulationOutput, 
        tempOutput_All[, .(ecoregionGroup, NofCell, Year = as.integer(time(sim)), 
            Biomass = asInteger(Biomass/NofCell), ANPP = asInteger(ANPP/NofCell), 
            Mortality = asInteger(Mortality/NofCell), Regeneration = asInteger(Regeneration/NofCell))]))
    names(sim$pixelGroupMap) <- "pixelGroup"
    sim$simulatedBiomassMap <- rasterizeReduced(summaryBGMtable, 
        sim$pixelGroupMap, "uniqueSumB")
    setColors(sim$simulatedBiomassMap) <- c("light green", "dark green")
    sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, 
        "uniqueSumANPP")
    setColors(sim$ANPPMap) <- c("light green", "dark green")
    sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, 
        "uniqueSumMortality")
    setColors(sim$mortalityMap) <- c("light green", "dark green")
    if (!is.null(P(sim)$calcSummaryBGM)) 
        sim$vegTypeMap <- vegTypeMapGenerator(sim$cohortData, 
            sim$pixelGroupMap, P(sim)$vegLeadingProportion, mixedType = P(sim)$mixedType, 
            sppEquiv = sim$sppEquiv, sppEquivCol = P(sim)$sppEquivCol, 
            colors = sim$sppColorVect, doAssertion = getOption("LandR.assertions", 
                TRUE))
    rm(cutpoints, pixelGroups, tempOutput_All, summaryBGMtable)
    return(invisible(sim))
})

plotAvgVegAttributes <- compiler::cmpfun(function(sim) {
    LandR::assertSpeciesPlotLabels(sim$species$species, sim$sppEquiv)
    checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
    pixelCohortData <- addNoPixel2CohortData(sim$cohortData, 
        sim$pixelGroupMap, cohortDefinitionCols = P(sim)$cohortDefinitionCols)
    thisPeriod <- pixelCohortData[, list(year = time(sim), sumB = sum(B * 
        noPixels, na.rm = TRUE), maxAge = asInteger(max(age, 
        na.rm = TRUE)), sumANPP = asInteger(sum(aNPPAct * noPixels, 
        na.rm = TRUE)))]
    denominator <- length(sim$pixelGroupMap[!is.na(sim$pixelGroupMap)]) * 
        100
    thisPeriod[, `:=`(sumB, asInteger(sumB/denominator))]
    thisPeriod[, `:=`(sumANPP, asInteger(sumANPP/denominator))]
    if (is.null(sim$summaryLandscape)) {
        summaryLandscape <- thisPeriod
    }
    else {
        summaryLandscape <- rbindlist(list(sim$summaryLandscape, 
            thisPeriod))
    }
    if (length(unique(summaryLandscape$year)) > 1) {
        df2 <- melt(summaryLandscape, id.vars = "year")
        varLabels <- c(sumB = "Biomass", maxAge = "Age", sumANPP = "aNPP")
        if (any(P(sim)$.plots == "screen")) {
            dev(mod$statsWindow)
        }
        Plots(df2, fn = landscapeAttributesPlot, types = mod$plotTypes, 
            filename = "landscape_biomass_aNPP_max_age", path = file.path(outputPath(sim), 
                "figures"), ggsaveArgs = list(width = 10, height = 5, 
                units = "in", dpi = 300), varLabels = varLabels)
    }
    sim$summaryLandscape <- summaryLandscape
    return(invisible(sim))
})

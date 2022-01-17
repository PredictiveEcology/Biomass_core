plotSummaryBySpecies <- compiler::cmpfun(function(sim) {
    LandR::assertSpeciesPlotLabels(sim$species$species, sim$sppEquiv)
    checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
    thisPeriod <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap, 
        cohortDefinitionCols = P(sim)$cohortDefinitionCols)
    for (column in names(thisPeriod)) if (is.integer(thisPeriod[[column]])) 
        set(thisPeriod, NULL, column, as.numeric(thisPeriod[[column]]))
    thisPeriod <- thisPeriod[, list(year = time(sim), BiomassBySpecies = sum(B * 
        noPixels, na.rm = TRUE), AgeBySppWeighted = sum(age * 
        B * noPixels, na.rm = TRUE)/sum(B * noPixels, na.rm = TRUE), 
        aNPPBySpecies = sum(aNPPAct * noPixels, na.rm = TRUE), 
        OldestCohortBySpp = max(age, na.rm = TRUE)), by = .(speciesCode)]
    cohortData <- addNoPixel2CohortData(sim$cohortData, sim$pixelGroupMap, 
        cohortDefinitionCols = P(sim)$cohortDefinitionCols)
    cohortData[, `:=`(bWeightedAge, floor(sum(age * B)/sum(B)/10) * 
        10), .(pixelGroup)]
    overstory <- cohortData[age >= bWeightedAge, .(overstoryBiomass = sum(as.numeric(B) * 
        noPixels)), .(speciesCode)]
    thisPeriod <- thisPeriod[overstory, on = "speciesCode"]
    if (is.null(sim$summaryBySpecies)) {
        summaryBySpecies <- thisPeriod
    }
    else {
        summaryBySpecies <- rbindlist(list(sim$summaryBySpecies, 
            thisPeriod))
    }
    vtm <- raster::mask(sim$vegTypeMap, sim$studyAreaReporting)
    freqs <- table(na.omit(factorValues2(vtm, vtm[], att = 2)))
    tabl <- as.vector(freqs)
    summaryBySpecies1 <- data.frame(year = rep(floor(time(sim)), 
        length(freqs)), leadingType = names(freqs), counts = tabl, 
        stringsAsFactors = FALSE)
    whMixedLeading <- which(summaryBySpecies1$leadingType == 
        "Mixed")
    summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType, 
        sim$sppEquiv, "EN_generic_short")
    summaryBySpecies1$leadingType[whMixedLeading] <- "Mixed"
    colours <- equivalentName(names(sim$sppColorVect), sim$sppEquiv, 
        "EN_generic_short")
    whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")
    colours[whMixedSppColors] <- "Mixed"
    colorIDs <- match(summaryBySpecies1$leadingType, colours)
    summaryBySpecies1$cols <- sim$sppColorVect[colorIDs]
    if (!is.null(sim$summaryBySpecies1)) {
        summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, 
            summaryBySpecies1))
    }
    if (length(unique(summaryBySpecies1$year)) > 1) {
        df <- sim$species[, list(speciesCode, species)][summaryBySpecies, 
            on = "speciesCode"]
        df$species <- equivalentName(df$species, sim$sppEquiv, 
            "EN_generic_short")
        colorIDs <- match(df$species, colours)
        df$cols <- sim$sppColorVect[colorIDs]
        cols2 <- df$cols
        names(cols2) <- df$species
        if (!any(is.na(P(sim)$.plots))) {
            if (any(P(sim)$.plots == "screen")) {
                dev(mod$statsWindow)
            }
        }
        Plots(df, fn = speciesBiomassPlot, filename = "biomass_by_species", 
            path = file.path(outputPath(sim), "figures"), types = mod$plotTypes, 
            ggsaveArgs = list(width = 7, height = 5, units = "in", 
                dpi = 300), y = "BiomassBySpecies", cols = cols2, 
            ylab = "Biomass", plotTitle = paste0("Total biomass by species\n", 
                "across pixels"))
        maxNpixels <- length(sim$activePixelIndexReporting)
        cols3 <- summaryBySpecies1$cols
        names(cols3) <- summaryBySpecies1$leadingType
        Plots(summaryBySpecies1, fn = speciesLeadingPlot, filename = "N_pixels_leading", 
            path = file.path(outputPath(sim), "figures"), types = mod$plotTypes, 
            ggsaveArgs = list(width = 7, height = 5, units = "in", 
                dpi = 300), cols = cols3, maxNpixels = maxNpixels)
        Plots(df, fn = speciesAgeANPPPlot, filename = "biomass-weighted_species_age", 
            path = file.path(outputPath(sim), "figures"), types = mod$plotTypes, 
            ggsaveArgs = list(width = 7, height = 5, units = "in", 
                dpi = 300), y = "AgeBySppWeighted", cols = cols2, 
            ylab = "Age", plotTitle = paste0("Biomass-weighted species age\n", 
                "averaged across pixels"))
        if (P(sim)$plotOverstory) {
            Plots(df, fn = speciesBiomassPlot, filename = "overstory_biomass", 
                path = file.path(outputPath(sim), "figures"), 
                types = mod$plotTypes, ggsaveArgs = list(width = 7, 
                  height = 5, units = "in", dpi = 300), y = "overstoryBiomass", 
                cols = cols2, ylab = "Overstory Biomass", plotTitle = "Overstory biomass by species")
        }
        else {
            Plots(df, fn = speciesAgeANPPPlot, filename = "oldest_cohorts", 
                path = file.path(outputPath(sim), "figures"), 
                types = mod$plotTypes, ggsaveArgs = list(width = 7, 
                  height = 5, units = "in", dpi = 300), y = "OldestCohortBySpp", 
                cols = cols2, ylab = "Age", plotTitle = paste("Oldest cohort age\n", 
                  "across pixels"))
        }
        Plots(df, fn = speciesAgeANPPPlot, filename = "total_aNPP_by_species", 
            path = file.path(outputPath(sim), "figures"), types = mod$plotTypes, 
            ggsaveArgs = list(width = 7, height = 5, units = "in", 
                dpi = 300), y = "aNPPBySpecies", cols = cols2, 
            ylab = "aNPP", plotTitle = paste0("Total aNPP by species\n", 
                "across pixels"))
    }
    sim$summaryBySpecies <- summaryBySpecies
    sim$summaryBySpecies1 <- summaryBySpecies1
    return(invisible(sim))
})

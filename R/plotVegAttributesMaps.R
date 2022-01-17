plotVegAttributesMaps <- compiler::cmpfun(function(sim) {
    LandR::assertSpeciesPlotLabels(sim$species$species, sim$sppEquiv)
    plotTypes <- "screen"
    biomassMapForPlot <- raster::mask(sim$simulatedBiomassMap, 
        sim$studyAreaReporting)
    ANPPMapForPlot <- raster::mask(sim$ANPPMap, sim$studyAreaReporting)
    mortalityMapForPlot <- raster::mask(sim$mortalityMap, sim$studyAreaReporting)
    if (is.null(sim$reproductionMap)) {
        reproductionMapForPlot <- biomassMapForPlot
        reproductionMapForPlot[!is.na(reproductionMapForPlot)][] <- 0
    }
    else {
        reproductionMapForPlot <- raster::mask(sim$reproductionMap, 
            sim$studyAreaReporting)
    }
    levs <- raster::levels(sim$vegTypeMap)[[1]]
    levelsName <- names(levs)[2]
    sppEquiv <- sim$sppEquiv[!is.na(sim$sppEquiv[[P(sim)$sppEquivCol]]), 
        ]
    levsLeading <- equivalentName(levs[[levelsName]], sppEquiv, 
        "Leading")
    if (any(grepl("Mixed", levs[[levelsName]]))) {
        hasOnlyMixedAsOther <- sum(is.na(levsLeading) == 1) && 
            levs[[levelsName]][is.na(levsLeading)] == "Mixed"
        if (!isTRUE(hasOnlyMixedAsOther)) {
            stop("'plotVegAttributesMaps' in Biomass_core can only deal with 'Mixed' category or the ones in sim$sppEquiv")
        }
    }
    whMixedLevs <- which(levs[[levelsName]] == "Mixed")
    whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")
    levsLeading[whMixedLevs] <- "Mixed"
    shortNames <- equivalentName(levsLeading, sppEquiv, "EN_generic_short")
    shortNames[whMixedLevs] <- "Mixed"
    levs[[levelsName]] <- shortNames
    levels(sim$vegTypeMap) <- levs
    colsLeading <- equivalentName(names(sim$sppColorVect), sppEquiv, 
        "Leading")
    colsLeading[whMixedSppColors] <- "Mixed"
    sppColorVect <- sim$sppColorVect
    names(sppColorVect) <- colsLeading
    colours <- sppColorVect[na.omit(match(levsLeading, colsLeading))]
    setColors(sim$vegTypeMap, levs$ID) <- colours
    vegTypeMapForPlot <- raster::mask(sim$vegTypeMap, sim$studyAreaReporting)
    mapsToPlot <- vegTypeMapForPlot
    names(mapsToPlot) <- c("Leading vegetation")
    if (!is.null(reproductionMapForPlot)) {
        mapsToPlot <- stack(reproductionMapForPlot, mapsToPlot)
        names(mapsToPlot)[1] <- "Reproduction"
    }
    if (!is.null(mortalityMapForPlot)) {
        mapsToPlot <- stack(mortalityMapForPlot, mapsToPlot)
        names(mapsToPlot)[1] <- "Mortality"
    }
    if (!is.null(ANPPMapForPlot)) {
        mapsToPlot <- stack(ANPPMapForPlot, mapsToPlot)
        names(mapsToPlot)[1] <- "ANPP"
    }
    if (!is.null(biomassMapForPlot)) {
        mapsToPlot <- stack(biomassMapForPlot, mapsToPlot)
        names(mapsToPlot)[1] <- "Biomass"
    }
    if (any(P(sim)$.plots == "screen")) {
        dev(mod$mapWindow)
        clearPlot()
    }
    Plots(mapsToPlot, type = plotTypes, title = names(mapsToPlot), 
        new = TRUE)
    if (any(P(sim)$.plots == "screen")) {
        grid.rect(0.93, 0.97, width = 0.2, height = 0.06, gp = gpar(fill = "white", 
            col = "white"))
        grid.text(label = paste0("Year = ", round(time(sim))), 
            x = 0.93, y = 0.97)
    }
    return(invisible(sim))
})

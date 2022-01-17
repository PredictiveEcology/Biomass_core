summaryRegen <- compiler::cmpfun(function(sim) {
    if (all(!is.na(P(sim)$.plots))) {
        pixelGroupMap <- sim$pixelGroupMap
        names(pixelGroupMap) <- "pixelGroup"
        pixelAll <- sim$cohortData[age <= P(sim)$successionTimestep + 
            1, .(uniqueSumReproduction = sum(B, na.rm = TRUE)), 
            by = pixelGroup]
        if (!is.integer(pixelAll[["uniqueSumReproduction"]])) 
            set(pixelAll, NULL, "uniqueSumReproduction", asInteger(pixelAll[["uniqueSumReproduction"]]))
        if (NROW(pixelAll) > 0) {
            reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, 
                "uniqueSumReproduction")
            setColors(reproductionMap) <- c("light green", "dark green")
        }
        else {
            reproductionMap <- setValues(pixelGroupMap, 0L)
        }
        rm(pixelAll)
        sim$reproductionMap <- reproductionMap
        rm(pixelGroupMap)
    }
    return(invisible(sim))
})

Dispersal <- function(sim) {
    treedFirePixelTableCurYr <- sim$treedFirePixelTableSinceLastDisp[burnTime == 
        time(sim)]
    pixelsFromCurYrBurn <- treedFirePixelTableCurYr$pixelIndex
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% 
        pixelsFromCurYrBurn)]
    if (P(sim)$seedingAlgorithm == "noDispersal") {
        sim <- NoDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
    }
    else if (P(sim)$seedingAlgorithm == "universalDispersal") {
        sim <- UniversalDispersalSeeding(sim, tempActivePixel, 
            pixelsFromCurYrBurn)
    }
    else if (P(sim)$seedingAlgorithm == "wardDispersal") {
        sim <- WardDispersalSeeding(sim, tempActivePixel, pixelsFromCurYrBurn)
    }
    else if (!P(sim)$seedingAlgorithm == "noSeeding") {
        stop("Undefined seed dispersal type!")
    }
    sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableCurYr
    return(invisible(sim))
}

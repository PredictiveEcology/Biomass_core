Save <- compiler::cmpfun(function(sim) {
    raster::projection(sim$simulatedBiomassMap) <- raster::projection(sim$ecoregionMap)
    raster::projection(sim$ANPPMap) <- raster::projection(sim$ecoregionMap)
    raster::projection(sim$mortalityMap) <- raster::projection(sim$ecoregionMap)
    raster::projection(sim$reproductionMap) <- raster::projection(sim$ecoregionMap)
    writeRaster(sim$simulatedBiomassMap, file.path(outputPath(sim), 
        "figures", paste0("simulatedBiomassMap_Year", round(time(sim)), 
            ".tif")), datatype = "INT4S", overwrite = TRUE)
    writeRaster(sim$ANPPMap, file.path(outputPath(sim), "figures", 
        paste0("ANPP_Year", round(time(sim)), ".tif")), datatype = "INT4S", 
        overwrite = TRUE)
    writeRaster(sim$mortalityMap, file.path(outputPath(sim), 
        "figures", paste0("mortalityMap_Year", round(time(sim)), 
            ".tif")), datatype = "INT4S", overwrite = TRUE)
    writeRaster(sim$reproductionMap, file.path(outputPath(sim), 
        "figures", paste0("reproductionMap_Year", round(time(sim)), 
            ".tif")), datatype = "INT4S", overwrite = TRUE)
    return(invisible(sim))
})

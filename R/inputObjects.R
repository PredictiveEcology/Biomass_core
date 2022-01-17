.inputObjects <- compiler::cmpfun(function(sim) {
    cacheTags <- c(currentModule(sim), "function:.inputObjects")
    dPath <- asPath(getOption("reproducible.destinationPath", 
        dataPath(sim)), 1)
    if (getOption("LandR.verbose", TRUE) > 0) 
        message(currentModule(sim), ": using dataPath '", dPath, 
            "'.")
    if (!suppliedElsewhere("studyArea", sim)) {
        stop("Please provide a 'studyArea' polygon")
    }
    if (is.na(P(sim)$.studyAreaName)) {
        params(sim)[[currentModule(sim)]][[".studyAreaName"]] <- reproducible::studyAreaName(sim$studyArea)
        message("The .studyAreaName is not supplied; derived name from sim$studyArea: ", 
            params(sim)[[currentModule(sim)]][[".studyAreaName"]])
    }
    needRTM <- FALSE
    if (is.null(sim$rasterToMatch)) {
        if (!suppliedElsewhere("rasterToMatch", sim)) {
            needRTM <- TRUE
            message("There is no rasterToMatch supplied; will attempt to use rawBiomassMap")
        }
        else {
            stop("rasterToMatch is going to be supplied, but ", 
                currentModule(sim), " requires it ", "as part of its .inputObjects. Please make it accessible to ", 
                currentModule(sim), " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)", 
                " or in a module that gets loaded prior to ", 
                currentModule(sim))
        }
    }
    if (needRTM) {
        if (!suppliedElsewhere("rawBiomassMap", sim) || !compareRaster(sim$rawBiomassMap, 
            sim$studyArea, stopiffalse = FALSE)) {
            rawBiomassMapURL <- paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/", 
                "canada-forests-attributes_attributs-forests-canada/", 
                "2001-attributes_attributs-2001/", "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif")
            rawBiomassMapFilename <- "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif"
            rawBiomassMap <- Cache(prepInputs, targetFile = rawBiomassMapFilename, 
                url = rawBiomassMapURL, destinationPath = dPath, 
                studyArea = sim$studyArea, rasterToMatch = NULL, 
                maskWithRTM = FALSE, useSAcrs = FALSE, method = "bilinear", 
                datatype = "INT2U", filename2 = NULL, userTags = c(cacheTags, 
                  "rawBiomassMap"), omitArgs = c("destinationPath", 
                  "targetFile", "userTags", "stable"))
        }
        else {
            rawBiomassMap <- Cache(postProcess, x = sim$rawBiomassMap, 
                studyArea = sim$studyArea, useSAcrs = FALSE, 
                maskWithRTM = FALSE, method = "bilinear", datatype = "INT2U", 
                filename2 = NULL, overwrite = TRUE, userTags = cacheTags, 
                omitArgs = c("destinationPath", "targetFile", 
                  "userTags", "stable"))
        }
        warning("rasterToMatch is missing and will be created", 
            " from rawBiomassMap and studyAreaLarge.", " If this is wrong, provide raster.")
        sim$rasterToMatch <- rawBiomassMap
        RTMvals <- getValues(sim$rasterToMatch)
        sim$rasterToMatch[!is.na(RTMvals)] <- 1
        sim$rasterToMatch <- Cache(writeOutputs, sim$rasterToMatch, 
            filename2 = .suffix(file.path(dPath, "rasterToMatch.tif"), 
                paste0("_", P(sim)$.studyAreaName)), datatype = "INT2U", 
            overwrite = TRUE, userTags = c(cacheTags, "rasterToMatch"), 
            omitArgs = c("userTags"))
    }
    if (!compareCRS(sim$studyArea, sim$rasterToMatch)) {
        warning(paste0("studyArea and rasterToMatch projections differ.\n", 
            "studyArea will be projected to match rasterToMatch"))
        sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
        sim$studyArea <- fixErrors(sim$studyArea)
    }
    if (!suppliedElsewhere("studyAreaReporting", sim)) {
        if (getOption("LandR.verbose", TRUE) > 0) 
            message("'studyAreaReporting' was not provided by user. Using the same as 'studyArea'.")
        sim$studyAreaReporting <- sim$studyArea
    }
    if (!suppliedElsewhere("sufficientLight", sim)) {
        mainInput <- prepInputsMainInput(url = extractURL("sufficientLight"), 
            dPath, cacheTags = c(cacheTags, "mainInput"))
        sufficientLight <- data.frame(mainInput, stringsAsFactors = FALSE)
        startRow <- which(sufficientLight$col1 == "SufficientLight")
        sufficientLight <- sufficientLight[(startRow + 1):(startRow + 
            5), 1:7]
        sufficientLight <- data.table(sufficientLight)
        sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]
        names(sufficientLight) <- c("speciesshadetolerance", 
            "X0", "X1", "X2", "X3", "X4", "X5")
        sim$sufficientLight <- data.frame(sufficientLight, stringsAsFactors = FALSE)
    }
    if (!suppliedElsewhere("sppEquiv", sim)) {
        if (!is.null(sim$sppColorVect)) 
            stop("If you provide sppColorVect, you MUST also provide sppEquiv")
        data("sppEquivalencies_CA", package = "LandR", envir = environment())
        sim$sppEquiv <- as.data.table(sppEquivalencies_CA)
        sim$sppEquiv[KNN == "Abie_Las", `:=`(LandR, "Abie_sp")]
        if (P(sim)$sppEquivCol == "Boreal") {
            message(paste("There is no 'sppEquiv' table supplied;", 
                "will attempt to use species listed under 'Boreal'", 
                "in the 'LandR::sppEquivalencies_CA' table"))
        }
        else {
            if (grepl(P(sim)$sppEquivCol, names(sim$sppEquiv))) {
                message(paste("There is no 'sppEquiv' table supplied,", 
                  "will attempt to use species listed under", 
                  P(sim)$sppEquivCol, "in the 'LandR::sppEquivalencies_CA' table"))
            }
            else {
                stop("You changed 'sppEquivCol' without providing 'sppEquiv',", 
                  "and the column name can't be found in the default table ('LandR::sppEquivalencies_CA').", 
                  "Please provide conforming 'sppEquivCol', 'sppEquiv' and 'sppColorVect'")
            }
        }
        sim$sppEquiv <- sim$sppEquiv[!"", on = P(sim)$sppEquivCol]
        sim$sppEquiv <- na.omit(sim$sppEquiv, P(sim)$sppEquivCol)
        sim$sppColorVect <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol, 
            newVals = "Mixed", palette = "Accent")
    }
    else {
        if (is.null(sim$sppColorVect)) {
            message("'sppEquiv' is provided without a 'sppColorVect'. Running:\n              LandR::sppColors with column ", 
                P(sim)$sppEquivCol)
            sim$sppColorVect <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol, 
                newVals = "Mixed", palette = "Accent")
        }
    }
    if (P(sim)$vegLeadingProportion > 0 & is.na(sim$sppColorVect["Mixed"])) {
        stop("vegLeadingProportion  is > 0 but there is no 'Mixed' color in sim$sppColorVect. ", 
            "Please supply sim$sppColorVect with a 'Mixed' color or set vegLeadingProportion to zero.")
    }
    if (!suppliedElsewhere("treedFirePixelTableSinceLastDisp", 
        sim)) {
        sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(), 
            pixelGroup = integer(), burnTime = numeric())
    }
    if (!suppliedElsewhere("speciesLayers", sim)) {
        message("No RasterStack map of biomass X species is provided; using KNN")
        url <- paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/", 
            "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/")
        sim$speciesLayers <- Cache(loadkNNSpeciesLayers, dPath = dPath, 
            rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea, 
            sppEquiv = sim$sppEquiv, knnNamesCol = "KNN", sppEquivCol = P(sim)$sppEquivCol, 
            thresh = 10, url = url, userTags = c(cacheTags, "speciesLayers"), 
            omitArgs = c("userTags"))
    }
    if (!suppliedElsewhere("species", sim)) {
        speciesTable <- getSpeciesTable(dPath = dPath, url = extractURL("species"), 
            cacheTags = c(cacheTags, "speciesTable"))
        sim$species <- prepSpeciesTable(speciesTable = speciesTable, 
            sppEquiv = sim$sppEquiv[get(P(sim)$sppEquivCol) %in% 
                names(sim$speciesLayers)], sppEquivCol = P(sim)$sppEquivCol)
    }
    if (P(sim)$growthAndMortalityDrivers != "LandR") {
        if (!suppliedElsewhere("cceArgs", sim)) {
            sim$cceArgs <- list(quote(CMI), quote(ATA), quote(CMInormal), 
                quote(mcsModel), quote(gcsModel))
            names(sim$cceArgs) <- paste(sim$cceArgs)
        }
    }
    gc()
    return(invisible(sim))
})

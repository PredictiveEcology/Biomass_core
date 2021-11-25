test_that("test Ward dispersal seeding algorithm",{
  opts <- options(reproducible.useGDAL = FALSE,
                  spades.moduleCodeChecks = FALSE,
                  reproducible.useMemoise = TRUE,
                  spades.useRequire = FALSE,
                  LandR.assertions = FALSE,
                  spades.recoveryMode = FALSE)
  on.exit(options(opts))
  require("raster")
  require("data.table")
  require("LandR")
  require("SpaDES.core")
  module <- "Biomass_core"
  modulePath <- getwd()
  while( grepl(module, modulePath)) modulePath <- dirname(modulePath)
  outputPath <- checkPath(file.path(tempdir(), rndstr(1)), create = TRUE)
  path <- list(modulePath = modulePath, # TODO: use general path
               outputPath = outputPath) # TODO: use general path
  parameters <- list(Biomass_core = list(.saveInitialTime = NA))

# 1. testing how the species seeds spread into the neighbor empty cells
# the cohort data is set to not allow on site regeneration


  sppEquiv <- LandR::sppEquivalencies_CA[nzchar(LANDIS_test)]
  speciesURL <- moduleInputs(module, modulePath)[objectName == "species"]$sourceURL
  species <- getSpeciesTable(dPath = outputPath, url = speciesURL)[Area == "BSW"]
  species <- species[sppEquiv, on = c("LandisCode" = "LANDIS_traits"), nomatch = 0]
  setnames(species, c("Maturity", "SeedEffDist", "SeedMaxDist", "Shade", "Longevity", "MortalityCurve", "GrowthCurve"),
           c("sexualmature", "seeddistance_eff", "seeddistance_max", "shadetolerance", "longevity", "mortalityshape", "growthcurve"))
  spFac <- factor(species$LandR)
  species[, speciesCode := spFac]

  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = factor(1L),
                           speciesCode = spFac[7:10], age = 41, B = 8000L,
                           mortality = 50, aNPPAct = 1079.75965551773)


  # sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)  # Jan 2021 we agreed to force user to provide a SA/SAL
  rasterToMatch <- raster(xmn = 50 - 1.3e6, xmx = 50 + 99 * 100 - 1.3e6,
                        ymn = 50  + 6.9e6, ymx = 50 + 99 * 100 + 6.9e6,
                        res = c(100, 100), val = 1)
  crs(rasterToMatch) <- CRS("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  studyArea <- rasterToPolygons(rasterToMatch)

  ecoregionFiles <- makeDummyEcoregionFiles(rasterToMatch, rasterToMatch, rasterToMatch)
  ecoregionMap <- ecoregionFiles$ecoregionMap
  levs <- raster::levels(ecoregionMap)[[1]]
  colnames(levs) <- gsub("^ecoregion$", "ecoregionGroup", colnames(levs))
  levels(ecoregionMap) <- levs

  pixelGroupMap <- setValues(raster(ecoregionMap), 2)
  nPixelsBurned <- 3000
  pixelGroupMap[1:nPixelsBurned] <- 0
  c <- expand.grid(data.frame(a = seq(5, 99, by = 9), b = seq(5, 99, by = 9)))
  pixelindex <- (c$a - 1) * 99 + c$b #121
  pixelGroupMap[pixelindex] <- 1
  minRelativeB <- data.table(ecoregion = "eco1", X1 = 0.15,
                             X2 = 0.25, X3 = 0.5, X4 = 0.8,
                             X5 = 0.95, ecoregionGroup = factor(1))
  # sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
  #                             header = TRUE, stringsAsFactor = FALSE)
  lastFireYear <- "noFire"
  activePixelIndex <- 1:9801
  lastReg <- 0
  successionTimestep <- 1
  calibrate <- TRUE
  regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                   Year = numeric(), numberOfReg = numeric())


  speciesEcoregion <- data.table(year = 0,
                                 ecoregionGroup = factor(1),
                                 speciesCode = spFac,
                                 establishprob = 0.22,
                                 maxANPP = 1096,
                                 maxB = 32880L)
  # #species <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/species.csv",
  # #                    header = TRUE, stringsAsFactors = FALSE)
  # species <- data.table(species)[, speciesCode := 1:16]
  # speciesEcoregion <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/speciesEcoregion.csv",
  #                              header = TRUE, stringsAsFactors = FALSE)
  # speciesEcoregion <- data.table(speciesEcoregion)[, ecoregionGroup := as.numeric(as.factor(ecoregion))]
  # tempsp <- setkey(species[,.(species,speciesCode)], species)
  # speciesEcoregion <- setkey(speciesEcoregion,species)[tempsp]
  # sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
  #                             header = TRUE, stringsAsFactors = FALSE)
  seedingAlgorithm <- "wardDispersal"

  sppEquivCol <- "LANDIS_test"
  sppEquiv <- LandR::sppEquivalencies_CA[LANDIS_test == "poputrem"]
  sppColorVect <- LandR::sppColors(sppEquiv, sppEquivCol, palette = "Accent")
  initialBiomassSource <- "cohortData"

  pixelCohortData <- LandR::addPixels2CohortData(cohortData, pixelGroupMap)
  ecoregion <- LandR::makeEcoregionDT(pixelCohortData, speciesEcoregion)

  objects <- mget(c("pixelGroupMap", "studyArea", "ecoregion",
                    "rasterToMatch", "speciesEcoregion",
                    "species", "sppEquiv", "successionTimestep", "calibrate",
                    "seedingAlgorithm", "minRelativeB", "lastFireYear",
                    "activePixelIndex", "lastReg", "regenerationOutput", "cohortData",
                    "ecoregionMap"))

  parameters$Biomass_core$initialBiomassSource = initialBiomassSource

  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  # set.seed(1)

  # source(file.path(modulePath(mySim), "Biomass_core", "R", "seedDispersalLANDIS.R"))
  mySim$treedFirePixelTableSinceLastDisp <-
    rbindlist(list(mySim$treedFirePixelTableSinceLastDisp,
                   data.table(pixelIndex = 1:nPixelsBurned, pixelGroup = pixelGroupMap[][1:nPixelsBurned], burnTime = time(mySim))))

  output <- spades(mySim)

  expect_is(output$cohortData, "data.table")
  news <- output$cohortData[!cohortData, on = c("speciesCode", "age")]
  expect_true(NROW(new) > 0)
  # Each pixel group is unique
  expect_true(all(!duplicated(news[, fastdigest::fastdigest(speciesCode), by = "pixelGroup"]$V1)))

  # remainder of seed dispersal testing is in LandR package

})

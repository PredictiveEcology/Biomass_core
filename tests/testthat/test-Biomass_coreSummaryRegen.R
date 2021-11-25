test_that("test summary regeneration. ",{
  opts <- options(reproducible.useGDAL = FALSE,
                  spades.moduleCodeChecks = FALSE,
                  reproducible.useMemoise = TRUE,
                  spades.useRequire = FALSE,
                  LandR.assertions = FALSE,
                  spades.recoveryMode = FALSE)
  on.exit(options(opts))
  require("raster")
  require("data.table")
  module <- "Biomass_core"
  modulePath <- getwd()
  while( grepl(module, modulePath)) modulePath <- dirname(modulePath)
  outputPath <- checkPath(file.path(tempdir(), rndstr(1)), create = TRUE)
  path <- list(modulePath = modulePath, # TODO: use general path
               outputPath = outputPath) # TODO: use general path
  parameters <- list(Biomass_core = list(.saveInitialTime = NA))

  cohortData <- data.table(expand.grid(age = c(1, 10, 15),
                                       pixelGroup = 1:5))[,B:=10000]
  cohortData[age == 1, B := seq(100, by = 50, length = 5)]

  cellSize <- 100
  pixelGroupMap <- raster(xmn=50,xmx=50+5*100,
                          ymn=50,ymx=50+5*100,
                          res=c(100,100),
                          val=c(rep(5, 5), rep(4, 5), rep(3, 5),
                                rep(2, 3), rep(-1, 2), rep(1, 3),
                                rep(-1, 2)))

  objects <- list("cohortData" = cohortData,
                  "cellSize" = cellSize,
                  "pixelGroupMap" = pixelGroupMap)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("SummaryRegen")){
    simOutput <- SummaryRegen(mySim)
  } else {
    simOutput <- mySim$.mods$Biomass_core$SummaryRegen(mySim)
  }
  expect_is(simOutput$reproductionMap, "RasterLayer")
  expect_equal(getValues(simOutput$reproductionMap),
               c(rep(300, 5), rep(250, 5), rep(200, 5),
                 rep(150, 3), NA, NA, rep(100, 3), NA, NA))
  rm(simOutput, mySim, objects)

  cohortData[age == 1, age := 2]
  objects <- list("cohortData" = cohortData,
                  "cellSize" = cellSize,
                  "pixelGroupMap" = pixelGroupMap)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("SummaryRegen")){
    simOutput <- SummaryRegen(mySim)
  } else {
    simOutput <- mySim$.mods$Biomass_core$SummaryRegen(mySim)
  }
  expect_is(simOutput$reproductionMap, "RasterLayer")
  expect_equal(getValues(simOutput$reproductionMap),
               c(rep(0, 25)))


})

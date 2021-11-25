test_that("test process of age reclassification",{
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

  successionTimestep <- 10
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(11,50), B = 40,
                           mortality = 150, aNPPAct = 999)
  if(exists("ageReclassification")){
    output <- ageReclassification(cohortData, successionTimestep, stage="spinup")
  } else {
    output <- mySim$.mods$Biomass_core$ageReclassification(cohortData, successionTimestep, stage="spinup")
  }
  cohortData_output <- setkey(output,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(10,50), B = 40,
                           mortality = 150, aNPPAct = 999),age)
  expect_equal(cohortData_output,cohortData_output_compared)

  rm(cohortData,cohortData_output,cohortData_output_compared,output)
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(1:10,49), B = c(1:11),
                           mortality = 150, aNPPAct = 999)
  if(exists("ageReclassification")){
    output <- ageReclassification(cohortData, successionTimestep, stage="mainsimulaiton")
  } else {
    output <- mySim$.mods$Biomass_core$ageReclassification(cohortData, successionTimestep, stage="mainsimulaiton")
  }

  cohortData_output <- setkey(output,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                                speciesCode = 16, age = c(11,49), B = c(55,11),
                                mortality = c(1500,150), aNPPAct = c(9990,999)),age)

  expect_equal(cohortData_output,cohortData_output_compared)
})

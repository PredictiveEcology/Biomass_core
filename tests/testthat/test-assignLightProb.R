test_that("test assign light probability for a given species tolerance and site shade",{
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

  sufficientLight <- data.frame(speciesshadetolerance=1:5,
                                X0=seq(1,0.8,length=5),
                                X1=seq(0.8,0.6,length=5),
                                X2=seq(0.6,0.4,length=5),
                                X3=seq(0.4,0.2,length=5),
                                X4=seq(0.2,0.0,length=5),
                                X5=seq(0,1,length=5))
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(expand.grid(shadetolerance=1:5,siteShade=0:5))

  if(exists("assignLightProb")){
    output <- assignLightProb(sufficientLight, newCohortData = cohortData)
  } else {
    output <- mySim$.mods$Biomass_core$assignLightProb(sufficientLight, newCohortData = cohortData)
  }
  cohortData[,lightProb:=c(seq(1,0.8,length=5),seq(0.8,0.6,length=5),
                           seq(0.6,0.4,length=5),seq(0.4,0.2,length=5),
                           seq(0.2,0.0,length=5),seq(0,1,length=5))]
  output_compared <- cohortData
  expect_equal(output,output_compared)
})

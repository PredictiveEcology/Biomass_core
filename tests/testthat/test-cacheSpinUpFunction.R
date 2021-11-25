test_that("test cache function for spinUp",{
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

  useCache <- TRUE
  objects <- list("useCache"=useCache)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cachePath <- tempdir()
  if(dir.exists(file.path(cachePath,"spinUp"))){
    unlink(file.path(cachePath,"spinUp"), recursive = TRUE)
  }
  if(exists("cacheSpinUpFunction")){
    output <- cacheSpinUpFunction(mySim, cachePath = cachePath)
  } else {
    output <- mySim$.mods$Biomass_core$cacheSpinUpFunction(mySim, cachePath = cachePath)
  }
  expect_is(output$spinUpCache,"function")
  expect_true(dir.exists(file.path(cachePath,"spinUp")))
  expect_true(file.exists(file.path(cachePath,"spinUp","backpack.db")))
  unlink(file.path(cachePath,"spinUp"), recursive = TRUE)

  useCache <- FALSE
  objects <- list("useCache" = useCache)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("cacheSpinUpFunction")){
    output <- cacheSpinUpFunction(mySim, cachePath = cachePath)
  } else {
    output <- mySim$.mods$Biomass_core$cacheSpinUpFunction(mySim, cachePath = cachePath)
  }
  expect_is(output$spinUpCache,"function")
  expect_false(dir.exists(file.path(cachePath,"spinUp")))

})

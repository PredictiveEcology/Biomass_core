## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(biomassMap                       = "SpatRaster",
      cceArgs                          = "list",
      cohortData                       = "data.table",
      columnsForPixelGroups            = "character",
      ecoregion                        = "data.table",
      ecoregionMap                     = "SpatRaster",
      lastReg                          = "numeric",
      pixelGroupMap                    = "SpatRaster",
      rasterToMatch                    = "SpatRaster",
      species                          = "data.table",
      speciesEcoregion                 = "data.table",
      speciesLayers                    = "SpatRaster",
      sppColorVect                     = "character",
      sppEquiv                         = "data.table",
      sppNameVector                    = "character",
      studyArea                        = "SpatVector",
      studyAreaReporting               = "SpatVector",
      sufficientLight                  = "data.frame",
      treedFirePixelTableSinceLastDisp = "data.table")
  )
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(
    outputs[order(names(outputs))],
    c(activePixelIndex                 = "integer",
      activePixelIndexReporting        = "integer",
      ANPPMap                          = "SpatRaster",
      biomassMap                       = "SpatRaster",
      cohortData                       = "data.table",
      ecoregion                        = "data.table",
      ecoregionMap                     = "SpatRaster",
      inactivePixelIndex               = "logical",
      inactivePixelIndexReporting      = "integer",
      lastFireYear                     = "numeric",
      lastReg                          = "numeric",
      minRelativeB                     = "data.frame",
      mortalityMap                     = "SpatRaster",
      pixelGroupMap                    = "SpatRaster",
      regenerationOutput               = "data.table",
      reproductionMap                  = "SpatRaster",
      simulatedBiomassMap              = "SpatRaster",
      simulationOutput                 = "data.table",
      simulationTreeOutput             = "data.table",
      species                          = "data.table",
      speciesEcoregion                 = "data.table",
      speciesLayers                    = "SpatRaster",
      spinupOutput                     = "data.table",
      sppColorVect                     = "character",
      sppNameVector                    = "character",
      summaryBySpecies                 = "data.table",
      summaryBySpecies1                = "data.table",
      summaryLandscape                 = "data.table",
      treedFirePixelTableSinceLastDisp = "data.table",
      vegTypeMap                       = "SpatRaster")
  )
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    sort(c(".maxMemory", ".plotInitialTime", ".plotInterval", ".plotMaps", ".plots",
           ".plotTransitionField", ".plotTransitionTimes", ".runName",
           ".saveInitialTime", ".saveInterval", ".sslVerify", ".studyAreaName",
           ".useCache", ".useParallel", "calcSummaryBGM", "calibrate",
           "cohortDefinitionCols", "cutpoint", "dataYear", "gmcsGrowthLimits",
           "gmcsMinAge", "growthAndMortalityDrivers", "growthInitialTime", "initialB",
           "initialBiomassSource", "keepClimateCols", "minCohortBiomass", "mixedType",
           "plotOverstory", "seedingAlgorithm", "spinupMortalityfraction",
           "sppEquivCol", "sppEquivPlotCol", "successionTimestep",
           "vegLeadingProportion"))
  )
})

## When no `speciesLayers` are supplied, .inputObjects makes them with
## LandR::prepSpeciesLayers_SCANFI(). That function's year argument is `dataYear`; the module
## used to pass `year =`, which disappeared into `...`, so the `dataYear` parameter had no
## effect and 2020 was always used. The mock records the call's arguments and stops simInit()
## there, so nothing is downloaded.

test_that("the dataYear parameter reaches prepSpeciesLayers_SCANFI()", {
  studyArea <- terra::vect("POLYGON ((0 0, 960 0, 960 960, 0 960, 0 0))", crs = "EPSG:3978")
  ## supplied so .inputObjects does not download it
  sufficientLight <- data.frame(speciesshadetolerance = 1:5, X0 = 1, X1 = 0, X2 = 0, X3 = 0,
                                X4 = 0, X5 = 0)
  sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% c("Pice_mar", "Popu_tre")]
  ## the formals of the installed LandR, plus the legacy names it reads from `...`; taken
  ## before the mock replaces the function
  known <- c(names(formals(LandR::prepSpeciesLayers_SCANFI)), "studyArea", "rasterToMatch",
             "studyAreaName")
  captured <- new.env()
  mockSCANFI <- function(...) {
    captured$args <- list(...)
    stop("prepSpeciesLayers_SCANFI mock reached")
  }
  ## the module finds the function through the LandR namespace (package rendition) or the
  ## attached LandR package (module sourced into the simList)
  local_mocked_bindings(prepSpeciesLayers_SCANFI = mockSCANFI, .package = "LandR")
  local_mocked_bindings(prepSpeciesLayers_SCANFI = mockSCANFI)

  expect_error(
    suppressMessages(SpaDES.core::simInit(
      times = list(start = 0, end = 1),
      params = list(Biomass_core = list(dataYear = 2010, .studyAreaName = "test")),
      modules = moduleName,
      objects = list(studyArea = studyArea, sufficientLight = sufficientLight,
                     sppEquiv = sppEquiv, sppNameVector = c("Pice_mar", "Popu_tre")),
      paths = testPaths
    )),
    "prepSpeciesLayers_SCANFI mock reached"
  )
  expect_identical(captured$args$dataYear, 2010)
  expect_false("year" %in% names(captured$args))
  expect_true(all(names(captured$args) %in% known))
})

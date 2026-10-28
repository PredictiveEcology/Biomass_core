## When no module supplies `speciesLayers`, .inputObjects builds them from SCANFI.
## Init stops unless they match `rasterToMatch`, so they must be built on `studyArea`
## and `rasterToMatch`. They were built on `studyArea_biomassParam` and
## `rasterToMatch_biomassParam`, which this module neither declares nor creates, so a
## standalone run passed NULL for both.

test_that("default speciesLayers are built on studyArea and rasterToMatch", {
  withr::local_options(reproducible.useCache = FALSE)
  crsLCC <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  rtm <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 2500, ymin = 0, ymax = 2500,
                     crs = crsLCC, vals = 1)
  sa <- terra::as.polygons(terra::ext(rtm), crs = crsLCC)

  seen <- new.env()
  local_mocked_bindings(prepSpeciesLayers_SCANFI = function(studyArea, rasterToMatch, ...) {
    seen$studyArea <- studyArea
    seen$rasterToMatch <- rasterToMatch
    terra::rast(rasterToMatch, nlyrs = 1, names = "Pice_mar", vals = 50)
  }, .package = "LandR")

  sppEquiv <- LandR::sppEquivalencies_CA[LandR == "Pice_mar"]
  SpaDES.core::simInit(
    times = list(start = 0, end = 1),
    modules = moduleName,
    paths = testPaths,
    params = list(Biomass_core = list(.studyAreaName = "test", sppEquivCol = "LandR")),
    objects = list(studyArea = sa, rasterToMatch = rtm, sppEquiv = sppEquiv)
  )

  expect_false(is.null(seen$studyArea))
  expect_false(is.null(seen$rasterToMatch))
  expect_true(terra::compareGeom(seen$rasterToMatch, rtm, stopOnError = FALSE))
  expect_equal(terra::expanse(seen$studyArea), terra::expanse(sa))
})

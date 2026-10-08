## vegTransitionsByZone() builds the dataset behind the plotTransitions event. These tests
## check that `.plotTransitionNaRm` reaches LandR::vegTransitions() as `na.rm`: pixels with
## no vegetation type are dropped (TRUE) or kept and labelled "_NA_" (FALSE).

## Two vegetation type maps on a 3 x 3 grid of 100 m pixels, made as the buildVTM event
## makes them. Pixel 1 is not simulated (NA in pixelGroupMap) in both years; pixels 2 and 7
## lose all their cohorts (pixelGroup 0) by year 10, e.g. burned and not regenerated.
makeVTMs <- function(dir) {
  pgm <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 300, ymin = 0, ymax = 300,
                     crs = "EPSG:32611")
  cohortData <- data.table::data.table(
    pixelGroup = c(1L, 2L), speciesCode = c("Pice_gla", "Popu_tre"),
    ecoregionGroup = factor("1"), age = 50L, B = 1000L
  )
  sppEquiv <- LandR::sppEquivalencies_CA[LandR::sppEquivalencies_CA$LandR %in% c("Pice_gla", "Popu_tre"), ]
  cols <- LandR::sppColors(sppEquiv, "LandR", newVals = "Mixed", palette = "Accent")
  pgs <- list(
    `0`  = c(NA, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L),
    `10` = c(NA, 0L, 1L, 1L, 2L, 2L, 0L, 1L, 2L)
  )
  vapply(names(pgs), function(yr) {
    terra::values(pgm) <- pgs[[yr]]
    vtm <- suppressMessages(LandR::vegTypeMapGenerator(
      cohortData, pgm, 0.8, mixedType = 2, sppEquiv = sppEquiv, sppEquivCol = "LandR",
      colors = cols, doAssertion = FALSE
    ))
    f <- file.path(dir, paste0("vegTypeMap_year", yr, ".tif"))
    terra::writeRaster(vtm, f, overwrite = TRUE)
    f
  }, character(1))
}

## Reporting polygons: the left column of pixels (1, 4, 7) is "west", the rest "east".
makeZones <- function() {
  zones <- terra::vect(c("POLYGON ((0 0, 100 0, 100 300, 0 300, 0 0))",
                         "POLYGON ((100 0, 300 0, 300 300, 100 300, 100 0))"),
                       crs = "EPSG:32611")
  zones$zoneName <- c("west", "east")
  zones
}

test_that("na.rm = TRUE drops pixels with no vegetation type", {
  tmp <- withr::local_tempdir()
  df <- vegTransitionsByZone(vtm = makeVTMs(tmp), studyAreaReporting = makeZones(),
                             field = NA_character_, studyAreaName = "mySA",
                             times = c(0L, 10L), na.rm = TRUE, dest = tmp) |>
    dplyr::collect()
  expect_false(any(df$vegType == "_NA_"))
  expect_setequal(df$pixelID[df$time == 0], 2:9)
  expect_setequal(df$pixelID[df$time == 10], c(3:6, 8:9))
})

test_that("na.rm = FALSE keeps pixels with no vegetation type as \"_NA_\"", {
  tmp <- withr::local_tempdir()
  df <- vegTransitionsByZone(vtm = makeVTMs(tmp), studyAreaReporting = makeZones(),
                             field = NA_character_, studyAreaName = "mySA",
                             times = c(0L, 10L), na.rm = FALSE, dest = tmp) |>
    dplyr::collect()
  expect_setequal(df$pixelID[df$time == 0], 1:9)
  expect_setequal(df$pixelID[df$time == 10], 1:9)
  ## both the unsimulated pixel and the pixels that lost their cohorts
  expect_setequal(df$pixelID[df$time == 0 & df$vegType == "_NA_"], 1L)
  expect_setequal(df$pixelID[df$time == 10 & df$vegType == "_NA_"], c(1L, 2L, 7L))
})

test_that("zones come from `field`, or are dissolved into one named after the study area", {
  tmp <- withr::local_tempdir()
  vtm <- makeVTMs(tmp)
  one <- vegTransitionsByZone(vtm = vtm, studyAreaReporting = makeZones(),
                              field = NA_character_, studyAreaName = "mySA",
                              times = c(0L, 10L), na.rm = TRUE,
                              dest = file.path(tmp, "one")) |>
    dplyr::collect()
  expect_identical(unique(one$zone), "mySA")

  two <- vegTransitionsByZone(vtm = vtm, studyAreaReporting = makeZones(),
                              field = "zoneName", studyAreaName = "mySA",
                              times = c(0L, 10L), na.rm = TRUE,
                              dest = file.path(tmp, "two")) |>
    dplyr::collect()
  expect_setequal(two$pixelID[two$zone == "west" & two$time == 0], c(4L, 7L))
  expect_setequal(two$pixelID[two$zone == "east" & two$time == 0], c(2:3, 5:6, 8:9))
})

test_that("plotVegTransitions() draws the \"_NA_\" stratum kept by na.rm = FALSE", {
  tmp <- withr::local_tempdir()
  ds <- vegTransitionsByZone(vtm = makeVTMs(tmp), studyAreaReporting = makeZones(),
                             field = NA_character_, studyAreaName = "mySA",
                             times = c(0L, 10L), na.rm = FALSE, dest = tmp)
  ## plotVegTransitions() passes an unused `linewidth` to geom_text_repel(), which warns;
  ## that is LandR's, not what is tested here.
  ggs <- suppressWarnings(LandR::plotVegTransitions(ds))
  expect_named(ggs, "mySA")
  built <- suppressWarnings(ggplot2::ggplot_build(ggs[["mySA"]]))
  strata <- unique(unlist(lapply(built$data, function(d) as.character(d$stratum))))
  expect_true("_NA_" %in% strata)
  f <- file.path(tmp, "transitions.png")
  suppressWarnings(ggplot2::ggsave(f, ggs[["mySA"]], width = 6, height = 4))
  expect_true(file.exists(f))
})

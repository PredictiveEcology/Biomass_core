## The `year` column of speciesEcoregion is in time(sim) units: traits in a row apply from that
## year on. Until #80's replacement, Init renumbered the years to start at 0, so a table with
## several years had its traits applied at the wrong times (see that PR for the cases).
library(data.table)

## one species in two ecoregions; maxB carries the source year so the test can see which row was used
sppEco <- function(years) {
  data.table(year = rep(years, each = 2), speciesCode = factor("Pice_mar"),
             ecoregionGroup = factor(c("1_01", "1_02")), establishprob = 0.5,
             maxB = rep(as.integer(years), each = 2) + 1000L, maxANPP = 100)
}
cohorts <- function() {
  data.table(pixelGroup = 1:2, speciesCode = factor("Pice_mar"),
             ecoregionGroup = factor(c("1_01", "1_02")), age = 20L, B = 500L)
}
traitsAt <- function(se, t) {
  unique(updateSpeciesEcoregionAttributes(se, currentTime = t, cohortData = cohorts())$maxB) - 1000L
}

test_that("one calendar year at start(sim) is used unchanged", {
  se <- sppEco(2020)
  expect_identical(speciesEcoregionStartYear(se, 2020), se)
  expect_equal(traitsAt(se, 2020), 2020)
  expect_equal(traitsAt(se, 2060), 2020)
})

test_that("several years starting at 0 are applied in their own year", {
  se <- speciesEcoregionStartYear(sppEco(c(0, 10, 20, 30, 40)), 0)
  expect_equal(nrow(se), 10)
  expect_equal(sapply(c(0, 5, 10, 15, 40), traitsAt, se = se), c(0, 0, 10, 10, 40))
})

test_that("several calendar years are applied in their own year", {
  se <- speciesEcoregionStartYear(sppEco(seq(2020, 2060, by = 10)), 2020)
  expect_equal(sapply(c(2020, 2025, 2030, 2059, 2060), traitsAt, se = se),
               c(2020, 2020, 2030, 2050, 2060))
})

test_that("traits starting after start(sim) are copied back to start(sim)", {
  expect_message(se <- speciesEcoregionStartYear(sppEco(c(2025, 2030)), 2020),
                 "using its earliest year \\(2025\\)")
  expect_equal(sort(unique(se$year)), c(2020, 2025, 2030))
  expect_equal(sapply(c(2020, 2025, 2030), traitsAt, se = se), c(2025, 2025, 2030))
})

test_that("cohorts with no traits stop the run instead of being dropped", {
  ## no row at or before currentTime
  expect_error(updateSpeciesEcoregionAttributes(sppEco(2030), currentTime = 2020,
                                                cohortData = cohorts()),
               "no traits at or before year 2020")
  ## an ecoregion missing from speciesEcoregion
  expect_error(updateSpeciesEcoregionAttributes(sppEco(2020)[ecoregionGroup == "1_01"],
                                                currentTime = 2020, cohortData = cohorts()),
               "Pice_mar x 1_02")
})

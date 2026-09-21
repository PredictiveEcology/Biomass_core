## Rewritten from the 2019-2021 test of the same name: the function is now a package
## function (the module is converted to a package before the tests run), so it is called
## directly rather than through simInit() + sim$.mods. The inputs and the expected values
## are the originals; only the harness and the renamed `stage` level ("mainsimulation" is
## now "nonSpinup") changed.
library(data.table)

test_that("calculateSumB() sums biomass over the cohorts old enough to count", {
  ## Every cohort in a pixelGroup carries the group total. `wh` decides which cohorts are
  ## summed -- LANDIS-II leaves the young ones out -- not which cohorts feel the result.
  ## Between the Dec-2021 rewrite (9342cf1) and the fix for #111, young cohorts kept
  ## sumB = 0 and so competed as though the site were empty; this is that regression test.
  ## Group 2 is all young, so it has no cohort old enough to count and correctly stays 0.
  successionTimestep <- 10
  cohortData <- data.table(pixelGroup = c(rep(1, 13), rep(2, 10)), ecoregionGroup = 1,
                           speciesCode = 16, age = c(1:10, 20, 30, 50, 1:10), B = c(1:13, 1:10),
                           mortality = 150, aNPPAct = 999)
  ref <- function(sumB) {
    setkey(data.table(pixelGroup = c(rep(1, 13), rep(2, 10)), ecoregionGroup = 1,
                      speciesCode = 16, age = c(1:10, 20, 30, 50, 1:10), B = c(1:13, 1:10),
                      mortality = 150, aNPPAct = 999, sumB = sumB), pixelGroup, age)
  }
  ## last regeneration at 10; at time 18 the cohorts that regenerated then are 8 years old
  output <- calculateSumB(copy(cohortData), lastReg = 10, currentTime = 18, successionTimestep,
                          verbose = FALSE)
  expect_equal(setkey(output, pixelGroup, age), ref(c(rep(36, 13), rep(0, 10))))
  for (i in 19:27) {
    output <- calculateSumB(copy(cohortData), lastReg = 10, currentTime = i, successionTimestep,
                            verbose = FALSE)
    expect_equal(setkey(output, pixelGroup, age), ref(c(rep(46, 13), rep(10, 10))))
  }
})

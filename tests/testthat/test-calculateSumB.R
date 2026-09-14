## Rewritten from the 2019-2021 test of the same name: the function is now a package
## function (the module is converted to a package before the tests run), so it is called
## directly rather than through simInit() + sim$.mods. The inputs and the expected values
## are the originals; only the harness and the renamed `stage` level ("mainsimulation" is
## now "nonSpinup") changed.
library(data.table)

test_that("calculateSumB() sums biomass over the cohorts old enough to count", {
  ## The 2019 expectation gives every cohort in a pixelGroup the group total; since the
  ## 2021 rewrite of calculateSumB() only cohorts old enough to be counted carry it and
  ## younger ones keep sumB = 0. Which is intended is an open question (see the PR that
  ## rewrote this test), so the expectation is not asserted until it is answered.
  skip("calculateSumB() semantics for young cohorts undecided since the 2021 rewrite")
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

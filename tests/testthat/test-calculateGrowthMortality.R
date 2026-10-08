## Rewritten from the 2019-2021 test of the same name: the function is now a package
## function (the module is converted to a package before the tests run), so it is called
## directly rather than through simInit() + sim$.mods. The inputs and the expected values
## are the originals; only the harness and the renamed `stage` level ("mainsimulation" is
## now "nonSpinup") changed.
library(data.table)

test_that("calculateGrowthMortality() matches the reference mBio at both stages", {
  cohortData <- data.table(expand.grid(bAP = seq(0.2, 2, by = 0.2), bPM = seq(0.2, 1, by = 0.2), age = 0:1))
  cohortData[, `:=`(maxANPP = 1096, B = 700)]
  ref <- c(73.0667, 125.2571, 164.4, 194.8444, 219.2, 219.2,
           219.2, 219.2, 219.2, 219.2, 146.1333, 250.5143, 328.8, 389.6889,
           438.4, 438.4, 438.4, 438.4, 438.4, 438.4, 219.2, 375.7714, 493.2,
           584.5333, 657.6, 657.6, 657.6, 657.6, 657.6, 657.6, 292.2667,
           501.0286, 657.6, 700, 700, 700, 700, 700, 700, 700, 365.3333,
           626.2857, 700, 700, 700, 700, 700, 700, 700, 700)
  output <- calculateGrowthMortality(cohortData, stage = "spinup")
  expect_equal(round(output$mBio, 4), c(rep(NA, 50), ref))
  output <- calculateGrowthMortality(cohortData, stage = "nonSpinup")
  expect_equal(round(output$mBio, 4), rep(ref, 2))
})

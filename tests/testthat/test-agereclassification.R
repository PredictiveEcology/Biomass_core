## Rewritten from the 2019-2021 test of the same name: the function is now a package
## function (the module is converted to a package before the tests run), so it is called
## directly rather than through simInit() + sim$.mods. The inputs and the expected values
## are the originals; only the harness and the renamed `stage` level ("mainsimulation" is
## now "nonSpinup") changed.
library(data.table)

test_that("ageReclassification() collapses cohorts younger than the succession time step", {
  successionTimestep <- 10
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(11,50), B = 40,
                           mortality = 150, aNPPAct = 999)
  output <- ageReclassification(cohortData, successionTimestep, stage="spinup")
  cohortData_output <- setkey(output,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(10,50), B = 40,
                           mortality = 150, aNPPAct = 999),age)
  expect_equal(cohortData_output, cohortData_output_compared, check.attributes = FALSE)

  rm(cohortData,cohortData_output,cohortData_output_compared,output)
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(1:10,49), B = c(1:11),
                           mortality = 150, aNPPAct = 999)
  output <- ageReclassification(cohortData, successionTimestep, stage = "nonSpinup")

  cohortData_output <- setkey(output,age)
  ## The squashed cohort is given age successionTimestep + 1 (since 2019; the original
  ## test, written before that change, expected successionTimestep - 1).
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                                speciesCode = 16, age = c(11,49), B = c(55,11),
                                mortality = c(1500,150), aNPPAct = c(9990,999)),age)

  expect_equal(cohortData_output, cohortData_output_compared, check.attributes = FALSE)
})

ageReclassification <- function(cohortData, successionTimestep, stage) {

  # Slight faster to check only, if not needed, than always convert
  if (!is.integer(successionTimestep))
    successionTimestep <- asInteger(successionTimestep)

  successionTimestepPlusOne <- successionTimestep + 1L

  if (stage == "spinup") {
    # for spin up stage
    cohortData[age == successionTimestepPlusOne, age := successionTimestep]
  } else {

    # non- spinup stage
    targetData <- cohortData[age <= successionTimestepPlusOne, ]

    # Squash multiple cohorts that regenerated within the successionTimestep
    #   into a single cohort
    targetData <- targetData[, .(ecoregionGroup = unique(ecoregionGroup),
                                 age = successionTimestepPlusOne,
                                 B = sum(B, na.rm = TRUE),
                                 mortality = sum(mortality, na.rm = TRUE),
                                 aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                             by = .(pixelGroup, speciesCode)]
    targetData <- targetData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                 B, mortality, aNPPAct)]
    cohortData <- cohortData[age > successionTimestepPlusOne]
    cohortData <- rbindlist(list(cohortData, targetData))
  }
  if (isTRUE(getOption("LandR.assertions"))) {
    if (!identical(NROW(cohortData), NROW(unique(cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }

  }
  return(cohortData)
}

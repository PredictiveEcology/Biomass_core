ageReclassification <- compiler::cmpfun(function(cohortData, successionTimestep, stage,
                                                 byGroups = c("pixelGroup", "speciesCode", "age")) {

  byGroupsNoAge <- byGroups[!byGroups %in% "age"] # age is what will be lumped
  #byGroups default added for backwards compatibility
  # Slight faster to check only, if not needed, than always convert
  if (!is.integer(successionTimestep)) {
    successionTimestep <- asInteger(successionTimestep)
  }
  successionTimestepPlusOne <- successionTimestep + 1L

  if (stage == "spinup") {
    # for spin up stage
    cohortData[age == successionTimestepPlusOne, age := successionTimestep]
  } else {
    # non- spinup stage
    targetData <- cohortData[age <= successionTimestepPlusOne, ]

    # Squash multiple cohorts that regenerated within the successionTimestep
    #   into a single cohort
    # NOTE: We do not need to squash if there is nothing to squash, i.e., cases with 1 species in a pixelGroup that is <successionTimestep old,
    #       don't need to be squashed.
    anyDuplicates <- duplicated(targetData, by = byGroupsNoAge)
    cdColNames <- colnames(cohortData)
    message("  Setting all ages <= ", successionTimestep, " to ", successionTimestepPlusOne)
    if (any(anyDuplicates)) {
      # pull out only duplicated types. NOTE "which = TRUE" gives only the indices of the joined rows;
      # will use the inverse below
      tdDuplicates <- targetData[unique(targetData[anyDuplicates]), nomatch = NULL,
                                 on = byGroupsNoAge, which = TRUE]

      td <- targetData[tdDuplicates]

      td <- td[, .(ecoregionGroup = unique(ecoregionGroup),
                   age = successionTimestepPlusOne,
                   B = sum(B, na.rm = TRUE),
                   mortality = sum(mortality, na.rm = TRUE),
                   aNPPAct = sum(aNPPAct, na.rm = TRUE)),
               by = byGroupsNoAge]
      cdColNames <- intersect(colnames(cohortData), colnames(td))
      td <- td[, ..cdColNames] # keep only the columns, in the correct order, as cohortData
      tdNonDups <- targetData[-tdDuplicates]
      #age the non-duplicates, else unique 1 year-old cohorts stay age 1 for another successionTimestep
      tdNonDups <- tdNonDups[, age := successionTimestepPlusOne]
      targetData <- rbindlist(list(td, tdNonDups), fill = TRUE)
    } else {
      message("  No age reclassification to do")
      targetData[, age := successionTimestepPlusOne]
    }

    cohortData <- cohortData[age > successionTimestepPlusOne]
    cohortData <- rbindlist(list(cohortData, targetData), fill = TRUE)
  }
  if (isTRUE(getOption("LandR.assertions"))) {
    if (!identical(NROW(cohortData), NROW(unique(cohortData, by = byGroups)))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode, age and biomass")
    }
  }
  return(cohortData)
})

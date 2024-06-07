#' Reclassify cohort ages
#'
#' Collapses cohorts into age bins defined by `successionTimestep`,
#'  thereby reducing the number of cohorts in a `pixelGroup`.
#'
#' @param cohortData A `data.table` with columns: `pixelGroup`, `ecoregionGroup`,
#'   `speciesCode`, and optionally `age`, `B`, `mortality`, `aNPPAct`, and `sumB`.
#' @param successionTimestep The time between successive seed dispersal events.
#'   In LANDIS-II, this is called "Succession Timestep".
#' @param stage `character`. Either "spinup" or "nonSpinup", depending on whether
#'   the functions is running during spin-up stage or not. See details.
#' @param byGroups columns in `cohortData` defining that will define groups of
#'   cohorts whose ages will be collapsed.
#'
#' @detail at each step defined by `successionTimestep` (i.e. if `successionTimestep` = 10,
#'   at every 10 years), cohorts are collapsed into age bins defined by
#'   `successionTimestep` (i.e. if `successionTimestep` = 10, 10-year bins). When
#'   collapsing cohorts, their biomass (B), lost biomass (mortality) and primary
#'   productivity (aNPPact) are summed.
#'   If `stage == "spinup"` cohorts are not collapsed.
#'
#' @references Scheller, R.M. & Miranda, B.R. (2015). LANDIS-II Biomass Succession v3.2 Extension  – User Guide.
#'
#' @return
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom LandR asInteger
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

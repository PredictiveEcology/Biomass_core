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
      tdDuplicates <- targetData[unique(targetData[anyDuplicates], by = byGroupsNoAge), nomatch = NULL,
                                 on = byGroupsNoAge, which = TRUE]

      td <- targetData[tdDuplicates]

      set(td, NULL, "age", successionTimestepPlusOne)
      td <- aggregateCohorts(td, c(byGroupsNoAge, "age"), colnames(cohortData))
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

#' Sum cohorts that share the same grouping
#'
#' Internal helper shared by `ageReclassification()` and `squashCohortAges()`.
#'
#' @param cohortData A `data.table` with `ecoregionGroup`, `B`, `mortality` and `aNPPAct`.
#' @param by columns defining the cohorts to be merged (including `age`).
#' @param cols column names to keep, in order (usually `colnames(cohortData)`);
#'   only those present in the result are returned.
#'
#' @return a `data.table` with one row per `by` group; `B`, `mortality` and `aNPPAct` are summed.
#' @noRd
aggregateCohorts <- function(cohortData, by, cols) {
  out <- cohortData[, .(ecoregionGroup = unique(ecoregionGroup),
                        B = sum(B, na.rm = TRUE),
                        mortality = sum(mortality, na.rm = TRUE),
                        aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                    by = by]
  cdColNames <- intersect(cols, colnames(out))
  out[, ..cdColNames] # keep only the columns, in the correct order, as cohortData
}

#' Periodically squash old cohorts into age windows
#'
#' At squash times (every `squashInterval` years from `startTime`), cohorts of the same species in a
#' `pixelGroup` whose ages fall in the same window just below a multiple of `squashInterval` are
#' merged into one cohort. Window k is (k * `squashInterval` - w_k, k * `squashInterval`], where
#' w_k is `bandWidth[min(k, length(bandWidth))]`, i.e. the last width repeats. Ages between windows
#' are not changed. `B`, `mortality` and `aNPPAct` are summed; `age` is the
#' B-weighted mean age rounded to the `successionTimestep` grid (`ts * k + 1`). Merged cohorts that
#' land on the same age are summed. Cohorts alone in their window are not changed.
#'
#' @inheritParams ageReclassification
#' @param squashInterval years between squashes, counted from `startTime`; `NA` means never.
#' @param currentTime,startTime current and start time of the simulation. Nothing is done
#'   at `startTime` or when `currentTime - startTime` is not a multiple of `squashInterval`.
#' @param bandWidth widths (years) of the windows, starting with the one ending at `squashInterval`;
#'   the last value repeats.
#'
#' @return `cohortData`, with squashed cohorts if it is a squash time; otherwise `cohortData` itself.
#' @export
#' @importFrom data.table rbindlist set
squashCohortAges <- function(cohortData, squashInterval, currentTime, startTime,
                             bandWidth, successionTimestep,
                             byGroups = c("pixelGroup", "speciesCode", "age")) {
  if (is.na(squashInterval) || currentTime == startTime ||
      (currentTime - startTime) %% squashInterval != 0) {
    return(cohortData)
  }
  byGroupsNoAge <- setdiff(byGroups, "age")
  k <- ceiling(cohortData$age / squashInterval)
  inWindow <- cohortData$age > k * squashInterval - bandWidth[pmin(k, length(bandWidth))]
  old <- !is.na(inWindow) & inWindow
  if (!any(old)) return(cohortData)

  sq <- cohortData[old, ]
  set(sq, NULL, ".band", k[old])
  ## ages on the successionTimestep grid are ts * k + 1
  sq[, age := if (.N > 1L) {
    w <- if (sum(B, na.rm = TRUE) > 0) B else rep(1, .N)
    as.integer(round(sum((age - 1) * w, na.rm = TRUE) / sum(w, na.rm = TRUE) / successionTimestep) *
                 successionTimestep + 1)
  } else age[1L],
  by = c(byGroupsNoAge, ".band")]
  set(sq, NULL, ".band", NULL)
  sq <- aggregateCohorts(sq, c(byGroupsNoAge, "age"), colnames(cohortData))

  rbindlist(list(cohortData[!old, ], sq), fill = TRUE)
}

#' Which cohorts are old and suppressed?
#'
#' A cohort qualifies when its `B` is below `relativeB` times the total `B` of its `pixelGroup`
#' (all species) and its `age` is above `minAgeFrac * longevity`. A species' last cohort in a
#' `pixelGroup` is never removed: if all of its cohorts qualify, the one with the largest `B`
#' (first, in a tie) is kept.
#'
#' @param cohortData A `data.table` with `pixelGroup`, `speciesCode`, `age`, `B` and `longevity`.
#' @param relativeB fraction of the `pixelGroup`'s total `B`; `NA` removes nothing.
#' @param minAgeFrac fraction of `longevity` that `age` must exceed.
#'
#' @return a logical vector, one per row of `cohortData`, `TRUE` for cohorts to remove.
#' @export
suppressedCohortsToRemove <- function(cohortData, relativeB, minAgeFrac) {
  if (is.na(relativeB)) return(rep(FALSE, NROW(cohortData)))
  dt <- data.table::data.table(pg = cohortData$pixelGroup, sp = cohortData$speciesCode,
                               B = cohortData$B, age = cohortData$age,
                               longevity = cohortData$longevity)
  dt[, totB := sum(B, na.rm = TRUE), by = pg]
  dt[, remove := !is.na(B) & !is.na(age) & B < relativeB * totB & age > minAgeFrac * longevity]
  dt[, remove := remove & !(all(remove) & seq_len(.N) == which.max(B)), by = .(pg, sp)]
  dt$remove
}

#' Check the cohort-reduction parameters
#'
#' Stops with a message if `squashInterval` is not `NA` and is not a multiple of
#' `successionTimestep`, or if any `bandWidth` is not a positive multiple of `successionTimestep`
#' or exceeds `squashInterval` (windows would overlap).
#'
#' @param squashInterval,bandWidth,successionTimestep see the `cohortSquash*` and
#'   `successionTimestep` parameters.
#'
#' @return `NULL`, invisibly.
#' @export
checkCohortSquashParams <- function(squashInterval, bandWidth, successionTimestep) {
  if (!is.na(squashInterval)) {
    if (squashInterval %% successionTimestep != 0) {
      stop("cohortSquashInterval (", squashInterval, ") must be a multiple of successionTimestep (",
           successionTimestep, ")")
    }
    if (anyNA(bandWidth) || !all(bandWidth > 0 & bandWidth %% successionTimestep == 0)) {
      stop("cohortSquashBandWidth (", paste(bandWidth, collapse = ", "),
           ") must be positive multiples of successionTimestep (", successionTimestep, ")")
    }
    if (any(bandWidth > squashInterval)) {
      stop("cohortSquashBandWidth (", paste(bandWidth, collapse = ", "),
           ") must not exceed cohortSquashInterval (", squashInterval, "), or windows overlap")
    }
  }
  invisible(NULL)
}

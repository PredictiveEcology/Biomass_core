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

#' Squash cohorts that reach a multiple of an age interval
#'
#' Run at every age reclassification (every `successionTimestep`). With `ya = age - 1` (years since
#' establishment), a cohort is an anchor if it crossed a positive multiple of `squashInterval`
#' since the previous reclassification, i.e. `floor(ya / I) > floor((ya - successionTimestep) / I)`,
#' with `k = floor(ya / I)`. Each cohort is thus an anchor exactly once per multiple, on or off the
#' `ts * j + 1` age grid. An anchor is merged with all cohorts of the same `pixelGroup` x species whose `ya`
#' is in [`k * I - w_k`, `k * I + successionTimestep`) (anchors are those with `ya >= k * I`),
#' where `w_k` is `bandWidth[min(k, length(bandWidth))]` (the last width repeats).
#' `B`, `mortality` and `aNPPAct` are summed; `age` is the B-weighted mean age, rounded to the nearest
#' year (`age` is an integer column). Merged cohorts that land on the same age are summed.
#' An anchor with no partner is not changed.
#'
#' @inheritParams ageReclassification
#' @param squashInterval age interval (years); `NA` means no squashing.
#' @param bandWidth widths (years) of the windows below each multiple of `squashInterval`;
#'   the k-th multiple uses the k-th width and the last width repeats.
#'
#' @return `cohortData`, with squashed cohorts.
#' @export
#' @importFrom data.table rbindlist set
squashCohortAges <- function(cohortData, squashInterval, bandWidth, successionTimestep,
                             byGroups = c("pixelGroup", "speciesCode", "age")) {
  if (is.na(squashInterval)) return(cohortData)
  byGroupsNoAge <- setdiff(byGroups, "age")
  ya <- cohortData$age - 1
  isAnchor <- !is.na(ya) & ya >= squashInterval &
    floor(ya / squashInterval) > floor((ya - successionTimestep) / squashInterval)
  if (!any(isAnchor)) return(cohortData)

  anchors <- cohortData[isAnchor, ..byGroupsNoAge]
  set(anchors, NULL, "k", floor(ya[isAnchor] / squashInterval))
  set(anchors, NULL, "hi", anchors$k * squashInterval + successionTimestep)
  set(anchors, NULL, "lo", anchors$k * squashInterval - bandWidth[pmin(anchors$k, length(bandWidth))])
  anchors <- unique(anchors)
  set(anchors, NULL, ".anchor", seq_len(NROW(anchors)))
  rows <- cohortData[, ..byGroupsNoAge]
  set(rows, NULL, "rowi", seq_len(NROW(rows)))
  set(rows, NULL, "ya", ya)
  hits <- anchors[rows, on = c(byGroupsNoAge, "lo<=ya", "hi>ya"), nomatch = NULL,
                  .(rowi = rowi, anchor = .anchor)]
  hits <- hits[!duplicated(hits$rowi), ]
  band <- rep(NA_integer_, NROW(cohortData))
  band[hits$rowi] <- hits$anchor
  old <- !is.na(band)

  sq <- cohortData[old, ]
  set(sq, NULL, ".band", band[old])
  ## B-weighted mean age, to the nearest year (age is an integer column)
  sq[, age := if (.N > 1L) {
    w <- if (sum(B, na.rm = TRUE) > 0) B else rep(1, .N)
    as.integer(round(sum((age - 1) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)) + 1)
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
#' Each of `cohortSquash`, `cohortRemoveSuppressed` and `cohortEstablishmentLimit` must be `NA`
#' (off) or a numeric vector of the right form; stops with a message naming the parameter and the
#' form otherwise.
#'
#' * `cohortSquash`: `c(interval, width, ...)`, at least two values, each a positive multiple of
#'   `successionTimestep`.
#' * `cohortRemoveSuppressed`: `c(relativeB, minAgeFrac)`, both between 0 and 1.
#' * `cohortEstablishmentLimit`: `c(interval, minCohorts)`, `interval` a positive multiple of
#'   `successionTimestep` and `minCohorts` >= 0.
#'
#' @param cohortSquash,cohortRemoveSuppressed,cohortEstablishmentLimit,successionTimestep see the
#'   parameters of the same names.
#'
#' @return `NULL`, invisibly.
#' @export
checkCohortReductionParams <- function(cohortSquash, cohortRemoveSuppressed, cohortEstablishmentLimit,
                                       successionTimestep) {
  isMultiple <- function(x) all(x > 0 & x %% successionTimestep == 0)
  check <- function(x, name, form, nValues, valid, why) {
    if (length(x) == 1L && is.na(x)) return(invisible(NULL)) # off
    if (!is.numeric(x) || anyNA(x) || !nValues(length(x)) || !valid(x)) {
      stop(name, " must be NA or ", form, why, "; got c(", paste(x, collapse = ", "), ")", call. = FALSE)
    }
  }
  check(cohortSquash, "cohortSquash", "c(interval, width, ...)",
        function(n) n >= 2L, isMultiple,
        paste0(", with every value a positive multiple of successionTimestep (", successionTimestep, ")"))
  check(cohortRemoveSuppressed, "cohortRemoveSuppressed", "c(relativeB, minAgeFrac)",
        function(n) n == 2L, function(x) all(x >= 0 & x <= 1), ", both between 0 and 1")
  check(cohortEstablishmentLimit, "cohortEstablishmentLimit", "c(interval, minCohorts)",
        function(n) n == 2L, function(x) isMultiple(x[1]) && x[2] >= 0,
        paste0(", with interval a positive multiple of successionTimestep (", successionTimestep,
               ") and minCohorts >= 0"))
  invisible(NULL)
}

#' Which (pixelGroup, species) may not receive new seed-established cohorts now?
#'
#' A species in a `pixelGroup` that already has more than `minCohorts` cohorts older than the
#' newly established ones (`age > 1`; `age == 1` cohorts come from serotiny or resprouting this
#' step) is blocked, unless `currentTime` is a multiple of `interval`. Nothing is blocked when
#' `interval` is `NA`. Seeding paths drop these pairs from their seed receivers; serotiny and
#' resprouting are not affected.
#'
#' @param cohortData A `data.table` with `pixelGroup`, `speciesCode` and `age`.
#' @param currentTime the simulation time, `time(sim)`.
#' @param interval years between the times when blocked pairs may establish; `NA` blocks nothing.
#' @param minCohorts a pair is blocked when it has more than this many cohorts.
#'
#' @return a `data.table` with columns `pixelGroup` and `speciesCode` (no rows if nothing is blocked).
#' @export
conditionalTimestepBlocked <- function(cohortData, currentTime, interval, minCohorts) {
  byCols <- c("pixelGroup", "speciesCode")
  if (is.na(interval) || round(currentTime) %% interval == 0) {
    return(cohortData[integer(0), ..byCols])
  }
  n <- cohortData[age > 1L, .N, by = byCols]
  n[N > minCohorts, ..byCols]
}

#' Drop blocked (pixelGroup, species) pairs from a seed table
#'
#' @param x a `data.table` with `pixelGroup` and `speciesCode`.
#' @param blocked the result of [conditionalTimestepBlocked()]. When it has no rows (the default, with
#'   the option off) `x` is returned as is, so the anti-join and its copy of `x` are skipped.
#'
#' @return `x` without the rows in `blocked`.
#' @export
dropBlockedSeeds <- function(x, blocked) {
  if (nrow(blocked) == 0L) x else x[!blocked, on = names(blocked)]
}

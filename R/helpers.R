#' updateSpeciesEcoregionAttributes
#'
#' TODO: description and title needed
#'
#' @param speciesEcoregion TODO: description needed
#' @param currentTime TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
#' @importFrom LandR speciesEcoregionLatestYear
updateSpeciesEcoregionAttributes <- function(speciesEcoregion, currentTime, cohortData) {
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  colNams <- colnames(cohortData)
  speciesEcoregionTraitNames <- c("maxB", "maxANPP", "maxB_eco")

  # First determine whether cohortData already has all the info it needs.
  #  There are 3 reasons it doesn't: 1. first time, 2. cohortData table is different, 3. new year of data in speciesEcoregion
  needJoin <- TRUE
  if (all(speciesEcoregionTraitNames %in% colNams)) {
    if (!anyNA(cohortData$maxB)) { # if there is an NA, it means that a cohort has no data
      if (!any(currentTime %in% unique(speciesEcoregion$year))) # refresh based on speciesEcoregion year
        needJoin <- FALSE
    }
  }

  # Second, if needed, then update the cohortData table with the speciesEcoregion traits:
  #  i.e., "do the join"
  if (needJoin) {
    colsToRm <- intersect(speciesEcoregionTraitNames, colNams)
    if (length(colsToRm))
      set(cohortData, NULL, colsToRm, NULL)
    # cohortData <- cohortData[, -..colsToRm]
    specieseco_current <- speciesEcoregionLatestYear(speciesEcoregion, currentTime)
    specieseco_current <- setkey(specieseco_current[, .(speciesCode, maxANPP, maxB, ecoregionGroup)],
                                 speciesCode, ecoregionGroup)
    specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]

    # The "update" line
    cohortData <- specieseco_current[cohortData, on = c("speciesCode", "ecoregionGroup"), nomatch = 0]
  }


  return(cohortData)
}

#' updateSpeciesAttributes
#'
#' TODO: description and title needed
#'
#' @param species TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
updateSpeciesAttributes <- function(species, cohortData) {
  # to assign longevity, mortalityshape, growthcurve to cohortData
  colNams <- colnames(cohortData)
  speciesTraitNames <- c("growthcurve", "longevity", "mortalityshape")
  needJoin <- TRUE
  # First determine whether cohortData already has all the info it needs.
  #  There are 2 reasons it doesn't: 1. first time, 2. cohortData table is different
  if (all(speciesTraitNames %in% colNams)) {
    if (!anyNA(cohortData$longevity)) {
      needJoin <- FALSE
    }
  }
  # Second, if needed, then update the cohortData table with the species traits:
  #  i.e., "do the join"
  if (needJoin) {
    colsToRm <- intersect(speciesTraitNames, colNams)
    if (length(colsToRm))
      cohortData <- cohortData[, -..colsToRm]
    species_temp <- setkey(species[, .(speciesCode, longevity, mortalityshape, growthcurve)], speciesCode)
    setkey(cohortData, speciesCode)
    cohortData <- cohortData[species_temp, nomatch = 0]
  }
  return(cohortData)
}

#' Calculate total biomass
#'
#' Calculate the total stand biomass that does not include the new cohorts.
#' The new cohorts are defined as the age younger than simulation time step.
#' TODO: update description.
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param lastReg TODO: description needed
#' @param currentTime TODO: description needed -- rename this to 'time' to match others
#' @param successionTimestep TODO: description needed
#' @param doAssertion TODO: description needed (see LandR for description)
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table copy rbindlist setkey
calculateSumB <- compiler::cmpfun(function(cohortData, lastReg, currentTime, successionTimestep,
                                           doAssertion = getOption("LandR.assertions", TRUE)) {
  nrowCohortData <- NROW(cohortData)

  is2YrsBeforeSuccessionTS <- (currentTime == lastReg + successionTimestep - 2)

  # algo <- 1 + (nrowCohortData < 1e6) # algo 1 is faster when large
  # if (isTRUE(doAssertion)) {
  #   message("LandR::vegTypeMapGenerator: NROW(cohortData) == ", nrowCohortData)
  #   algo <- 1:2
  # }
  # algo <- 1

  ## use new vs old algorithm based on size of cohortData. new one (2) is faster in most cases.
  ## enable assertions to view timings for each algorithm before deciding which to use.
  ## Eliot update -- Nov 30 2021 -- this no longer seems to be true. Old is now always faster

  if (is2YrsBeforeSuccessionTS) {
    wh <- which(cohortData$age > successionTimestep)
  } else {
    wh <- which(cohortData$age >= successionTimestep)

  }

  if (NROW(wh)) {
    cohortData[wh, sumB := sum(B, na.rm = TRUE), by = "pixelGroup"]
    #
    # if (1 %in% algo) {
    #   ## this older version is typically much slower than the newer one below (Eliot June 2, 2019)
    #   if (isTRUE(doAssertion)) {
    #     old1 <- Sys.time()
    #   }
    #   cohortData1 <- if (isTRUE(doAssertion)) copy(cohortData) else cohortData
    #   cohortData1[wh, sumB := sum(B, na.rm = TRUE), by = "pixelGroup"]
    #
    #   if (isTRUE(doAssertion)) {
    #     old2 <- Sys.time()
    #   }
    # }
    #
    # if (2 %in% algo) {
    #   # This seems to be a lot faster when small cohortData, and somewhat faster even on the largest sizes tried (40M rows)
    #   if (isTRUE(doAssertion))
    #     new1 <- Sys.time()
    #   cohortData2 <- dtBy(dt = cohortData, rowSubset = wh, sumCol = "B",
    #                       by = "pixelGroup", resultColName = "sumB", byFn = sum)
    #   if (isTRUE(doAssertion))
    #     new2 <- Sys.time()
    # }
    #
    # cohortData <- if (1 %in% algo) cohortData1 else cohortData2
    # if (!is.integer(cohortData[["sumB"]]))
    #   set(cohortData, NULL, "sumB", asInteger(cohortData[["sumB"]]))
    #
    # if (isTRUE(doAssertion)) {
    #
    #   mod <- get("mod")
    #   if (!exists("oldAlgoSumB", envir = mod, inherits = FALSE)) mod$oldAlgoSumB <- 0
    #   if (!exists("newAlgoSumB", envir = mod, inherits = FALSE)) mod$newAlgoSumB <- 0
    #   mod$oldAlgoSumB <- mod$oldAlgoSumB + (old2 - old1)
    #   mod$newAlgoSumB <- mod$newAlgoSumB + (new2 - new1)
    #
    #   print(paste("Biomass_core:calculateSumB: new algo", mod$newAlgoSumB))
    #   print(paste("Biomass_core:calculateSumB: old algo", mod$oldAlgoSumB))
    #
    #   setkeyv(cohortData1, c("pixelGroup", "speciesCode", "age"))
    #   setkeyv(cohortData2, c("pixelGroup", "speciesCode", "age"))
    #
    #   if (!identical(cohortData1$sumB, cohortData2$sumB)) {
    #     stop("calculateSumB: new algorithm differs from old algorithm")
    #   }
    #}
  } else {
    set(cohortData, NULL, "sumB", 0L)
    message("Skipping sumB calculation because there are no cohorts older than successionTimestep")
  }
  return(cohortData)
})

#' calculateAgeMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#' @param spinupMortalityfraction TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
calculateAgeMortality <- function(cohortData, stage = "nonSpinup", spinupMortalityfraction) {
  # for age-related mortality calculation
  if (stage == "spinup") {
    cohortData[age > 0, mAge := B*(exp((age) / longevity*mortalityshape) / exp(mortalityshape))]
    cohortData[age > 0, mAge := mAge + B*spinupMortalityfraction]
    cohortData[age > 0, mAge := pmin(B, mAge)]
  } else {
    set(cohortData, NULL, "mAge",
        cohortData$B * (exp((cohortData$age) / cohortData$longevity * cohortData$mortalityshape) /
                          exp(cohortData$mortalityshape)))
    set(cohortData, NULL, "mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

#' calculateANPP
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
calculateANPP <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0, aNPPAct := maxANPP * exp(1) * (bAP^growthcurve) *
                 exp(-(bAP^growthcurve)) * bPM]
    cohortData[age > 0, aNPPAct := pmin(maxANPP * bPM, aNPPAct)]
  } else {
    # if (any(cohortData$pixelGroup == 519359 & cohortData$age > 200)) browser()
    bAPExponentGrowthCurve <- cohortData$bAP^cohortData$growthcurve
    aNPPAct <- cohortData$maxANPP * exp(1) * (bAPExponentGrowthCurve) *
      exp(-(bAPExponentGrowthCurve)) * cohortData$bPM

    # aNPPAct <- cohortData$maxANPP * exp(1) * (cohortData$bAP^cohortData$growthcurve) *
    #   exp(-(cohortData$bAP^cohortData$growthcurve)) * cohortData$bPM

    set(cohortData, NULL, "aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM, aNPPAct))
  }
  return(cohortData)
})

#' calculateGrowthMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
#' @importFrom fpCompare %>>% %<=%
calculateGrowthMortality <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP) / (1 + bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    bAPLarge <- cohortData$bAP %>>% 1.0
    whbAPLarge <- which(bAPLarge)
    whbAPSmall <- which(!bAPLarge)
    # whNot <- which(cohortData$bAP %<=% 1.0)
    set(cohortData, whbAPLarge, "mBio", cohortData$maxANPP*cohortData$bPM)
    set(cohortData, whbAPSmall, "mBio", cohortData$maxANPP*(2*cohortData$bAP)/(1 + cohortData$bAP)*cohortData$bPM)
    # cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    # cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, NULL, "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, NULL, "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
})

#' calculateCompetition
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table key setkeyv
calculateCompetition <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  # two competition indics are calculated bAP and bPM
  if (stage == "spinup") {
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, NULL, "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier), by = pixelGroup]
    cohortData[age > 0, bPM := cMultiplier / cMultTotal]
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, NULL, "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))  ## differs from manual, follows source code
    set(cohortData, NULL, "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, NULL, "bPot", NULL)
    set(cohortData, NULL, "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))

    # These 2 lines are 5x slower compared to replacement 6 lines below -- Eliot June 2, 2019
    #  Still faster on Nov 2021 by Eliot, for cohortData of ~800,000 rows
    if (FALSE) {
      cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
      set(cohortData, NULL, "bPM", cohortData$cMultiplier / cohortData$cMultTotal)
    }

    # Faster replacement -- 1) sort on pixelGroup, 2) sum by group and .N by group, but don't reassign to full table
    #                       3) rep the sumByGroup each .N times  4) now reassign vector back to data.table
    oldKey <- checkAndChangeKey(cohortData, "pixelGroup")
    cMultTotalTmp <- cohortData[, list(N = .N, Sum = sum(cMultiplier)), by = pixelGroup]
    cMultTotal <- rep.int(cMultTotalTmp$Sum, cMultTotalTmp$N)
    set(cohortData, NULL, "bPM", cohortData$cMultiplier / cMultTotal)
    if (!is.null(oldKey))
      setkeyv(cohortData, oldKey)


    set(cohortData, NULL, c("cMultiplier"), NULL)
  }
  return(cohortData)
})

checkAndChangeKey <- function(obj, key) {
  oldKey <- key(obj)
  oldKeyWasFine <- !identical(oldKey, key)
  returnKey <- if (oldKeyWasFine) {
    setkeyv(obj, key)
    oldKey
  } else {
    NULL
  }
  returnKey
}

maxRowsDT <- function(maxLen, maxMem, startClockTime, groupSize,
                      modEnv) {

  updateMaxMemoryTime <- FALSE
  if (!exists("groupSize", envir = modEnv)) {
    updateMaxMemoryTime <- TRUE
  } else {
    if (startClockTime >= attr(groupSize, "groupSizeTime")) {
      updateMaxMemoryTime <- TRUE
    }
  }
  if (updateMaxMemoryTime) {
    am <- suppressWarnings(availableMemory())
    if (!is.null(am)) {
      maxMemAdj <- min(as.numeric(am) / 1e9, maxMem) ## memory (GB) avail.
      maxLenAdj <- try(as.integer(log(maxMemAdj + 2)^5 * 1e4), silent = TRUE)
      if (is.numeric(maxLenAdj))
        if (maxLenAdj > 1e5)
          maxLen <- maxLenAdj
    }
    # maxLen <- maxRowsDT(maxLen = 1e7, maxMem = P(sim)$.maxMemory, sim)
    groupSize <- maxLen
    attr(groupSize, "groupSizeTime") <- Sys.time()
    # sim$.maxMemoryTime <- Sys.time()
  }

  return(groupSize)
}


#' A faster alternative to dt[, something:=somefn(somestuff), by = someCol]
#'
#' It is not clear why this is faster, but it is... basical
dtBy <- function(dt, rowSubset, sumCol = "B", by = "pixelGroup", resultColName = "sumB", byFn = sum) {
  oldKey <- key(dt)
  keepCols <- union(c(sumCol, by, "age"), oldKey)
  # keepCols <- union(c(sumCol, by, "age"), oldKey)
  cohortData2 <- copy(dt[, ..keepCols])
  set(cohortData2, NULL, "origOrd", seq(NROW(dt)))
  new1 <- Sys.time()
  oldKeyToDelete <- checkAndChangeKey(cohortData2, by)
  # if (is2YrsBeforeSuccessionTS) {
  #   wh <- which(cohortData2$age > successionTimestep)
  # } else {
  #   wh <- which(cohortData2$age >= successionTimestep)
  # }
  sumBtmp <- cohortData2[rowSubset, list(N = .N, sumB = byFn(B, na.rm = TRUE)), by = by]
  if ("sumB" %in% names(dt)) set(dt, NULL, "sumB", NULL)
  # create empty column as there are some cases with rowSubset is length 0
  if (length(rowSubset) == 0)
    set(cohortData2, NULL, "sumB", NA_integer_)
  set(cohortData2, rowSubset, "sumB", rep.int(sumBtmp[["sumB"]], sumBtmp[["N"]]))
  setorderv(cohortData2, c("sumB"), na.last = TRUE)
  a <- cohortData2[, list(sumB2 = sumB[1]), by = by]
  setorderv(cohortData2, c(by, "sumB"), na.last = TRUE)
  sumB <- a[cohortData2, on = by][["sumB2"]]
  sumB[is.na(sumB)] <- 0L
  set(cohortData2, NULL, "sumB", sumB)
  if (!is.null(oldKey))
    setkeyv(cohortData2, oldKey)
  setorderv(cohortData2, "origOrd")
  set(cohortData2, NULL, "origOrd", NULL)
  rejoinCols <- setdiff(colnames(dt), keepCols)
  for (rc in rejoinCols)
    set(cohortData2, NULL, rc, dt[[rc]])
  setcolorder(cohortData2, colnames(dt))
  new2 <- Sys.time()
  if (!is.integer(cohortData2[["sumB"]]))
    set(cohortData2, NULL, "sumB", asInteger(cohortData2[["sumB"]]))
  cohortData2
}

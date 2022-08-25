#' Updates the speciesEcoregion used in current year from original speciesEcoregion file
#'
#' A LANDIS-II speciesEcoregion file has a column called "year", which specifies the
#' year after which the speciesEcoregion values should be used. This function
#' compares the `currentTime` in the simulation with the `year` column in the speciesEcoregion
#' file and updates accordingly, if needed.
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

  if (is2YrsBeforeSuccessionTS) {
    wh <- cohortData$age > successionTimestep
  } else {
    wh <- cohortData$age >= successionTimestep
  }

  set(cohortData, NULL, "sumB", 0L) # this is faster than only doing for !wh
  if (any(wh)) {
    if (FALSE) {  # This is the "normal" data.table way. But it is slow, surprisingly
      cohortData[wh, sumB := sum(B, na.rm = TRUE), by = "pixelGroup"]
    } else {
      # Faster replacement -- 1) sort on pixelGroup, 2) sum by group and .N by group, but don't reassign to full table
      #                       3) rep the sumByGroup each .N times  4) now reassign vector back to data.table
      oldKey <- checkAndChangeKey(cohortData, "pixelGroup")
      tmp <- cohortData[wh, list(N = .N, Sum = sum(B, na.rm = TRUE)), by = "pixelGroup"]
      set(cohortData, which(wh), "sumB", rep.int(tmp$Sum, tmp$N))
      if (!is.null(oldKey)) setkeyv(cohortData, oldKey) # marginally faster to avoid
    }

  } else {
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
    set(cohortData, NULL, "mAge", pmin(cohortData$B, cohortData$mAge))
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
    bAPExponentGrowthCurve <- cohortData$bAP^cohortData$growthcurve
    aNPPAct <- cohortData$maxANPP * exp(1) * (bAPExponentGrowthCurve) *
      exp(-(bAPExponentGrowthCurve)) * cohortData$bPM
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
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
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

    bPot <- pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B)  ## differs from manual, follows source code
    set(cohortData, NULL, "bAP", cohortData$B/bPot)
    set(cohortData, NULL, "cMultiplier", pmax(cohortData$B^0.95, 1))

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
    if (!is.null(oldKey)) setkeyv(cohortData, oldKey)

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


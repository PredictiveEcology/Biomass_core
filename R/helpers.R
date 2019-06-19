#' updateSpeciesEcoregionAttributes
#'
#' TODO: description and title needed
#'
#' @param speciesEcoregion TODO: description needed
#' @param time TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
#' @importFrom LandR speciesEcoregionLatestYear
updateSpeciesEcoregionAttributes <- function(speciesEcoregion, time, cohortData) {
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregionLatestYear(speciesEcoregion, time)

  #specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[, .(speciesCode, maxANPP, maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]

  cohortData <- specieseco_current[cohortData, on = c("speciesCode", "ecoregionGroup"), nomatch = 0]
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
  species_temp <- setkey(species[, .(speciesCode, longevity, mortalityshape, growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch = 0]
  return(cohortData)
}

#' Calculate total biomass
#'
#' TODO: description needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param lastReg TODO: description needed
#' @param simuTime TODO: description needed -- rename this to 'time' to match others
#' @param successionTimestep TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table rbindlist setkey
calculateSumB <- function(cohortData, lastReg, simuTime, successionTimestep) {
  # this function is used to calculate total stand biomass that does not include the new cohorts
  # the new cohorts are defined as the age younger than simulation time step
  # reset sumB
  if (getOption("LandR.assertions")) {
    cohortData2 <- data.table::copy(cohortData)

    uniqueCohortDataPixelGroup <- unique(cohortData$pixelGroup)
    pixelGroups <- setDT(list(pixelGroupIndex = uniqueCohortDataPixelGroup,
                              temID = 1:length(uniqueCohortDataPixelGroup)))
    if (getOption("LandR.assertions")) { # old algorithm -- May 29 Eliot changed to above
      pixelGroups2 <- data.table(pixelGroupIndex = uniqueCohortDataPixelGroup,
                                 temID = 1:length(uniqueCohortDataPixelGroup))
      if (!identical(pixelGroups, pixelGroups2))
        stop()
    }

    cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
    if (length(cutpoints) == 1) {cutpoints <- c(cutpoints, cutpoints + 1)}
    pixelGroups[, groups := cut(temID, breaks = cutpoints,
                                labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                                include.lowest = TRUE)]
    for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
      subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
      set(subCohortData, NULL, "sumB", 0L)
      if (simuTime == lastReg + successionTimestep - 2) {
        sumBtable <- subCohortData[age > successionTimestep,
                                   .(tempsumB = sum(B, na.rm = TRUE)), by = pixelGroup]
      } else {
        sumBtable <- subCohortData[age >= successionTimestep,
                                   .(tempsumB = sum(B, na.rm = TRUE)), by = pixelGroup]
      }
      if (!is.integer(sumBtable[["tempsumB"]]))
        set(sumBtable, NULL, "tempsumB", asInteger(sumBtable[["tempsumB"]]))

      subCohortData <- merge(subCohortData, sumBtable, by = "pixelGroup", all.x = TRUE)
      subCohortData[is.na(tempsumB), tempsumB := 0L][, ':='(sumB = tempsumB, tempsumB = NULL)]

      if (subgroup == "Group1") {
        newcohortData <- subCohortData
      } else {
        newcohortData <- rbindlist(list(newcohortData, subCohortData))
      }
      rm(subCohortData, sumBtable)
    }

    rm(cohortData, pixelGroups, cutpoints)
    cohortData <- data.table::copy(cohortData2)
  }

  # if (getOption("LandR.assertions")) {
  #   cohortData[wh, sumB := sum(B, na.rm = TRUE), by = "pixelGroup"]
  # }
  # Faster than above by 5x
  new1 <- Sys.time()
  oldKey <- checkAndChangeKey(cohortData, "pixelGroup")
  wh <- which(cohortData$age >= successionTimestep)
  sumBtmp <- cohortData[wh, list(N = .N, sumB = sum(B, na.rm = TRUE)), by = "pixelGroup"]
  if ("sumB" %in% names(cohortData)) set(cohortData, NULL, "sumB", NULL)
  set(cohortData, wh, "sumB", rep.int(sumBtmp$sumB, sumBtmp$N))
  setorderv(cohortData, c("sumB"), na.last = TRUE)
  a <- cohortData[, list(sumB2 = sumB[1]), by = "pixelGroup"]
  setorderv(cohortData, c("pixelGroup", "sumB"), na.last = TRUE)
  sumB <- a[cohortData, on = "pixelGroup"]$sumB2
  sumB[is.na(sumB)] <- 0L
  set(cohortData, NULL, "sumB", sumB)
  if (!is.null(oldKey))
    setkeyv(cohortData, oldKey)

  new2 <- Sys.time()
  if (!is.integer(cohortData[["sumB"]]))
    set(cohortData, NULL, "sumB", asInteger(cohortData[["sumB"]]))

  if (getOption("LandR.assertions")) { # much slower than next lines 5x  -- Eliot June 2, 2019
    old1 <- Sys.time()

    cohortData[age >= successionTimestep, sumB2 := sum(B, na.rm = TRUE), by = "pixelGroup"]
    setorderv(cohortData, c("sumB2"), na.last = TRUE)
    a2 <- cohortData[, list(sumB3 = sumB2[1]), by = "pixelGroup"]
    sumB2 <- a2[cohortData, on = "pixelGroup"]$sumB3
    sumB2[is.na(sumB2)] <- 0L
    set(cohortData, NULL, "sumB2", sumB2)
    if (!isTRUE(all.equal(cohortData$sumB, cohortData$sumB2)))
      stop("Failed test in calculateSumB")
    set(cohortData, NULL, "sumB2", NULL)
    old2 <- Sys.time()

    if (!exists("oldAlgo")) oldAlgo <<- 0
    if (!exists("newAlgo")) newAlgo <<- 0

    oldAlgo <<- oldAlgo + (old2 - old1)
    newAlgo <<- newAlgo + (new2 - new1)
  }

  if  (isTRUE(getOption("LandR.assertions"))) {
    setkeyv(newcohortData, c("pixelGroup", "speciesCode", "age"))
    setkeyv(cohortData, c("pixelGroup", "speciesCode", "age"))
    setcolorder(newcohortData, names(cohortData))

    if (!identical(newcohortData, cohortData)) {
      stop("calculateSumB new algorithm differs from old algorithm")
    }
  }
  newcohortData <- cohortData
  return(newcohortData)
}

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
    cohortData[age > 0, mAge := mAge+B*spinupMortalityfraction]
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
calculateANPP <- function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0, aNPPAct := maxANPP * exp(1) * (bAP^growthcurve) *
                 exp(-(bAP^growthcurve)) * bPM]
    cohortData[age > 0, aNPPAct := pmin(maxANPP * bPM, aNPPAct)]
  } else {
    aNPPAct <- cohortData$maxANPP * exp(1) * (cohortData$bAP^cohortData$growthcurve) *
          exp(-(cohortData$bAP^cohortData$growthcurve)) * cohortData$bPM
    set(cohortData, NULL, "aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM, aNPPAct))
  }
  return(cohortData)
}

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
calculateGrowthMortality <- function(cohortData, stage = "nonSpinup") {
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
}

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
#' @importFrom data.table set
calculateCompetition <- function(cohortData, stage = "nonSpinup") {
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
    if (FALSE) {
      cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
      set(cohortData, NULL, "bPM", cohortData$cMultiplier / cohortData$cMultTotal)
    }

    # Faster replacement
    oldKey <- checkAndChangeKey(cohortData, "pixelGroup")
    cMultTotalTmp <- cohortData[, list(N = .N, Sum = sum(cMultiplier)), by = pixelGroup]
    cMultTotal <- rep.int(cMultTotalTmp$Sum, cMultTotalTmp$N)
    set(cohortData, NULL, "bPM", cohortData$cMultiplier / cMultTotal)
    if (!is.null(oldKey))
      setkeyv(cohortData, oldKey)


    set(cohortData, NULL, c("cMultiplier"), NULL)
  }
  return(cohortData)
}

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

addNewCohorts <- function(newCohortData, cohortData, pixelGroupMap, time, speciesEcoregion) {
  # this function is for 1) adding new cohort data into cohortdata
  # 2) assign initial biomass and age for new cohort
  # 3) assign the new pixelgroup to the pixels that have new cohort
  # 4) update the pixelgroup map
  # newCohortData must have the original pixelgroup, regenerated species and pixelindex
  # it also would be better if it has the columns of cohortData plus pixelIndex
  browser()
  setdiff(unique(pixelGroupMap[]), unique(cohortData$pixelGroup) )

  newCohortData[, pixelGroup := getValues(pixelGroupMap)[pixelIndex]]
  set(newCohortData, NULL, "temppixelGroup", as.integer(as.factor(newCohortData$pixelGroup)))
  set(newCohortData, NULL, "speciesposition", 2^(as.integer(newCohortData$speciesCode)))
  # newCohortDataExtra is used to connect the original pixelGroup to the newPixelGroup
  # communities are any unique combinations of species
  newCohortDataExtra <- newCohortData[, .(community = sum(speciesposition),
                                          pixelGroup = mean(pixelGroup),
                                          temppixelGroup = mean(temppixelGroup)),
                                      by = pixelIndex]
  set(newCohortData, NULL, c("temppixelGroup", "speciesposition"), NULL)
  set(newCohortDataExtra, NULL, "community",
      as.integer(as.factor(newCohortDataExtra$community)))
  ## make unique ids for the combination of communities and pix groups
  ## if there more communities, start IDs >max(communities)
  ## if there more/= pix groups, start IDs >max(pix groups)
  if (max(newCohortDataExtra$community) > max(newCohortDataExtra$temppixelGroup)) {
    set(newCohortDataExtra, NULL,  "community",
        newCohortDataExtra$community +
          max(newCohortDataExtra$community) * newCohortDataExtra$temppixelGroup)
  } else {
    set(newCohortDataExtra, NULL, "community",
        newCohortDataExtra$temppixelGroup +
          max(newCohortDataExtra$temppixelGroup) * newCohortDataExtra$community)
  }

  ## make new pixel groups by adding community IDs to previous max pix group ID
  maxPixelGroup <- max(max(cohortData$pixelGroup), maxValue(pixelGroupMap))
  set(newCohortDataExtra, NULL,  "newpixelGroup",
      as.integer(as.factor(newCohortDataExtra$community)) + maxPixelGroup)
  set(newCohortDataExtra, NULL, c("community", "temppixelGroup"), NULL)
  setkey(newCohortData, pixelIndex)
  setkey(newCohortDataExtra, pixelIndex)
  newCohortData <- newCohortData[,pixelGroup := NULL][newCohortDataExtra][,pixelIndex := NULL]
  newCohortData <- unique(newCohortData, by = c("newpixelGroup", "speciesCode"))

  ## extract total pix group biomass, and join to new data - WHY there are no common pixIDs?
  sumTable <- cohortData[, .(pixelGroup,sumB)] %>% unique(., by = c("pixelGroup"))
  newCohortData <- sumTable[newCohortData, on = "pixelGroup"]
  #newCohortData <- dplyr::left_join(newCohortData, sumTable, by = "pixelGroup") %>% data.table()
  newCohortData[is.na(sumB),sumB := 0]
  set(cohortData, NULL, "sumB", NULL)

  # Place new pixel group into pixel group column, creating a new pixel group
  set(newCohortData, NULL, "pixelGroup", newCohortData$newpixelGroup)
  set(newCohortData, NULL, c("newpixelGroup"), NULL)

  ## get spp "productivity traits" per ecoregion/present year
  ## calculate maximum biomass per ecoregion, join to new cohort data
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP, maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current <- na.omit(specieseco_current) # There are many with no data on maxB or maxANPP
  specieseco_current[, maxB_eco := max(maxB, na.rm = TRUE), by = ecoregionGroup]
  newCohortData <- setkey(newCohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch = 0]

  browser()

  set(newCohortData, NULL, "age", 1)  ## set age to 1
  ## set biomass - if B=0, it's getting maxANPP ???
  set(newCohortData, NULL, "B",
      as.integer(pmax(1, newCohortData$maxANPP*exp(-1.6*newCohortData$sumB/newCohortData$maxB_eco))))
  set(newCohortData, NULL, "B", as.integer(pmin(newCohortData$maxANPP, newCohortData$B)))

  newCohortData <- newCohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B,
                                     mortality = 0, aNPPAct = 0)]
  newCohortDataExtra2 <- unique(newCohortDataExtra, by = c("pixelGroup", "newpixelGroup"))
  # newCohortDataExtra2 is further simplified form
  # identify which pixelGroups in cohortData have new regeneration ???
  ## Ceres: its seems to me like this is actually checking for overlapping data
  existingData <- cohortData[pixelGroup %in% unique(newCohortDataExtra2$pixelGroup)]
  setkey(newCohortDataExtra2, pixelGroup)
  setkey(existingData, pixelGroup)
  existingData <- existingData[newCohortDataExtra2, allow.cartesian = TRUE]
  existingData <- existingData[!is.na(ecoregionGroup)]
  set(existingData, NULL, "pixelGroup", existingData$newpixelGroup)
  set(existingData, NULL, c("pixelIndex", "newpixelGroup"), NULL)
  existingData <- unique(existingData, by = c("pixelGroup", "speciesCode", "age"))
  rm(newCohortDataExtra2)
  cohortData <- setkey(rbindlist(list(cohortData, newCohortData, existingData)),
                       pixelGroup, speciesCode, age)
  pixelGroupMap[as.integer(newCohortDataExtra$pixelIndex)] <- newCohortDataExtra$newpixelGroup

  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)),]
  pixelGroupMap_new <- pixelGroupMap

  temppixelIndex11 <- which(!(getValues(pixelGroupMap) %in% c(0, -1)))
  pgmTemp <- getValues(pixelGroupMap)[temppixelIndex11]
  pixelGroupMap_new[temppixelIndex11] <- as.integer(as.factor(pgmTemp))
  pixelGroupConnection <- data.table(pixelGroup = pgmTemp,
                                     newPixelGroup = getValues(pixelGroupMap_new)[temppixelIndex11]) %>%
    unique(by = "pixelGroup")
  setkey(pixelGroupConnection, pixelGroup)
  setkey(cohortData, pixelGroup)
  cohortData <- cohortData[pixelGroupConnection, nomatch = 0]
  set(cohortData, NULL, "pixelGroup", cohortData$newPixelGroup)
  set(cohortData, NULL, "newPixelGroup", NULL)
  pixelGroupMap <- setValues(pixelGroupMap_new, as.integer(pixelGroupMap_new[]))
  return(list(cohortData = cohortData,pixelGroupMap = pixelGroupMap))
}

ageReclassification <- function(cohortData, successionTimestep, stage) {
  if (stage == "spinup") {
    # for spin up stage
    cohortData[age == successionTimestep + 1, age := successionTimestep]
  } else {
    # non- spinup stage
    targetData <- cohortData[age <= (successionTimestep + 1), ]

    # Squash multiple cohorts that regenerated within the successionTimestep
    #   into a single cohort
    targetData <- targetData[, .(ecoregionGroup = unique(ecoregionGroup),
                                 age = successionTimestep + 1,
                                 B = sum(B, na.rm = TRUE),
                                 mortality = sum(mortality, na.rm = TRUE),
                                 aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                             by = .(pixelGroup, speciesCode)]
    targetData <- targetData[, .(pixelGroup, ecoregionGroup, speciesCode, age,
                                 B, mortality, aNPPAct)]
    cohortData <- cohortData[age >= successionTimestep + 2]
    cohortData <- rbindlist(list(cohortData, targetData))
  }
  return(cohortData)
}

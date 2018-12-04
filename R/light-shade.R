assignLightProb <- function(sufficientLight, newCohortData) {
  ## for each line, get the survival probability from sufficientLight table
  ## note that sufficentLight is a table of survival probs for each tolerance level (row) by and shade level (column)
  ## siteShade + 2 is necessary to skip the first column
  newCohortData[ , lightProb := sufficientLight[cbind(shadetolerance, siteShade + 2)]]
}

calcSiteShade <- function(time, cohortData, speciesEcoregion, minRelativeB) {
  # the siteshade was calculated based on the code:
  # https://github.com/LANDIS-II-Foundation/Extensions-Succession/blob/master/biomass-succession/trunk/src/PlugIn.cs
  if (nrow(cohortData[age > 5,]) > 0) {
    bAMterm1 <- cohortData[age > 5, ':='(prevMortality = sum(mortality, na.rm = TRUE),
                                         sumB = sum(B, na.rm = TRUE)),
                           by = .(pixelGroup, ecoregionGroup)]
    bAMterm1[is.na(sumB), sumB := 0]
    bAMterm1[is.na(prevMortality), prevMortality := 0]
    bAMterm1 <- unique(bAMterm1, by = c("pixelGroup", "ecoregionGroup"))
    set(cohortData, NULL, "prevMortality", NULL)
  } else {
    bAMterm1 <- unique(cohortData, by = c("pixelGroup", "ecoregionGroup"))[
      , .(pixelGroup, ecoregionGroup)][
        , ':='(prevMortality = 0, sumB = 0)]
  }
  #bAM <- data.table(speciesEcoregion)[year <= time(sim) & (year > (time(sim)-P(sim)$successionTimestep))]
  bAM <- speciesEcoregion[year <= time]
  bAM <- bAM[year == max(bAM$year)]
  bAM <- bAM[, .(maxMaxB = max(maxB)), by = ecoregionGroup]
  setkey(bAM, ecoregionGroup)
  setkey(bAMterm1, ecoregionGroup)
  bAMterm1 <- bAMterm1[bAM, nomatch = 0]
  bAMterm1[, sumB := pmin((maxMaxB - prevMortality), sumB)]
  bAMterm1[, bAM := sumB/maxMaxB]
  minRelativeB <- data.table(minRelativeB)
  setkey(minRelativeB, ecoregionGroup)
  bAMterm1 <- bAMterm1[minRelativeB, nomatch = 0]
  bAMterm1$bAM <- round(bAMterm1$bAM, 3)
  bAMterm1[, siteShade := cut(bAM, sort(unique(c(0, X1, X2, X3, X4, X5, 1))),
                              labels = FALSE, right = FALSE, include.lowest = TRUE) - 1,
           by = pixelGroup]
  bAMterm1 <- bAMterm1[, .(pixelGroup, siteShade)]
  return(bAMterm1)
}

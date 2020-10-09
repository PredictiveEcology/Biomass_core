adj2 <- function(pixelGroupMapVec, pixelGroupMap, potentialReceivers, numCols, numCells, effDist, maxDist, cellSize,
                 dispersalFn, k, b, successionTimestep, dtSrcShort,
                 speciesSrcRasterVecList, dists, spRcvCommCodesList) {
  # numColsUnits <- numCols * cellSize
  setorderv(potentialReceivers, c("fromInit", "RcvCommunity"))
  cells <- potentialReceivers$fromInit
  # numRowUnits <- numCells / numCols * cellSize
  # types1 <- cbind(x = c(-1, 0, 1, 0), y = c(0, -1, 0, 1)) * cellSize#, spiralDir = rep(c(-1, 1), each = 4))
  # types2 <- cbind(x = rep(types1[, 1], 2), y = rep(types1[, 2], 2), spiralDir = rep(c(-1, 1), each = 4))
  # whType <- sample(4, 1)
  # whDir <- sample(2, 1)
  # type <- types1[whType,, drop = FALSE]
  # ord <- if (whDir == 1) ((whType - 1):(whType - 4) - 1)  %% 4 + 1 else ((whType + 1):(whType + 4) - 1)  %% 4 + 1
  # cell <- matrix(vector(mode = "integer", 10), ncol = 2)
  cellsXY <- xyFromCell(pixelGroupMap, cells)
  # cell[1, ] <- cellsXY[1, ]
  # cell[2, ] <- cellsXY[1, 1:2] + types1[whType, 1:2]

  # keepers <- !((((toCells - 1)%%numCell + 1) != toCells) | ((fromCells%%numCol + toCells%%numCol) == 1))

  # lenStraight <- 0
  # iter <- 2
  # i <- 1
  # seedsArrived <- rep(FALSE, length(speciesSrcRasterVecList))
  # names(seedsArrived) <- names(spRcvCommCodesList)
  # overAllMaxDist <- max(dists[, "maxDist"])
  # underMaxDist <- TRUE
  srcSpeciesCodes <- seq_along(speciesSrcRasterVecList)
  srcSpeciesCodes <- srcSpeciesCodes[!unlist(lapply(speciesSrcRasterVecList, is.null))]
  # sqrt2 <- sqrt(2)
  ymin <- pixelGroupMap@extent@ymin
  xmin <- pixelGroupMap@extent@xmin
  rcvSpeciesByIndex <- speciesCodeFromCommunity(potentialReceivers$RcvCommunity)
  rcvSpeciesCodes <- sort(unique(unlist(rcvSpeciesByIndex)))
  if (!all(rcvSpeciesCodes %in% srcSpeciesCodes)) {
    keep <- unlist(lapply(rcvSpeciesByIndex, function(rsbi) {
      all(rsbi %in% srcSpeciesCodes)
    }))
    rcvSpeciesByIndex <- rcvSpeciesByIndex[keep]
    cellsXY <- cellsXY[keep, , drop = FALSE]
  }
  speciesTableInner <- dists
  speciesTableInner <- unique(speciesTableInner[, c("speciesCode", "effDist", "maxDist")], )
  maxSpCode <- max(speciesTableInner[, "speciesCode"])

  speciesTableInner2 <- lapply(seq_len(maxSpCode), function(ind) {
    hasRow <- (speciesTableInner[, "speciesCode"] %in% ind )
    if (any(hasRow)) {
      speciesTableInner[hasRow,]
    } else {
      as.matrix(t(rep(NA, NCOL(speciesTableInner))))
    }

  })
  speciesTableInner <- do.call(rbind, speciesTableInner2)

  out <- spiralSeedDispersal(cellCoords = cellsXY, #cellsXY[inds,],
                             rcvSpeciesByIndex = rcvSpeciesByIndex, #rcvSpeciesByIndex[inds], #pixel = potentialReceivers$fromInit,
                             # overallMaxDist = overAllMaxDist,
                             speciesTable = speciesTableInner,
                             # speciesNamesNumeric = na.omit(speciesTableInner[, "speciesCode"]),
                             speciesVectorsList = speciesSrcRasterVecList,
                             cellSize = cellSize, numCells = numCells, xmin = xmin,
                             ymin = ymin, numCols = numCols,
                             b = b, k = k, successionTimestep = successionTimestep)
  colNum <- seq(ncol(out))
  names(colNum) <- paste0("spCode", seq(colNum))
  # if (FALSE) {
  #   dev()
  #   clearPlot()
  #   ras <- raster(pixelGroupMap)
  #   pixels <- raster::cellFromXY(ras, cbind(out$x, out$y) * 100 +
  #                                  rep(cbind(ncol(ras), nrow(ras)) / 2
  #                                      * 100, each = NROW(out$x) ))
  #   ras[pixels] <- out$dispersalProbKeep ^ 0.5
  #   rasDist <- raster(pixelGroupMap)
  #   rasDist[pixels] <- out$dis
  #   Plot(ras, rasDist, new=T)
  #
  #   outs <- lapply(colNum, function(col) {
  #     a <- out$seedsArrived[, col]
  #     rasInner <- raster(pixelGroupMap)
  #     rasInner[cells[keep][a]] <- col
  #     rasInner
  #   })
  #   Plot(outs, new  = TRUE)
  # }
  seedsArrivedList <- lapply(colNum, function(col) {
    a <- out[, col]
    cells1 <- cells[keep][a]
  })


  seedsArrived <- data.table(pixelIndex = do.call(c, seedsArrivedList),
                             speciesCode = unlist(lapply(seq(seedsArrivedList),
                                                         function(x) rep(x, length(seedsArrivedList[[x]])))))
  return(seedsArrived)
}

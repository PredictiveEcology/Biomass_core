adj2 <- function(pixelGroupMapVec, pixelGroupMap, potentialReceivers, numCols, numCells, effDist, maxDist, cellSize,
                 dispersalFn, k, b, successionTimestep, dtSrcShort,
                 speciesSrcRasterVecList, dists, spRcvCommCodesList) {
  numColsUnits <- numCols * cellSize
  setorderv(potentialReceivers, c("fromInit", "RcvCommunity"))
  cells <- potentialReceivers$fromInit
  numRowUnits <- numCells / numCols * cellSize
  if (TRUE) {
    types1 <- cbind(x = c(-1, 0, 1, 0), y = c(0, -1, 0, 1)) * cellSize#, spiralDir = rep(c(-1, 1), each = 4))
    # types2 <- cbind(x = rep(types1[, 1], 2), y = rep(types1[, 2], 2), spiralDir = rep(c(-1, 1), each = 4))
    whType <- sample(4, 1)
    whDir <- sample(2, 1)
    # type <- types1[whType,, drop = FALSE]
    ord <- if (whDir == 1) ((whType - 1):(whType - 4) - 1)  %% 4 + 1 else ((whType + 1):(whType + 4) - 1)  %% 4 + 1
    cell <- matrix(vector(mode = "integer", 10), ncol = 2)
    cellsXY <- xyFromCell(pixelGroupMap, cells)
    cell[1, ] <- cellsXY[1, ]
    cell[2, ] <- cellsXY[1, 1:2] + types1[whType, 1:2]

    # keepers <- !((((toCells - 1)%%numCell + 1) != toCells) | ((fromCells%%numCol + toCells%%numCol) == 1))

    # lenStraight <- 0
    iter <- 2
    i <- 1
    seedsArrived <- rep(FALSE, length(speciesSrcRasterVecList))
    names(seedsArrived) <- names(spRcvCommCodesList)
    overAllMaxDist <- max(dists[, "maxDist"])
    underMaxDist <- TRUE
    srcSpeciesCodes <- seq_along(speciesSrcRasterVecList)
    srcSpeciesCodes <- srcSpeciesCodes[!unlist(lapply(speciesSrcRasterVecList, is.null))]
    sqrt2 <- sqrt(2)
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
    # names(aa) <- paste0(as.character(seq(length(aa))), "_")
    # bb <- unlist(aa)
    # cc <- data.table(speciesCode = as.character(bb), index = as.integer(gsub("_.*", "", names(bb))))
    # set(cc, NULL, "pixel", potentialReceivers$fromInit[cc$index])
    # set(cc, NULL, "x", cellsXY[cc$index, "x"])
    # set(cc, NULL, "y", cellsXY[cc$index, "y"])
    # setkeyv(cc, "pixel")
    # dd <- split(cc[, c("pixel", "x", "y")], cc$speciesCode)

    inds <- 400:401
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



    #mb <- microbenchmark::microbenchmark(
    out <- Spiral2(cellCoords = cellsXY, #cellsXY[inds,],
                   rcvSpeciesByIndex = rcvSpeciesByIndex, #rcvSpeciesByIndex[inds], #pixel = potentialReceivers$fromInit,
                   overallMaxDist = overAllMaxDist,
                   speciesTable = speciesTableInner,
                   # speciesNamesNumeric = na.omit(speciesTableInner[, "speciesCode"]),
                   speciesVectorsList = speciesSrcRasterVecList,
                   cellSize = cellSize, numCells = numCells, xmin = xmin,
                   ymin = ymin, numCols = numCols,
                   b = b, k = k, successionTimestep = successionTimestep)
    colNum <- seq(ncol(out$seedsArrived))
    names(colNum) <- paste0("spCode", seq(colNum))
    if (FALSE) {
      dev()
      clearPlot()
      ras <- raster(pixelGroupMap)
      pixels <- raster::cellFromXY(ras, cbind(out$x, out$y) * 100 + rep(cbind(ncol(ras), nrow(ras)) / 2 * 100, each = NROW(out$x) ))
      ras[pixels] <- out$dispersalProbKeep ^ 0.5
      rasDist <- raster(pixelGroupMap)
      rasDist[pixels] <- out$dis
      Plot(ras, rasDist, new=T)

      outs <- lapply(colNum, function(col) {
        a <- out$seedsArrived[, col]
        rasInner <- raster(pixelGroupMap)
        rasInner[cells[keep][a]] <- col
        rasInner
      })
      Plot(outs, new  = TRUE)
    }
    #, times = 5)
    #   print(mb)
    # browser()
    seedsArrivedList <- lapply(colNum, function(col) {
      a <- out$seedsArrived[, col]
      cells1 <- cells[keep][a]
    })


    seedsArrived <- data.table(pixel = do.call(c, seedsArrivedList),
                               speciesCode = unlist(lapply(seq(seedsArrivedList),
                                                           function(x) rep(x, length(seedsArrivedList[[x]])))))
    return(seedsArrived)
    while (any(!seedsArrived) && underMaxDist) {
      for (Ord in ord) {
        if (!underMaxDist) break
        i <- i + 0.5
        j <- trunc(i)
        for (reps in rep(Ord, j)) {
          if (!underMaxDist) break
          # lenStraight <- lenStraight + 1
          if (cell[iter, 1] < 0 || cell[iter, 1] > numColsUnits || cell[iter, 2] > numRowUnits || cell[iter, 2] < 0) {
            # spiral -- overwrite old cell coords as they were off map
            cell[iter, ] <- cell[iter, , drop = FALSE] + types1[Ord,, drop = FALSE]
          } else {
            # pythagorus
            dis <- sqrt((cell[1, 1] - cell[iter, 1]) ^ 2 + (cell[1, 2] - cell[iter, 2]) ^ 2 )
            if (dis > overAllMaxDist * sqrt2) {
              underMaxDist <- FALSE
            } else {
              distsOverMax <- dis > dists[, "maxDist"] * sqrt2
              dim(distsOverMax) <- NULL
              if (any(distsOverMax)) {
                spCodes <- spCodes[!distsOverMax]
                dists <- dists[!distsOverMax,, drop = FALSE]
              }
              # convert coords to pixel so can lookup in vector of values
              pixel <- (numCols - ((cell[iter,2] - ymin + cellSize/2)/cellSize)) * numCols +
                (cell[iter, 1] - xmin + cellSize/2)/cellSize

              # seems faster than lapply
              #spVal <- integer(length(speciesSrcRasterVecList));
              #for (spCode in spCodes) spVal[spCode] <- speciesSrcRasterVecList[[spCode]][pixel]

              spVal <- unlist(lapply(speciesSrcRasterVecList, function(spCode) {
                spCode[pixel]
              }))
              nas <- is.na(spVal)
              if (any(!nas)) {
                browser()
                effDist <- dists[, "effDist"][!nas]
                maxDist <- dists[, "maxDist"][!nas]
                dis <- rep(dis, length(effDist))
                dispersalProb <- eval(dispersalFn)#, envir = environment())
                dispersalProb <- 1 - (1 - dispersalProb) ^ successionTimestep
                seedsArrived[!nas] <- runif(length(dis)) < dispersalProb
                if (any(seedsArrived)) {
                  spCodes <- spCodes[!seedsArrived[names(spCodes)]]
                  dists <- dists[!seedsArrived[names(spCodes)], , drop = FALSE ]
                }
              }

              #dis <- dis[keepers][potentialsWithSeedDT]
              #effDist <- rep.int(effDist, times = length(dis))



              # spiral -- keep old cell coords as they were on map
              cell[iter + 1, ] <- cell[iter, , drop = FALSE] + types1[Ord,, drop = FALSE]
              iter <- iter + 1
              if (NROW(cell) == iter){
                cell <- rbind(cell, matrix(NA, nrow = NROW(cell), ncol = 2))
              }
            }
          }

        }
        #print(cell)
      }
    }
    #while ()

    return(seedsArrived)
  }

  browser()



  numToCells <- 8
  fromCells <- cells
  numCol <- as.integer(numCols)
  numCell <- as.integer(numCells)
  top <- cells - numCol
  lef <- cells - 1L
  rig <- cells + 1L
  bot <- cells + numCol

  topl <- cells - numCol - 1L
  topr <- cells - numCol + 1L
  botl <- cells + numCol - 1L
  botr <- cells + numCol + 1L
  toCells <- c(topl, top, topr, lef, rig, botl, bot, botr)

  # on
  keepers <- !((((toCells - 1)%%numCell + 1) != toCells) | ((fromCells%%numCol + toCells%%numCol) == 1))

  sqrt2 <- sqrt(2)
  dis <- c(sqrt2, 1, sqrt2, 1, 1, sqrt2, 1, sqrt2) * cellSize
  # adj <- cbind(from = fromCells, to = toCells, dis = dis)
  # fromCells <- fromCells[keepers]
  toCells <- toCells[keepers]
  pixelGroupToCells <- pixelGroupMapVec[toCells]
  potentialsWithSeedDT <- pixelGroupToCells %in% dtSrcShort

  if (any(potentialsWithSeedDT)) {
    browser()
    dis <- dis[keepers][potentialsWithSeedDT]
    effDist <- rep.int(effDist, times = length(dis))
    dispersalProb <- eval(dispersalFn)#, envir = environment())
    dispersalProb <- 1 - (1 - dispersalProb) ^ successionTimestep
    any(runif(numToCells) < dispersalProb)
  } else {
    FALSE
  }

  # set(potentials, NULL, "pixelGroup", pixelGroupMap[][potentials$to])
  #setkey(dtSrcShort, speciesCode, pixelGroup)
  #setkey(potentials, speciesCode, pixelGroup)

  #potentialsWithSeedDT <- potentials[dtSrcShort, nomatch = 0]

  #adj <- cbind(from = fromCells, to = toCells, dis = ptDis)

}

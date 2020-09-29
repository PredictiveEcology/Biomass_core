adj2 <- function(pixelGroupMapVec, cells, cellsXY, numCols, numCells, effDist, maxDist, cellSize,
                 dispersalFn, k, b, successionTimestep, dtSrcShort,
                 speciesRasterVecsList, dists, spRcvCommCodesList, xmin, ymin) {
  env1 <- new.env(parent = emptyenv())
  numColsUnits <- numCols * cellSize
  succs <- list()
  numRowUnits <- numCells / numCols * cellSize
  if (TRUE) {
    types1 <- cbind(x = c(-1, 0, 1, 0), y = c(0, -1, 0, 1)) * cellSize#, spiralDir = rep(c(-1, 1), each = 4))
    # types2 <- cbind(x = rep(types1[, 1], 2), y = rep(types1[, 2], 2), spiralDir = rep(c(-1, 1), each = 4))
    whType <- sample(4, 1)
    whDir <- sample(2, 1)
    # type <- types1[whType,, drop = FALSE]
    ord <- if (whDir == 1) ((whType - 1):(whType - 4) - 1)  %% 4 + 1 else ((whType + 1):(whType + 4) - 1)  %% 4 + 1
    cell <- matrix(vector(mode = "integer", 10), ncol = 2)
    # cellsXY <- xyFromCell(pixelGroupMap, cells)
    cellOrig <- cellsXY
    colnames(cellOrig) <- c("xOrig", "yOrig")
    cell2 <- cbind(t(t(cellsXY[, 1:2]) + types1[whType, 1:2]), cellOrig, fromInit = cells, numReceived = 0)

    # keepers <- !((((toCells - 1)%%numCell + 1) != toCells) | ((fromCells%%numCol + toCells%%numCol) == 1))

    lenStraight <- 0
    iter <- 2
    i <- 1
    spCodes <- names(speciesRasterVecsList)
    names(spCodes) <- spCodes
    # seedsArrivedPixelList <- lapply(spCodes, function(x) integer())
    overAllMaxDist <- max(dists[, "maxDist"])
    underMaxDist <- TRUE
    sqrt2 <- sqrt(2)
    #ymin <- pixelGroupMap@extent@ymin
    #xmin <- pixelGroupMap@extent@xmin
    # seedsArrivedAll <- rep(FALSE, NROW(cell2))
    while (underMaxDist) {
      #while (any(!unlist(seedsArrivedPixelList)) && underMaxDist) {
      for (Ord in ord) {
        if (!underMaxDist) break
        i <- i + 0.5
        j <- trunc(i)
        for (reps in rep(Ord, j)) {
          # cell2 <- cell2[!seedsArrivedAll,]
          if (!underMaxDist) break
          lenStraight <- lenStraight + 1
          onMap <- which(cell2[, 1] >= xmin & cell2[, 1] <= numColsUnits & cell2[, 2] <= numRowUnits & cell2[, 2] >= ymin)

          # pythagorus -- all pixel pairs are the same, just need one "distance" calculation -- pick first
          dis <- unname(sqrt((cell2[1, 3] - cell2[1, 1]) ^ 2 + (cell2[1, 4] - cell2[1, 2]) ^ 2 ))
          if (all(dis > overAllMaxDist * sqrt2)) {
            underMaxDist <- FALSE
          } else {
            distsOverMax <- unlist(lapply(dists[, "maxDist"] * sqrt2, function(d) dis > d))
            # dim(distsOverMax) <- NULL
            if (any(unlist(distsOverMax))) {
              spCodes <- spCodes[!distsOverMax]
              dists <- dists[!distsOverMax,, drop = FALSE]
            }
            # convert coords to pixel so can lookup in vector of values
            pixel <- (numCols - ((cell2[,2] - ymin + cellSize/2)/cellSize)) * numCols +
              (cell2[, 1] - xmin + cellSize/2)/cellSize

            pixelOnMap <- pixel[onMap]
            # seems faster than lapply
            #spVal <- integer(length(speciesRasterVecsList));
            #for (spCode in spCodes) spVal[spCode] <- speciesRasterVecsList[[spCode]][pixel]


            # NOW BY SPECIES -- so now List by Species
            spVal <- lapply(speciesRasterVecsList, function(spCode) {
              spCode[pixelOnMap]
            })

            canReceiveList <- lapply(spVal, function(x) onMap[!is.na(x)])
            if (length(!unlist(canReceiveList))) {
              effDist <- as.list(dists[, "effDist"])
              maxDist <- as.list(dists[, "maxDist"])
              # dis <- lapply(canReceiveList, function(canReceiveList) dis)
              envs <- lapply(spVal, function(x) new.env())
              Map(effDist = effDist, maxDist = maxDist, dis = dis, env = envs,
                  function(effDist, maxDist, dis, env) {
                list2env(list(effDist = effDist, maxDist = maxDist, dis = dis), envir = env)
              })
              dispersalProb <- lapply(envs, function(env) eval(dispersalFn, envir = env))
              dispersalProb <- lapply(dispersalProb, function(dp) 1 - (1 - dp) ^ successionTimestep)
              cell3 <- cell2
              cell2 <- cell3
              succ <- mapply(canReceiveInner = canReceiveList, dispersalProbInner = dispersalProb,
                          function(canReceiveInner, dispersalProbInner) {
                N <- length(canReceiveInner)
                canReceiveInner[runif(N) < dispersalProbInner]
              }, SIMPLIFY = FALSE)

              tabu <- tabulate(unlist(succ))
              wh <- which(tabu > 0)
              cell2[wh, "numReceived"] <- tabu[wh]
              # for (ind in seq(length(succ)))
              #   cell2[succ[[ind]], "numReceived"] <- cell2[succ[[ind]], "numReceived"] + 1
              # seedsArrivedCur <- Map(dis = dis, canReceiveInner = canReceiveList, dispersalProb = dispersalProb,
              #                           function(dis, canReceiveInner, dispersalProb) {
              #                             N <- length(canReceiveInner)
              #                             success <- runif(N) < dispersalProb
              #                             cell2[onMap,][canReceiveInner, "numReceived"] <<- cell2[onMap,][canReceiveInner, "numReceived"] + success
              #
              #                             list(success = success, pixelSuccess = cell2[canReceiveInner,][success, "fromInit"])
              #                      })

              #for (ind in 1:2) {
              #  seedsArrivedPixelList[[ind]] <- append(seedsArrivedPixelList[[ind]], succ[[ind]])
              #succs[[iter]] <- succ
              # assign(paste("succ", iter), value = succ, envir = env1)
              # seedsArrivedPixelList <- c(seedsArrivedPixelList, succ)
              #}
              #seedsArrivedPixelList <- Map(sac = seedsArrivedCur, sa = seedsArrivedPixelList,
              #                    function(sa, sac) {
              #                      sa[sac$pixelSuccess] <- TRUE
              #                      sa})
              cell2 <- cell2[cell2[, "numReceived"] != length(spVal),]
              #whSeedsArrived <- lapply(seedsArrived, row)
              #if (any(length(whSeedsArrived))) {
              #  browser()

                #### STOPPED HERE CONVERTING TO VECTORIZED
                # spCodes <- spCodes[!seedsArrived[names(spCodes)]]
                # dists <- dists[!seedsArrived[names(spCodes)], , drop = FALSE ]
              #}
            }

            #dis <- dis[keepers][potentialsWithSeedDT]
            #effDist <- rep.int(effDist, times = length(dis))



            # spiral -- keep old cell coords as they were on map
            cell2[, 1] <- cell2[, 1] + types1[Ord, 1]
            cell2[, 2] <- cell2[, 2] + types1[Ord, 2]
            iter <- iter + 1
            # if (NROW(cell) == iter){
            #   cell <- rbind(cell, matrix(NA, nrow = NROW(cell), ncol = 2))
            # }
          }


        }
       #print(cell)
      }
    }
    #while ()

    return(succs)# seedsArrivedPixelList)
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


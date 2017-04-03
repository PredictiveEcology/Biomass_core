
##############################################################
#' Simulate a seedDispRcv process on a landscape.
#'
#' Simulate seed dispersal using user defined function. This is a "receiving pixel" focused dispersal approach.
#' It is the "potentially receiving" cell that looks around itself for potential seed sources. If it finds
#' a single seed source, that passes the probability function described by the dispersalFn, then the
#' cluster ends and the receiving cell index is returned as part of a vector of indices of all
#' successfully cells that received seeds. This function can therefore only be used for a relatively
#' specific situation where there is a yes/no returned for each potential receiving cell, i.e., not abundance.
#' This function is also not cumulative, i.e,. there is no higher abundance of seeds received if
#' a receiving cell has lots of seed sources around it vs. a single seed source. The difference will
#' come with a higher probability of successfully receiving a "seed".
#'
#' \code{dispersalFn} must be an expression that returns a probability distribution. Because
#' it is a dispersal kernal, it must be a probability distribution. The expression that can
#' take an argument named "dis" (without quotes) as this will be calculated internally and
#' represents the distance from the initial (receiving) pixel and all active pixels within that
#' cluster of active pixels. \code{SpaDES} includes the \code{\link{Ward}} kernel as defined in the
#' Landis documentation.
#'
#' @param seedSrc  A \code{RasterLayer} object where pixels indicate the presence (or abundance) of seed source
#' pixels
#'
#' @param seedRcv  A \code{RasterLayer} object where pixels indicate the potential pixels to receive seeds
#'
#' @param dispersalFn  An expression that can take a "dis" argument. See details. Default is "Ward"
#'
#' @param plot.it  If TRUE, then plot the raster at every iteraction, so one can watch the
#' seedDispRcv event grow.
#' @param effDist Landis species- and ecoregion-specific effective distance parameter
#'
#' @param maxDist  Landis species- and ecoregion-specific effective distance parameter
#'
#' @param b  Landis ward seed dispersal calibration coefficient (set to 0.01 in Landis)
#'
#' @param k  Landis ward seed dispersal the probability that seed will disperse within
#' the effective distance (eg., 0.95)
#'
#' @param ...   Additional parameters. Currently none
#'
#' @return A numeric vector of raster pixel indices, in the same resolution and extent as
#' \code{seedSrc} raster.
#'
#' @import data.table
#' @import raster
#' @import dplyr
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name seedDispRcv
#' @aliases seedDispRcv
#' @rdname seedDispRcv
setGeneric("seedDispRcv", function(seedSrc, seedRcv=seedSrc,
                                   dispersalFn=Ward,
                                   effDist=100, maxDist=150, b=0.01, k=0.95,
                                   plot.it=FALSE, ...) {
  standardGeneric("seedDispRcv")
})

#' @rdname seedDispRcv
#' @examples
#' library(raster)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e4,0,1e4),res=100)
#' hab <- gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab)="hab"
#'
#' seedSrc <- hab>5
#' setColors(seedSrc,1) <- c("white","black")
#'
#' seedRcv <- hab>5
#' system.time(seeds <- seedDispRcv(seedSrc, seedRcv=seedRcv,
#'   maxDist=250, plot.it=TRUE))
#' seedRcvRaster <- raster(seedSrc)
#' if(length(seeds)>0) {
#'   seedRcvRaster[seeds] <- 1
#'   Plot(seedRcvRaster, cols="black")
#' }
seedDispRcv <- function(seedSrc, seedRcv, dispersalFn,
                        effDist, maxDist, b, k,
                        plot.it=FALSE, ...) {
  cellSize <- unique(res(seedSrc))
  
  seedSrcVec <- getValues(seedSrc)
  seedRcv <- Which(seedRcv>0, cells=TRUE)
  if(length(cellSize)>1) stop("seedSrc resolution must be same in x and y dimension")
  ### should sanity check map extents
  if (is.null(seedRcv))  {
    # start it in the centre cell
    seedRcv <- (nrow(seedSrc)/2L + 0.5) * ncol(seedSrc)
  }
  lociReturn <- data.table(fromInit=seedRcv,key="fromInit")
  seedsArrived <- data.table(fromInit=numeric(),key="fromInit")
  
  if(plot.it) {
    wardSeedDispersalHab1 <- raster(seedSrc)
    wardSeedDispersalHab1[] <- NA
    assignGlobal("wardSeedDispersalHab1",wardSeedDispersalHab1)
    Plot(seedSrc, new=TRUE)
  }
  
  n <- cellSize
  
  potentials <- data.table("fromInit"=seedRcv,key="fromInit")
  potentials[,from:=fromInit]
  setkey(potentials,"from", "fromInit")
  
  while (length(seedRcv) & ((n-cellSize)<=maxDist)) { # while there are active cells and less than maxDistance
    
    # identify neighbours
    adjCells <- adj(seedSrc, seedRcv, directions=8, pairs=TRUE) %>%
      data.table(key="from")
    if(n>cellSize) {
      # replace "from" column with the values from the previous "to" column
      potentials[,`:=`(from=NULL,dis=NULL)][,from:=to][,to:=NULL]
      setkey(potentials,"from", "fromInit")
    }
    potentials <- potentials[adjCells, allow.cartesian=TRUE]
    
    if(plot.it) {
      wardSeedDispersalHab1[potentials[,from]] <- n
      assignGlobal("wardSeedDispersalHab1",wardSeedDispersalHab1)
      Plot(wardSeedDispersalHab1, addTo="seedSrc")
    }
    
    
    # Section - omit cells based on one of three criteria
    # 1. Can't spread backwards, within a cluster
    # keep only neighbours that have not been seedDispRcv to yet, within each
    # cluster. This means that a cell can't seedDispRcv backwards, but two different
    # clusters can be on the same cell
    potentials <- potentials[from!=to,.SD,by="fromInit"]
    
    # 2. Can't have more than one "arrival" in a potential "to" cell, by cluster
    # Don't know how to do next within data.table syntax - remove duplicate "to"
    #  within a cluster
    #  unique(., by=c("fromInit", "to"))
    
    potentials  <- potentials %>%
      group_by(fromInit) %>%
      filter(!duplicated(to)) %>%
      data.table
    
    # 3. remove any that are not within a 1 unit doughnut of
    # discard those that more than "n" units from a "from" cell. This keeps spreading
    #   in a circle. It is somewhat wasteful, because the distances are calculated above
    #   and then deleted here, but this may be the most efficient way
    nr <- NROW(potentials)
    
    xys <- xyFromCell(seedSrc, as.matrix(potentials[,list(fromInit,to)]))
    potentials[,dis:=pointDistance(xys[1:nr,], xys[(nr+1):(2*nr),], lonlat=FALSE)]
    potentials <- potentials[((n-cellSize) < dis) & (dis <= min(n,maxDist)),]
    #potentials <- potentials[abs(dis - n)<=min((n-cellSize),(maxDist-cellSize)),]
    
    # for speeding up. If no pixels within the doughnut are a seed source,
    #  just skip next block
    potentialsWithSeed <- as.logical(seedSrcVec[potentials[,to]])
    if(any(potentialsWithSeed)) {
      
      potentialsWithSeedDT  <- potentials[potentialsWithSeed,]
      nr <- NROW(potentialsWithSeedDT)
      setkey(potentialsWithSeedDT, "fromInit")
      
      potentialsWithSeedDT[,receivesSeeds:=runif(nr)<eval(dispersalFn)]
      receivedSeeds <- potentialsWithSeedDT[,any(receivesSeeds), by="fromInit"]
      
      #drop any that received seeds from potentials, as they are now in lociReturn
      if(NROW(receivedSeeds[V1==TRUE])>0) {
        seedsArrived <- rbindlist(list(seedsArrived,lociReturn[receivedSeeds[V1==TRUE]][,V1:=NULL]))
        setkey(seedsArrived, "fromInit")
        setkey(potentials, "fromInit")
        potentials <- potentials[!seedsArrived]
      }
    }
    
    n <- n+cellSize
    
    # refresh so that "to" cells become new "from" cells
    seedRcv <- potentials[,to]
    
  }
  return(seedsArrived$fromInit)
  
}


##############################################################
#' Ward Seed Dispersal kernel
#'
#' A probability distribution used in Landis.
#'
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name Ward
#' @rdname Ward
Ward <- expression(if(cellSize<=effDist) {
  ifelse(dis<=effDist,
         exp((dis-cellSize)*log(1-k)/effDist)-
           exp(dis*log(1-k)/effDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
} else {
  ifelse(dis<=cellSize,
         exp((dis-cellSize)*log(1-k)/effDist)-(1-k)*
           exp((dis-effDist)*log(b)/maxDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
})



##############################################################
#' WardFast Seed Dispersal kernel
#'
#' A probability distribution used in Landis.
#'
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name WardFast
#' @rdname WardFast
WardFast <- expression(ifelse(cellSize<=effDist, {
  ifelse(dis<=effDist,
         exp((dis-cellSize)*log(1-k)/effDist)-
           exp(dis*log(1-k)/effDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
} , {
  ifelse(dis<=cellSize,
         exp((dis-cellSize)*log(1-k)/effDist)-(1-k)*
           exp((dis-effDist)*log(b)/maxDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
}))



##############################################################
#' Simulate a LANDISDisp process on a landscape.
#'
#' Simulate seed dispersal using user defined function. This is a "receiving pixel" focused dispersal approach.
#' It is the "potentially receiving" cell that looks around itself for potential seed sources. If it finds
#' a single seed source, that passes the probability function described by the dispersalFn, then the
#' cluster ends and the receiving cell index is returned as part of a vector of indices of all
#' successfully cells that received seeds. This function can therefore only be used for a relatively
#' specific situation where there is a yes/no returned for each potential receiving cell, i.e., not abundance.
#' This function is also not cumulative, i.e,. there is no higher abundance of seeds received if
#' a receiving cell has lots of seed sources around it vs. a single seed source. The difference will
#' come with a higher probability of successfully receiving a "seed".
#'
#' \code{dispersalFn} must be an expression that returns a probability distribution. Because
#' it is a dispersal kernal, it must be a probability distribution. The expression that can
#' take an argument named "dis" (without quotes) as this will be calculated internally and
#' represents the distance from the initial (receiving) pixel and all active pixels within that
#' cluster of active pixels. \code{SpaDES} includes the \code{\link{Ward}} kernel as defined in the
#' Landis documentation.
#'
#' @param sim A simList object
#'
#' @param dtSrc data.table
#' 
#' @param dtRcv data.table
#'
#' @param pixelGroupMap map
#' 
#' @param species Landis object from initial species file
#'
#' @param dispersalFn  An expression that can take a "dis" argument. See details. Default is "Ward"
#'
#' @param plot.it  If TRUE, then plot the raster at every iteraction, so one can watch the
#' LANDISDisp event grow.
#' @param effDist Landis species- and ecoregion-specific effective distance parameter
#'
#' @param maxDist  Landis species- and ecoregion-specific effective distance parameter
#'
#' @param b  Landis ward seed dispersal calibration coefficient (set to 0.01 in Landis)
#'
#' @param k  Landis ward seed dispersal the probability that seed will disperse within
#' the effective distance (eg., 0.95)
#' 
#' @param maxPotentialsLength numeric, number of unique pixels to treat simultaneously. Smaller reduces memory use.
#' 
#' @param verbose Logical. Whether a somewhat verbose output to screen occurs. For debugging.
#' 
#' @param useParallel any. if the class of this arguement is logical, whether the function creates the cluster based on TRue and FALSE. 
#'                         if the class of this arguement is cluster, the function will use this cluster to implement the parallel computation.
#'
#' @param ...   Additional parameters. Currently none
#'
#' @return A numeric vector of raster pixel indices, in the same resolution and extent as
#' \code{seedSrc} raster.
#'
#' @import data.table
#' @import raster
#' @import dplyr
#' @importFrom R.utils intToBin
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name LANDISDisp
#' @aliases LANDISDisp
#' @rdname LANDISDisp
#' library(raster)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e4,0,1e4),res=100)
#' hab <- gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab)="hab"
#'
#' seedSrc <- hab>5
#' setColors(seedSrc,1) <- c("white","black")
#'
#' seedRcv <- hab>5
#' system.time(seeds <- LANDISDisp(seedSrc, seedRcv=seedRcv,
#'   maxDist=250, plot.it=TRUE))
#' seedRcvRaster <- raster(seedSrc)
#' if(length(seeds)>0) {
#'   seedRcvRaster[seeds] <- 1
#'   Plot(seedRcvRaster, cols="black")
#' }
LANDISDisp <- function(sim, dtSrc, dtRcv, pixelGroupMap, 
                       species,
                       dispersalFn=WardFast, 
                       b=0.01, k=0.95,
                       plot.it=FALSE, 
                       maxPotentialsLength=1e3, 
                       verbose=FALSE,
                       useParallel, ...) {
  
  cellSize=unique(res(pixelGroupMap))
  pixelGroupMapVec <- getValues(pixelGroupMap)
  seedsReceived <- raster(pixelGroupMap) 
  seedsReceived[] <- 0L
  sc <- species %>%
    dplyr::select(speciesCode, seeddistance_eff, seeddistance_max) %>%
    rename(effDist=seeddistance_eff, maxDist=seeddistance_max) %>%
    data.table
  
  setkey(sc, speciesCode)
  setkey(dtSrc, speciesCode)
  setkey(dtRcv, speciesCode)
  
  speciesSrcPool <- sc[dtSrc] %>%
    group_by(pixelGroup) %>%
    summarise(speciesSrcPool=sum(2^speciesCode))
  
  speciesRcvPool <- sc[dtRcv] %>%
    group_by(pixelGroup) %>%
    summarise(speciesRcvPool=sum(2^speciesCode))
  
  setkey(sc, speciesCode)
  
  seedSourceMap <- rasterizeReduced(speciesSrcPool, fullRaster = pixelGroupMap, mapcode = "pixelGroup", plotCol="speciesSrcPool")
  seedReceiveMap <- rasterizeReduced(speciesRcvPool, fullRaster = pixelGroupMap, mapcode = "pixelGroup", plotCol="speciesRcvPool")
  seedSourceMap[] <- as.integer(seedSourceMap[])
  seedReceiveMap[] <- as.integer(seedReceiveMap[])
  
  seedRcvOrig <- Which(!is.na(seedReceiveMap), cells=TRUE)
  seedSrcOrig <- Which(seedSourceMap,cells=TRUE)
  
  xysAll <- xyFromCell(seedSourceMap, 1:ncell(seedSourceMap))
  
  if (is.null(seedRcvOrig))  {
    # start it in the centre cell
    activeCell <- (nrow(seedSrcOrig)/2L + 0.5) * ncol(seedSrcOrig)
  }
  #seedSrcVec <- getValues(seedSourceMap)
  if(length(cellSize)>1) stop("pixelGroupMap resolution must be same in x and y dimension")
  ### should sanity check map extents
  if(plot.it) {
    wardSeedDispersalHab1 <- raster(seedSourceMap)
    wardSeedDispersalHab1[] <- NA
    Plot(seedSourceMap, new=TRUE)
  }
  
  
  lociReturn <- data.table(fromInit=seedRcvOrig,key="fromInit")
  
  
  #            scFull <- sc[speciesComm(getValues(seedSourceMap))] %>%
  #              setkey(.,pixelIndex)
  potentialsOrig <- data.table("fromInit"=seedRcvOrig,"RcvCommunity"=seedReceiveMap[][seedRcvOrig],
                               key="fromInit")
  potentialsOrig[,from:=fromInit]
  
  #maxPotentialsLength=3e3
  nPotentials <- length(seedRcvOrig)
  
  if(nPotentials>maxPotentialsLength) {
    subSamp <- ceiling(nPotentials / maxPotentialsLength)
  } else {
    subSamp <- 1
  }
  ultimateMaxDist <- max(dtRcv$seeddistance_max)
  splitFactor <- sort(rep(seq_len(subSamp), length.out = nPotentials))
  subSampList <- purrr::transpose(list(activeCell=split(seedRcvOrig, splitFactor), 
                                       potentials=split(potentialsOrig, splitFactor)))
  
  if(is(useParallel, "logical")){
    if(useParallel){
      numCores <- min(length(subSampList), parallel::detectCores()-1)
      if(Sys.info()[["sysname"]] == "Windows"){
        cl <- parallel::makeCluster(numCores)
        allarguementsInMainFunc <- formalArgs(seedDispInnerFn)
        parallel::clusterExport(cl, c("seedDispInnerFn", 
                                      "data.table", "%>%", "setkey", "adj",
                                      "%<=%", "set", "rbindlist", "%<<%",
                                      allarguementsInMainFunc[!(allarguementsInMainFunc %in% c("activeCell", "potentials", "n"))]),
                                envir = environment())
      } else {
        cl <- parallel::makeForkCluster(numCores)
      }
      message("Running seed dispersal in parallel on ", length(cl), " clusters")
      seedsArrived <- parallel::parLapply(cl, subSampList,
                                             function(y) seedDispInnerFn(activeCell = y[[1]],
                                                                         potentials = y[[2]], 
                                                                         n = cellSize,
                                                                         speciesRcvPool, sc,
                                                                         seedReceiveMap, ultimateMaxDist, 
                                                                         cellSize, xysAll, 
                                                                         dtSrc, pixelGroupMapVec,
                                                                         dispersalFn,
                                                                         k, b, lociReturn, 
                                                                         speciesComm, 
                                                                         pointDistance)) %>%
        rbindlist()
      parallel::stopCluster(cl)
      #seedsArrived <- rbindlist(allSeedsArrived)
    } else {
      allSeedsArrived <- list()
      for(y in seq_along(subSampList)) {
        allSeedsArrived[[y]] <- seedDispInnerFn(activeCell = subSampList[[y]][[1]],
                                                potentials = subSampList[[y]][[2]], 
                                                n = cellSize,
                                                speciesRcvPool, sc,
                                                seedReceiveMap, ultimateMaxDist, 
                                                cellSize, xysAll, 
                                                dtSrc, pixelGroupMapVec,
                                                dispersalFn,
                                                k, b, lociReturn, 
                                                speciesComm, 
                                                pointDistance)
      }
      seedsArrived <- rbindlist(allSeedsArrived)
      
      # seedsArrived <- data.table(fromInit=numeric(),speciesCode=integer(),
      #                            key=c("fromInit","speciesCode"))
      # for(spatialSubset in 2:length(subSamp)) {
      #   n <- cellSize
      #   # do the spatial subsetting -> potentials becomes the subset only
      #   activeCell <- seedRcvOrig[(subSamp[spatialSubset-1]+1):(subSamp[spatialSubset])]
      #   potentials <- potentialsOrig[(subSamp[spatialSubset-1]+1):(subSamp[spatialSubset])] 
      #   # Go to species level
      #   
      #   spRcvCommCodes <- speciesComm(unique(speciesRcvPool$speciesRcvPool), sc=sc)
      #   #spRcvCommCodes <- speciesComm(unique(potentials$RcvCommunity))
      #   setkey(spRcvCommCodes, RcvCommunity)
      #   setkey(potentials, RcvCommunity)
      #   
      #   # Make potentials have all Rcv pixels, with each species as unique line
      #   potentials = spRcvCommCodes[potentials, allow.cartesian=TRUE][,`:=`(RcvCommunity=NULL)]
      #   setkey(potentials,"from")
      #   # identify which are 8 neighbours from each "active cell"
      #   adjCells <- adj(seedReceiveMap, unique(activeCell), directions=8, pairs=TRUE,include=TRUE) %>%
      #     data.table(key=c("from"))
      #   while (NROW(potentials) & ((n-cellSize)%<=%ultimateMaxDist)) { # while there are active cells and less than maxDistance
      #     #                  browser(expr=round(time(sim))>=20)
      #     # If this is second or greater time through this while loop, make active cells (i.e., the "from")
      #     #  be the previous round's "to" column. Also, delete "to" and "dis" columns
      #     #                  browser()
      #     if(n>cellSize) {
      #       potentials[,`:=`(from=NULL)][,from:=to][,`:=`(to=NULL,dis=NULL)]
      #       setkey(potentials,"from")
      #     }
      #     ################ original
      #     # join these to the potentials object
      #     potentials <- potentials[adjCells, allow.cartesian=TRUE] %>% 
      #       unique(by=c("fromInit", "to", "speciesCode"))
      #     potentials <- potentials[!is.na(fromInit),]
      #     
      #     # because there will be duplicate "from - to" pairs, say from 2 different species, only calculate
      #     #   distance once, then re-join the shorter version back to longer version by species
      #     shortPotentials <- setkey(potentials, fromInit, to) %>% unique(., by = c("fromInit", "to")) %>% .[,list(fromInit, to)] 
      #     set(shortPotentials, , "dis", pointDistance(xysAll[shortPotentials[,fromInit],], xysAll[shortPotentials[,to],], 
      #                                                 lonlat=FALSE)) 
      #     
      #     # merge shorter object with no duplicate from-to pairs back with potentials, which does have duplicate from-to pairs
      #     #   due to multiple species having same from-to pair
      #     if(n-cellSize==0){ # the first loop incudes on site regeneration
      #       potentials <- shortPotentials[((n-cellSize) %<=% dis) & (dis %<=% n),][
      #         potentials, nomatch=0][
      #           (dis %<=% maxDist),]
      #     } else {
      #       potentials <- shortPotentials[((n-cellSize) %<<% dis) & (dis %<=% n),][
      #         potentials, nomatch=0][
      #           (dis %<=% maxDist),]
      #     }
      #     
      #     if(verbose) print(paste0("SimTime=",time(sim),", dist=",n,", spatialSubset=",spatialSubset-1,", nrow potentials=",NROW(potentials),
      #                              ", numSpec=",length(unique(potentials$speciesCode)),", 1st spec=",unique(potentials$speciesCode)[1]))
      #     
      #     
      #     if(plot.it) {
      #       wardSeedDispersalHab1[potentials[,from]] <- 
      #         n + rnorm(NROW(potentials),0,0.001)
      #       Plot(wardSeedDispersalHab1, axes=T,new=T)#, zoomExtent = extent(0,300,0,300))
      #     }
      #     if(NROW(potentials)>0) {
      #       dtSrcShort <- dtSrc[,list(pixelGroup,speciesCode)] 
      #       set(potentials,,"pixelGroup",pixelGroupMapVec[potentials[,to]])
      #       setkey(dtSrcShort, speciesCode, pixelGroup)
      #       setkey(potentials, speciesCode, pixelGroup)
      #       
      #       
      #       potentialsWithSeedDT <- potentials[dtSrcShort, nomatch=0]
      #       if(NROW(potentialsWithSeedDT)>0) {
      #         #                     cat("\n Year:",round(time(sim)),"\n")
      #         #                     dd <- potentialsWithSeedDT[,.(NumberofCell=length(from)),by=c("dis","to")]
      #         #                     print(dd)
      #         #potentialsWithSeedDT  <- potentials[potentialsWithSeed,]
      #         #setkey(potentialsWithSeedDT, "fromInit")
      #         
      #         
      #         set(potentialsWithSeedDT,,"receivesSeeds",NA)
      #         nr <- NROW(potentialsWithSeedDT)
      #         
      #         
      #         # back to Ward
      #         # Don't include the ones that were already, calculate probability
      #         potentialsWithSeedDT[is.na(receivesSeeds) & dis==0,
      #                              dispersalProb:=1]
      #         potentialsWithSeedDT[is.na(receivesSeeds) & dis!=0,
      #                              dispersalProb:=eval(dispersalFn)]
      #         potentialsWithSeedDT <- potentialsWithSeedDT[,.(receivesSeeds=runif(nr)<dispersalProb,
      #                                                         fromInit,speciesCode)]
      #         receivedSeeds <- potentialsWithSeedDT[,any(receivesSeeds), by=c("fromInit,speciesCode")]
      #         setkey(receivedSeeds, fromInit, speciesCode)
      #         
      #         #drop any that received seeds from potentials, as they are now in lociReturn
      #         if(NROW(receivedSeeds[V1==TRUE])>0)  {
      #           seedsArrived <- rbindlist(list(seedsArrived,
      #                                          lociReturn[receivedSeeds[V1==TRUE]][,V1:=NULL]))
      #           setkey(seedsArrived, fromInit, speciesCode)
      #           setkey(potentials, fromInit, speciesCode)
      #           potentials <- potentials[!receivedSeeds[V1==TRUE]]
      #           #potentials <- potentials[!receivedSeeds[V1==TRUE]]
      #         }
      #         #potentials <- specPotentials[,list(RcvCommunity=sum(2^speciesCode)), by=c("fromInit","from","to")]
      #         
      #       }
      #       plot.it2  <-  FALSE
      #       if(plot.it2) {
      #         
      #         seedsReceived[seedsArrived[species=="querrubr",fromInit]] <- 
      #           ifelse(as.numeric(seedsReceived[seedsArrived[species=="querrubr",fromInit]]==0), 
      #                  n,
      #                  seedsReceived[seedsArrived[species=="querrubr",fromInit]])
      #         assignGlobal("seedsReceived")
      #         #seedsReceived[seedReceive[species==unique(seedReceive$species)[i],pixelIndex]] <- 1; 
      #         Plot(seedsReceived, cols=c("red","blue"), na.color = "white", zero.color="white", new=TRUE);
      #         
      #         activeCells <- raster(seedsReceived)
      #         activeCells[] <- NA
      #         pots <- potentials %>% filter(species=="querrubr") %>% group_by(to) %>% summarise(len=.N)
      #         activeCells[pots[,to]] <- pots[,len]
      #         assignGlobal("activeCells")
      #         Plot(activeCells)
      #         
      #         potSeedsReceived <- pixelGroupMap %in% dtRcv[species=="querrubr",pixelGroup]
      #         assignGlobal("potSeedsReceived")
      #         #Plot(potSeedsReceived, cols="orange", na.color = "white", zero.color="white");
      #         
      #         seedsSource <- pixelGroupMap %in% dtSrc[species=="querrubr",pixelGroup]
      #         seedsSource[seedsSource==0] <- NA
      #         assignGlobal("seedsSource")
      #         #Plot(seedsSource, cols="orange", na.color = "white", zero.color="white");
      #         
      #         disSeedSourceMap <- round(distance(seedsSource, doEdge=FALSE),0)
      #         assignGlobal("disSeedSourceMap")
      #         #Plot(disSeedSourceMap)
      #         
      #         disPotSeedsReceived <- potSeedsReceived * disSeedSourceMap
      #         assignGlobal("disPotSeedsReceived")
      #         #Plot(disPotSeedsReceived)
      #         
      #         print(unique(disPotSeedsReceived[seedsReceived==n]))
      #         print(unique(seedsReceived[disPotSeedsReceived==n]))
      #         
      #         
      #         #i=i+1; 
      #         #print(unique(seedingData$species)[i-1])
      #         #potSeedsReceived <- pixelGroupMap %in% potentials[species=="querrubr",pixelGroup]
      #       }
      #     }
      #     n <- n+cellSize
      #     # refresh so that "to" cells become new "from" cells
      #     activeCell <- potentials[,to]
      #   }
      # }
    }
  } else if (is(useParallel, "cluster")){
    if(Sys.info()[["sysname"]] == "Windows"){
      allarguementsInMainFunc <- formalArgs(seedDispInnerFn)
      parallel::clusterExport(useParallel, c("seedDispInnerFn", 
                                             "data.table", "%>%", "setkey", "adj",
                                             "%<=%", "set", "rbindlist", "%<<%",
                                             allarguementsInMainFunc[!(allarguementsInMainFunc %in% c("activeCell", "potentials", "n"))]),
                              envir = environment())
    }
    message("Running seed dispersal in parallel on ", length(useParallel), " clusters")
    seedsArrived <- parallel::parLapply(useParallel, subSampList,
                                        function(y) seedDispInnerFn(activeCell = y[[1]],
                                                                    potentials = y[[2]], 
                                                                    n = cellSize,
                                                                    speciesRcvPool, sc,
                                                                    seedReceiveMap, ultimateMaxDist, 
                                                                    cellSize, xysAll, 
                                                                    dtSrc, pixelGroupMapVec,
                                                                    dispersalFn,
                                                                    k, b, lociReturn, 
                                                                    speciesComm, 
                                                                    pointDistance)) %>%
      rbindlist()
    #seedsArrived <- rbindlist(allSeedsArrived)
  } else {
    stop("Please specify the useParallel arguement correctly. Currently, it takes either logical or cluster object")
  }
  # potentialsOrig <- copy(potentials)
  # potentials <- copy(potentialsOrig)
  setnames(seedsArrived,"fromInit","pixelIndex") 
  return(seedsArrived)
  
}


speciesComm <- function(num, sc){
  indices <- lapply(strsplit(R.utils::intToBin(num), split=""),
                    function(x) rev(as.logical(as.numeric(x))))
  
  speciesCode <- lapply(indices, function(x) (seq_len(length(x))-1)[x])
  data.table(RcvCommunity=as.integer(rep(num, sapply(speciesCode,length))),
             speciesCode=unlist(speciesCode),key="speciesCode")[!is.na(speciesCode)] %>%
    sc[.]
  
}

ringCells <- function(x, index, minDist, maxDist) {
  ras <- raster(x)
  ras[index] <- 1
  xOuter <- buffer(ras, width=maxDist)
  xInner <- buffer(ras, width=minDist)
  xOuter[xInner==1] <- NA
  return(Which(xOuter==1, cells=TRUE))
  
  #  xOuter[xInner==1] <- NA
  #  return(xOuter)
}
# 
# 
# ringWeight <- function(x, minDist, maxDist) {
#   b = focalWeight(x, minDist, "circle")
#   dis = focalWeight(x, maxDist, "circle")
#   colsRmv <- (ncol(dis)-ncol(b))/2
#   indices <- (1:ncol(dis))[-c((1:colsRmv),(ncol(dis)-colsRmv+1))]
#   keep <- expand.grid(indices, indices) %>% as.matrix
#   dis[keep] <- pmax(0,dis[keep]-b)
#   
#   aRas <- raster(dis, xmn=0, ymn=0, xmx=ncol(dis)*res(x)[1], ymx=ncol(dis)*res(x)[1])
#   
#   p1 <- xyFromCell(aRas,ceiling(ncell(dis)/2))
#   p2 <- xyFromCell(aRas,Which(aRas>0,cell=TRUE))
#   dis[dis>0] <- pointDistance(p1,p2,lonlat=FALSE)
#   dis[dis==0] <- NA
#   return(dis)
# }
# 
WardEqn <- function(dis, cellSize, effDist, maxDist, k, b) {
  if(cellSize%<=%effDist) {
    ifelse(dis%<=%effDist,
           exp((dis-cellSize)*log(1-k)/effDist)-
             exp(dis*log(1-k)/effDist),
           (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
             (1-k)*exp((dis-effDist)*log(b)/maxDist))
  } else {
    ifelse(dis%<=%cellSize,
           exp((dis-cellSize)*log(1-k)/effDist)-(1-k)*
             exp((dis-effDist)*log(b)/maxDist),
           (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
             (1-k)*exp((dis-effDist)*log(b)/maxDist))
  }
}
# 
# WardFn <- function(dis, maxDist=3000, effDist=30, ...) {
#   #effDist = 100
#   #maxDist = 150
#   dis[dis==0] <- NA
#   dis <- as.numeric( na.omit(dis))
#   cellSize = 100
#   k = 0.95
#   b = 0.01
#   nr <- length(dis)
#   if(nr>0) {
#     e = runif(nr)<WardEqn(dis=dis, cellSize=100, effDist=effDist, maxDist=maxDist, k=0.95, b=0.01)
#   } else {
#     e <- FALSE
#   }
#   return(any(e))
# }
# 
# 
# #effDist=species[species==speciesCode,seeddistance_eff]
# 
# #seedReceiveMap[seedReceiveMap==0] <- NA
# 
# seedReceiveMap = pixelGroupMap %in% seedReceive[species==speciesCode]$pixelGroup
# seedSourceMap[is.na(seedSourceMap)] <- 0
# Plot(seedSourceMap, cols=c("white","light grey"), new=TRUE)
# 
# seedReceiveMap2 <- raster(seedReceiveMap)
# seedReceiveMap2[] <- 0
# 
# st <- system.time({
# for(i in 1:30*100) {
#   a  <-  focal(seedSourceMap, w=ringWeight(seedSourceMap,i-100,i), fun=WardFn, pad=TRUE, padValue=0)
#   a[seedReceiveMap==0] <- 0
# #  Plot(a)
#   seedReceiveMap <- seedReceiveMap*(a==0)
#   seedReceiveMap2[a==1] <- seedReceiveMap2[a==1] + i
# }
# })
# 
# Plot(seedReceiveMap2)
# seedReceiveMap = pixelGroupMap %in% seedReceive[species==speciesCode]$pixelGroup
# Plot(seedReceiveMap2, addTo="seedSourceMap", zero.color="#00000000")
# Plot(seedReceiveMap2, seedReceiveMap)
# Plot(seedReceiveMap2, new=T)
# Plot(seedSourceMap, addTo="seedReceiveMap2", zero.color="#00000000", cols="#FFFFFF11")
# 
# Plot(seedSourceMap, new=T, cols=c("white","black"))
# Plot(seedReceiveMap2, addTo="seedSourceMap", zero.color="#00000000", cols="#11FF1155")
# Plot(seedReceiveMap, cols=c("white","black"))
# Plot(seedReceiveMap2, addTo="seedReceiveMap", zero.color="#00000000", cols="#11881100")
# 
# h <- raster(seedReceiveMap)
# h[]=0
# h[seedingData[species=="querrubr",pixelIndex]] <- 1
# Plot(h)

seedDispInnerFn <- function(activeCell, potentials, n, 
                            speciesRcvPool, sc,
                            seedReceiveMap, ultimateMaxDist, 
                            cellSize, xysAll, 
                            dtSrc, pixelGroupMapVec,
                            dispersalFn,
                            k, b, lociReturn, 
                            speciesComm, 
                            pointDistance){
  seedsArrived <- data.table(fromInit=integer(),speciesCode=integer(),
                             key=c("fromInit","speciesCode"))
  # Go to species level
  spRcvCommCodes <- speciesComm(unique(speciesRcvPool$speciesRcvPool), sc=sc)
  #spRcvCommCodes <- speciesComm(unique(potentials$RcvCommunity))
  setkey(spRcvCommCodes, RcvCommunity)
  setkey(potentials, RcvCommunity)
  
  # Make potentials have all Rcv pixels, with each species as unique line
  potentials = spRcvCommCodes[potentials, allow.cartesian=TRUE][,`:=`(RcvCommunity=NULL)]
  setkey(potentials,"from")
  # identify which are 8 neighbours from each "active cell"
  adjCells <- adj(seedReceiveMap, unique(activeCell), directions=8, pairs=TRUE,include=TRUE) %>%
    data.table(key=c("from"))
  while (NROW(potentials) & ((n-cellSize)%<=%ultimateMaxDist)) { # while there are active cells and less than maxDistance
    #                  browser(expr=round(time(sim))>=20)
    # If this is second or greater time through this while loop, make active cells (i.e., the "from")
    #  be the previous round's "to" column. Also, delete "to" and "dis" columns
    #                  browser()
    
    if(n>cellSize) {
      potentials[,`:=`(from=NULL)][,from:=to][,`:=`(to=NULL,dis=NULL)]
      setkey(potentials,"from")
    }
    ################ original
    # join these to the potentials object
    potentials <- potentials[adjCells, allow.cartesian=TRUE] %>% 
      unique(by=c("fromInit", "to", "speciesCode"))
    potentials <- potentials[!is.na(fromInit),]
    
    # because there will be duplicate "from - to" pairs, say from 2 different species, only calculate
    #   distance once, then re-join the shorter version back to longer version by species
    shortPotentials <- setkey(potentials, fromInit, to) %>% unique(., by = c("fromInit", "to")) %>% .[,list(fromInit, to)] 
    set(shortPotentials, , "dis", as.integer(pointDistance(xysAll[shortPotentials[,fromInit],], xysAll[shortPotentials[,to],], 
                                                lonlat=FALSE)))
    
    # merge shorter object with no duplicate from-to pairs back with potentials, which does have duplicate from-to pairs
    #   due to multiple species having same from-to pair
    if(n-cellSize==0){ # the first loop incudes on site regeneration
      potentials <- shortPotentials[((n-cellSize) %<=% dis) & (dis %<=% n),][
        potentials, nomatch=0][
          (dis %<=% maxDist),]
    } else {
      potentials <- shortPotentials[((n-cellSize) %<<% dis) & (dis %<=% n),][
        potentials, nomatch=0][
          (dis %<=% maxDist),]
    }
    
    if(NROW(potentials)>0) {
      dtSrcShort <- dtSrc[,list(pixelGroup,speciesCode)] 
      set(potentials,,"pixelGroup",pixelGroupMapVec[potentials[,to]])
      setkey(dtSrcShort, speciesCode, pixelGroup)
      setkey(potentials, speciesCode, pixelGroup)
      
      
      potentialsWithSeedDT <- potentials[dtSrcShort, nomatch=0]
      if(NROW(potentialsWithSeedDT)>0) {
        #                     cat("\n Year:",round(time(sim)),"\n")
        #                     dd <- potentialsWithSeedDT[,.(NumberofCell=length(from)),by=c("dis","to")]
        #                     print(dd)
        #potentialsWithSeedDT  <- potentials[potentialsWithSeed,]
        #setkey(potentialsWithSeedDT, "fromInit")
        
        
        set(potentialsWithSeedDT,,"receivesSeeds",NA)
        nr <- NROW(potentialsWithSeedDT)
        
        # back to Ward
        # Don't include the ones that were already, calculate probability
        potentialsWithSeedDT[#is.na(receivesSeeds) & 
                               dis==0,
                             dispersalProb:=1]
        potentialsWithSeedDT[#is.na(receivesSeeds) & 
                               dis!=0,
                             dispersalProb:=eval(dispersalFn)]
        potentialsWithSeedDT <- potentialsWithSeedDT[,.(receivesSeeds=runif(nr)<dispersalProb,
                                                        fromInit,speciesCode)]
        receivedSeeds <- potentialsWithSeedDT[,any(receivesSeeds), by=c("fromInit,speciesCode")]
        setkey(receivedSeeds, fromInit, speciesCode)
        
        #drop any that received seeds from potentials, as they are now in lociReturn
        if(NROW(receivedSeeds[V1==TRUE])>0)  {
          seedsArrived <- rbindlist(list(seedsArrived,
                                         lociReturn[receivedSeeds[V1==TRUE]][,V1:=NULL]))
          setkey(seedsArrived, fromInit, speciesCode)
          setkey(potentials, fromInit, speciesCode)
          potentials <- potentials[!receivedSeeds[V1==TRUE]]
          #potentials <- potentials[!receivedSeeds[V1==TRUE]]
        }
        #potentials <- specPotentials[,list(RcvCommunity=sum(2^speciesCode)), by=c("fromInit","from","to")]
      }
    }
    n <- n+cellSize
    # refresh so that "to" cells become new "from" cells
    activeCell <- potentials[,to]
  }
  return(seedsArrived)
}


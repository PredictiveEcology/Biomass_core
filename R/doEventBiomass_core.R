#' @export
doEvent.Biomass_core <- function(sim, eventTime, eventType, debug = FALSE) {
    if (is.numeric(P(sim)$.useParallel)) {
        a <- data.table::setDTthreads(P(sim)$.useParallel)
        if (getOption("LandR.verbose", TRUE) > 0) {
            if (data.table::getDTthreads() > 1L) 
                message("Biomass_core should be using >100% CPU")
        }
        on.exit(data.table::setDTthreads(a), add = TRUE)
    }
    dispEvtPriority <- 5
    GMEvtPriority <- 6
    agingEvtPriotity <- 7
    summRegenPriority <- 8
    summBGMPriority <- list(start = dispEvtPriority - 1, postDisp = dispEvtPriority + 
        0.25, postRegen = 4, postGM = GMEvtPriority + 0.25, postAging = agingEvtPriotity + 
        0.25, end = summRegenPriority + 0.25)
    if (!is.null(P(sim)$calcSummaryBGM)) 
        if (!any(P(sim)$calcSummaryBGM == "end")) 
            params(sim)$Biomass_core$calcSummaryBGM <- c(P(sim)$calcSummaryBGM, 
                "end")
    summBGMPriority <- summBGMPriority[P(sim)$calcSummaryBGM]
    plotPriority <- 9
    savePriority <- 10
    switch(eventType, init = {
        if (is.na(P(sim)$.plotInterval)) params(sim)$Biomass_core$.plotInterval <- P(sim)$successionTimestep
        if (is.na(P(sim)$.saveInterval)) params(sim)$Biomass_core$.saveInterval <- P(sim)$successionTimestep
        if (anyPlotting(P(sim)$.plots) && any(P(sim)$.plots == 
            "screen")) {
            if (is.null(dev.list())) {
                dev(x = dev.cur() + 1, height = 7, width = 14)
                clearPlot()
            } else {
                if (dev.size()[2] < 14) {
                  dev(x = dev.cur() + 1, height = 7, width = 14)
                  clearPlot()
                }
            }
            mod$statsWindow <- dev.cur()
            if (P(sim)$.plotMaps) {
                mod$mapWindow <- mod$statsWindow + 1
                dev(x = mod$mapWindow, height = 8, width = 10)
            }
        } else {
            params(sim)[[currentModule(sim)]]$.plotMaps <- FALSE
        }
        if (time(sim) != end(sim)) {
            if (any(is.na(P(sim)$.plots))) {
                mod$plotTypes <- NA
            } else if (any(P(sim)$.plots == "screen")) {
                mod$plotTypes <- "screen"
            } else {
                mod$plotTypes <- NA
            }
        }
        if (is.na(P(sim)$.plotInitialTime)) {
            params(sim)[[currentModule(sim)]]$.plotInitialTime <- start(sim)
            message("Using .plotInitialTime == NA no longer turns off plotting. Please use .plots == NA instead.")
        }
        sim <- Init(sim)
        if (!is.null(summBGMPriority$start)) sim <- scheduleEvent(sim, 
            start(sim) + P(sim)$successionTimestep, "Biomass_core", 
            "summaryBGMstart", eventPriority = summBGMPriority$start)
        sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "Dispersal", eventPriority = dispEvtPriority)
        sim <- scheduleEvent(sim, P(sim)$growthInitialTime, "Biomass_core", 
            "mortalityAndGrowth", GMEvtPriority)
        if (!is.null(summBGMPriority$postDisp)) sim <- scheduleEvent(sim, 
            start(sim) + P(sim)$successionTimestep, "Biomass_core", 
            "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
        if (!is.null(summBGMPriority$postRegen)) sim <- scheduleEvent(sim, 
            start(sim) + P(sim)$successionTimestep, "Biomass_core", 
            "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
        if (!is.null(summBGMPriority$postGM)) sim <- scheduleEvent(sim, 
            start(sim) + P(sim)$successionTimestep, "Biomass_core", 
            "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
        if (P(sim)$successionTimestep != 1) {
            sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, 
                "Biomass_core", "cohortAgeReclassification", 
                eventPriority = agingEvtPriotity)
            if (!is.null(summBGMPriority$postAging)) sim <- scheduleEvent(sim, 
                start(sim) + P(sim)$successionTimestep, "Biomass_core", 
                "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
        }
        if (!is.null(P(sim)$calcSummaryBGM)) {
            sim <- scheduleEvent(sim, start(sim), "Biomass_core", 
                "summaryBGM", eventPriority = summBGMPriority$end)
            sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, 
                "Biomass_core", "summaryRegen", eventPriority = summRegenPriority)
            sim <- scheduleEvent(sim, start(sim), "Biomass_core", 
                "plotSummaryBySpecies", eventPriority = plotPriority)
            sim <- scheduleEvent(sim, end(sim), "Biomass_core", 
                "plotSummaryBySpecies", eventPriority = plotPriority)
        }
        if (anyPlotting(P(sim)$.plots)) {
            if (P(sim)$.plotMaps) {
                sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, 
                  "Biomass_core", "plotMaps", eventPriority = plotPriority + 
                    0.25)
            }
            sim <- scheduleEvent(sim, start(sim), "Biomass_core", 
                "plotAvgs", eventPriority = plotPriority + 0.5)
            sim <- scheduleEvent(sim, end(sim), "Biomass_core", 
                "plotAvgs", eventPriority = plotPriority + 0.5)
        }
        if (!is.na(P(sim)$.saveInitialTime)) {
            if (P(sim)$.saveInitialTime < start(sim) + P(sim)$successionTimestep) {
                message(crayon::blue(paste(".saveInitialTime should be >=", 
                  start(sim) + P(sim)$successionTimestep, ". First save changed to", 
                  start(sim) + P(sim)$successionTimestep)))
                params(sim)$Biomass_core$.saveInitialTime <- start(sim) + 
                  P(sim)$successionTimestep
            }
            sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, 
                "Biomass_core", "save", eventPriority = savePriority)
        }
    }, summaryBGMstart = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGMstart", eventPriority = summBGMPriority$start)
    }, Dispersal = {
        sim <- Dispersal(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "Dispersal", eventPriority = dispEvtPriority)
    }, mortalityAndGrowth = {
        sim <- MortalityAndGrowth(sim)
        sim <- scheduleEvent(sim, time(sim) + 1, "Biomass_core", 
            "mortalityAndGrowth", eventPriority = GMEvtPriority)
    }, summaryBGMpostDisp = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGMpostDisp", eventPriority = summBGMPriority$postDisp)
    }, summaryBGMpostRegen = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGMpostRegen", eventPriority = summBGMPriority$postRegen)
    }, summaryBGMpostGM = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGMpostGM", eventPriority = summBGMPriority$postGM)
    }, cohortAgeReclassification = {
        sim <- CohortAgeReclassification(sim)
        if (P(sim)$successionTimestep != 1) {
            sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
                "Biomass_core", "cohortAgeReclassification", 
                eventPriority = agingEvtPriotity)
        }
    }, summaryBGMpostAging = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGMpostAging", eventPriority = summBGMPriority$postAging)
    }, summaryRegen = {
        sim <- summaryRegen(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryRegen", eventPriority = summRegenPriority)
    }, summaryBGM = {
        sim <- SummaryBGM(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, 
            "Biomass_core", "summaryBGM", eventPriority = summBGMPriority$end)
    }, plotSummaryBySpecies = {
        if (time(sim) == end(sim)) {
            mod$plotTypes <- P(sim)$.plots
        }
        sim <- plotSummaryBySpecies(sim)
        if (!is.na(P(sim)$.plotInterval)) {
            if (!(time(sim) + P(sim)$.plotInterval) == end(sim)) sim <- scheduleEvent(sim, 
                time(sim) + P(sim)$.plotInterval, "Biomass_core", 
                "plotSummaryBySpecies", eventPriority = plotPriority)
        }
    }, plotAvgs = {
        if (time(sim) == end(sim)) {
            mod$plotTypes <- P(sim)$.plots
        }
        sim <- plotAvgVegAttributes(sim)
        if (!is.na(P(sim)$.plotInterval)) {
            if (!(time(sim) + P(sim)$.plotInterval) == end(sim)) sim <- scheduleEvent(sim, 
                time(sim) + P(sim)$.plotInterval, "Biomass_core", 
                "plotAvgs", eventPriority = plotPriority + 0.5)
        }
    }, plotMaps = {
        sim <- plotVegAttributesMaps(sim)
        if (P(sim)$.plotMaps) sim <- scheduleEvent(sim, time(sim) + 
            P(sim)$.plotInterval, "Biomass_core", "plotMaps", 
            eventPriority = plotPriority + 0.25)
    }, save = {
        sim <- Save(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, 
            "Biomass_core", "save", eventPriority = savePriority)
    }, warning(paste("Undefined event type: '", current(sim)[1, 
        "eventType", with = FALSE], "' in module '", current(sim)[1, 
        "moduleName", with = FALSE], "'", sep = "")))
    return(invisible(sim))
}

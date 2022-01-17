CohortAgeReclassification <- function(sim) {
    if (time(sim) != start(sim)) {
        sim$cohortData <- ageReclassification(cohortData = sim$cohortData, 
            successionTimestep = P(sim)$successionTimestep, stage = "mainSimulation", 
            byGroups = P(sim)$cohortDefinitionCols)
        return(invisible(sim))
    }
    else {
        return(invisible(sim))
    }
}

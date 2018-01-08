library(SpaDES)
library(magrittr)

modulePath <- "~/Documents/GitHub/LandWeb/m/"

inputDir <- file.path(dirname(tempdir()), "LBMR", "inputs") %>% checkPath(create = TRUE)
outputDir <- file.path(dirname(tempdir()), "LBMR", "outputs") 
times <- list(start = 0, end = 10)
parameters <- list()
modules <- list("LBMR")
paths <- list(
  cachePath = file.path(outputDir, "cache"),
  modulePath = modulePath,
  inputPath = inputDir,
  outputPath = outputDir
)

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

mySimOut <- spades(mySim, debug = TRUE)

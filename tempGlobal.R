library(SpaDES)
moduleName <- "LBMR"
spadesModulesDirectory <- "C:/Ian/Campbell/Cariboo_RIA/Land-R/modules/"
inputDir <- file.path(dirname(spadesModulesDirectory), "inputs") %>% checkPath(create = TRUE)
outputDir <- file.path(dirname(spadesModulesDirectory), "outputs") 
times <- list(start = 0, end = 10)
parameters <- list(
  .globals = list(verbose = FALSE),
  LBMR = list(.plotInitialTime = 0,
              successionTimestep = 5,
              seedingAlgorithm = "wardDispersal")
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list(moduleName)
objects <- list()

dev()

setPaths(cachePath = file.path(outputDir, "cache"),
         modulePath = file.path(spadesModulesDirectory),
         inputPath = inputDir,
         outputPath = outputDir)
paths <- getPaths()

mySim <- simInit(times = times, params = parameters, modules = modules, objects = objects, paths = paths)

mySimOut <- spades(mySim, debug = TRUE)

test_that("test growth and mortality at main simulation stage",{
  opts <- options(reproducible.useGDAL = FALSE,
                  spades.moduleCodeChecks = FALSE,
                  reproducible.useMemoise = TRUE,
                  spades.useRequire = FALSE,
                  LandR.assertions = FALSE,
                  spades.recoveryMode = FALSE)
  on.exit(options(opts))
  Require::Require(c("SpaDES.core", "reproducible", "data.table", "raster", "LandR"))
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="..",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA, .useCache = FALSE))

  endYr <- 3
  startAge <- 1L
  startB <- 1096L
  pixelGroupMap <- raster(xmn=50,xmx=50+1*100,
                          ymn=50,ymx=50+1*100,
                          res = c(100,100),
                          val = 11)
  # studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)  # Jan 2021 we agreed to force user to provide a SA/SAL
  studyArea <- rasterToPolygons(pixelGroupMap)
  speciesEcoregion <- data.table(year = 1,
                                 ecoregionGroup = as.factor(1),
                                 speciesCode = as.factor("tsugcana"),
                                 establishprob = 0.22,
                                 maxANPP = 1096,
                                 maxB = 32880L)
  LandR::assertColumns(speciesEcoregion,
                       c(ecoregionGroup = "factor", speciesCode = "factor",
                         establishprob = "numeric", maxB = "integer", maxANPP = "numeric"))
  successionTimestep <- 1
  species <- data.table(species = "tsugcana",
                        Area = as.factor("BP"),
                        longevity = 500L,
                        sexualmature = 30L,
                        shadetolerance = 5,
                        firetolerance = 2L,
                        seeddistance_eff = 30L,
                        seeddistance_max = 100L,
                        resproutprob = 0,
                        resproutage_min = 0L,
                        resproutage_max = 0L,
                        postfireregen = as.factor("none"),
                        leaflongevity = 3L,
                        wooddecayrate = 0.04,
                        mortalityshape = 10L,
                        growthcurve = 0.25,    ## closest to Yong's results. tried 0.7, 0.3, 0.2 and 1.0
                        leafLignin = 0.2,
                        hardsoft = as.factor("soft"),
                        speciesCode = as.factor("tsugcana"))
  species <- data.table(species)[species=="tsugcana",][,':='(sexualmature=longevity)]
  LandR::assertColumns(species, colClasses = c(species = "character", Area = "factor", longevity = "integer",
                                               sexualmature = "integer", shadetolerance = "numeric",
                                               firetolerance = "integer", seeddistance_eff = "integer",
                                               seeddistance_max = "integer", resproutprob = "numeric",
                                               resproutage_min = "integer", resproutage_max = "integer",
                                               postfireregen = "factor", leaflongevity = "integer",
                                               wooddecayrate = "numeric", mortalityshape = "integer",
                                               growthcurve = "numeric", leafLignin = "numeric",
                                               hardsoft = "factor", speciesCode = "factor"))
  sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                X0 = c(rep(1, 4), 0),
                                X1 = c(0, rep(1, 3), 0),
                                X2 = c(0, 0, rep(1, 3)),
                                X3 = c(rep(0, 3), rep(1, 2)),
                                X4 = c(rep(0, 4), 1),
                                X5 = c(rep(0, 4), 1))
  cohortData <- data.table(pixelGroup = 11, ecoregionGroup = as.factor(1),
                           speciesCode = as.factor("tsugcana"),
                           age = startAge, B = startB,
                           mortality = 0L, aNPPAct = 0)
  pixelCohortData <- LandR::addPixels2CohortData(cohortData, pixelGroupMap)
  ecoregion <- LandR::makeEcoregionDT(pixelCohortData, speciesEcoregion)
  ecoregionMap <- setValues(pixelGroupMap, 1)
  ecoregionMap <- LandR::makeEcoregionMap(list(ecoregionMape = ecoregionMap),
                                          pixelCohortData = addPixels2CohortData(cohortData, pixelGroupMap))
  sppEquivCol <- "LANDIS_test"
  sppEquiv <- LandR::sppEquivalencies_CA[LANDIS_test == "tsugcana"]
  sppColorVect <- LandR::sppColors(sppEquiv, sppEquivCol, palette = "Accent")
  parameters <- list(Biomass_core = list(
    "successionTimestep" = successionTimestep,
    "vegLeadingProportion" = 0,
    ".useCache" = FALSE))
  biomassMap <- setValues(pixelGroupMap, startB)
  rasterToMatch <- raster(biomassMap)
  treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(),
                                                     pixelGroup = integer(),
                                                     burnTime = numeric())

  cceArgs <- list(quote(CMI),
                      quote(ATA),
                      quote(CMInormal),
                      quote(mcsModel),
                      quote(gcsModel))
  names(cceArgs) <- paste(cceArgs)
  minRelativeB <- makeMinRelativeB(cohortData)
  speciesLayers <- pixelGroupMap
  # studyAreaReporting <- studyArea
  lastReg <- 999

  outputFull <- list()
  for (successionTimestep in c(1,4, 10)) {
    objects <- mget(setdiff(moduleInputs("Biomass_core", "..")$objectName, "studyAreaReporting"))
    parameters$Biomass_core <- modifyList(parameters$Biomass_core, list("successionTimestep" = successionTimestep))
    output <- data.table(Year = 0, Biomass = startB, successionTimestep = successionTimestep)
    for (i in 1:endYr) {
      if (i %% successionTimestep == 0) {
        print(i)
        opts <- options(spades.debug = FALSE)
        sim <- simInit(times = list(start = i, end = i + successionTimestep - 1),
                       params = parameters,
                       modules = module,
                       objects = objects,
                       paths = path)
        #ll <- setdiff(ls(sim$.mods$Biomass_core, all.names = TRUE), c("Par", "mod", ".objects", "._sourceFilename"))
        #keepers <- sim$.mods$Biomass_core
        #rm(list = ll, envir = sim$.mods$Biomass_core)

        #rm("Biomass_core", envir = sim$.mods) # it is locked; so must delete it first
        #sim$.mods$Biomass_core <- environment(MortalityAndGrowth)
        #list2env(as.list(keepers), sim$.mods$Biomass_core)

        simOutput <- suppressMessages(
          spades(sim, events = c(load = "init", Biomass_core = "mortalityAndGrowth"),
                           .plotInitialTime = NA)
        )
        options(opts)
        cohortData2 <- simOutput$cohortData
        if (i == 2 | i == 3){
          expect_is(cohortData2, "data.table")
          expect_true(length(setdiff(names(cohortData2),
                                     c("pixelGroup", "ecoregionGroup", "speciesCode", "age",
                                       "B", "mortality", "aNPPAct"))) == 0)
        }

        output_added <- cohortData2[,.(Year = i, Biomass=B)]
        output <- rbind(output, cbind(output_added, successionTimestep = successionTimestep))
        objects <- modifyList(objects, list("cohortData" = cohortData2))
      }

    }
    outputFull[[as.character(successionTimestep)]] <- output
  }

  output_compared1 <-
    setDT(list(Year = 0:493,
         Biomass = c(1096L, 1856L,
                     2631L, 3399L, 4152L, 4884L, 5594L, 6282L, 6947L, 7590L, 8211L,
                     8811L, 9391L, 9952L, 10495L, 11020L, 11528L, 12020L, 12497L,
                     12959L, 13407L, 13842L, 14264L, 14674L, 15072L, 15458L, 15834L,
                     16199L, 16554L, 16899L, 17235L, 17562L, 17881L, 18191L, 18493L,
                     18787L, 19074L, 19354L, 19627L, 19893L, 20152L, 20405L, 20652L,
                     20893L, 21128L, 21358L, 21582L, 21801L, 22015L, 22224L, 22428L,
                     22627L, 22822L, 23012L, 23198L, 23380L, 23558L, 23732L, 23902L,
                     24068L, 24231L, 24390L, 24546L, 24698L, 24847L, 24993L, 25136L,
                     25275L, 25411L, 25545L, 25676L, 25804L, 25929L, 26051L, 26171L,
                     26288L, 26403L, 26515L, 26625L, 26733L, 26838L, 26941L, 27042L,
                     27141L, 27238L, 27333L, 27426L, 27517L, 27606L, 27693L, 27778L,
                     27861L, 27943L, 28023L, 28101L, 28177L, 28252L, 28325L, 28397L,
                     28467L, 28535L, 28602L, 28667L, 28731L, 28794L, 28855L, 28915L,
                     28973L, 29030L, 29086L, 29140L, 29193L, 29245L, 29296L, 29345L,
                     29393L, 29440L, 29486L, 29531L, 29575L, 29618L, 29659L, 29699L,
                     29738L, 29776L, 29813L, 29849L, 29884L, 29918L, 29951L, 29983L,
                     30014L, 30044L, 30073L, 30101L, 30128L, 30154L, 30180L, 30205L,
                     30229L, 30252L, 30274L, 30295L, 30315L, 30334L, 30352L, 30370L,
                     30387L, 30403L, 30418L, 30432L, 30445L, 30457L, 30469L, 30480L,
                     30490L, 30499L, 30507L, 30515L, 30522L, 30528L, 30533L, 30537L,
                     30540L, 30543L, 30545L, 30546L, 30546L, 30545L, 30544L, 30542L,
                     30539L, 30535L, 30530L, 30525L, 30519L, 30512L, 30504L, 30495L,
                     30485L, 30475L, 30464L, 30452L, 30439L, 30425L, 30410L, 30394L,
                     30378L, 30361L, 30343L, 30324L, 30304L, 30283L, 30261L, 30238L,
                     30214L, 30190L, 30165L, 30139L, 30112L, 30084L, 30055L, 30025L,
                     29994L, 29962L, 29929L, 29895L, 29860L, 29824L, 29787L, 29749L,
                     29710L, 29670L, 29629L, 29586L, 29542L, 29497L, 29451L, 29404L,
                     29356L, 29307L, 29257L, 29206L, 29153L, 29099L, 29044L, 28988L,
                     28931L, 28872L, 28812L, 28751L, 28689L, 28625L, 28560L, 28494L,
                     28427L, 28358L, 28288L, 28217L, 28144L, 28070L, 27994L, 27917L,
                     27839L, 27759L, 27678L, 27596L, 27512L, 27427L, 27340L, 27252L,
                     27162L, 27071L, 26978L, 26884L, 26788L, 26691L, 26592L, 26492L,
                     26390L, 26287L, 26182L, 26076L, 25968L, 25859L, 25748L, 25636L,
                     25522L, 25406L, 25289L, 25170L, 25050L, 24928L, 24805L, 24680L,
                     24554L, 24426L, 24297L, 24166L, 24034L, 23900L, 23765L, 23628L,
                     23490L, 23350L, 23209L, 23066L, 22922L, 22776L, 22629L, 22481L,
                     22331L, 22180L, 22028L, 21874L, 21719L, 21563L, 21405L, 21246L,
                     21086L, 20925L, 20763L, 20600L, 20436L, 20271L, 20105L, 19938L,
                     19770L, 19601L, 19431L, 19260L, 19089L, 18917L, 18744L, 18570L,
                     18396L, 18221L, 18046L, 17870L, 17694L, 17517L, 17340L, 17163L,
                     16986L, 16808L, 16630L, 16452L, 16274L, 16096L, 15918L, 15740L,
                     15562L, 15384L, 15207L, 15030L, 14853L, 14677L, 14501L, 14325L,
                     14150L, 13976L, 13802L, 13629L, 13456L, 13284L, 13113L, 12943L,
                     12773L, 12594L, 12407L, 12213L, 12013L, 11807L, 11597L, 11383L,
                     11166L, 10947L, 10727L, 10506L, 10284L, 10063L, 9842L, 9623L,
                     9405L, 9189L, 8976L, 8766L, 8558L, 8354L, 8153L, 7956L, 7762L,
                     7572L, 7386L, 7204L, 7026L, 6852L, 6682L, 6516L, 6354L, 6196L,
                     6042L, 5891L, 5744L, 5601L, 5462L, 5327L, 5195L, 5066L, 4941L,
                     4819L, 4700L, 4585L, 4472L, 4362L, 4255L, 4151L, 4050L, 3951L,
                     3855L, 3762L, 3671L, 3582L, 3495L, 3411L, 3329L, 3249L, 3170L,
                     3094L, 3019L, 2947L, 2876L, 2807L, 2740L, 2674L, 2610L, 2548L,
                     2487L, 2427L, 2369L, 2312L, 2257L, 2203L, 2151L, 2100L, 2050L,
                     2001L, 1953L, 1907L, 1861L, 1817L, 1774L, 1732L, 1691L, 1651L,
                     1611L, 1573L, 1535L, 1499L, 1463L, 1428L, 1394L, 1361L, 1329L,
                     1297L, 1266L, 1236L, 1207L, 1178L, 1150L, 1123L, 1096L, 1070L,
                     1045L, 1020L, 996L, 972L, 949L, 926L, 904L, 883L, 862L, 841L,
                     821L, 801L, 782L, 764L, 745L, 728L, 710L, 693L, 677L, 661L, 645L,
                     629L, 614L, 600L, 585L, 571L, 558L, 544L, 531L, 519L, 506L, 494L,
                     482L, 471L, 459L, 448L, 438L, 427L, 417L, 407L, 397L)))
  # output_compared <- data.table(Year=0:(endYr - 1),
  #                               Biomass=c(1096, 1855, 2629, 3397, 4149, 4881, 5591, 6278, 6943, 7585,
  #                                         8206, 8806, 9386, 9947, 10489, 11014, 11522, 12014, 12491, 12953,
  #                                         13401, 13836, 14258, 14667, 15065, 15451, 15826, 16191, 16546,
  #                                         16891, 17227, 17554, 17872, 18182, 18484, 18778, 19065, 19344,
  #                                         19616, 19882, 20141, 20394, 20641, 20882, 21117, 21346, 21570,
  #                                         21789, 22003, 22212, 22416, 22615, 22810, 23000, 23186, 23368,
  #                                         23546, 23720, 23890, 24056, 24218, 24377, 24532, 24684, 24833,
  #                                         24978, 25120, 25259, 25395, 25528, 25659, 25787, 25912, 26034,
  #                                         26154, 26271, 26386, 26498, 26608, 26716, 26821, 26924, 27025,
  #                                         27124, 27221, 27316, 27409, 27500, 27589, 27676, 27761, 27844,
  #                                         27925, 28005, 28083, 28159, 28234, 28307, 28378, 28448, 28516,
  #                                         28583, 28648, 28712, 28774, 28835, 28895, 28953, 29010, 29066,
  #                                         29120, 29173, 29225, 29276, 29325, 29373, 29420, 29466, 29511,
  #                                         29555, 29597, 29638, 29678, 29717, 29755, 29792, 29828, 29863,
  #                                         29897, 29930, 29962, 29993, 30023, 30052, 30080, 30107, 30133,
  #                                         30158, 30183, 30207, 30230, 30252, 30273, 30293, 30312, 30330,
  #                                         30347, 30364, 30380, 30395, 30409, 30422, 30434, 30446, 30457,
  #                                         30467, 30476, 30484, 30491, 30498, 30504, 30509, 30513, 30516,
  #                                         30519, 30521, 30522, 30522, 30522, 30522, 30521, 30519, 30516,
  #                                         30512, 30507, 30502, 30496, 30489, 30481, 30472, 30462, 30451,
  #                                         30440, 30428, 30415, 30401, 30386, 30370, 30353, 30335, 30317,
  #                                         30298, 30278, 30257, 30235, 30212, 30188, 30163, 30137, 30110,
  #                                         30082, 30053, 30023, 29992, 29960, 29927, 29893, 29858, 29822,
  #                                         29785, 29747, 29708, 29668, 29627, 29585, 29542, 29498, 29453,
  #                                         29407, 29359, 29310, 29260, 29209, 29157, 29104, 29049, 28993,
  #                                         28936, 28878, 28819, 28758, 28696, 28633, 28568, 28502, 28435,
  #                                         28367, 28297, 28226, 28153, 28079, 28004, 27927, 27849, 27770,
  #                                         27689, 27607, 27523, 27438, 27351, 27263, 27173, 27082, 26990,
  #                                         26896, 26801, 26704, 26605, 26505, 26403, 26300, 26195, 26089,
  #                                         25981, 25872, 25761, 25649, 25535, 25420, 25303, 25184, 25064,
  #                                         24942, 24819, 24694, 24568, 24440, 24311, 24180, 24048, 23914,
  #                                         23779, 23642, 23504, 23364, 23223, 23080, 22936, 22790, 22643,
  #                                         22495, 22345, 22194, 22042, 21888, 21733, 21577, 21419, 21260,
  #                                         21100, 20939, 20777, 20614, 20450, 20285, 20119, 19952, 19784,
  #                                         19615, 19445, 19274, 19102, 18930, 18757, 18583, 18409, 18234,
  #                                         18059, 17883, 17707, 17530, 17353, 17176, 16998, 16820, 16642,
  #                                         16464, 16286, 16108, 15930, 15752, 15574, 15396, 15218, 15041,
  #                                         14864, 14687, 14511, 14335, 14160, 13985, 13811, 13638, 13465,
  #                                         13293, 13122, 12951, 12781, 12602, 12415, 12221, 12020, 11814,
  #                                         11604, 11390, 11173, 10954, 10733, 10512, 10290, 10068, 9847,
  #                                         9628, 9410, 9194, 8981, 8770, 8562, 8358, 8157, 7959, 7765, 7575,
  #                                         7389, 7207, 7029, 6855, 6685, 6519, 6357, 6199, 6045, 5894, 5747,
  #                                         5604, 5465, 5329, 5197, 5069, 4944, 4822, 4703, 4587, 4475, 4365,
  #                                         4258, 4154, 4053, 3954, 3858, 3764, 3673, 3584, 3497, 3413, 3331,
  #                                         3251, 3172, 3096, 3021, 2948, 2877, 2808, 2741, 2675, 2611, 2549,
  #                                         2488, 2429, 2371, 2314, 2259, 2205, 2152, 2101, 2051, 2002, 1954,
  #                                         1908, 1863, 1818, 1775, 1733, 1692, 1652, 1612, 1574, 1536, 1500,
  #                                         1464, 1429, 1395, 1362, 1330, 1298, 1267, 1237, 1208, 1179, 1151,
  #                                         1124, 1097, 1071, 1045, 1020, 996, 973, 950, 927, 905, 883, 862,
  #                                         842, 822, 802, 783, 764, 746, 728, 711, 694, 677, 661, 645, 630,
  #                                         615, 600, 586, 572, 558, 545, 532, 519, 507, 494, 483, 471, 460,
  #                                         449, 438, 427, 417, 407))
  # ### CERES: I STOPPED HERE ####

  outputFullDT <- rbindlist(outputFull)
  outputFullDT[, successionTimestep := as.character(successionTimestep)]
  rows <- seq(endYr + 1)
  expect_equal(outputFull[[1]][rows, 1:2],output_compared1[rows,])

  if (FALSE)
    ggplot(outputFullDT, aes(x = Year, y = Biomass, color = successionTimestep)) +
    geom_line() +
    theme_bw()

  deviations <- outputFullDT[, list(B = mean(Biomass), dev = abs(diff(range(Biomass)))), by = Year]
  devvs <- sort(deviations$dev / deviations$B, decreasing = TRUE)
  expect_true(devvs[1] < 0.012) # allowed 1.2% deviation -- this is at year 10
  expect_true(devvs[2] < 0.005) # allowed 0.5% deviation for 2nd most
  expect_true(devvs[3] < 0.003) # allowed 0.3% deviation for 3rd most

})

test_that("test bigger .inputObjects",{

  opts <- options(reproducible.useGDAL = FALSE,
                  spades.moduleCodeChecks = FALSE,
                  reproducible.useMemoise = TRUE,
                  spades.useRequire = FALSE,
                  LandR.assertions = FALSE,
                  spades.recoveryMode = FALSE)
  on.exit(options(opts))
  Require::Require(c("SpaDES.core", "reproducible", "data.table", "raster", "LandR"))
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="..",
               outputPath="~/output")
  parameters <- list(.globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA, .useCache = FALSE,
                                        "vegLeadingProportion" = 0))
  sppEquivCol <- "LANDIS_test"
  sppEquiv <- LandR::sppEquivalencies_CA[LANDIS_test == "poputrem"]
  sppColorVect <- LandR::sppColors(sppEquiv, sppEquivCol, palette = "Accent")
  objects <- mget(c("sppEquivCol", "sppEquiv", "sppColorVect"))
  if (FALSE) {
    endYr <- 3
    startAge <- 1L
    startB <- 1096L
    pixelGroupMap <- raster(xmn=50,xmx=50+1*100,
                            ymn=50,ymx=50+1*100,
                            res = c(100,100),
                            val = 11)
    # studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)  # Jan 2021 we agreed to force user to provide a SA/SAL
    studyArea <- rasterToPolygons(pixelGroupMap)
    speciesEcoregion <- data.table(year = 1,
                                   ecoregionGroup = as.factor(1),
                                   speciesCode = as.factor("tsugcana"),
                                   establishprob = 0.22,
                                   maxANPP = 1096,
                                   maxB = 32880L)
    LandR::assertColumns(speciesEcoregion,
                         c(ecoregionGroup = "factor", speciesCode = "factor",
                           establishprob = "numeric", maxB = "integer", maxANPP = "numeric"))
    successionTimestep <- 1
    species <- data.table(species = "tsugcana",
                          Area = as.factor("BP"),
                          longevity = 500L,
                          sexualmature = 30L,
                          shadetolerance = 5,
                          firetolerance = 2L,
                          seeddistance_eff = 30L,
                          seeddistance_max = 100L,
                          resproutprob = 0,
                          resproutage_min = 0L,
                          resproutage_max = 0L,
                          postfireregen = as.factor("none"),
                          leaflongevity = 3L,
                          wooddecayrate = 0.04,
                          mortalityshape = 10L,
                          growthcurve = 0.25,    ## closest to Yong's results. tried 0.7, 0.3, 0.2 and 1.0
                          leafLignin = 0.2,
                          hardsoft = as.factor("soft"),
                          speciesCode = as.factor("tsugcana"))
    species <- data.table(species)[species=="tsugcana",][,':='(sexualmature=longevity)]
    LandR::assertColumns(species, colClasses = c(species = "character", Area = "factor", longevity = "integer",
                                                 sexualmature = "integer", shadetolerance = "numeric",
                                                 firetolerance = "integer", seeddistance_eff = "integer",
                                                 seeddistance_max = "integer", resproutprob = "numeric",
                                                 resproutage_min = "integer", resproutage_max = "integer",
                                                 postfireregen = "factor", leaflongevity = "integer",
                                                 wooddecayrate = "numeric", mortalityshape = "integer",
                                                 growthcurve = "numeric", leafLignin = "numeric",
                                                 hardsoft = "factor", speciesCode = "factor"))
    sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                  X0 = c(rep(1, 4), 0),
                                  X1 = c(0, rep(1, 3), 0),
                                  X2 = c(0, 0, rep(1, 3)),
                                  X3 = c(rep(0, 3), rep(1, 2)),
                                  X4 = c(rep(0, 4), 1),
                                  X5 = c(rep(0, 4), 1))
    cohortData <- data.table(pixelGroup = 11, ecoregionGroup = as.factor(1),
                             speciesCode = as.factor("tsugcana"),
                             age = startAge, B = startB,
                             mortality = 0L, aNPPAct = 0)
    pixelCohortData <- LandR::addPixels2CohortData(cohortData, pixelGroupMap)
    ecoregion <- LandR::makeEcoregionDT(pixelCohortData, speciesEcoregion)
    ecoregionMap <- setValues(pixelGroupMap, 1)
    ecoregionMap <- LandR::makeEcoregionMap(list(ecoregionMape = ecoregionMap),
                                            pixelCohortData = addPixels2CohortData(cohortData, pixelGroupMap))
    parameters <- list(Biomass_core = list(
      "successionTimestep" = successionTimestep,
      "vegLeadingProportion" = 0,
      ".useCache" = FALSE))
    biomassMap <- setValues(pixelGroupMap, startB)
    rasterToMatch <- raster(biomassMap)
    treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(),
                                                   pixelGroup = integer(),
                                                   burnTime = numeric())

    cceArgs <- list(quote(CMI),
                    quote(ATA),
                    quote(CMInormal),
                    quote(mcsModel),
                    quote(gcsModel))
    names(cceArgs) <- paste(cceArgs)
    minRelativeB <- makeMinRelativeB(cohortData)
    speciesLayers <- pixelGroupMap
    # studyAreaReporting <- studyArea
    lastReg <- 999
  }

  # objects <- mget(setdiff(moduleInputs("Biomass_core", "..")$objectName, "studyAreaReporting"))
  # parameters$Biomass_core <- modifyList(parameters$Biomass_core, list("successionTimestep" = successionTimestep))
  # output <- data.table(Year = 0, Biomass = startB, successionTimestep = successionTimestep)
  sim <- simInit(times = list(start = 1, end = 1),
                 params = parameters,
                 modules = module,
                 objects = objects,
                 paths = path)
  expect_error(regexp = "contrasts can be applied only to factors with 2 or more levels",
               simOutput <- suppressMessages(
    spades(sim, # events = c(load = "init", Biomass_core = "mortalityAndGrowth"),
           .plotInitialTime = NA)
  ))

})

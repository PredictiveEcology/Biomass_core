Known issues: <https://github.com/PredictiveEcology/Biomass_core/issues>

# Biomass_core (development version)

* Two optional, off-by-default ways to reduce cohort numbers in long runs; with the defaults nothing changes. `suppressedCohortRelativeB` (with `suppressedCohortMinAgeFrac`) removes old cohorts whose B is a tiny fraction of their pixelGroup's B (tested at 0.01 and 0.75). `cohortSquashInterval` (with `cohortSquashBandWidth`) merges a cohort that reaches a multiple of that age with same-species cohorts in its pixelGroup up to the band width younger, into one cohort with the B-weighted mean age in whole years; the width can be a vector so that older cohorts are merged more coarsely (tested at 100 and 20). Both options change results; test results will be posted in a comment on the pull request. New functions `suppressedCohortsToRemove()`, `squashCohortAges()` and `checkCohortSquashParams()`; `ageReclassification()` shares its aggregation with `squashCohortAges()` and gives the same output.

* A third optional, off-by-default way to limit cohort numbers: a conditional succession timestep. With `conditionalTimestepInterval` (years, a multiple of `successionTimestep`; `NA` is off) set, a species in a pixelGroup that already has more than `conditionalTimestepMinCohorts` (default 2) cohorts older than newly established ones can receive new seed-established cohorts only when `time(sim)` is a multiple of the interval; at other succession steps its establishment there is skipped. It applies to all three seeding algorithms through one helper, `conditionalTimestepBlocked()` (parameters checked by `checkConditionalTimestepParams()`); serotiny and resprouting are not restricted. Tested at an interval of 20 with 2 and with 5 cohorts. It changes results.

* LandR-CBM: `cohortSquashInterval` changes what a cohort is, so it breaks the LandR-CBM module/package (it converts LandR cohorts to CBM growth curves), which would need changes before it can be used with squashing. `conditionalTimestepInterval` and any cap on the number of cohorts only limit establishment and need no changes there.

* When no other module supplies `speciesLayers`, the module builds them from SCANFI on `studyArea` and `rasterToMatch` again. Since 2.1.0 it asked for `studyArea_biomassParam` and `rasterToMatch_biomassParam`, which this module does not have, so a standalone run passed nothing for the area. Runs with Biomass_borealDataPrep or Biomass_speciesData are unaffected.
* The `year` column of `speciesEcoregion` is now read in `time(sim)` units, as `LandR::speciesEcoregionLatestYear()` already assumed. Init used to renumber the years to start at 0, so a table with several years had its traits applied at the wrong times (calendar years: the last year's traits from the first growth step on; years from 0: one `successionTimestep` early). If the earliest year is after `start(sim)`, its rows are copied to `start(sim)` with a message (new `speciesEcoregionStartYear()`). `updateSpeciesEcoregionAttributes()` now stops, naming the species and ecoregion, instead of silently dropping cohorts that have no traits. Tables with a single year, which is every current input, give the same results. Builds on #80. Requires LandR >= 1.2.0.9053 (PredictiveEcology/LandR#271): older LandR adds an all-NA row to `speciesEcoregion` for a species with no traits, and Init now stops on it.
* `WardDispersalSeeding()` reads the `pixelGroupMap` values once and passes them to `LANDISDisp()` as `pgv` (burned pixels set to `NA`), instead of copying the raster and reading its values again inside `LANDISDisp()`. Results are unchanged; saves about 1.7 s per call on a 9M-cell map. Requires LandR >= 1.2.0.9052 (PredictiveEcology/LandR#255).

# Biomass_core 2.1.0

This release brings the module's development since early 2025 to the main branch. It works with forest inputs from SCANFI, the national satellite forest inventory, and adds plots of how forest types change over time, along with clearer summary plots. Climate-sensitive runs now follow the current method in the LandR.CS package.

Young trees under an older canopy now feel competition from it. Before, they grew as if in the open, so young cohorts were too productive; growth and mortality in mixed-age stands change as a result. A stand is now called "leading" by a species at 75% instead of 80%, which shifts vegetation type maps, and a study area with no tree species no longer stops the run. One issue is unresolved: in climate-sensitive runs, part of the climate effect on mortality may be counted twice (see https://github.com/PredictiveEcology/Biomass_core/pull/101). Runs that do not use climate sensitivity are not affected.

* `calculateSumB()`: every cohort in a `pixelGroup` again carries that group's total biomass
  (closes #111). Since the December 2021 rewrite (`9342cf1`) only cohorts at or past
  `successionTimestep` carried it and younger ones kept `sumB = 0`, so for a young cohort
  `calculateCompetition()` computed `bPot = max(1, maxB - sumB + B)` as though the site were
  empty. The larger `bPot` lowered the cohort's `bAP`, and with it the growth-related
  mortality (`mBio` rises with `bAP`), so young cohorts under a canopy grew as though in the
  open: net growth was overstated, mostly through understated mortality (ANPP was unchanged
  for `growthcurve = 0` and slightly lower otherwise). The mask selects which cohorts are
  *summed* (LANDIS-II leaves the young ones out of the total), not which cohorts feel the
  result. The 2019 test that encodes this is no longer skipped.
* `calculateSumB()` no longer re-keys `cohortData`. The group total is now matched on
  `pixelGroup` rather than `rep.int()` over a sorted table, which removes two latent hazards:
  the `wh` mask and `which(wh)` were computed *before* `checkAndChangeKey()` sorted the rows,
  so both referred to the pre-sort order; and when the table had no prior key that sort was
  never undone, silently re-keying the caller's object.
* Two calls that could not run: `.gc()` (large maps, over 3e7 cells) is `gc()` -- `.gc` (an ecosystem helper that calls `gc()` repeatedly) is not provided by any package in the module's reqdPkgs -- and `maxValue(sim$biomassMap)`, a raster-only function called on a terra object, is `terra::minmax()`, which reads the stored min/max rather than every value (in the `initialBiomassSource = "biomassMap"` branch, which currently stops before reaching it).
* `reqdPkgs` now lists `curl`, `httr`, `lme4`, `Require`, `tidyterra` and `viridis`, which the module's code uses.
* `vegLeadingProportion` now defaults to `LandR::leadingSpeciesProp()` (option
  `LandR.leadingSpeciesProp`, which takes `LandR.mixedwoodProp`, 0.75, unless set), so the
  leading-species threshold is set once for every module and LandR function instead of being
  hard-coded per module. **The default changes from 0.8 to 0.75**, which changes vegetation type
  maps. Requires LandR >= 1.2.0.9024 (PredictiveEcology/LandR#234).
* New parameter `.plotTransitionNaRm` (default `TRUE`, as before) is passed as `na.rm` to
  `LandR::vegTransitions()` in the `plotTransitions` event. Set `FALSE` to keep pixels with no
  vegetation type (no cohorts, e.g. burned and not regenerated, or not simulated) in the
  transitions dataset and plots, labelled `"_NA_"`.
* Added `ggalluvial` and `ggrepel` to `reqdPkgs`: `LandR::plotVegTransitions()` stops without
  them, and LandR only suggests them.

# Biomass_core 2.0.2 (2026-06-02)

* Metadata cleanup: removed the unused `dataSource` field; added `speciesLayers` and `sppNameVector` to `createsOutput` so they are declared module outputs.
* Added `# nolint` annotations to satisfy linting; minor message-formatting fix.

# Biomass_core 2.0.1 (2026-03-12)

* Added species transition plots (large refactor/reorganization of `Biomass_core.R`).
* Climate-sensitive fixes: corrected handling for `LandR.CS` and switched to its development version rather than master.
* Fixed summary plotting for `ggplot2` v4.0 compatibility.
* Fixed partial argument-name matches; merged upstream contributor PR #99.

# Biomass_core 2.0.0 (2025-10-21)

* Major bump to support SCANFI-derived inputs.
* Replaced the deprecated `crayon` package with `cli` for console messaging.
* Simplified `.inputObjects`, removing the unused SAL input.
* Correctness fixes: guarded `minCohortBiomass`, added a check for `NA` `initialB`, resolved lat/lon handling with `sf`, fixed cohort-handling issues, and removed a double-`spades()` typo.
* Plotting/reporting: changed default `.plots` to `png`, fixed a plotting bug, fixed `SpatRaster` colours, resolved warnings/colours, and repaired Rmd manual rendering; removed a leftover `browser()`.
* Dependency bumps (`LandR`, `SpaDES.core`, `quickplot`) and CI/workflow updates for module-Rmd rendering.

# Biomass_core 1.4.3 (2024-06-07)

* Documentation/manual overhaul with a GitHub Actions workflow to rebuild the `.Rmd` module manual; badge and formatting fixes.
* SSL handling for the NFI FTP server via `P(sim)$.sslVerify` (as integer); require `openxlsx`.
* Bugfix: `Plots` now requires `usePlot = TRUE` when `fn` is not passed.
* `SpatRaster` colours fix and `quickplot`/`SpaDES.core` dependency bumps.

# Biomass_core 1.3.x (2021 to 2022)

* Baseline of the 1.3 series (pre-2023): core LANDIS-II-style biomass succession simulation, module metadata, and manual establishment.
* Several speed-ups (see [PR #54](https://github.com/PredictiveEcology/Biomass_core/pull/54)).

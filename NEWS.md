Known issues: <https://github.com/PredictiveEcology/Biomass_core/issues>

# Biomass_core (development version)

* `WardDispersalSeeding()` reads the `pixelGroupMap` values once and passes them to `LANDISDisp()` as `pgv` (burned pixels set to `NA`), instead of copying the raster and reading its values again inside `LANDISDisp()`. Results are unchanged; saves about 1.7 s per call on a 9M-cell map. Needs the `pgv` argument of `LANDISDisp()` (PredictiveEcology/LandR#255); no version floor is set until that is released.
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

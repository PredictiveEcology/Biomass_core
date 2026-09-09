Known issues: <https://github.com/PredictiveEcology/Biomass_core/issues>

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

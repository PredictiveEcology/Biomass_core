library(data.table)

squashCD <- function() {
  ## sp 1: 31 (untouched), 91 (alone in (80,100]), 101 and 111 (both in (100,120]), 181 (alone)
  ## sp 2: 91 (alone)
  data.table(pixelGroup = 1L, ecoregionGroup = 1L, speciesCode = c(rep(1L, 5), 2L),
             age = c(31L, 91L, 101L, 111L, 181L, 91L),
             B = c(100, 50, 100, 300, 70, 40),
             mortality = c(1, 2, 3, 4, 5, 6), aNPPAct = c(10, 20, 30, 40, 50, 60))
}

## cohorts given as years since establishment (ya); age = ya + 1
mk <- function(ya, B = 100, sp = 1L) data.table(pixelGroup = 1L, ecoregionGroup = 1L, speciesCode = sp,
                                              age = as.integer(ya + 1), B = B, mortality = 1, aNPPAct = 10)
sq <- function(cd, w, interval = 100) squashCohortAges(cd, interval, w, successionTimestep = 10)
yaOf <- function(cd) sort(cd$age - 1)

test_that("squashCohortAges() merges an anchor with the cohorts in its window (single width)", {
  cd <- rbind(mk(c(60, 80, 100, 130), B = c(50, 100, 300, 70)), mk(100, B = 40, sp = 2L))
  out <- sq(cd, 20)
  setkeyv(out, c("speciesCode", "age"))
  expect_equal(NROW(out), 4L)
  expect_identical(colnames(out), colnames(cd))
  expect_false(anyDuplicated(out, by = c("pixelGroup", "speciesCode", "age")) > 0)
  ## 80 (B100) and 100 (B300): B-weighted mean ya = 95, not moved to the grid
  m <- out[out$speciesCode == 1L & out$age == 96L, ]
  expect_equal(c(m$B, m$mortality, m$aNPPAct), c(400, 2, 20))
  expect_equal(yaOf(out[out$speciesCode == 1L, ]), c(60, 95, 130))
  expect_equal(out[out$speciesCode == 1L & out$age == 61L, ]$B, 50)
  expect_equal(out[out$speciesCode == 2L, ]$B, 40)  # lone anchor: unchanged
  expect_equal(sum(out$B), sum(cd$B))
})

test_that("squashCohortAges() follows the worked example with widening widths", {
  w <- c(20, 30, 40, 50, 60, 70, 80)
  cd <- mk(c(10, 30, 50, 70, 90))
  ageBy <- function(cd, n) { cd <- copy(cd); cd$age <- cd$age + as.integer(n); cd }
  cd <- sq(ageBy(cd, 10), w)  # 20 40 60 80 100: 100 anchors, merges 80
  expect_equal(yaOf(cd), c(20, 40, 60, 90))
  expect_equal(max(cd$B), 200)
  cd <- sq(ageBy(cd, 40), w)  # 60 80 100 130: 100 anchors, merges 80
  expect_equal(yaOf(cd), c(60, 90, 130))
  cd <- sq(ageBy(cd, 60), w)  # 120 150 190: no anchor
  expect_equal(yaOf(cd), c(120, 150, 190))
  cd <- sq(ageBy(cd, 10), w)  # 130 160 200: anchor 200 (k = 2, w = 30), nothing in [170, 200]
  expect_equal(yaOf(cd), c(130, 160, 200))
  ## the last width repeats: with w = 20 only, k = 2 reaches back to 180
  expect_equal(NROW(sq(mk(c(170, 180, 200)), 20)), 2L)
  expect_equal(NROW(sq(mk(c(170, 180, 200)), c(20, 30))), 1L)
})

test_that("squashCohortAges() anchors an off-grid cohort that crosses a multiple of the interval", {
  ## ya 93 is off the grid; ten years later it is 103, having crossed 100, and merges with 83 (in [80, 110))
  cd <- mk(c(73, 93))
  cd$age <- cd$age + 10L
  out <- sq(cd, 20)
  expect_equal(yaOf(out), 93)  # (83 + 103) / 2, equal B
  expect_equal(out$B, 200)
  ## a cohort at ya 93 does not anchor: it has not crossed 100
  expect_identical(sq(mk(c(73, 93)), 20), mk(c(73, 93)))
  expect_type(out$age, "integer")
})

test_that("squashCohortAges() returns the input with a NA interval", {
  cd <- mk(c(80, 100))
  expect_identical(squashCohortAges(cd, NA, 20, 10), cd)
})

test_that("suppressedCohortsToRemove() removes only old, small cohorts and keeps a species' largest", {
  cd <- data.table(pixelGroup = 1L, speciesCode = c(1L, 1L, 1L, 2L, 3L, 3L),
                   age = c(90L, 20L, 90L, 90L, 90L, 90L),
                   longevity = 100L,
                   B = c(5, 5, 5000, 3000, 4, 3))
  ## total B = 8017; threshold 0.01 -> 80.17
  res <- suppressedCohortsToRemove(cd, relativeB = 0.01, minAgeFrac = 0.75)
  expect_identical(res, c(TRUE,   # old, small, species 1 has a large cohort
                          FALSE,  # young, small
                          FALSE,  # old, large
                          FALSE,  # old large
                          FALSE,  # species 3: both qualify; largest (B = 4) kept
                          TRUE))
  expect_false(any(suppressedCohortsToRemove(cd, NA, 0.75)))
})

test_that("checkCohortSquashParams() validates interval and widths", {
  expect_null(checkCohortSquashParams(NA, 7, 10))
  expect_null(checkCohortSquashParams(100, c(20, 30, 40), 10))
  expect_error(checkCohortSquashParams(105, 20, 10), "multiple of successionTimestep")
  expect_error(checkCohortSquashParams(100, c(20, 35), 10), "multiples of successionTimestep")
  expect_error(checkCohortSquashParams(100, 0, 10), "multiples of successionTimestep")
})

## pixelGroup 1: species 1 has 3 older cohorts and a new one (age 1), species 2 has 2 older cohorts;
## pixelGroup 2: species 1 has 3 older cohorts
condCD <- function() {
  data.table(pixelGroup = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L),
             speciesCode = c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L),
             age = c(1L, 21L, 41L, 61L, 31L, 71L, 11L, 21L, 31L))
}
condBlocked <- function(time, interval = 20, n = 2) conditionalTimestepBlocked(condCD(), time, interval, n)

test_that("conditionalTimestepBlocked() blocks nothing by default (NA interval)", {
  out <- condBlocked(10, interval = NA)
  expect_identical(colnames(out), c("pixelGroup", "speciesCode"))
  expect_identical(NROW(out), 0L)
})

test_that("conditionalTimestepBlocked() blocks species with more than n cohorts off the interval only", {
  off <- condBlocked(10)
  setorderv(off, c("pixelGroup", "speciesCode"))
  ## new (age 1) cohorts do not count: species 1 in pixelGroup 1 has 3 older cohorts > 2
  expect_identical(off$pixelGroup, c(1L, 2L))
  expect_identical(off$speciesCode, c(1L, 1L))
  expect_identical(NROW(condBlocked(20)), 0L)  # on the interval
  expect_identical(NROW(condBlocked(40)), 0L)
  ## n = 3: nothing has more than 3 older cohorts; n = 0: every pair with an older cohort
  expect_identical(NROW(condBlocked(10, n = 3)), 0L)
  expect_identical(NROW(condBlocked(10, n = 0)), 3L)
})

test_that("conditionalTimestepBlocked() never blocks a species with <= n cohorts", {
  off <- condBlocked(10)
  expect_false(any(off$pixelGroup == 1L & off$speciesCode == 2L))  # 2 older cohorts
})

test_that("checkConditionalTimestepParams() needs a positive multiple of successionTimestep", {
  expect_null(checkConditionalTimestepParams(NA, 10))
  expect_null(checkConditionalTimestepParams(20, 10))
  expect_error(checkConditionalTimestepParams(25, 10), "multiple of successionTimestep")
  expect_error(checkConditionalTimestepParams(0, 10), "multiple of successionTimestep")
  expect_error(checkConditionalTimestepParams(-10, 10), "multiple of successionTimestep")
})

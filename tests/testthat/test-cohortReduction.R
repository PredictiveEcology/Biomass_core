library(data.table)

squashCD <- function() {
  ## sp 1: 31 (untouched), 91 (alone in (80,100]), 101 and 111 (both in (100,120]), 181 (alone)
  ## sp 2: 91 (alone)
  data.table(pixelGroup = 1L, ecoregionGroup = 1L, speciesCode = c(rep(1L, 5), 2L),
             age = c(31L, 91L, 101L, 111L, 181L, 91L),
             B = c(100, 50, 100, 300, 70, 40),
             mortality = c(1, 2, 3, 4, 5, 6), aNPPAct = c(10, 20, 30, 40, 50, 60))
}

mk <- function(age, B = 100) data.table(pixelGroup = 1L, ecoregionGroup = 1L, speciesCode = 1L,
                                         age = as.integer(age), B = B, mortality = 1, aNPPAct = 10)
sq <- function(cd, t, w, interval = 100) squashCohortAges(cd, interval, t, 0, w, successionTimestep = 10)

test_that("squashCohortAges() merges cohorts in the same window and leaves the rest", {
  cd <- mk(c(81, 91, 101, 161, 171, 181, 191), B = c(100, 300, 50, 60, 10, 10, 20))
  cd <- rbind(cd, data.table(pixelGroup = 1L, ecoregionGroup = 1L, speciesCode = 2L, age = 91L,
                             B = 40, mortality = 1, aNPPAct = 10))
  out <- sq(cd, 100, c(20, 30))
  setkeyv(out, c("speciesCode", "age"))
  expect_equal(NROW(out), 5L)
  expect_identical(colnames(out), colnames(cd))
  expect_false(anyDuplicated(out, by = c("pixelGroup", "speciesCode", "age")) > 0)
  ## 81 (B100) and 91 (B300): mean(age - 1) = 87.5 -> round(8.75) = 9 -> 91
  m1 <- out[out$speciesCode == 1L & out$age == 91L, ]
  expect_equal(c(m1$B, m1$mortality, m1$aNPPAct), c(400, 2, 20))
  ## 171, 181, 191 (B 10, 10, 20): mean(age - 1) = 182.5 -> round(18.25) = 18 -> 181
  m2 <- out[out$speciesCode == 1L & out$age == 181L, ]
  expect_equal(c(m2$B, m2$mortality, m2$aNPPAct), c(40, 3, 30))
  expect_equal(out[out$speciesCode == 1L & out$age == 101L, ]$B, 50)
  expect_equal(out[out$speciesCode == 1L & out$age == 161L, ]$B, 60)
  expect_equal(out[out$speciesCode == 2L, ]$B, 40)  # a lone cohort in its window
  expect_equal(sum(out$B), sum(cd$B))
})

test_that("squashCohortAges() repeats the last width", {
  cd <- mk(c(171, 181, 191))
  expect_equal(NROW(sq(cd, 100, 20)), 2L)         # k = 2 window is (180, 200]: 181 + 191 merge
  expect_equal(NROW(sq(mk(c(181, 191)), 100, 20)), 1L)
  expect_equal(NROW(sq(cd, 100, c(20, 30, 40))), 1L)
})

test_that("squashCohortAges() does nothing with NA interval, at start, or off schedule", {
  cd <- mk(c(81, 91))
  expect_identical(squashCohortAges(cd, NA, 100, 0, 20, 10), cd)
  expect_identical(sq(cd, 0, 20), cd)
  expect_identical(sq(cd, 50, 20), cd)
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

test_that("squashCohortAges(): widening windows join a merged cohort with neighbours at the next squash", {
  w <- c(20, 30, 40)
  cd <- mk(c(81, 91, 131, 151, 171), B = 100)
  s1 <- sq(cd, 100, w)  # 81 and 91 merge (-> 91); 131, 151, 171 are between windows or alone
  expect_equal(NROW(s1), 4L)
  s1$age <- s1$age + 100L   # 100 years later: 191 (merged), 231, 251, 271
  s1 <- rbind(s1, mk(c(171, 181), B = 100))  # cohorts now in (170, 200] with the merged 191
  s2 <- sq(s1, 200, w)
  expect_equal(max(s2$B), 400)  # 191 (B 200) + 171 + 181 (100 each) merged
  expect_lt(NROW(s2), NROW(s1))
  ## one width: the merged cohort's age modulo the interval is unchanged and nothing joins it
  f <- sq(rbind(mk(c(81, 91)), mk(171)), 100, 20)
  f$age <- f$age + 100L
  expect_equal(NROW(sq(f, 200, 20)), NROW(f))
})

test_that("checkCohortSquashParams() validates interval and widths", {
  expect_null(checkCohortSquashParams(NA, 7, 10))
  expect_null(checkCohortSquashParams(100, c(20, 30, 40), 10))
  expect_error(checkCohortSquashParams(105, 20, 10), "multiple of successionTimestep")
  expect_error(checkCohortSquashParams(100, c(20, 35), 10), "multiples of successionTimestep")
  expect_error(checkCohortSquashParams(100, 120, 10), "must not exceed")
})

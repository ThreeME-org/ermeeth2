## A scenario should leave the base year, which is the calibration, alone.

calib_fixture <- function() {
  data.frame(year = 2018:2021,
             exo_a = c(1, 1, 1, 1),
             exo_b = c(10, 10, 10, 10),
             endo  = c(5, 5, 5, 5))
}

test_that("base_year_changes names the exogenous variables changed at the base year", {
  ref <- calib_fixture()
  ch <- ref[, c("year", "exo_a", "exo_b", "endo")]
  ch$exo_a[ch$year == 2019] <- 2      # base year: flagged
  ch$exo_b[ch$year >= 2020] <- 20     # after the base year: fine
  ch$endo[ch$year == 2019] <- 6       # endogenous: not checked

  expect_equal(base_year_changes(ch, ref, 2019, endogenous = "ENDO"), "exo_a")
  expect_equal(base_year_changes(ch, ref, 2019), c("exo_a", "endo"))
})

test_that("base_year_changes is quiet when the base year is untouched", {
  ref <- calib_fixture()
  ch <- ref[, c("year", "exo_a")]
  ch$exo_a[ch$year >= 2020] <- 3
  expect_length(base_year_changes(ch, ref, 2019), 0)
  # rounding noise is not a change
  ch$exo_a[ch$year == 2019] <- 1 + 1e-14
  expect_length(base_year_changes(ch, ref, 2019), 0)
  # a script that does not return the base year at all
  expect_length(base_year_changes(ch[ch$year > 2019, ], ref, 2019), 0)
  # a variable the reference does not have is not compared
  ch$brand_new <- 1
  expect_length(base_year_changes(ch, ref, 2019), 0)
})

test_that("base_year_changes sees a value that appears or disappears", {
  ref <- calib_fixture()
  ch <- ref[, c("year", "exo_a")]
  ch$exo_a[ch$year == 2019] <- NA
  expect_equal(base_year_changes(ch, ref, 2019), "exo_a")
})

test_that("lag_year_changes flags every variable changed before the base year", {
  ref <- data.frame(year = 2016:2021, exo = 1, endo = 5, other = 2)
  ch <- ref
  ch$endo[ch$year == 2017] <- 6       # endogenous, before the base year: flagged
  ch$exo[ch$year == 2019] <- 9        # the base year is not this check's business
  ch$other[ch$year == 2021] <- 3      # after: fine

  expect_equal(lag_year_changes(ch, ref, 2019), "endo")
  ch$exo[ch$year == 2016] <- 0
  expect_equal(lag_year_changes(ch, ref, 2019), c("exo", "endo"))
  # a script that returns no year before the base year
  expect_length(lag_year_changes(ch[ch$year >= 2019, ], ref, 2019), 0)
  # rows in another order are matched by year
  expect_equal(lag_year_changes(ch[rev(seq_len(nrow(ch))), ], ref, 2019), c("exo", "endo"))
})

test_that("warn_base_year_changes only speaks when there is something to say", {
  expect_silent(warn_base_year_changes(character(0), "Scenario ct1", 2019))
  expect_message(warn_base_year_changes(c("exo_a", "exo_b"), "Scenario ct1", 2019),
                 "Scenario ct1 changes 2 exogenous variables at the base year \\(2019\\)")
  expect_message(warn_base_year_changes("endo", "Scenario ct1", 2019, before = TRUE),
                 "Scenario ct1 changes 1 variable before the base year \\(2019\\)")
})

test_that("model_prg_endogenous takes the first variable on the left-hand side", {
  expect_equal(
    model_prg_endogenous(c("y = c + i", "p*q = v", "dlog(w) = dlog(p)", "(k + 1) = 2 * l")),
    c("y", "p", "w", "k"))
})

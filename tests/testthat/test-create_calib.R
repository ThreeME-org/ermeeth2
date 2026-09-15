test_that("calib file names follow the convention readconfig() expects", {
  # readconfig() builds configuration/scenarii_calib/1_calib_<scenario_baseline>.R
  expect_equal(calib_file_name("baseline-steady", "baseline"), "1_calib_baseline-steady.R")
  expect_equal(calib_file_name("baseline", "baseline"), "1_calib_baseline.R")
  expect_equal(calib_file_name("ct1", "shock"), "2_calib_shock_ct1.R")
  expect_equal(calib_path("ct1", "shock", path = "x"), file.path("x", "2_calib_shock_ct1.R"))
})

test_that("a baseline name gains the baseline prefix the file convention needs", {
  expect_equal(calib_scenario_name("ademe", "baseline"), "baseline_ademe")
  expect_equal(calib_scenario_name("baseline_ademe", "baseline"), "baseline_ademe")
  expect_equal(calib_scenario_name("baseline-steady", "baseline"), "baseline-steady")
  # shocks are left alone
  expect_equal(calib_scenario_name("ct1", "shock"), "ct1")
})

test_that("invalid scenario names are refused, with the reason", {
  expect_error(calib_scenario_name("CT1", "shock"), "lower case")
  expect_error(calib_scenario_name("1ct", "shock"), "start with a letter")
  expect_error(calib_scenario_name("ct 1", "shock"), "start with a letter")
  expect_error(calib_scenario_name("ct/1", "shock"), "start with a letter")
  expect_error(calib_scenario_name("", "shock"), "non-empty")
  expect_error(calib_scenario_name(c("a", "b"), "shock"), "single non-empty")
})

test_that("create_shock writes a script that parses and defines shock_ch", {
  d <- withr::local_tempdir()
  f <- create_shock("ct2", title = "2 GDP points", path = d, open = FALSE, quiet = TRUE)
  expect_true(file.exists(f))
  expect_equal(basename(f), "2_calib_shock_ct2.R")
  txt <- readLines(f)
  expect_no_error(parse(text = txt))
  expect_true(any(grepl("shock_ch", txt)))
  expect_true(any(grepl("2 GDP points", txt)))
  expect_false(any(grepl("\\{\\{", txt)))   # every placeholder filled
})

test_that("create_baseline writes a script that parses and defines baseline_ch", {
  d <- withr::local_tempdir()
  f <- create_baseline("ademe", path = d, open = FALSE, quiet = TRUE)
  expect_equal(basename(f), "1_calib_baseline_ademe.R")
  txt <- readLines(f)
  expect_no_error(parse(text = txt))
  expect_true(any(grepl("baseline_ch", txt)))
  expect_true(any(grepl("OGcalib", txt)))
})

test_that("the templates apply a neutral transformation, so a new scenario is a no-op", {
  d <- withr::local_tempdir()
  # world demand multiplied by 1: the skeleton runs, and changes nothing
  txt <- readLines(create_shock("neutral", path = d, open = FALSE, quiet = TRUE))
  expect_true(any(grepl("dwd_c01 \\* 1\\b", txt)))
  expect_true(any(grepl("shockyear", txt)))
})

test_that("create_calib refuses to clobber an existing scenario", {
  d <- withr::local_tempdir()
  create_shock("ct2", path = d, open = FALSE, quiet = TRUE)
  expect_error(create_shock("ct2", path = d, open = FALSE, quiet = TRUE), "already exists")
  expect_no_error(create_shock("ct2", path = d, open = FALSE, quiet = TRUE,
                               overwrite = TRUE))
})

test_that("create_calib says where it is meant to be run from", {
  expect_error(create_shock("ct2", path = file.path(tempdir(), "nope"),
                            open = FALSE, quiet = TRUE),
               "no such folder")
})

test_that("create_calib tells you how to point the configuration at the new file", {
  d <- withr::local_tempdir()
  expect_message(create_shock("ct2", path = d, open = FALSE), 'scenario = c\\("ct2"\\)')
  expect_message(create_baseline("x", path = d, open = FALSE),
                 'scenario_baseline = "baseline_x"')
})

test_that("list_calibs reads back what create_* wrote", {
  d <- withr::local_tempdir()
  create_baseline("steady", path = d, open = FALSE, quiet = TRUE)
  create_shock("ct1", path = d, open = FALSE, quiet = TRUE)
  create_shock("wd1", path = d, open = FALSE, quiet = TRUE)
  cal <- list_calibs(d)
  expect_setequal(cal$name, c("baseline_steady", "ct1", "wd1"))
  expect_equal(sum(cal$type == "shock"), 2L)
  expect_setequal(list_calibs(d, "shock")$name, c("ct1", "wd1"))
  # a folder that is not there is empty, not an error
  expect_equal(nrow(list_calibs(file.path(d, "nope"))), 0L)
})

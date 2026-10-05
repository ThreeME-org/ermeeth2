## The aggregation rules are a csv file, one table for both dimensions.

test_that("the packaged rules read, one table per dimension", {
  f <- system.file("aggregation_rules.csv", package = "ermeeth2")
  expect_true(nzchar(f))
  for (which in c("sectors", "commodities")) {
    r <- read_aggregation_rules(f, which)
    expect_named(r, c("var_root", "sum", "mean", "weighted_mean", "weight_var"))
    expect_gt(nrow(r), 200)
    expect_false(anyDuplicated(r$var_root) > 0)
    # exactly one rule applies to each variable
    expect_true(all(r$sum + r$mean + r$weighted_mean == 1))
    expect_type(r$sum, "double")
    expect_type(r$weight_var, "character")
    # an empty weight is missing, not an empty string
    expect_false(any(r$weight_var == "", na.rm = TRUE))
    # every weighted mean names its weight
    expect_false(any(is.na(r$weight_var[r$weighted_mean == 1])))
  }
})

test_that("read_aggregation_rules takes a semicolon file and refuses another table", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("sec_com;var_root;sum;mean;weighted_mean;weight_var",
               "sectors;Y;1;0;0;",
               "sectors;PY;0;0;1;Y",
               "commodities;CH;1;0;0;"), f)
  s <- read_aggregation_rules(f, "sectors")
  expect_equal(s$var_root, c("Y", "PY"))
  expect_equal(s$weight_var, c(NA, "Y"))
  expect_equal(read_aggregation_rules(f, "commodities")$var_root, "CH")

  writeLines(c("var_root,sum", "Y,1"), f)
  expect_error(read_aggregation_rules(f), "sec_com")
  expect_error(read_aggregation_rules(file.path(tempdir(), "nope.csv")), "no such file")
})

test_that("the packaged table says where each rule comes from", {
  all_rules <- data.table::fread(system.file("aggregation_rules.csv", package = "ermeeth2"),
                                 data.table = FALSE, na.strings = c("", "NA"))
  expect_true(all(c("manual_add", "checked") %in% names(all_rules)))
  expect_true(all(all_rules$manual_add %in% c(0, 1)))
  expect_true(all(all_rules$checked %in% c(0, 1)))
  expect_true(any(all_rules$manual_add == 1) && any(all_rules$manual_add == 0))
  # one weight at most, and only for a weighted mean
  expect_false(any(grepl("[ ,;+*]", all_rules$weight_var), na.rm = TRUE))
  expect_true(all(is.na(all_rules$weight_var[all_rules$weighted_mean == 0])))
  expect_false(anyDuplicated(all_rules[, c("sec_com", "var_root")]) > 0)
})

test_that("the default rules are the packaged ones, whatever the project holds", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  expect_silent(n <- nrow(aggregation_rules("aggregation_rules", "sectors")))
  expect_gt(n, 200)

  dir.create(file.path("src", "bridges"), recursive = TRUE)
  writeLines(c("sec_com,var_root,sum,mean,weighted_mean,weight_var",
               "sectors,Y,1,0,0,"), file.path("src", "bridges", "aggregation_rules.csv"))
  # a copy in the project is not read, and that is said
  expect_message(r <- aggregation_rules("aggregation_rules", "sectors"), "not read")
  expect_equal(nrow(r), n)

  # a named set of rules found in the project is used
  writeLines(c("sec_com,var_root,sum,mean,weighted_mean,weight_var",
               "commodities,CH,0,1,0,"), file.path("src", "bridges", "mine.csv"))
  expect_equal(aggregation_rules("mine", "commodities")$var_root, "CH")
})

test_that("a leftover workbook is pointed out rather than silently ignored", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create(file.path("src", "bridges"), recursive = TRUE)
  file.create(file.path("src", "bridges", "aggregation_rules.xlsx"))
  expect_message(r <- aggregation_rules("aggregation_rules", "sectors"), "not read")
  expect_gt(nrow(r), 200)
})

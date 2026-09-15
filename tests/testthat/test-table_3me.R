mini <- function() {
  readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
}

test_that("table_3me returns a gt table with the expected shape", {
  d <- mini()
  tb <- table_3me(d, c("Y", "CH", "I"), theme = "none")
  expect_s3_class(tb, "gt_tbl")

  body <- as.data.frame(tb)
  expect_equal(nrow(body), 3)
  expect_equal(body$label, c("Y", "CH", "I"))
  ## unit column, plus one column per horizon (t, t+1, t+2, t+5, t+10, long term)
  expect_equal(ncol(body), 1 + 1 + 6)
})

test_that("the shock year is detected from the data", {
  d <- mini()
  expect_equal(detect_shock_year(d), 2021)

  ## With no divergence at all, it falls back to the first year.
  flat <- d
  flat$values <- flat$values_ref
  expect_equal(detect_shock_year(flat), min(d$year))
})

test_that("horizons past the end of the simulation are dropped", {
  d <- mini()
  tb <- table_3me(d, "Y", horizons = c(0, 1, 500), theme = "none")
  ## t, t+1 and the long term column survive; t+500 does not.
  expect_equal(ncol(as.data.frame(tb)), 1 + 1 + 3)
})

test_that("variables given as a named list become row groups", {
  d <- mini()
  tb <- table_3me(
    d,
    variables = list(Activity = c("Y", "CH"), Policy = c("G")),
    theme = "none"
  )
  expect_true("group" %in% names(tb[["_data"]]))
  expect_setequal(unique(tb[["_data"]]$group), c("Activity", "Policy"))
})

test_that("transformations can be mixed, and the unit column records them", {
  d <- mini()
  tb <- table_3me(d, c("Y", "MU"), transformation = c(MU = "ppdiff", "reldiff"),
                  theme = "none")
  body <- as.data.frame(tb)
  expect_equal(body$unit[body$label == "Y"], "%")
  expect_equal(body$unit[body$label == "MU"], "pp")
})

test_that("percentage transformations are displayed on a percent scale", {
  d <- mini()
  tb <- table_3me(d, "Y", horizons = 5, theme = "none")
  raw <- threeme_transform(d[d$variable == "Y", ], "reldiff")
  expected <- raw$value[raw$year == detect_shock_year(d) + 5] * 100
  ## as.data.frame() on a gt table returns the formatted cells, as strings.
  expect_equal(unname(unlist(as.data.frame(tb)[1, 3])),
               format(round(expected, 2), nsmall = 2))
})

test_that("a second database adds a spanner level rather than more rows", {
  d <- mini()
  d2 <- d
  d2$values <- ifelse(d2$scenario != "baseline", d2$values * 1.01, d2$values)

  tb <- table_3me(d, c("Y", "CH"), data_secondary = d2,
                  model_names = c("v4.4", "v4.5"), theme = "none")
  body <- as.data.frame(tb)
  expect_equal(nrow(body), 2)

  spanners <- tb[["_spanners"]]
  expect_true(all(c("v4.4", "v4.5") %in% unlist(spanners$spanner_label)))
  expect_true(any(spanners$spanner_level == 2))
})

test_that("labels rename rows, and missing variables are reported", {
  d <- mini()
  tb <- table_3me(d, c("Y", "CH"), labels = c(Y = "GDP"), theme = "none")
  expect_equal(as.data.frame(tb)$label, c("GDP", "CH"))

  expect_warning(table_3me(d, c("Y", "NOT_A_VAR"), theme = "none"), "not found")
  expect_error(table_3me(d, "NOT_A_VAR", theme = "none"), "None of these variables")
})

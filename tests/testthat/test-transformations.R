mini <- function() {
  readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
}

test_that("the registry describes every transformation consistently", {
  specs <- threeme_transformations()
  expect_true(all(c("level", "reldiff", "diff", "ppdiff", "gr", "index100") %in%
                    names(specs)))
  for (nm in names(specs)) {
    s <- specs[[nm]]
    expect_identical(s$id, nm)
    expect_type(s$needs_ref, "logical")
    expect_type(s$drop_baseline, "logical")
    expect_type(s$percent, "logical")
  }
  ## Only the transformations measured against a reference drop the baseline.
  for (nm in names(specs)) {
    if (specs[[nm]]$drop_baseline) expect_true(specs[[nm]]$needs_ref)
  }
})

test_that("reldiff and diff match their definitions and drop the baseline", {
  d <- mini()
  rel <- threeme_transform(d, "reldiff")
  expect_false("baseline" %in% rel$scenario)
  expect_equal(rel$value, rel$values / rel$values_ref - 1)
  expect_true(all(rel$unit == "%"))

  dif <- threeme_transform(d, "diff")
  expect_equal(dif$value, dif$values - dif$values_ref)
})

test_that("the growth rate is computed within a series, not across them", {
  d <- mini()
  gr <- threeme_transform(d, "gr")
  ## One NA per (variable, scenario) series: the first year has no lag.
  n_series <- nrow(unique(d[, c("variable", "scenario")]))
  expect_equal(sum(is.na(gr$value)), n_series)

  one <- gr[gr$variable == "Y" & gr$scenario == "baseline", ]
  one <- one[order(one$year), ]
  expect_equal(one$value[-1], one$values[-1] / head(one$values, -1) - 1)
})

test_that("index100 is 100 in the base year", {
  d <- mini()
  idx <- threeme_transform(d, "index100", base_year = 2016)
  expect_true(all(idx$value[idx$year == 2016] == 100))

  idx2 <- threeme_transform(d, "index100", base_year = 2030)
  expect_true(all(idx2$value[idx2$year == 2030] == 100))
})

test_that("transformations can be mixed by variable, with an unnamed default", {
  d <- mini()
  out <- threeme_transform(d, c(MU = "ppdiff", "level"))
  by_var <- unique(out[, c("variable", "transformation")])
  expect_equal(by_var$transformation[by_var$variable == "MU"], "ppdiff")
  expect_true(all(by_var$transformation[by_var$variable != "MU"] == "level"))
})

test_that("missing inputs are reported rather than silently ignored", {
  d <- mini()
  expect_error(threeme_transform(d[, c("year", "variable")], "level"),
               "missing the column")
  expect_error(threeme_transform(d[, setdiff(names(d), "values_ref")], "reldiff"),
               "values_ref")
  expect_error(threeme_transform(d, "nonsense"))
})

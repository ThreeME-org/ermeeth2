mini <- function() {
  readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
}

test_that("simple_plot returns a ggplot and honours the transformation", {
  d <- mini()
  p <- simple_plot(d, c("Y", "CH", "I"))
  expect_s3_class(p, "ggplot")

  ## The baseline is dropped for reldiff, so only the shock is plotted.
  expect_false("baseline" %in% p$data$scenario)
  expect_setequal(unique(p$data$variable), c("Y", "CH", "I"))

  lvl <- simple_plot(d, "Y", transformation = "level")
  expect_true("baseline" %in% lvl$data$scenario)
})

test_that("the scenario argument is not shadowed by the scenario column", {
  ## The src original filtered with `scenario %in% scenario`, which is always
  ## TRUE. Passing a scenario that exists must actually narrow the data.
  d <- mini()
  p <- simple_plot(d, "Y", scenarios = "g", transformation = "level")
  expect_setequal(unique(p$data$scenario), "g")
})

test_that("the year window is respected", {
  d <- mini()
  p <- simple_plot(d, "Y", startyear = 2025, endyear = 2035)
  expect_gte(min(p$data$year), 2025)
  expect_lte(max(p$data$year), 2035)
})

test_that("labels rename the series without touching the codes", {
  d <- mini()
  p <- simple_plot(d, c("Y", "CH"), labels = c(Y = "GDP"))
  expect_true("GDP" %in% p$data$label)
  expect_true("CH" %in% p$data$label)
  expect_setequal(unique(p$data$variable), c("Y", "CH"))
})

test_that("unknown variables warn, and an entirely unknown set errors", {
  d <- mini()
  expect_warning(simple_plot(d, c("Y", "NOT_A_VAR")), "not found")
  expect_error(simple_plot(d, "NOT_A_VAR"), "None of these variables")
})

test_that("a plot refuses mixed transformations", {
  d <- mini()
  expect_error(simple_plot(d, c("Y", "MU"), transformation = c(MU = "ppdiff", "reldiff")),
               "single `transformation`")
})

test_that("the interactive version adds a tooltip layer", {
  d <- mini()
  p <- simple_plot(d, "Y", interactive = TRUE)
  ## ofce::girafy() returns a plain ggplot outside html/interactive contexts,
  ## so what is checked here is that the interactive layer was added.
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("interactive", layers, ignore.case = TRUE)))
  expect_true(all(c("level", "baseline", "growth", "rel. diff") |>
                    vapply(function(x) any(grepl(x, p$data$.tooltip, fixed = TRUE)),
                           logical(1))))
})

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
  # the variable `labels` does not name takes its label from the dictionary
  expect_true("Household consumption" %in% p$data$label)
  expect_setequal(unique(p$data$variable), c("Y", "CH"))
})

test_that("the labels come from the dictionary, in the language asked for", {
  d <- mini()
  expect_setequal(simple_plot(d, c("Y", "CH"))$data$label, c("Output", "Household consumption"))
  expect_setequal(simple_plot(d, c("Y", "CH"), lang = "fr")$data$label,
                  c("Production", "Consommation des ménages"))
  # FALSE shows the codes, a project dictionary comes on top of the packaged one
  expect_setequal(simple_plot(d, c("Y", "CH"), labels = FALSE)$data$label, c("Y", "CH"))
  mine <- data.frame(code = "Y", label_en = "My output")
  expect_true("My output" %in% simple_plot(d, c("Y", "CH"), dict = mine)$data$label)
})

test_that("the code of an indexed variable is cut back to the root the dictionary knows", {
  expect_equal(dictionary_labels_resolved("CH_CIND", n_index = 1), "Household consumption")
  expect_equal(dictionary_labels_resolved("CH_CIND"), "Household consumption (CIND)")
  # what is not the sector or the commodity stays in brackets
  expect_equal(dictionary_labels_resolved("EMS_CO2_SSER", n_index = 1), "GHG emissions (CO2)")
  expect_equal(dictionary_labels_resolved("EMS_CO2_SSER", n_index = 2), "GHG emissions (CO2)")
  # the longest root wins, an unknown variable keeps its code
  expect_equal(dictionary_labels_resolved("F_L_SSER", n_index = 1), dict_label("F_L"))
  expect_equal(dictionary_labels_resolved("NOT_A_VARIABLE"), "NOT_A_VARIABLE")
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

test_that("a dictionary label longer than force_short gives way to the code", {
  mine <- data.frame(code = c("Y", "CH"),
                     label_en = c("Output", "A label that is much longer than twenty-five characters"))
  expect_equal(unname(pretty_labels(c("Y", "CH"), dict = mine)), c("Output", "CH"))
  # 0: no limit; any other number is the limit
  expect_equal(unname(pretty_labels(c("Y", "CH"), dict = mine, force_short = 0)),
               c("Output", "A label that is much longer than twenty-five characters"))
  expect_equal(unname(pretty_labels(c("Y", "CH"), dict = mine, force_short = 3)), c("Y", "CH"))
  expect_equal(unname(pretty_labels("Y", dict = mine, force_short = 6)), "Output")
  # the labels the caller wrote are kept whatever their length
  long <- c(CH = "Written by hand, and also longer than the limit")
  expect_equal(unname(pretty_labels(c("Y", "CH"), labels = long, dict = mine)), c("Output", unname(long)))
  expect_error(pretty_labels("Y", force_short = "a"), "force_short")

  d <- mini()
  expect_setequal(simple_plot(d, c("Y", "CH"), dict = mine)$data$label, c("Output", "CH"))
  expect_true(any(nchar(simple_plot(d, c("Y", "CH"), dict = mine, force_short = 0)$data$label) > 25))
  # a table: the short label where there is one, and the same limit
  expect_equal(as.data.frame(table_3me(d, c("Y", "CH"), dict = mine, theme = "none"))$label,
               c("Output", "CH"))
  mine$label_short_en <- c(NA, "Consumption")
  expect_equal(as.data.frame(table_3me(d, c("Y", "CH"), dict = mine, theme = "none"))$label,
               c("Output", "Consumption"))
})

## readconfig() on incomplete files: what is compulsory, what has a default.

minimal_config <- function(drop = character(0), extra = character(0), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  lines <- c(
    project_name      = "project_name = \"test\"",
    model_folder      = "model_folder = \"threeme\"",
    scenario_baseline = "scenario_baseline = \"baseline-steady\"",
    scenario          = "scenario = c(\"ct1\", \"G1\")",
    baseyear          = "baseyear = 2019",
    lastyear          = "lastyear = 2050",
    lists_files       = "lists_files = \"lists.mdl\"",
    calib_files       = "calib_files = c(lists_files, \"calib.mdl\")",
    model_files       = "model_files = c(lists_files, \"eq.mdl\")")
  input <- file.path(d, "config_input_min.R")
  output <- file.path(d, "config_output_min.R")
  writeLines(c(unname(lines[setdiff(names(lines), drop)]), extra), input)
  writeLines("# nothing", output)
  list(input = input, output = output)
}

test_that("the compulsory options are exactly what a minimal file holds", {
  expect_setequal(config_required(),
                  c("project_name", "model_folder", "scenario_baseline", "scenario",
                    "baseyear", "lastyear",
                    "lists_files", "calib_files", "model_files"))
  # an option is compulsory or has a default, never both
  expect_length(intersect(config_required(), names(config_defaults())), 0)
})

test_that("a file with only the compulsory options reads, the rest by default", {
  f <- minimal_config()
  expect_message(cfg <- readconfig(f$input, f$output), "options not set in")
  inp <- cfg$input
  adv <- inp$advanced_config
  d <- config_defaults()

  expect_equal(inp$project_name, "test")
  expect_equal(inp$scenario, c("ct1", "g1"))
  expect_equal(inp$scenario_name, c("ct1_", "g1_"))
  expect_equal(inp$shocks_nb, 2)
  expect_equal(inp$iso3, "")
  expect_equal(inp$max_lags, 3)
  expect_equal(inp$shockyear, 2021)
  expect_equal(inp$firstyear, 2016)
  expect_false(inp$automated_shocks)
  expect_length(inp$variables_to_keep, 0)
  expect_equal(inp$calib_baseline,
               file.path(calib_folder_default(), "1_calib_baseline-steady.R"))
  expect_true(adv$Rsolver)
  expect_true(adv$recompile_model)
  expect_false(adv$skip_compiler)
  expect_equal(adv$tolerance_calib_check, 1e-3)
  expect_length(adv$output_saved, 0)
  expect_equal(adv$path_eviews_exe, "")
  expect_equal(adv[names(config_solver_defaults())], config_solver_defaults())
  # nothing in the configuration list is left empty for want of a default
  for (nm in intersect(names(d), names(adv))) expect_false(is.null(adv[[nm]]), info = nm)
  expect_equal(cfg$output, list(quartos_to_render = list(), quartos_parameters = list()))
})

test_that("a missing compulsory option stops the read and is named", {
  for (nm in config_required()) {
    f <- minimal_config(drop = nm)
    expect_error(readconfig(f$input, f$output, quiet = TRUE), nm, info = nm)
  }
  f <- minimal_config(drop = c("baseyear", "lastyear"))
  err <- tryCatch(readconfig(f$input, f$output, quiet = TRUE), error = function(e) conditionMessage(e))
  expect_match(err, "baseyear")
  expect_match(err, "lastyear")
  # set, but to nothing
  f <- minimal_config(drop = "scenario", extra = "scenario = c()")
  expect_error(readconfig(f$input, f$output, quiet = TRUE), "scenario")
})

test_that("what the file sets wins, and is not reported as defaulted", {
  f <- minimal_config(extra = c("max_lags = 5", "shockyear = 2025", "Rsolver = FALSE", "save_files_res = FALSE",
                                "output_saved = c(\"com\", \"sec\")", "iso3 = \"FRA\""))
  msgs <- testthat::capture_messages(cfg <- readconfig(f$input, f$output))
  expect_equal(cfg$input$max_lags, 5)
  expect_equal(cfg$input$shockyear, 2025)
  expect_equal(cfg$input$firstyear, 2014)
  expect_false(cfg$input$advanced_config$Rsolver)
  expect_false(cfg$input$advanced_config$save_files_res)
  expect_equal(cfg$input$advanced_config$output_saved, c("com", "sec"))
  expect_equal(cfg$input$scenario_name, c("ct1_fra", "g1_fra"))
  listed <- paste(msgs, collapse = " ")
  expect_false(grepl("max_lags|save_files_res|output_saved", listed))
  expect_match(listed, "Rcpp")
})

test_that("a file can use an option it leaves to its default", {
  f <- minimal_config(extra = "firstyear = baseyear - max_lags - 1")
  expect_equal(readconfig(f$input, f$output, quiet = TRUE)$input$firstyear, 2015)
  expect_silent(readconfig(f$input, f$output, quiet = TRUE))
  # and the addins read such a file too
  expect_equal(read_config_values(f$input)$firstyear, 2015)
  expect_false("max_lags" %in% names(read_config_values(f$input)))
})

test_that("a complete file is read without a word about defaults", {
  f <- minimal_config(extra = vapply(names(config_defaults()), function(nm) {
    paste0(nm, " = ", paste(deparse(config_defaults()[[nm]]), collapse = ""))
  }, character(1)))
  expect_silent(readconfig(f$input, f$output))
})

# Extracted from test-readconfig.R:34

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ermeeth2", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
minimal_config <- function(drop = character(0), extra = character(0), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  lines <- c(
    project_name      = "project_name = \"test\"",
    model_folder      = "model_folder = \"threeme\"",
    scenario_baseline = "scenario_baseline = \"baseline-steady\"",
    scenario          = "scenario = c(\"ct1\", \"G1\")",
    baseyear          = "baseyear = 2019",
    lastyear          = "lastyear = 2050",
    shockyear         = "shockyear = 2021",
    lists_files       = "lists_files = \"lists.mdl\"",
    calib_files       = "calib_files = c(lists_files, \"calib.mdl\")",
    model_files       = "model_files = c(lists_files, \"eq.mdl\")")
  input <- file.path(d, "config_input_min.R")
  output <- file.path(d, "config_output_min.R")
  writeLines(c(unname(lines[setdiff(names(lines), drop)]), extra), input)
  writeLines("# nothing", output)
  list(input = input, output = output)
}

# test -------------------------------------------------------------------------
f <- minimal_config()
expect_message(cfg <- readconfig(f$input, f$output), "left to their default")

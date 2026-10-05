# Extracted from test-solver_config.R:64

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ermeeth2", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
solver_config_files <- function(solver_lines, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  input <- file.path(d, "config_input_test.R")
  output <- file.path(d, "config_output_test.R")
  writeLines(c(
    "iso3 = \"FRA\"",
    "classification = \"c4_s4\"",
    "model_folder = \"threeme\"",
    "project_name = \"test\"",
    "scenario_baseline = \"baseline-steady\"",
    "scenario = \"ct1\"",
    "baseyear = 2019",
    "lastyear = 2050",
    "shockyear = 2021",
    "max_lags = 3",
    "firstyear = baseyear - max_lags",
    "automated_shocks = FALSE",
    "Rsolver = TRUE",
    "recompile_model = TRUE",
    solver_lines
  ), input)
  writeLines(c("quartos_to_render <- list()", "quartos_parameters <- list()"), output)
  list(input = input, output = output)
}

# test -------------------------------------------------------------------------
adv <- function(lines) {
    f <- solver_config_files(lines)
    readconfig(f$input, f$output)$input$advanced_config
  }

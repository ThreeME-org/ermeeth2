# Extracted from test-solver_config.R:184

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
solve_prg <- function(env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".prg", .local_envir = env)
  writeLines(c(
    "subroutine solvemodel(string %solveopt)",
    "  if %solveopt=\"u0\" then",
    "    {%modelname}.track {%tracklist}         ' Specify endogenous variables",
    "    {%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)               ' Simulation of the model / OPTION :",
    "    ' o= Algorithm solution method: g (Gauss-Seidel), n (NeWton), b(Broyden).",
    "    ' g= Number of digits to round solution.",
    "    ' m= Maximum number of iterations for solution (maximum 100 000)",
    "    return",
    "  endif",
    "  if %solveopt=\"u1\" then",
    "    {%modelname}.solve(o=g, g=10, m=5500, c=1e-8, z=1e-8,j=a)               ' Simulation of the model / OPTION : see above",
    "    return",
    "  endif",
    "  if %solveopt=\"d\" then",
    "    {%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)",
    "  else",
    "    {%modelname}.solve     ' Simulation of the model (Default option).",
    "  endif",
    "endsub"), f)
  f
}

# test -------------------------------------------------------------------------
f <- solver_config_files(c(
    "baseline_scenario_folder = file.path(\"configuration\", \"scenarii_calib\", \"bases\")",
    "shock_scenario_folder = \"configuration/scenarii_calib/ademe\""))
inp <- readconfig(f$input, f$output)$input

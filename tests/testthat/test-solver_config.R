## The solver options: what readconfig() makes of them, and how the EViews ones
## reach solve.prg.

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
    "lists_files = \"lists.mdl\"",
    "calib_files = c(lists_files, \"calib.mdl\")",
    "model_files = c(lists_files, \"eq.mdl\")",
    "Rsolver = TRUE",
    "recompile_model = TRUE",
    solver_lines
  ), input)
  writeLines(c("quartos_to_render <- list()", "quartos_parameters <- list()"), output)
  list(input = input, output = output)
}

test_that("readconfig fills in the solver options a file does not set", {
  f <- solver_config_files(character(0))
  adv <- readconfig(f$input, f$output)$input$advanced_config

  defaults <- config_solver_defaults()
  expect_equal(adv[names(defaults)], defaults)
  # the size cap inherited from tresthor is gone
  expect_false("max_tresthor_capability" %in% names(adv))
})

test_that("readconfig reads the solver options a file sets", {
  f <- solver_config_files(c(
    "Rcpp = FALSE",
    "Rsolver_decompose = FALSE",
    "Rsolver_rtol = 1e-8",
    "Rsolver_max_iter = 50",
    "eviews_algorithm = \"Gauss-Seidel\"",
    "eviews_digits = 8",
    "eviews_max_iter = 10000"))
  adv <- readconfig(f$input, f$output)$input$advanced_config

  expect_false(adv$Rcpp)
  expect_false(adv$Rsolver_decompose)
  expect_equal(adv$Rsolver_rtol, 1e-8)
  expect_equal(adv$Rsolver_max_iter, 50)
  expect_equal(adv$eviews_algorithm, "gauss-seidel")
  expect_equal(adv$eviews_digits, 8)
  expect_equal(adv$eviews_max_iter, 10000)
  # untouched options keep their defaults
  expect_equal(adv$Rsolver_atol, config_solver_defaults()$Rsolver_atol)
})

test_that("readconfig reads Rsolver_reuse_jacobian as auto or a logical", {
  adv <- function(lines) {
    f <- solver_config_files(lines)
    readconfig(f$input, f$output)$input$advanced_config
  }
  expect_equal(adv(character(0))$Rsolver_reuse_jacobian, "auto")
  expect_false(adv(character(0))$Rsolver_sequential)
  expect_true(adv("Rsolver_reuse_jacobian = TRUE")$Rsolver_reuse_jacobian)
  expect_false(adv("Rsolver_reuse_jacobian = \"false\"")$Rsolver_reuse_jacobian)
  expect_equal(adv("Rsolver_reuse_jacobian = \"Auto\"")$Rsolver_reuse_jacobian, "auto")
  expect_true(adv("Rsolver_sequential = TRUE")$Rsolver_sequential)
  expect_error(adv("Rsolver_reuse_jacobian = \"sometimes\""), "Rsolver_reuse_jacobian")
})

test_that("show_thor_equations prints the equations an error message names", {
  fake <- methods::setClass("fake_thor_model",
                            methods::representation(equations = "data.frame"),
                            where = environment())
  model <- fake(equations = data.frame(name = c("eq_1", "eq_2", "eq_12"),
                                       equation = c("a = b", "pk * f = v", "c = d"),
                                       stringsAsFactors = FALSE))
  msg <- "Sequential solve failed on block 'prologue' at row 3: equation 'eq_2' could not be solved for 'pk'"
  expect_message(found <- show_thor_equations(msg, model), "eq_2")
  expect_equal(found, c(eq_2 = "pk * f = v"))
  # nothing to show, nothing said
  expect_silent(show_thor_equations("the block did not converge", model))
  expect_silent(show_thor_equations("equation 'eq_99'", model))
})

test_that("readconfig still understands rcpp_option, and Rcpp wins over it", {
  f <- solver_config_files("rcpp_option = FALSE")
  expect_false(readconfig(f$input, f$output)$input$advanced_config$Rcpp)

  f <- solver_config_files(c("rcpp_option = FALSE", "Rcpp = TRUE"))
  expect_true(readconfig(f$input, f$output)$input$advanced_config$Rcpp)
})

test_that("readconfig refuses an EViews algorithm it does not know", {
  f <- solver_config_files("eviews_algorithm = \"simplex\"")
  expect_error(readconfig(f$input, f$output), "eviews_algorithm")
})

## The shape of ThreeME_V4's src/EViews/solve.prg.
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

test_that("eviews_solve_options rewrites o, g and m and nothing else", {
  f <- solve_prg()
  before <- readLines(f)
  call <- eviews_solve_options("newton", digits = 8, max_iter = 12000, file = f)
  after <- readLines(f)

  expect_equal(call, "{%modelname}.solve(o=n, g=8, m=12000, c=1e-8, z=1e-8,j=a,i=p,v=t)")
  expect_equal(which(before != after), 4L)
  expect_match(after[4], "' Simulation of the model / OPTION :$")
  expect_match(after[4], "^    \\{%modelname\\}\\.solve\\(o=n, g=8, m=12000, c=1e-8, z=1e-8,j=a,i=p,v=t\\) ")
})

test_that("eviews_solve_options only touches the branch it is asked for", {
  f <- solve_prg()
  before <- readLines(f)
  eviews_solve_options("broyden", digits = 6, max_iter = 100, file = f, solveopt = "u1")
  after <- readLines(f)

  expect_equal(which(before != after), 11L)
  expect_match(after[11], "solve(o=b, g=6, m=100, c=1e-8, z=1e-8,j=a)", fixed = TRUE)
})

test_that("eviews_solve_options leaves the file alone when nothing changes", {
  f <- solve_prg()
  Sys.setFileTime(f, Sys.time() - 3600)
  stamp <- file.mtime(f)
  eviews_solve_options("broyden", digits = 10, max_iter = 5500, file = f)
  expect_equal(file.mtime(f), stamp)
})

test_that("eviews_solve_options refuses what EViews would not take", {
  f <- solve_prg()
  expect_error(eviews_solve_options("simplex", file = f))
  expect_error(eviews_solve_options(max_iter = 200000, file = f), "max_iter")
  expect_error(eviews_solve_options(digits = 2.5, file = f), "digits")
  expect_error(eviews_solve_options(file = f, solveopt = "u9"), "solveopt")
  expect_equal(readLines(f), readLines(solve_prg()))
})

test_that("readconfig looks for the calibrations in scenarii_calib by default", {
  f <- solver_config_files(character(0))
  inp <- readconfig(f$input, f$output)$input

  expect_equal(inp$baseline_scenario_folder, calib_folder_default())
  expect_equal(inp$shock_scenario_folder, calib_folder_default())
  expect_equal(inp$calib_baseline,
               file.path("configuration", "scenarii_calib", "1_calib_baseline-steady.R"))
  expect_equal(inp$calib_scenario,
               file.path("configuration", "scenarii_calib", "2_calib_shock_ct1.R"))
})

test_that("readconfig follows the scenario folders the file sets", {
  f <- solver_config_files(c(
    "baseline_scenario_folder = file.path(\"configuration\", \"scenarii_calib\", \"bases\")",
    "shock_scenario_folder = \"configuration/scenarii_calib/ademe\""))
  inp <- readconfig(f$input, f$output)$input

  expect_equal(inp$calib_baseline,
               file.path("configuration", "scenarii_calib", "bases", "1_calib_baseline-steady.R"))
  expect_equal(inp$calib_scenario,
               "configuration/scenarii_calib/ademe/2_calib_shock_ct1.R")

  f <- solver_config_files("shock_scenario_folder = c(\"a\", \"b\")")
  expect_error(readconfig(f$input, f$output), "shock_scenario_folder")
})

test_that("format_duration picks the unit that reads best", {
  expect_equal(format_duration(c(0.523, 38.44, 59.96, 74.2, 3600, NA)),
               c("0.52 s", "38.4 s", "1 min 0 s", "1 min 14 s", "60 min 0 s", "n/a"))
})

test_that("show_solver_timings splits build and compile when thortwo reports them", {
  build <- structure(c(build = 38.4, compile = 44.1), from_cache = FALSE)
  expect_message(
    lines <- show_solver_timings(19.6, 82.5, build, c(baseline = 74, ct1 = 76)),
    "translation 19.6 s | build 38.4 s | compile 44.1 s", fixed = TRUE)
  expect_equal(lines[2], "solve: baseline 1 min 14 s | ct1 1 min 16 s")

  # a cache hit compiles nothing
  cached <- structure(c(build = 0.52, compile = NA), from_cache = TRUE)
  expect_equal(suppressMessages(show_solver_timings(19.6, 0.6, cached))[1],
               "translation 19.6 s | build 0.52 s (from the cache)")
  # a thortwo that does not report its timings: one figure, timed from outside
  expect_equal(suppressMessages(show_solver_timings(19.6, 82.5, NULL))[1],
               "translation 19.6 s | build and compile 1 min 22 s")
})

test_that("Rsolver_timings is on unless the configuration says otherwise", {
  f <- solver_config_files(character(0))
  expect_true(readconfig(f$input, f$output)$input$advanced_config$Rsolver_timings)
  f <- solver_config_files("Rsolver_timings = FALSE")
  expect_false(readconfig(f$input, f$output)$input$advanced_config$Rsolver_timings)
})

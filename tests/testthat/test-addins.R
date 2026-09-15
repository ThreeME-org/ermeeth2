## The addins are tested through shiny::testServer(), which drives the server
## logic directly. That is what makes them testable without clicking: the
## `*_app()` functions return the bare appobj.

skip_if_no_shiny <- function() {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
}

## A throwaway ThreeME project: configuration/ plus scenarii_calib/.
local_project <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "configuration", "scenarii_calib"), recursive = TRUE)
  dir.create(file.path(d, "data", "output"), recursive = TRUE)
  dir.create(file.path(d, "src"))

  writeLines(c(
    "iso3 = \"FRA\"",
    "classification = \"c8_s8\"",
    "model_folder = \"threeme\"",
    "project_name = \"nofit\"",
    "scenario_baseline = \"baseline-steady\"",
    "scenario = c(\"ct1\") |> set_names(c(\"Carbon tax\"))",
    "baseyear = 2019",
    "lastyear = 2100",
    "shockyear = 2021",
    "max_lags = 3",
    "firstyear = baseyear - max_lags",
    "automated_shocks = FALSE",
    "variables_to_keep = c()",
    "lists_files = c(\"lists.mdl\")",
    "calib_files = c(\"data/parameters.mdl\")  # ALL VERSIONS",
    "model_files = c(\"SU.mdl\")",
    "Rsolver = FALSE",
    "warning = FALSE",
    "tolerance_calib_check = 10^-3",
    "skip_compiler = FALSE",
    "recompile_model = TRUE",
    "save_files_res = TRUE",
    "output_saved = c(\"com\")",
    "path_eviews_exe = \"C:/EViews.exe\"",
    "eviews_timeout = 0",
    "rcpp_option = FALSE",
    "use.superlu = TRUE"
  ), file.path(d, "configuration", "config_input_threeme.R"))

  writeLines(c(
    "quartos_to_render <- list(",
    "  basic_results = FALSE,",
    "  model_equations = TRUE",
    ")",
    "quartos_parameters <- list(",
    "  basic_results = list(project_name = project_name)",
    ")"
  ), file.path(d, "configuration", "config_output_threeme.R"))

  cal <- file.path(d, "configuration", "scenarii_calib")
  create_baseline("steady", path = cal, open = FALSE, quiet = TRUE)
  file.rename(file.path(cal, "1_calib_baseline_steady.R"),
              file.path(cal, "1_calib_baseline-steady.R"))
  create_shock("ct1", path = cal, open = FALSE, quiet = TRUE)
  d
}

# ---- Addin 1: new calibration ----------------------------------------------

test_that("the calibration addin validates the name and names the target file", {
  skip_if_no_shiny()
  d <- local_project()
  cal <- file.path(d, "configuration", "scenarii_calib")

  shiny::testServer(calib_addin_app(path = cal), {
    session$setInputs(type = "shock", name = "ct2", title = "", overwrite = FALSE)
    expect_equal(scenario(), "ct2")

    session$setInputs(name = "CT2")
    expect_null(scenario())          # upper case is refused

    session$setInputs(type = "baseline", name = "ademe")
    expect_equal(scenario(), "baseline_ademe")
  })
})

test_that("the calibration addin fills the script from the template", {
  skip_if_no_shiny()
  d <- local_project()
  cal <- file.path(d, "configuration", "scenarii_calib")

  shiny::testServer(calib_addin_app(path = cal), {
    session$setInputs(type = "shock", name = "ct2", title = "My shock")
    expect_match(script_text(), "shock_ch")
    expect_no_match(script_text(), "\\{\\{")

    session$setInputs(type = "baseline")
    expect_match(script_text(), "baseline_ch")
    expect_match(script_text(), "OGcalib")
  })
})

test_that("the calibration addin writes what is in the script box", {
  skip_if_no_shiny()
  d <- local_project()
  cal <- file.path(d, "configuration", "scenarii_calib")

  shiny::testServer(calib_addin_app(path = cal), {
    session$setInputs(type = "shock", name = "ct2", title = "",
                      overwrite = FALSE, script = "## edited by hand\nshock_ch <- x")
    session$setInputs(create = 1)
    f <- file.path(cal, "2_calib_shock_ct2.R")
    expect_true(file.exists(f))
    expect_equal(readLines(f), c("## edited by hand", "shock_ch <- x"))
  })
})

test_that("the calibration addin will not clobber without the overwrite tick", {
  skip_if_no_shiny()
  d <- local_project()
  cal <- file.path(d, "configuration", "scenarii_calib")
  before <- readLines(file.path(cal, "2_calib_shock_ct1.R"))

  shiny::testServer(calib_addin_app(path = cal), {
    session$setInputs(type = "shock", name = "ct1", title = "",
                      overwrite = FALSE, script = "shock_ch <- 1")
    session$setInputs(create = 1)
    expect_equal(readLines(file.path(cal, "2_calib_shock_ct1.R")), before)

    session$setInputs(overwrite = TRUE)
    session$setInputs(create = 2)
    expect_equal(readLines(file.path(cal, "2_calib_shock_ct1.R")), "shock_ch <- 1")
  })
})

# ---- Addin 2: configuration -------------------------------------------------

test_that("the configuration addin loads an existing configuration", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")

  shiny::testServer(config_addin_app(path = cfg), {
    session$setInputs(existing = "threeme")
    expect_equal(values()$project_name, "nofit")
    expect_equal(values()$iso3, "FRA")
    expect_equal(unname(values()$scenario), "ct1")
    expect_true(is.list(out_values()$quartos_to_render))
  })
})

test_that("the configuration addin flags scenarios with no calibration file", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")

  shiny::testServer(config_addin_app(path = cfg), {
    session$setInputs(existing = "threeme")
    session$setInputs(f_scenario = c("ct1", "brandnew"))
    expect_equal(missing_calibs(), "brandnew")

    session$setInputs(f_scenario = "ct1")
    expect_length(missing_calibs(), 0)
  })
})

test_that("the configuration addin creates the missing calibration scripts", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")
  cal <- file.path(cfg, "scenarii_calib")

  shiny::testServer(config_addin_app(path = cfg), {
    session$setInputs(existing = "threeme", f_scenario = c("ct1", "brandnew"))
    session$setInputs(create_missing = 1)
    expect_true(file.exists(file.path(cal, "2_calib_shock_brandnew.R")))
    expect_length(missing_calibs(), 0)
  })
})

test_that("the configuration addin saves under a new name, preserving comments", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")

  shiny::testServer(config_addin_app(path = cfg), {
    session$setInputs(existing = "threeme")
    session$setInputs(name = "myrun", f_project_name = "myrun",
                      f_scenario = "ct1", f_scenario_names = "Carbon tax",
                      f_scenario_baseline = "baseline-steady")
    session$setInputs(save = 1)

    f <- file.path(cfg, "config_input_myrun.R")
    expect_true(file.exists(f))
    out <- readLines(f)
    expect_no_error(parse(text = out))
    expect_true(any(grepl('project_name = "myrun"', out, fixed = TRUE)))
    # the comment on calib_files survived the edit
    expect_true(any(grepl("ALL VERSIONS", out)))
    # and the original is untouched
    expect_true(any(grepl('project_name = "nofit"',
                          readLines(file.path(cfg, "config_input_threeme.R")),
                          fixed = TRUE)))
  })
})

test_that("the configuration addin rewrites quartos_to_render only", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")

  shiny::testServer(config_addin_app(path = cfg), {
    session$setInputs(existing = "threeme")
    session$setInputs(name = "myrun", q_basic_results = TRUE,
                      q_model_equations = FALSE, f_scenario = "ct1")
    session$setInputs(save = 1)

    fo <- file.path(cfg, "config_output_myrun.R")
    expect_true(file.exists(fo))
    v <- read_config_values(fo, c("quartos_to_render", "quartos_parameters"),
                            with = values())
    expect_true(v$quartos_to_render$basic_results)
    expect_false(v$quartos_to_render$model_equations)
    # quartos_parameters is carried through untouched
    expect_true(any(grepl("quartos_parameters", readLines(fo))))
  })
})

# ---- Addin 3: run -----------------------------------------------------------

test_that("the run addin prefills from the configuration", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")
  out <- file.path(d, "data", "output")

  shiny::testServer(run_addin_app(path = cfg, output_dir = out), {
    session$setInputs(config_in = "threeme", config_out = "threeme")
    expect_equal(cfg_values()$project_name, "nofit")
    expect_equal(run_params$project_name, "nofit")
    expect_equal(run_params$baseline, "baseline-steady")
    expect_equal(run_params$scenarios, "ct1")
  })
})

test_that("the run addin names the files a project name would overwrite", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")
  out <- file.path(d, "data", "output")
  file.create(file.path(out, c("nofit.rds", "nofit_com.parquet")))

  shiny::testServer(run_addin_app(path = cfg, output_dir = out), {
    session$setInputs(config_in = "threeme", config_out = "threeme")
    expect_setequal(basename(project_output_files("nofit", dir = out)),
                    c("nofit.rds", "nofit_com.parquet"))
    expect_length(project_output_files("a_fresh_name", dir = out), 0)
  })
})

test_that("the run addin refuses to run a scenario with no calibration file", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")
  out <- file.path(d, "data", "output")

  shiny::testServer(run_addin_app(path = cfg, output_dir = out), {
    session$setInputs(config_in = "threeme", config_out = "threeme")
    session$setInputs(scenarios = c("ct1", "nosuchshock"), baseline = "baseline-steady")
    expect_equal(missing_calibs(), "nosuchshock")

    # pressing run does not reach run_simulations(): nothing is written
    session$setInputs(project_name = "nofit", run = 1)
    expect_length(list.files(out), 0)
  })
})

test_that("the run addin flags a missing baseline too", {
  skip_if_no_shiny()
  d <- local_project()
  cfg <- file.path(d, "configuration")
  out <- file.path(d, "data", "output")

  shiny::testServer(run_addin_app(path = cfg, output_dir = out), {
    session$setInputs(config_in = "threeme", config_out = "threeme")
    session$setInputs(scenarios = "ct1", baseline = "baseline-nope")
    expect_equal(missing_calibs(), "baseline-nope")
  })
})


# ---- All three: the project guard -------------------------------------------

test_that("is_threeme_project names what is missing", {
  d <- withr::local_tempdir()
  chk <- is_threeme_project(d)
  expect_false(chk$ok)
  expect_setequal(chk$missing, c("configuration",
                                 file.path("configuration", "scenarii_calib"),
                                 "src"))

  dir.create(file.path(d, "configuration", "scenarii_calib"), recursive = TRUE)
  expect_equal(is_threeme_project(d)$missing, "src")

  dir.create(file.path(d, "src"))
  expect_true(is_threeme_project(d)$ok)
})

test_that("every addin refuses to launch outside a ThreeME project", {
  d <- withr::local_tempdir()
  expect_error(calib_addin(path = file.path(d, "configuration", "scenarii_calib")),
               "only works inside a ThreeME v4 project")
  expect_error(config_addin(path = file.path(d, "configuration")),
               "only works inside a ThreeME v4 project")
  expect_error(run_addin(path = file.path(d, "configuration")),
               "only works inside a ThreeME v4 project")
})

test_that("the refusal says which folders are missing and what to do", {
  d <- withr::local_tempdir()
  err <- tryCatch(config_addin(path = file.path(d, "configuration")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "configuration")
  expect_match(err, "src")
  expect_match(err, "\\.Rproj")
})

test_that("a real project shows no warning banner, a stray folder does", {
  skip_if_no_shiny()
  d <- local_project()
  expect_null(project_banner(d))
  expect_false(is.null(project_banner(withr::local_tempdir())))
})

## Parametric shocks: shock sheets with shock_parameters(), parameter sheets
## with shock_grid(), and how readconfig() / run_simulations() expand them.

## A project with a parametric carbon tax shock sheet and two parameter sheets.
local_parametric_project <- function(extra = character(0), params_dir = NULL,
                                     env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  cal <- file.path(d, "configuration", "scenarii_calib")
  dir.create(cal, recursive = TRUE)
  par <- if (is.null(params_dir)) cal else file.path(d, params_dir)
  dir.create(par, recursive = TRUE, showWarnings = FALSE)

  writeLines(c(
    "shock_parameters(ct_start = 100, ct_end = 200, redis_h = 0, redis_ls = 0)",
    "selection <- calib_new_base |> dplyr::select(year, rco2tax_vol)",
    "shock_ch <- dplyr::mutate(selection,",
    "  rco2tax_vol = ifelse(year >= shockyear,",
    "    ct_start + (ct_end - ct_start) * (year - shockyear) / (lastyear - shockyear),",
    "    rco2tax_vol),",
    "  redis_ct_h = redis_h, redis_ct_ls = redis_ls)"
  ), file.path(cal, "2_calib_shock_ct.R"))
  writeLines("shock_ch <- calib_new_base |> dplyr::select(year)",
             file.path(cal, "2_calib_shock_wd1.R"))
  writeLines("shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 1))",
             file.path(par, "3_params_ct_redis.R"))
  writeLines("shock_grid(ct_start = c(50, 100), ct_end = shockyear * 0 + 400)",
             file.path(par, "3_params_ct_traj.R"))

  input <- file.path(d, "configuration", "config_input_par.R")
  output <- file.path(d, "configuration", "config_output_par.R")
  writeLines(c(
    "project_name = \"ct_sensi\"",
    "model_folder = \"threeme\"",
    "scenario_baseline = \"baseline-steady\"",
    "baseyear = 2019",
    "lastyear = 2050",
    "lists_files = \"lists.mdl\"",
    "calib_files = c(lists_files, \"calib.mdl\")",
    "model_files = c(lists_files, \"eq.mdl\")",
    extra), input)
  writeLines("# nothing", output)
  list(root = d, cal = cal, params = par, input = input, output = output)
}

read_par <- function(p) withr::with_dir(p$root, readconfig(
  file.path("configuration", basename(p$input)),
  file.path("configuration", basename(p$output)), quiet = TRUE))

# ---- shock_grid() / shock_parameters() --------------------------------------

test_that("shock_grid gives every combination, the first parameter slowest", {
  g <- shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 1))
  expect_s3_class(g, "data.frame")
  expect_named(g, c("redis_h", "redis_ls"))
  expect_equal(nrow(g), 6)
  expect_equal(g$redis_h, c(0, 0, 0.5, 0.5, 1, 1))
  expect_equal(g$redis_ls, c(0, 1, 0, 1, 0, 1))
  # a repeated value is one scenario, not two
  expect_equal(nrow(shock_grid(a = c(1, 1, 2))), 2)
  # strings stay strings
  expect_type(shock_grid(kind = c("flat", "ramp"))$kind, "character")
})

test_that("shock_grid refuses what cannot be a parameter", {
  expect_error(shock_grid(), "at least one")
  expect_error(shock_grid(c(1, 2)), "named")
  expect_error(shock_grid(a = 1, a = 2), "more than once")
  expect_error(shock_grid(`a b` = 1), "valid R name")
  expect_error(shock_grid(scenario = 1), "cannot be a parameter")
  expect_error(shock_grid(a = list(1, 2)), "vector of numbers")
  expect_error(shock_grid(a = c(1, NA)), "NA")
  expect_error(shock_grid(a = numeric(0)), "vector of numbers")
})

test_that("shock_parameters assigns its defaults where it is called", {
  f <- function() {
    shock_parameters(level = 1, kind = "flat")
    list(level = level, kind = kind)
  }
  expect_equal(f(), list(level = 1, kind = "flat"))
})

test_that("shock_parameters takes the values of a parametric run over its defaults", {
  env <- new.env()
  assign(".shock_parameters", list(level = 3), envir = env)
  used <- evalq(shock_parameters(level = 1, kind = "flat"), env)
  expect_equal(env$level, 3)
  expect_equal(env$kind, "flat")
  expect_equal(used, list(level = 3, kind = "flat"))
  expect_equal(env$.shock_parameters_used, list(level = 3, kind = "flat"))

  assign(".shock_parameters", list(nope = 3), envir = env)
  expect_error(evalq(shock_parameters(level = 1), env), "does not declare")
})

# ---- files ------------------------------------------------------------------

test_that("parameter sheets are named 3_params_<shock>_<identifier>.R", {
  expect_equal(params_file_name("ct", "redis"), "3_params_ct_redis.R")
  expect_equal(params_path("ct", "traj", path = "x"), file.path("x", "3_params_ct_traj.R"))
  expect_error(params_file_name("CT", "redis"), "lower case")
  expect_error(params_file_name("ct", "Redis"), "identifier")
  expect_error(params_file_name("ct", "a b"), "identifier")
})

test_that("list_params tells the shock from the identifier, given the shocks", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("3_params_ct_redis.R", "3_params_ct_x_traj.R",
                             "2_calib_shock_ct.R", "3_params_.R")))
  l <- list_params(d, shocks = c("ct", "ct_x"))
  l <- l[order(l$identifier), ]
  expect_equal(l$shock, c("ct", "ct_x"))
  expect_equal(l$identifier, c("redis", "traj"))
  # without the shocks, the name is cut at its first underscore
  l <- list_params(d)
  expect_true("x_traj" %in% l$identifier)
  expect_equal(nrow(list_params(file.path(d, "nope"))), 0)
})

test_that("the parameters of a shock sheet are read without running it", {
  p <- local_parametric_project()
  sheet <- file.path(p$cal, "2_calib_shock_ct.R")
  expect_equal(shock_sheet_parameters(sheet), c("ct_start", "ct_end", "redis_h", "redis_ls"))
  expect_equal(unname(shock_sheet_parameters(sheet, defaults = TRUE)), c("100", "200", "0", "0"))
  expect_null(shock_sheet_parameters(file.path(p$cal, "2_calib_shock_wd1.R")))
})

test_that("create_params writes a sheet that runs the defaults as written", {
  p <- local_parametric_project()
  f <- create_params("ct", "fresh", path = p$params, open = FALSE, quiet = TRUE)
  expect_equal(basename(f), "3_params_ct_fresh.R")
  grid <- read_params_sheet(f, new.env())
  expect_equal(grid, data.frame(ct_start = 100, ct_end = 200, redis_h = 0, redis_ls = 0))
  expect_error(create_params("ct", "fresh", path = p$params, open = FALSE, quiet = TRUE),
               "already exists")
  # a shock sheet that declares nothing gets a placeholder to fill in
  f <- create_params("wd1", "a", path = p$params, open = FALSE, quiet = TRUE)
  expect_true(any(grepl("does not declare its parameters yet", readLines(f))))
})

# ---- readconfig() -----------------------------------------------------------

test_that("readconfig expands each parameter sheet into numbered scenarios", {
  p <- local_parametric_project(c(
    "scenario = c(\"World demand\" = \"wd1\")",
    "parametric_shocks = list(ct = c(\"redis\", \"traj\"))"))
  inp <- read_par(p)$input
  expect_equal(unname(inp$scenario),
               c("wd1", paste0("ct_redis_", 1:6), paste0("ct_traj_", 1:2)))
  # the classic scenario keeps its label, a parametric one is its own label
  expect_equal(names(inp$scenario)[1:2], c("World demand", "ct_redis_1"))
  expect_equal(inp$shocks_nb, 9)
  expect_equal(inp$scenario_name[2], "ct_redis_1_")
  sc <- file.path(calib_folder_default(), "2_calib_shock_")
  expect_equal(inp$calib_scenario, c(paste0(sc, "wd1.R"), rep(paste0(sc, "ct.R"), 8)))
  expect_equal(inp$parametric_shocks, list(ct = c("redis", "traj")))

  r <- inp$parametric_runs
  expect_named(r, paste0(c(rep("ct_redis_", 6), rep("ct_traj_", 2)), c(1:6, 1:2)))
  expect_equal(r$ct_redis_4[c("shock", "identifier", "variation")],
               list(shock = "ct", identifier = "redis", variation = 4L))
  expect_equal(r$ct_redis_4$values, list(redis_h = 0.5, redis_ls = 1))
  # a parameter sheet can use the values of the configuration
  expect_equal(r$ct_traj_2$values, list(ct_start = 100, ct_end = 400))
})

test_that("with parametric shocks, scenario can be left out", {
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  inp <- read_par(p)$input
  expect_equal(inp$scenario, paste0("ct_redis_", 1:6))

  p <- local_parametric_project()
  expect_error(read_par(p), "scenario")
})

test_that("the parameter sheets have a folder of their own if asked", {
  sub <- file.path("configuration", "scenarii_calib", "sensitivity")
  p <- local_parametric_project(c("parametric_shocks = list(ct = \"redis\")",
                                  paste0("shock_params_folder = \"", sub, "\"")),
                                params_dir = sub)
  inp <- read_par(p)$input
  expect_equal(inp$shock_params_folder, sub)
  expect_length(inp$parametric_runs, 6)
  # the shock sheet is still read from the shock folder
  expect_equal(unique(inp$calib_scenario),
               file.path(calib_folder_default(), "2_calib_shock_ct.R"))

  # not set, it is the shock folder, whatever that is
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  expect_equal(read_par(p)$input$shock_params_folder, calib_folder_default())
})

test_that("readconfig stops on a parametric shock it cannot run, saying why", {
  p <- local_parametric_project("parametric_shocks = list(ct = \"nope\")")
  expect_error(read_par(p), "3_params_ct_nope.R")

  p <- local_parametric_project("parametric_shocks = list(gone = \"redis\")")
  expect_error(read_par(p), "2_calib_shock_gone.R")

  # the shock sheet declares no parameters
  p <- local_parametric_project("parametric_shocks = list(wd1 = \"x\")")
  writeLines("shock_grid(a = 1)", file.path(p$params, "3_params_wd1_x.R"))
  expect_error(read_par(p), "does not declare its parameters")

  # the grid sets a parameter the shock sheet does not declare
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  writeLines("shock_grid(redis_x = 1)", file.path(p$params, "3_params_ct_redis.R"))
  expect_error(read_par(p), "redis_x")

  # a sheet that does not end on a grid
  writeLines("x <- 1", file.path(p$params, "3_params_ct_redis.R"))
  expect_error(read_par(p), "must end on a grid")

  # two identical rows
  writeLines("rbind(shock_grid(redis_h = 1), shock_grid(redis_h = 1))",
             file.path(p$params, "3_params_ct_redis.R"))
  expect_error(read_par(p), "twice")

  # a classic scenario that clashes with a parametric one
  p <- local_parametric_project(c("scenario = \"ct_redis_1\"",
                                  "parametric_shocks = list(ct = \"redis\")"))
  file.copy(file.path(p$cal, "2_calib_shock_wd1.R"), file.path(p$cal, "2_calib_shock_ct_redis_1.R"))
  expect_error(read_par(p), "same name")
})

test_that("a grid cut down with dplyr is what runs", {
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  writeLines("shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 1)) |> dplyr::filter(redis_h + redis_ls <= 1)",
             file.path(p$params, "3_params_ct_redis.R"))
  r <- read_par(p)$input$parametric_runs
  expect_length(r, 4)
  expect_equal(r$ct_redis_4$values, list(redis_h = 1, redis_ls = 0))
})

test_that("automated_shocks is no longer read, and a file that turns it on is told", {
  p <- local_parametric_project(c("scenario = \"wd1\"", "automated_shocks = TRUE"))
  expect_message(cfg <- read_par(p), "no longer read")
  expect_null(cfg$input$automated_shocks)
  p <- local_parametric_project(c("scenario = \"wd1\"", "automated_shocks = FALSE"))
  expect_silent(read_par(p))
})

# ---- running the sheets and the metadata --------------------------------------

test_that("a shock sheet runs with the values of its row, the others at default", {
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  inp <- read_par(p)$input
  parent <- list2env(list(
    calib_new_base = data.frame(year = 2019:2050, rco2tax_vol = 0),
    shockyear = 2021, lastyear = 2050))
  run <- withr::with_dir(p$root, run_shock_sheet(inp$calib_scenario[4], parent,
                                                 inp$parametric_runs$ct_redis_4$values))
  expect_equal(unique(run$shock_ch$redis_ct_h), 0.5)
  expect_equal(unique(run$shock_ch$redis_ct_ls), 1)
  expect_equal(run$shock_ch$rco2tax_vol[run$shock_ch$year == 2050], 200)
  expect_equal(run$values, list(ct_start = 100, ct_end = 200, redis_h = 0.5, redis_ls = 1))

  # a classic run of the same sheet: the defaults, and no values to report
  run <- withr::with_dir(p$root, run_shock_sheet(inp$calib_scenario[1], parent))
  expect_equal(unique(run$shock_ch$redis_ct_h), 0)
  expect_null(run$values)

  # a sheet that never calls shock_parameters() would ignore the grid
  expect_error(withr::with_dir(p$root, run_shock_sheet(
    file.path(calib_folder_default(), "2_calib_shock_wd1.R"), parent, list(a = 1))),
    "never calls")
})

test_that("the metadata traces each scenario to its values, and reads back", {
  p <- local_parametric_project("parametric_shocks = list(ct = c(\"redis\", \"traj\"))")
  runs <- read_par(p)$input$parametric_runs
  meta <- parametric_metadata(runs)
  expect_named(meta, c("scenario", "shock", "identifier", "variation",
                       "redis_h", "redis_ls", "ct_start", "ct_end"))
  expect_equal(meta$scenario[c(1, 7)], c("ct_redis_1", "ct_traj_1"))
  expect_equal(meta$variation, c(1:6, 1:2))
  expect_type(meta$redis_h, "double")
  expect_true(is.na(meta$ct_start[1]))

  # with the values the sheets ran with, defaults included
  used <- list(ct_redis_1 = list(ct_start = 100, ct_end = 200, redis_h = 0, redis_ls = 0))
  meta2 <- parametric_metadata(runs["ct_redis_1"], used = used)
  expect_equal(meta2$ct_end, 200)

  out <- file.path(p$root, "data", "output")
  expect_message(write_3me_params(meta, "ct_sensi", dir = out), "8 parametric scenarios")
  back <- read_3me_params("ct_sensi", dir = out)
  expect_equal(back$scenario, meta$scenario)
  expect_equal(back$redis_ls, meta$redis_ls)
  expect_equal(back$ct_start, meta$ct_start)

  # a later run of the project without parametric shocks removes the file
  expect_message(write_3me_params(NULL, "ct_sensi", dir = out), "Removed")
  expect_false(file.exists(file.path(out, "ct_sensi_params.csv")))
  expect_error(read_3me_params("ct_sensi", dir = out), "parametric_shocks")
})

test_that("the run addin counts the metadata among the files a run overwrites", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("p.parquet", "p_params.csv", "q_params.csv")))
  expect_setequal(basename(project_output_files("p", dir = d)), c("p.parquet", "p_params.csv"))
})

# ---- the calibration bubble -------------------------------------------------------

test_that("a parametric scenario can be chosen to test a shock sheet by hand", {
  p <- local_parametric_project("parametric_shocks = list(ct = \"redis\")")
  cfg <- read_par(p)
  env <- new.env()

  expect_message(use_parametric_scenario("ct_redis_4", cfg, envir = env), "ct_redis_4")
  # the sheet run line by line in that environment gets the scenario's values,
  # and says which ones are not the defaults
  expect_message(evalq(shock_parameters(ct_start = 100, ct_end = 200, redis_h = 0, redis_ls = 0), env),
                 "redis_h = 0.5, redis_ls = 1; `ct_start` and `ct_end` at their defaults")
  expect_equal(env$redis_h, 0.5)
  expect_equal(env$ct_end, 200)

  # back to the defaults
  expect_message(use_parametric_scenario(NULL, cfg, envir = env), "defaults")
  expect_silent(evalq(shock_parameters(ct_start = 100, ct_end = 200, redis_h = 0, redis_ls = 0), env))
  expect_equal(env$redis_h, 0)

  expect_error(use_parametric_scenario("ct_redis_99", cfg, envir = env), "ct_redis_1")
  cfg$input$parametric_runs <- list()
  expect_error(use_parametric_scenario("ct_redis_1", cfg, envir = env), "no parametric scenario")
})

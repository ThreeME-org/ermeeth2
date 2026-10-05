sample_config <- function() {
  c("## A ThreeME configuration",
    "",
    "iso3 = \"FRA\"                 # country",
    "classification = \"c8_s8\"",
    "project_name = \"nofit\"",
    "",
    "scenario_baseline = \"baseline-steady\"",
    "scenario = c(\"ct1\") |>",
    "  set_names(c(\"Carbon tax\"))",
    "",
    "baseyear = 2019",
    "max_lags = 3",
    "firstyear = baseyear - max_lags",
    "",
    "calib_files = c(",
    "  \"data/parameters.mdl\",   # ALL VERSIONS",
    "  # \"data/optional.mdl\",   # NESTED CES VERSION",
    "  \"ENDOFLINE.mdl\"          # ALL VERSIONS",
    ")",
    "",
    "Rsolver = FALSE",
    "if (Rsolver == TRUE) {",
    "  Sys.setenv(\"OMP_NUM_THREADS\" = \"1\")",
    "}")
}

test_that("config_assignments finds top-level assignments and their line spans", {
  a <- config_assignments(sample_config())
  expect_true(all(c("iso3", "project_name", "scenario", "calib_files") %in% a$name))
  # the multi-line scenario assignment spans both its lines
  sc <- a[a$name == "scenario", ]
  expect_equal(sc$first, 8L)
  expect_equal(sc$last, 9L)
  # the bare `if` block is not an assignment
  expect_false("Sys.setenv" %in% a$name)
})

test_that("config_set replaces one value and touches nothing else", {
  l <- sample_config()
  l2 <- config_set(l, "project_name", deparse("my_run"))
  expect_equal(length(l2), length(l))
  expect_equal(sum(l != l2), 1L)
  expect_true(any(grepl('project_name = "my_run"', l2, fixed = TRUE)))
  # comments and commented-out alternatives survive
  expect_equal(sum(grepl("ALL VERSIONS", l2)), sum(grepl("ALL VERSIONS", l)))
  expect_true(any(grepl("NESTED CES VERSION", l2)))
  # so does the live code block
  expect_true(any(grepl("Sys.setenv", l2)))
})

test_that("config_set collapses a multi-line assignment correctly", {
  l <- config_set(sample_config(), "scenario", 'c("ct1", "wd1")')
  expect_no_error(parse(text = l))
  expect_true(any(grepl('scenario = c("ct1", "wd1")', l, fixed = TRUE)))
  expect_false(any(grepl("set_names", l)))
})

test_that("config_set appends a name the file does not assign", {
  l <- config_set(sample_config(), "brand_new", "42")
  expect_true(any(grepl("brand_new = 42", l, fixed = TRUE)))
  expect_equal(config_set(sample_config(), "brand_new", "42", add = FALSE),
               sample_config())
})

test_that("config_edit writes a file that still parses", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(sample_config(), f)
  config_edit(f, list(project_name = "run2", baseyear = 2021, Rsolver = TRUE))
  out <- readLines(f)
  expect_no_error(parse(text = out))
  expect_true(any(grepl('project_name = "run2"', out, fixed = TRUE)))
  expect_true(any(grepl("baseyear = 2021", out, fixed = TRUE)))
  expect_true(any(grepl("Rsolver = TRUE", out, fixed = TRUE)))
})

test_that("config_edit takes raw code through I()", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(sample_config(), f)
  config_edit(f, list(scenario = I('c("a", "b") |> set_names(c("A", "B"))')))
  v <- read_config_values(f, c("scenario"))
  expect_equal(unname(v$scenario), c("a", "b"))
  expect_equal(names(v$scenario), c("A", "B"))
})

test_that("config_edit refuses an edit that would break the file", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(sample_config(), f)
  expect_error(config_edit(f, list(iso3 = I("c(\"unclosed\""))), "does not parse")
  # and the file on disk is untouched
  expect_equal(readLines(f), sample_config())
})

test_that("config_edit can write somewhere else, leaving the original alone", {
  f <- withr::local_tempfile(fileext = ".R")
  g <- withr::local_tempfile(fileext = ".R")
  writeLines(sample_config(), f)
  config_edit(f, list(project_name = "other"), out = g)
  expect_equal(readLines(f), sample_config())
  expect_true(any(grepl('project_name = "other"', readLines(g), fixed = TRUE)))
})

test_that("read_config_values evaluates the file as the pipeline would", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(sample_config(), f)
  v <- read_config_values(f)
  expect_equal(v$iso3, "FRA")
  expect_equal(v$baseyear, 2019)
  # firstyear is computed from the others, not written down
  expect_equal(v$firstyear, 2016)
  # set_names() and str_c() are available to the file without being attached
  expect_equal(names(v$scenario), "Carbon tax")
  expect_error(read_config_values("no-such-file.R"), "no such file")
})

test_that("config_fields is the list the addin builds its controls from", {
  f <- config_fields()
  expect_true(all(c("name", "section", "type", "label", "choices") %in% names(f)))
  expect_true(all(f$type %in% c("text", "number", "bool", "choice", "multi")))
  expect_false(anyDuplicated(f$name) > 0)
  expect_setequal(unique(f$section),
                  c("Basics", "Solver", "R solver", "EViews solver"))
  # every solver option with a default is editable, and nothing else is offered
  expect_true(all(names(config_solver_defaults()) %in% f$name))
  # the addin is in step with the configuration: every option readconfig()
  # knows is offered, and it offers no option readconfig() does not know
  options <- c(config_required(), names(config_defaults()))
  offered <- c(f$name, config_fields_custom())
  expect_setequal(offered, options)
  expect_false(anyDuplicated(offered) > 0)
  expect_false(any(c("rcpp_option", "skip_compiler", "sequential", "max_tresthor_capability") %in% f$name))
})

test_that("list_configs pairs input and output configurations by name", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("config_input_threeme.R", "config_output_threeme.R",
                             "config_input_training.R", "notaconfig.R")))
  cf <- list_configs(d)
  expect_setequal(cf$name, c("threeme", "threeme", "training"))
  expect_equal(sum(cf$kind == "output"), 1L)
  expect_equal(nrow(list_configs(file.path(d, "nope"))), 0L)
})

test_that("project_output_files finds what a run would overwrite", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("nofit.rds", "nofit_com.parquet", "nofit_sec.rds",
                             "other.rds", "nofit2.rds")))
  hit <- basename(project_output_files("nofit", dir = d))
  expect_setequal(hit, c("nofit.rds", "nofit_com.parquet", "nofit_sec.rds"))
  expect_equal(project_output_files("fresh", dir = d), character(0))
  expect_equal(project_output_files("", dir = d), character(0))
})

files_config <- function() {
  c("iso3 = \"FRA\"",
    "lists_files = c(",
    "  str_c(\"R_lists_\", iso3, \".mdl\"),   # ALL VERSIONS",
    "  \"lists.mdl\"                          # ALL VERSIONS",
    ")",
    "calib_files = c(lists_files,",
    "                \"data/parameters.mdl\",     # ALL VERSIONS",
    "                # \"data/nested.mdl\",       # NESTED CES VERSION",
    "                \"data/round0.mdl\",         # ALL VERSIONS",
    "",
    "                ## Optional blocks",
    "                # \"data/ets.mdl\",          # ETS",
    "                \"ENDOFLINE.mdl\")",
    "model_files = c(lists_files, \"SU.mdl\", # a comment",
    "                # \"Exception_ETS.mdl\",",
    "                \"ENDOFLINE.mdl\")")
}

textfile <- function(lines) {
  f <- tempfile(fileext = ".R")
  writeLines(lines, f)
  f
}

edit_files <- function(lines, values) {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, f)
  config_edit(f, lapply(values, config_files))
  list(lines = readLines(f), values = read_config_values(f))
}

test_that("a file list is edited in place and keeps its comments", {
  l <- files_config()
  want <- c("R_lists_FRA.mdl", "lists.mdl", "data/parameters.mdl", "data/new.mdl",
            "data/ets.mdl", "ENDOFLINE.mdl")
  res <- edit_files(l, list(calib_files = want))
  out <- res$lines
  expect_equal(res$values$calib_files, want)

  # the entry that is code is still code, on the first line
  expect_true("calib_files = c(lists_files," %in% out)
  # removed: commented out, with its comment
  expect_true(any(grepl("^\\s*# \"data/round0.mdl\",\\s+# ALL VERSIONS$", out)))
  # added from a commented-out alternative: uncommented, with its comment
  expect_true(any(grepl("^\\s*\"data/ets.mdl\",\\s+# ETS$", out)))
  # added from nothing: a new line, after the file before it
  expect_equal(grep("data/new.mdl", out), grep("data/parameters.mdl", out) + 1L)
  # every other comment is still there, and the other lists are untouched
  for (keep in c("NESTED CES VERSION", "## Optional blocks", "# a comment")) {
    expect_true(any(grepl(keep, out, fixed = TRUE)))
  }
  expect_equal(out[1:5], l[1:5])
  expect_equal(out[grep("^model_files", out) + 0:2], l[14:16])

  # and back: the same files as at the start
  back <- edit_files(out, list(calib_files = read_config_values(textfile(l))$calib_files))
  expect_equal(back$values$calib_files,
               c("R_lists_FRA.mdl", "lists.mdl", "data/parameters.mdl", "data/round0.mdl", "ENDOFLINE.mdl"))
})

test_that("a file list follows the order asked for and the commas stay right", {
  l <- files_config()
  # last entry removed, a file put first, half of an entry that is code
  res <- edit_files(l, list(lists_files = "R_lists_FRA.mdl",
                            calib_files = c("data/first.mdl", "R_lists_FRA.mdl",
                                            "data/round0.mdl", "data/parameters.mdl")))
  expect_equal(res$values$lists_files, "R_lists_FRA.mdl")
  expect_equal(res$values$calib_files, c("data/first.mdl", "R_lists_FRA.mdl",
                                         "data/round0.mdl", "data/parameters.mdl"))
  expect_true(any(grepl("str_c(\"R_lists_\", iso3", res$lines, fixed = TRUE)))
  expect_true(any(grepl("^\\s*# \"lists.mdl\",", res$lines)))
  expect_true(any(grepl("^\\s*# \"ENDOFLINE.mdl\",", res$lines)))
})

test_that("a file list not written one entry per line is written afresh, comments kept", {
  l <- files_config()
  res <- edit_files(l, list(model_files = c("lists.mdl", "Prices.mdl", "ENDOFLINE.mdl")))
  expect_equal(res$values$model_files, c("lists.mdl", "Prices.mdl", "ENDOFLINE.mdl"))
  expect_true(any(grepl("# \"Exception_ETS.mdl\",", res$lines, fixed = TRUE)))
  expect_equal(res$lines[1:13], l[1:13])
})

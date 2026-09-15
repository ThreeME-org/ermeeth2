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
    "use.superlu = TRUE",
    "if (use.superlu == TRUE) {",
    "  Sys.setenv(\"CPATH\" = \"/opt/homebrew/include\")",
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
  expect_true(all(c("name", "section", "type", "label") %in% names(f)))
  expect_true(all(f$type %in% c("text", "number", "bool")))
  expect_false(anyDuplicated(f$name) > 0)
  expect_setequal(unique(f$section), c("Basics", "Solver"))
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

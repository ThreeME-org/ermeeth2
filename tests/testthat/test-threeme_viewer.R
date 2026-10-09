mini_path <- function() {
  system.file("extdata", "minimodel.rds", package = "ermeeth2")
}

test_that("the addin is registered for RStudio", {
  dcf <- system.file("rstudio", "addins.dcf", package = "ermeeth2")
  expect_true(nzchar(dcf))
  fields <- read.dcf(dcf)
  expect_equal(unname(fields[1, "Binding"]), "threeme_viewer")
  expect_true(nzchar(fields[1, "Name"]))
})

test_that("code generation produces parseable R", {
  expect_equal(chr_vec_code("GDP"), '"GDP"')
  expect_equal(chr_vec_code(c("GDP", "CH")), 'c("GDP", "CH")')
  expect_equal(named_vec_code(c(GDP = "reldiff")), 'c(GDP = "reldiff")')
  ## The unnamed default rides along bare - `"" = "level"` would not parse.
  expect_equal(named_vec_code(c(GDP = "reldiff", "level")),
               'c(GDP = "reldiff", "level")')
  expect_silent(parse(text = named_vec_code(c(GDP = "reldiff", "level"))))
  expect_equal(named_list_code(list(A = c("x", "y"))), 'list(A = c("x", "y"))')

  ## Names that are not syntactic get backticked, so the code still parses.
  code <- named_list_code(list(`Public spending` = "G"))
  expect_equal(code, 'list(`Public spending` = "G")')
  expect_silent(parse(text = code))
})

test_that("threeme_candidates only picks out ThreeME-shaped dataframes", {
  env <- new.env()
  assign("good", readRDS(mini_path()), envir = env)
  assign("bad_df", data.frame(a = 1), envir = env)
  assign("not_a_df", 1:10, envir = env)

  expect_equal(threeme_candidates(env), "good")
})

test_that("the app object is built without launching it", {
  app <- threeme_viewer_app(mini_path())
  expect_s3_class(app, "shiny.appobj")
})

test_that("the viewer loads a file and fills its series controls", {
  skip_if_not_installed("shiny")

  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(source = "file", path = mini_path())
    expect_s3_class(dat(), "data.frame")
    expect_setequal(unique(dat()$scenario), c("baseline", "g"))
    ## The generated code reproduces the file it was given.
    expect_match(data_expr(), "readRDS", fixed = TRUE)
  })
})

test_that("per-variable overrides become the arguments the functions expect", {
  skip_if_not_installed("shiny")

  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(
      source = "file", path = mini_path(),
      baseline = "baseline", scenarios = "g",
      variables = c("Y", "MU"), transformation = "reldiff",
      label_Y = "GDP", label_MU = "",
      tr_Y = "", tr_MU = "ppdiff",
      group_Y = "Activity", group_MU = "Prices"
    )

    ## Only the variables actually renamed appear in `labels`.
    expect_equal(labels_vec(), c(Y = "GDP"))

    ## The override is named, the default rides along unnamed - which is what
    ## threeme_transform() reads.
    tr <- transformation_arg()
    expect_equal(unname(tr[names(tr) == "MU"]), "ppdiff")
    expect_true(any(names(tr) == ""))

    ## Groups turn `variables` into the named list table_3me() wants.
    expect_type(variables_arg(), "list")
    expect_equal(names(variables_arg()), c("Activity", "Prices"))
  })
})

test_that("no group names means a plain variable vector", {
  skip_if_not_installed("shiny")

  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(
      source = "file", path = mini_path(),
      baseline = "baseline", variables = c("Y", "CH"),
      transformation = "reldiff",
      group_Y = "", group_CH = ""
    )
    expect_equal(variables_arg(), c("Y", "CH"))
    expect_equal(transformation_arg(), "reldiff")
  })
})

test_that("horizons and model names are parsed from their text fields", {
  skip_if_not_installed("shiny")

  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(horizons = "0, 3, 7", model_names = "v4.4, v4.5")
    expect_equal(horizons(), c(0, 3, 7))
    expect_equal(model_names(), c("v4.4", "v4.5"))

    ## Garbage falls back to the defaults rather than erroring.
    session$setInputs(horizons = "", model_names = "only one")
    expect_equal(horizons(), c(0, 1, 2, 5, 10))
    expect_equal(model_names(), c("model 1", "model 2"))
  })
})

test_that("the generated code runs and reproduces the viewer's output", {
  skip_if_not_installed("shiny")

  code <- NULL
  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(
      source = "file", path = mini_path(),
      baseline = "baseline", scenarios = "g",
      variables = c("Y", "MU"), transformation = "reldiff",
      label_Y = "GDP", label_MU = "",
      tr_Y = "", tr_MU = "ppdiff",
      group_Y = "", group_MU = "",
      years = c(2016, 2050), colour_by = "", palette_type = "distinct",
      base_year = NA, plot_digits = 2, x_breaks = NA, plot_title = "",
      interactive = FALSE,
      horizons = "0, 5", shock_year = NA, end_year = NA, table_digits = 2,
      theme = "none", table_title = "", table_subtitle = "", caption = "",
      path2 = "", model_names = "model 1, model 2"
    )
    code <<- the_code()
  })

  expect_silent(parsed <- parse(text = code))
  ## Running it produces the same pair of objects the viewer displays.
  env <- new.env(parent = globalenv())
  results <- lapply(parsed, function(e) eval(e, envir = env))
  expect_s3_class(results[[2]], "ggplot")
  expect_s3_class(results[[3]], "gt_tbl")
})

test_that("the plot and table work off the selected variables only", {
  skip_if_not_installed("shiny")

  shiny::testServer(threeme_viewer_app(mini_path()), {
    session$setInputs(
      source = "file", path = mini_path(),
      baseline = "baseline", scenarios = "g",
      variables = c("Y", "MU"), base_year = NA
    )
    ## A full model is millions of rows; only the selection is handed on.
    expect_setequal(unique(dat_sel()$variable), c("Y", "MU"))
    expect_lt(nrow(dat_sel()), nrow(dat()))
    expect_null(dat2_sel())

    ## The slice must not move the default base year: it stays the first year
    ## of the full data, as the generated code would resolve it.
    expect_equal(base_year_arg(), min(dat()$year))
    session$setInputs(base_year = 2020)
    expect_equal(base_year_arg(), 2020)

    expect_equal(meta()$variables, sort(unique(dat()$variable)))
  })
})

test_that("the viewer queries a parquet file instead of reading it", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  rds <- file.path(d, "mini.rds")
  pq <- file.path(d, "mini.parquet")
  file.copy(mini_path(), rds)
  full <- readRDS(rds)

  ## an .rds with no parquet next to it is read whole
  expect_null(viewer_parquet_for(rds))
  expect_s3_class(viewer_open(rds), "data.frame")

  arrow::write_parquet(full, pq)
  expect_equal(viewer_parquet_for(rds), pq)
  ## a parquet older than its .rds belongs to an earlier run
  Sys.setFileTime(pq, file.mtime(rds) - 60)
  expect_null(viewer_parquet_for(rds))
  Sys.setFileTime(pq, file.mtime(rds) + 60)

  for (path in c(rds, pq)) {
    shiny::testServer(threeme_viewer_app(path), {
      var <- sort(unique(full$variable))[1]
      session$setInputs(source = "file", path = path, variables = var)
      expect_false(is.data.frame(dat()))
      expect_equal(meta()$variables, sort(unique(full$variable)))
      expect_setequal(meta()$scenarios, unique(full$scenario))
      expect_equal(meta()$years, range(full$year))
      ## only the selected variable is ever materialised
      expect_s3_class(dat_sel(), "data.frame")
      expect_equal(nrow(dat_sel()), sum(full$variable == var))
      expect_type(dat_sel()$scenario, "character")
    })
  }
  expect_equal(viewer_read_code(pq), paste0('arrow::read_parquet("', pq, '")'))
  expect_equal(viewer_read_code(rds), paste0('readRDS("', rds, '")'))
})

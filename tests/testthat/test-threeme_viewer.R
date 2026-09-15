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

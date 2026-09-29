## Translating a compiled model.
##
## The fixtures are written inline rather than shipped: each one is the
## smallest model that exercises a construct the previous implementation got
## wrong, so the test doubles as the record of what those failures were.

write_prg <- function(lines, dir = withr::local_tempdir()) {
  f <- file.path(dir, "model.prg")
  writeLines(paste("a_3me.append", lines), f)
  f
}

write_calib <- function(vars, dir = dirname(prg), prg = NULL, years = -1:2) {
  f <- file.path(dir, "calib.csv")
  d <- data.frame(year = years, baseyear = 0)
  for (nm in names(vars)) d[[nm]] <- rep_len(vars[[nm]], length(years))
  utils::write.csv(d, f, row.names = FALSE)
  f
}

test_that("a minimal model translates and is square", {
  dir <- withr::local_tempdir()
  prg <- write_prg(c("a = b + c",
                     "b = 2.0 * c"), dir)
  calib <- write_calib(list(a = 1, b = 2, c = 3), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_equal(tr$endo, c("a", "b"))
  expect_equal(tr$exo, "c")
  expect_length(tr$equations, 2L)
  expect_length(tr$warnings, 0L)
})

test_that("lags and differences become thoR syntax", {
  dir <- withr::local_tempdir()
  prg <- write_prg(c("a = a(-1) + b",
                     "b = d(log(c))"), dir)
  calib <- write_calib(list(a = 1, b = 2, c = 3), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_match(tr$equations[1], "lag(a,1)", fixed = TRUE)
  expect_match(tr$equations[2], "delta(1,log(c))", fixed = TRUE)
})

test_that("@elem becomes a coefficient valued from the calibration", {
  dir <- withr::local_tempdir()
  prg <- write_prg("a = b * @elem(c, 2019)", dir)
  calib <- write_calib(list(a = 1, b = 2, c = c(7, 11, 13, 17)), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_equal(tr$coef, "elem_c_2019")
  ## year 0 is 2019, the second row
  expect_equal(tr$elem$value, 11)
  expect_equal(tr$data$elem_c_2019, rep(11, 4))
  expect_match(tr$equations[1], "elem_c_2019", fixed = TRUE)
})

test_that("@elem with a nested lag is found and dated back", {
  ## Previously this shape had its own regex; anything it did not match was
  ## left in the equations and reached the solver as an undefined variable.
  dir <- withr::local_tempdir()
  prg <- write_prg("a = @elem(c(-1), 2019)", dir)
  calib <- write_calib(list(a = 1, c = c(7, 11, 13, 17)), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_equal(tr$coef, "elem_c_2018")
  expect_equal(tr$elem$value, 7)            # the 2018 row
  expect_false(any(grepl("@elem", tr$equations, fixed = TRUE)))
})

test_that("@elem over an arbitrary expression is evaluated, not pattern-matched", {
  ## The previous switch over +,-,*,/ handled exactly two operands; this has
  ## three and a function call.
  dir <- withr::local_tempdir()
  prg <- write_prg("a = @elem(b / c + log(d), 2019)", dir)
  calib <- write_calib(list(a = 1, b = 6, c = 3, d = exp(1)), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_length(tr$coef, 1L)
  expect_equal(tr$elem$value, 3, tolerance = 1e-12)   # 6/3 + log(e)
  expect_length(tr$warnings, 0L)
})

test_that("the same variable at two years gives two distinct coefficients", {
  ## The previous implementation keyed on the variable name alone, so both
  ## occurrences collapsed onto whichever value was seen last.
  dir <- withr::local_tempdir()
  prg <- write_prg("a = @elem(c, 2018) + @elem(c, 2020)", dir)
  calib <- write_calib(list(a = 1, c = c(7, 11, 13, 17)), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_setequal(tr$coef, c("elem_c_2018", "elem_c_2020"))
  expect_equal(sort(tr$elem$value), c(7, 13))
})

test_that("a model with only lagged @elem translates", {
  ## rbind(elem_table_1, elem_table_2) used to error here, because the table
  ## for the shape that does not occur was never created.
  dir <- withr::local_tempdir()
  prg <- write_prg("a = @elem(c(-1), 2019)", dir)
  calib <- write_calib(list(a = 1, c = 1:4), dir = dir)

  expect_no_error(prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE))
})

test_that("logical tests become indicators that evaluate to 0 or 1", {
  dir <- withr::local_tempdir()
  prg <- write_prg("a = (b/c<0.99999) * 10.0", dir)
  calib <- write_calib(list(a = 1, b = 1, c = 2), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_false(any(grepl("[<>]", tr$equations)))

  ## the rewritten test really is the indicator it replaced
  rhs <- sub("^a=", "", tr$equations[1])
  b <- 1; c <- 2
  expect_equal(eval(parse(text = rhs)), 10)      # 1/2 < 0.99999 -> 1
  b <- 4
  expect_equal(eval(parse(text = rhs)), 0)       # 4/2 < 0.99999 -> 0
})

test_that("a test whose left side is a function call is rewritten too", {
  ## The previous generic rewrite only matched <word><op><word><cmp><number>,
  ## so this shape was left in place; it was handled, if at all, by a
  ## hard-coded ThreeME substitution.
  dir <- withr::local_tempdir()
  prg <- write_prg("a = (log(b)-log(c)>0.0) * 5.0", dir)
  calib <- write_calib(list(a = 1, b = 3, c = 2), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_false(any(grepl("[<>]", tr$equations)))
  rhs <- sub("^a=", "", tr$equations[1])
  b <- 3; c <- 2
  expect_equal(eval(parse(text = rhs)), 5)       # log(3) > log(2)
  b <- 1
  expect_equal(eval(parse(text = rhs)), 0)
})

test_that("scientific notation is expanded so no phantom variable appears", {
  dir <- withr::local_tempdir()
  prg <- write_prg("a = b + 1e-05", dir)
  calib <- write_calib(list(a = 1, b = 2), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_false(any(grepl("e-", tr$equations, fixed = TRUE)))
  expect_false("e" %in% c(tr$endo, tr$exo, tr$coef))
})

test_that("a non-square model is reported rather than passed on", {
  dir <- withr::local_tempdir()
  prg <- write_prg(c("a = b", "a = c"), dir)          # a on both left sides
  calib <- write_calib(list(a = 1, b = 2, c = 3), dir = dir)

  tr <- prg_to_thor(prg, calib, base.year = 2019, verbose = FALSE)

  expect_true(any(grepl("more than one equation", tr$warnings)))
  expect_true(any(grepl("not square", tr$warnings)))
})

test_that("translate_modelprg still returns its old shape", {
  dir <- withr::local_tempdir()
  prg <- write_prg(c("a = b + c", "b = 2.0 * c"), dir)
  calib <- write_calib(list(a = 1, b = 2, c = 3), dir = dir)

  invisible(utils::capture.output(
    tr <- translate_modelprg(prg, calib, base.year = 2019, last.year = NULL)))

  expect_named(tr, c("equations", "endo", "exo", "coef", "data", "elem",
                     "warnings", "file", "errors"))
  expect_true(is.na(tr$errors[1]))
  expect_equal(tr$endo, c("a", "b"))
})

test_that("base.year is required", {
  dir <- withr::local_tempdir()
  prg <- write_prg("a = b", dir)
  calib <- write_calib(list(a = 1, b = 2), dir = dir)

  expect_error(prg_to_thor(prg, calib, verbose = FALSE), "base.year")
})

test_that("the output is written as parquet only by default", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  df <- readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))

  written <- save_3me_output(df, "proj", dir = d)
  expect_equal(basename(written), "proj.parquet")
  expect_false(file.exists(file.path(d, "proj.rds")))
  expect_equal(nrow(read_3me_output("proj", dir = d)), nrow(df))

  written <- save_3me_output(df, "proj", "sec", formats = c("rds", "parquet"), dir = d)
  expect_setequal(basename(written), c("proj_sec.rds", "proj_sec.parquet"))
})

test_that("read_3me_output reads the most recent of the two formats", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  df <- readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
  old <- df[df$scenario == "baseline", ]

  save_3me_output(old, "proj", formats = "rds", dir = d)
  Sys.setFileTime(file.path(d, "proj.rds"), Sys.time() - 3600)
  ## the earlier .rds is pointed out, and no longer read
  expect_message(save_3me_output(df, "proj", dir = d), "earlier run")
  expect_equal(nrow(read_3me_output("proj", dir = d)), nrow(df))

  ## a later run that only wrote the .rds wins over the parquet
  Sys.setFileTime(file.path(d, "proj.rds"), Sys.time() + 3600)
  expect_equal(nrow(read_3me_output("proj", dir = d)), nrow(old))

  ## the filters work on either format
  v <- df$variable[1]
  expect_equal(unique(read_3me_output("proj", variables = v, dir = d)$variable), v)
})

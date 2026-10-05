test_that("get_sec_com() finds a lists file whose name differs by case", {
  dir <- withr::local_tempdir()
  writeLines(c("%list_sec := sind sser", "%list_com := cind cser cenj", "%list_com_E := cenj"),
             file.path(dir, "R_lists_FRA_c4_s4.mdl"))

  res <- get_sec_com(file.path(dir, "R_lists_fra_c4_s4.mdl"))
  expect_equal(res$sectors, c("sind", "sser"))
  expect_equal(res$commodities, c("cind", "cser", "cenj"))
  expect_equal(res$energy_commodities, "cenj")
})

test_that("get_sec_com() can leave the energy commodities out", {
  dir <- withr::local_tempdir()
  writeLines(c("%list_sec := sind sser", "%list_com := cind cser"),
             file.path(dir, "R_lists_FRA_c4_s4.mdl"))

  res <- get_sec_com(file.path(dir, "R_lists_FRA_c4_s4.mdl"), energy_commodities = FALSE)
  expect_null(res$energy_commodities)
  expect_equal(res$sectors, c("sind", "sser"))
})

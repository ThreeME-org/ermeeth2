test_that("the package exports its main entry points", {
  expect_true(all(c("long_data", "wide_data", "simple_plot", "table_3me",
                    "threeme_transform") %in%
                    getNamespaceExports("ermeeth2")))
})

test_that("the superseded plot and table functions are gone", {
  gone <- c("simpleplot", "table_macro", "table_macro2", "table_macro_double",
            "table_reference", "table_reference2", "flextable_theme",
            "selectseries")
  expect_false(any(gone %in% getNamespaceExports("ermeeth2")))
})

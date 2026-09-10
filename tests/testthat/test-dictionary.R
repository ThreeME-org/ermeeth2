test_that("the packaged dictionary is well formed", {
  d <- threeme_dictionary()
  expect_true(all(dictionary_columns() %in% names(d)))
  expect_false(anyDuplicated(d$code) > 0)
  expect_true(all(nzchar(d$code)))
  # every label_en filled: the packaged copy is the fallback for everything else
  expect_false(any(is.na(d$label_en)))
  expect_false(any(is.na(d$label_fr)))
})

test_that("every default_transformation is a real transformation", {
  d <- threeme_dictionary()
  tr <- d$default_transformation[!is.na(d$default_transformation)]
  expect_true(all(tr %in% names(threeme_transformations())))
})

test_that("dict_label resolves, translates and falls back to the code", {
  expect_equal(dict_label("GDP"), "GDP")
  expect_equal(dict_label("UNR", lang = "fr"), "Taux de chômage (en %)")
  expect_equal(dict_label("NOT_A_VARIABLE"), "NOT_A_VARIABLE")
  expect_true(is.na(dict_label("NOT_A_VARIABLE", fallback = "na")))
  expect_length(dict_label(c("GDP", "CH", "NOPE")), 3)
})

test_that("short labels fall back to the long one", {
  expect_equal(dict_label("DISPINC_AT", short = TRUE), "Disp. income")
  # GDP has no distinct short form, so the long label comes back
  expect_equal(dict_label("GDP", short = TRUE), dict_label("GDP"))
})

test_that("dict_labels is named by code, ready for `labels =`", {
  lab <- dict_labels(c("GDP", "CH"), lang = "fr")
  expect_named(lab, c("GDP", "CH"))
  expect_equal(unname(lab["CH"]), "Consommation des ménages")
})

test_that("dict_transformations gives the per-variable defaults table_3me takes", {
  tr <- dict_transformations(c("GDP", "F_L", "UNR"))
  expect_equal(unname(tr), c("reldiff", "diff", "ppdiff"))
  expect_named(tr, c("GDP", "F_L", "UNR"))
  expect_equal(unname(dict_transformations("NOPE", default = "level")), "level")
})

test_that("dict_groups preserves the order the codes came in", {
  g <- dict_groups(c("UNR", "GDP", "CH", "NOPE"))
  expect_equal(names(g), c("Labour market", "Activity", "Other"))
  expect_equal(g[["Activity"]], c("GDP", "CH"))
})

test_that("a project dictionary layers over the packaged one", {
  mine <- data.frame(code = c("CARBTAX", "GDP"),
                     label_en = c("Carbon tax revenue", "Gross domestic product"))
  expect_equal(dict_label("CARBTAX", dict = mine), "Carbon tax revenue")
  # an overriding row wins over the packaged one
  expect_equal(dict_label("GDP", dict = mine), "Gross domestic product")
  # and the rest of the packaged dictionary is still there
  expect_equal(dict_label("UNR", dict = mine), dict_label("UNR"))
})

test_that("the ermeeth2.dictionary option is a layer too", {
  mine <- data.frame(code = "CARBTAX", label_en = "Carbon tax revenue")
  withr::with_options(list(ermeeth2.dictionary = mine), {
    expect_equal(dict_label("CARBTAX"), "Carbon tax revenue")
  })
  expect_equal(dict_label("CARBTAX"), "CARBTAX")
})

test_that("overlay = FALSE replaces rather than layers", {
  mine <- data.frame(code = "CARBTAX", label_en = "Carbon tax revenue")
  d <- threeme_dictionary(mine, overlay = FALSE)
  expect_equal(nrow(d), 1L)
  expect_error(threeme_dictionary(overlay = FALSE), "needs a `dict`")
})

test_that("a dictionary can be read from a CSV path", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(code = "CARBTAX", label_en = "Carbon tax revenue"),
                   f, row.names = FALSE)
  expect_equal(dict_label("CARBTAX", dict = f), "Carbon tax revenue")
  expect_error(threeme_dictionary("no-such-file.csv"), "no such file")
})

test_that("a malformed dictionary is rejected with a useful message", {
  expect_error(threeme_dictionary(data.frame(x = 1)), "no `code` column")
  expect_error(threeme_dictionary(data.frame(code = c("A", "A"))), "duplicated codes")
  expect_error(
    threeme_dictionary(data.frame(code = "A", default_transformation = "nope")),
    "unknown `default_transformation`"
  )
  expect_error(
    threeme_dictionary(data.frame(code = "A", indexed_by = "region")),
    "unknown `indexed_by`"
  )
  expect_error(threeme_dictionary(1:3), "must be a data frame")
})

test_that("indexed_by is parsed, including the two-index case", {
  expect_equal(index_split(c("sector", "sector,commodity", NA, "")),
               list("sector", c("sector", "commodity"), character(0), character(0)))
  # EMS is defined over both sectors and commodities in the v4 sources
  expect_true(dict_is_indexed("EMS"))
  expect_true(dict_is_indexed("EMS", by = "sector"))
  expect_true(dict_is_indexed("EMS", by = "commodity"))
  expect_true(dict_is_indexed("PM", by = "commodity"))
  expect_false(dict_is_indexed("PM", by = "sector"))
  # unknown variables claim no index
  expect_false(dict_is_indexed("NOPE"))
})

test_that("the index map is extensible", {
  expect_equal(unname(dictionary_index_map()[c("s", "c")]),
               c("sector", "commodity"))
  expect_equal(unname(dictionary_index_map(c(r = "region"))["r"]), "region")
})

test_that("dictionary_coverage reports what is missing", {
  cov <- dictionary_coverage(c("GDP", "NOPE"), quiet = TRUE)
  expect_equal(cov$code, c("GDP", "NOPE"))
  expect_equal(cov$labelled, c(TRUE, FALSE))
  expect_message(dictionary_coverage(c("GDP", "NOPE")), "Missing: NOPE")
})

test_that("dictionary_coverage accepts a long-format dataset", {
  d <- readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
  cov <- dictionary_coverage(d, quiet = TRUE)
  expect_setequal(cov$code, unique(d$variable))
  # every variable of the packaged fixture is labelled
  expect_true(all(cov$labelled))
  expect_error(dictionary_coverage(data.frame(x = 1)), "no `variable` column")
})

test_that("dictionary_skeleton derives indices from the model sources", {
  sk <- dictionary_skeleton(
    sources = "model_test.mdl",
    exo = "model_test-exovar.mdl",
    base.path = system.file(package = "ermeeth2"),
    missing_only = FALSE
  )
  expect_true(all(dictionary_columns() %in% names(sk)))
  expect_true(nrow(sk) > 0)
  expect_true(all(is.na(sk$indexed_by) |
                    sk$indexed_by %in% c("sector", "commodity", "sector,commodity")))
  # whatever it writes must round-trip through the validator
  expect_no_error(threeme_dictionary(sk, overlay = FALSE))
})

test_that("dictionary_skeleton takes a dataset and writes a CSV", {
  d <- readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
  f <- withr::local_tempfile(fileext = ".csv")
  sk <- suppressMessages(
    dictionary_skeleton(data = d, file = f, missing_only = FALSE)
  )
  expect_setequal(sk$code, unique(d$variable))
  expect_true(file.exists(f))
  expect_setequal(utils::read.csv(f)$code, unique(d$variable))
  # missing_only drops everything the dictionary already labels
  expect_equal(nrow(dictionary_skeleton(data = d)), 0L)
})

test_that("dictionary_skeleton needs exactly one source of codes", {
  expect_error(dictionary_skeleton(), "Give either `sources`")
  expect_error(dictionary_skeleton(sources = "a.mdl", data = data.frame()),
               "not both")
})

test_that("mdl_variables walks the whole tree, not just the lhs", {
  v <- mdl_variables(mdl_parse("PCH[c] * CH[c] = PCHD[c, s] * CHD[c, s]"))
  expect_setequal(v$name, c("PCH", "CH", "PCHD", "CHD"))
  expect_setequal(v$index[v$name == "CHD"], c("c", "s"))
  # an unindexed reference records NA rather than dropping out
  v2 <- mdl_variables(mdl_parse("GDP = CH + I"))
  expect_setequal(v2$name, c("GDP", "CH", "I"))
  expect_true(all(is.na(v2$index)))
})

test_that("label() is deprecated but still resolves", {
  expect_warning(out <- label("GDP"), "deprecated")
  expect_equal(out, "GDP")
  expect_warning(expect_equal(label("UNR", lang = "fr"), dict_label("UNR", lang = "fr")))
  expect_warning(expect_error(label("GDP", lang = "de"), "not carried"))
})

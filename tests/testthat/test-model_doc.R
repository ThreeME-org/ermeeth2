test_that("the tokenizer splits the .mdl vocabulary", {
  tok <- mdl_tokenize("d(log(F_n[f, s])) = @elem(X{-1}, %baseyear) * 1.5")
  expect_true(all(c("name", "punct", "op", "number") %in% tok$type))
  expect_true("@elem" %in% tok$value)
  expect_true("%baseyear" %in% tok$value)
  expect_equal(tok$value[tok$type == "number"], c("1", "1.5"))
})

test_that("operators have precedence, unlike the old pegr grammar", {
  ## a / b * c is (a / b) * c, not a / (b * c).
  eq <- mdl_parse("x = a / b * c")
  expect_equal(eq$rhs$op, "*")
  expect_equal(eq$rhs$lhs$op, "/")
  expect_equal(mdl_latex(eq), "x = \\frac{a}{b} \\; c")

  ## a - b + c is (a - b) + c.
  expect_equal(mdl_parse("x = a - b + c")$rhs$op, "+")
  ## ^ binds tighter than * and is right associative.
  expect_equal(mdl_parse("x = a * b ^ c")$rhs$op, "*")
  expect_equal(mdl_parse("x = a ^ b ^ c")$rhs$rhs$op, "^")
})

test_that("variables carry their indices and lag", {
  v <- mdl_parse("y = PROG[f, s]{-1}")$rhs
  expect_equal(v$name, "PROG")
  expect_equal(v$indices, c("f", "s"))
  expect_equal(v$lag, 1L)
  expect_equal(mdl_latex(v), "PROG_{f, s, t-1}")
})

test_that("qualifiers are parsed and kept off the rendered maths by default", {
  eq <- mdl_parse("Y[c, s] = PhiY[c, s] * YQ[c] if Y[c, s] <> 0")
  expect_false(is.null(eq$cond))
  expect_false(grepl("neq", mdl_latex(eq)))
  expect_true(grepl("\\\\text\\{if \\}", mdl_latex(eq, show_conditions = TRUE)))

  w <- mdl_parse("log(F[f, s]) = ADJUST0_F[f, s] if F[f, s] <> 0 where f in %list_F \\ K")
  expect_equal(w$over, "f")
  expect_equal(w$set, "%list_F")
  expect_equal(w$excluded, "K")
  expect_true(grepl("\\\\forall f \\\\in \\\\%list_F \\\\setminus K", mdl_latex(w)))

  expect_true(mdl_parse("@over PDS[c] = P")$over_keyword)
})

test_that("sum() renders with its index, from the `on` clause", {
  eq <- mdl_parse("F[f] = sum(F[f, s] on s)")
  expect_equal(mdl_latex(eq), "F_{f} = \\sum_{s} F_{f, s}")
  ## A sum of a sum, condition inside: parses, and the outer index wins.
  eq2 <- mdl_parse("CI_toe[ce] = sum(CI_toe[ce, s] if CI_toe[ce, s] <> 0 on s)")
  expect_true(grepl("\\\\sum_\\{s\\}", mdl_latex(eq2)))
})

test_that("@elem folds the base year into the subscript", {
  expect_equal(
    mdl_latex(mdl_parse("x = @elem(phi_MCH[c], %baseyear)")$rhs),
    "\\varphi^{MCH}_{c, t_0}"
  )
  ## A lag inside @elem is relative to the base year.
  expect_equal(
    mdl_latex(mdl_parse("x = @elem(K[s]{-1}, %baseyear)")$rhs),
    "K_{s, t_0-1}"
  )
})

test_that("names map to symbols without the old substring accidents", {
  expect_equal(mdl_name_latex("delta"), "\\delta")
  expect_equal(mdl_name_latex("PROD_L"), "PROD^{L}")
  expect_equal(mdl_name_latex("ADJUST0_F"), "\\alpha^{0,F}")
  expect_equal(mdl_name_latex("PhiY"), "\\varphi^{Y}")
  ## `tau` used to match inside any name containing it.
  expect_equal(mdl_name_latex("STATUS"), "STATUS")
  expect_equal(mdl_name_latex("kappa", mdl_symbols(c(kappa = "\\kappa"))), "\\kappa")
})

test_that("the dependent variable is found without deleting every 'd'", {
  ## The old dependentVar() stripped every literal `d`, turning delta into elta.
  expect_equal(mdl_dependent_name(mdl_parse("delta = 0.05")), "delta")
  expect_equal(mdl_dependent_name(mdl_parse("d(log(F_n[f, s])) = d(log(Y[s]))")),
               "F_n[f, s]")
  ## Numeric factors on the left are skipped.
  expect_equal(mdl_dependent_name(mdl_parse("PK[s] * F[K, s] = PI[s] * I[s]")),
               "PK[s]")
})

test_that("mdl_document() parses the packaged test model whole", {
  items <- mdl_document(sources = "model_test.mdl",
                        exo = "model_test-exovar.mdl",
                        base.path = system.file(package = "ermeeth2"))
  expect_true(all(is.na(items$error)))
  expect_equal(sum(items$kind == "equation"), 6)
  expect_equal(sum(items$kind == "exovar"), 7)

  eq <- items[items$kind %in% c("equation", "exovar"), ]
  expect_false(any(duplicated(eq$id)))
  expect_true(all(grepl("^eq-", eq$id)))
  ## Every equation of the source file is present: the CH equation used to be
  ## dropped silently by the pegr grammar.
  expect_true("CH" %in% eq$variable)
  expect_true("delta" %in% eq$variable)
  ## Descriptions come from the ##! line above the equation.
  expect_equal(items$description[items$variable %in% "Y" & !is.na(items$variable)],
               "Production (GDP)")
})

test_that("model_doc() writes a self-contained, cross-referenceable qmd", {
  out <- withr::local_tempdir()
  res <- model_doc(sources = "model_test.mdl", exo = "model_test-exovar.mdl",
                   base.path = system.file(package = "ermeeth2"),
                   out = "doc", out.path = out, quiet = TRUE)
  expect_equal(basename(res$files), "doc.qmd")
  qmd <- readLines(file.path(out, "doc.qmd"))

  expect_equal(qmd[1], "---")
  expect_true(any(grepl("^## Endogenous", qmd)))
  ## One display equation per equation, each with a quarto id.
  expect_equal(sum(grepl("^\\$\\$ \\{#eq-", qmd)), 13)
  ## The glossary links to those ids, and every link has a target.
  refs <- unique(gsub(".*\\[-@(eq-[^]]*)\\].*", "\\1", grep("\\[-@eq-", qmd, value = TRUE)))
  ids <- gsub("^\\$\\$ \\{#(eq-[^}]*)\\}$", "\\1", grep("^\\$\\$ \\{#eq-", qmd, value = TRUE))
  expect_true(all(refs %in% ids))
  expect_true(any(grepl("Households", qmd)))
})

test_that("model_doc() writes tex when asked, and both formats together", {
  out <- withr::local_tempdir()
  res <- model_doc(sources = "model_test.mdl", exo = "model_test-exovar.mdl",
                   base.path = system.file(package = "ermeeth2"),
                   out = "doc", out.path = out, format = "both", quiet = TRUE)
  expect_setequal(basename(res$files), c("doc.qmd", "doc.tex"))
  tex <- readLines(file.path(out, "doc.tex"))
  expect_true(any(grepl("^\\\\begin\\{document\\}", tex)))
  expect_equal(sum(grepl("^\\\\begin\\{dmath\\}", tex)), 13)
  expect_true(any(grepl("\\\\label\\{eq-model-test-ch\\}", tex)))
  ## Fragments leave the preamble out, for \input into a larger document.
  frag <- model_doc(sources = "model_test.mdl",
                    base.path = system.file(package = "ermeeth2"),
                    out = "frag", out.path = out, format = "tex",
                    standalone = FALSE, quiet = TRUE)
  expect_false(any(grepl("documentclass", readLines(frag$files))))
})

test_that("unparseable statements warn and survive into the output", {
  dir <- withr::local_tempdir()
  writeLines(c("##### Broken", "##! A fine equation", "Y = CH + I",
               "##! A broken one", "Z = ) 1 +"),
             file.path(dir, "broken.mdl"))
  expect_warning(
    res <- model_doc(sources = "broken.mdl", base.path = dir, out = "b",
                     out.path = dir, quiet = TRUE),
    "could not be parsed"
  )
  expect_equal(sum(!is.na(res$items$error)), 1)
  qmd <- readLines(file.path(dir, "b.qmd"))
  ## Shown verbatim rather than dropped on the floor.
  expect_true(any(grepl("Z = \\) 1 \\+", qmd)))
})

test_that("overrides replace the rendering of a single equation", {
  items <- mdl_document(sources = "model_test.mdl",
                        base.path = system.file(package = "ermeeth2"),
                        overrides = c(Y = "Y = C + I + G + X - M"))
  expect_equal(items$latex[items$variable %in% "Y" & !is.na(items$variable)],
               "Y = C + I + G + X - M")
})

test_that("the old entry points are deprecated but still land somewhere", {
  out <- withr::local_tempdir()
  expect_warning(
    teXdoc(sources = "model_test.mdl", exo = "model_test-exovar.mdl",
           base.path = system.file(package = "ermeeth2"),
           out = "old", out.path = out),
    "deprecated"
  )
  expect_true(file.exists(file.path(out, "old.tex")))
  expect_error(make_eq_qmd(), "model_doc")
})

header <- c(
  'title: "Demo"',
  "format:",
  "  revealjs:",
  "    slide-level: 2",
  "  html:",
  "    toc: true",
  "params:",
  "  pres: true",
  '  project_name: "threeme"   # a comment',
  "  group: !expr list(`Group one` = c('x','y'),",
  "               `Group two` = 'z')",
  "",
  "echo: false"
)

test_that("yaml blocks are found with their continuation lines", {
  expect_equal(yaml_block(header, "params"), header[7:11])
  expect_equal(yaml_block(header, "echo"), "echo: false")
  expect_null(yaml_block(header, "toc"))
  expect_equal(names(yaml_children(yaml_block(header, "params"))),
               c("pres", "project_name", "group"))
  expect_length(yaml_children(yaml_block(header, "params"))$group, 2)
})

test_that("parameters replace the defaults and keep their names", {
  block <- merge_params_block(yaml_block(header, "params"),
                              list(scenario = c(ct = "ct1"), project_name = "x", pres = FALSE))
  parsed <- yaml::yaml.load(paste(block, collapse = "\n"),
                            handlers = list(expr = function(x) eval(parse(text = x))))$params
  expect_equal(names(parsed), c("pres", "project_name", "group", "scenario"))
  expect_false(parsed$pres)
  expect_equal(parsed$project_name, "x")
  expect_equal(parsed$scenario, c(ct = "ct1"))
  expect_equal(parsed$group, list(`Group one` = c("x", "y"), `Group two` = "z"))
})

test_that("awkward values survive the round trip", {
  values <- list(a = "it's \"quoted\": # not a comment", b = NULL, c = 1e-6,
                 d = list(`x y` = 1:2), e = NA)
  parsed <- yaml::yaml.load(paste(merge_params_block(NULL, values), collapse = "\n"),
                            handlers = list(expr = function(x) eval(parse(text = x))))$params
  expect_equal(parsed$a, values$a)
  expect_null(parsed$b)
  expect_equal(parsed$c, 1e-6)
  expect_equal(parsed$d, values$d)
  expect_true(is.na(parsed$e))
})

test_that("the rendered format comes first", {
  block <- first_format_block(yaml_block(header, "format"), "html")
  expect_equal(names(yaml::yaml.load(paste(block, collapse = "\n"))$format), c("html", "revealjs"))
  block <- first_format_block(yaml_block(header, "format"), "typst")
  expect_equal(names(yaml::yaml.load(paste(block, collapse = "\n"))$format), c("typst", "revealjs", "html"))
  block <- first_format_block("format: html", "typst")
  expect_equal(names(yaml::yaml.load(paste(block, collapse = "\n"))$format), c("typst", "html"))
})

test_that("blocks indented by other than two spaces stay valid", {
  for (n in c(1, 4)) {
    pad <- strrep(" ", n)
    block <- c("format:", paste0(pad, "html:"), paste0(pad, pad, "center: true"))
    parsed <- yaml::yaml.load(paste(first_format_block(block, "typst"), collapse = "\n"))$format
    expect_equal(names(parsed), c("typst", "html"))
    expect_true(parsed$html$center)
  }
})

test_that("the copy of the template is self-contained", {
  dir <- withr::local_tempdir()
  writeLines(c("---", header, "---", "", "body"), file.path(dir, "demo.qmd"))
  out <- replace_yaml_block(header, "params",
                            merge_params_block(yaml_block(header, "params"), list(pres = FALSE)))
  expect_equal(out[length(out)], "echo: false")
  fm <- split_front_matter(c("---", out, "---", "body"))
  expect_false(fm$yaml$params$pres)
  expect_equal(fm$body, "body")
  expect_error(split_front_matter(c("no", "yaml")), "front matter")
})

# ermeeth2 1.3.0

## Console output goes through cli

* All console output uses `cli` instead of `cat()`, `crayon` and base
  `stop()` / `warning()` / `message()`. `crayon` is dropped from Imports, `cli`
  added.
* **Behaviour change:** the output is now a message (stderr), not stdout.
  `suppressMessages()` silences it; `capture.output()` no longer does.
* Errors use `cli::cli_abort()` and warnings `cli::cli_warn()`, with the detail
  as bullets. The wording is kept where tests or callers match on it.
* The `message_*()` helpers keep their names and arguments, now on top of cli:
  `message_ok()` is a success alert, `message_warning()` a warning alert,
  `message_main_step()` a heading, and so on. `message_3me()` is kept for
  compatibility.
* Diagnostic lists (uncalibrated variables, unidentified codes, roots without
  an aggregation rule) are printed in full and wrapped to the console width.
  Inline cli vectors would stop at 20 items.
* The base-year calibration check lists each off equation with its residual
  instead of printing a data frame.
* `translate_report()` prints a cli definition list.
* Fixed: `stop(message("..."))` raised an *empty* error, because `message()`
  returns `NULL`. This affected `get_sec_com()`, `get_vars()`, the spline
  functions, `loadResults()` and `get_remote_file()`, which now say what is
  wrong.

# ermeeth2 1.2.3

* roxygen markdown is switched on (`Roxygen: list(markdown = TRUE)`). The
  docs were already written in markdown, but rendered with literal backticks,
  and `[fn()]` cross-references were plain text. They are now `\code{}` and
  real links, which also clears the "Lost braces" Rd notes in `mdl_document`,
  `mdl_symbols` and `model_doc`.
* `dtplyr` is dropped from Imports (never used).
* The unit symbols in `threeme_transformations()` are written as `\u` escapes,
  so `R/transformations.R` is ASCII. The strings themselves are unchanged.
* `CLAUDE.md`, `PLAN.md` and `FROM_THORTWO.md` are excluded from the build.

# ermeeth2 1.2.2

* Every magrittr pipe (`%>%`) in the package code and in the `inst/` templates
  is now the native pipe (`|>`). Uses of the magrittr `.` placeholder were
  rewritten explicitly (`.[, col]` inside `mutate()` becomes `.data[[col]]`,
  `cbind(x, .)` / `rbind(x, .)` take the value as a direct argument, bare
  function names get their `()`). Outputs are unchanged, checked before and
  after on a full v4 run for `contrib()`, `contrib_longformat()`,
  `contrib.sub()`, the splines and the sector plots.
* `contrib.sub_longformat()` with a shock scenario no longer fails with
  "object '.' not found": a magrittr `.` had been left behind a native pipe.

# ermeeth2 1.2.1

## R solver fixes (`run_simulations(Rsolver = TRUE)`)

* `R_model_solver()` now adds the variables created by `prg_to_thor()` (`@elem`
  values, coefficients) to every scenario's database, not only to
  `calib_new_base`, which nothing read afterwards. On the full v4 model the
  solve stopped on 347 missing `elem_*` variables.
* `run_simulations()` passes the `rcpp_option` reconciled by `eviews_checks()`
  to `R_model_solver()`. When EViews was requested on a non-Windows OS, the
  switch to R used the raw config value and could fall back to the slow
  `dense-r` backend instead of `sparse`.
* `aggregate_com_sec()` returns early when neither `by_com` nor `by_sec` is
  requested, so models without a bridge file (e.g. the training model) no
  longer stop with "The bridge contains no usable commodity or sector code."

# ermeeth2 1.1.0

## Model translation

* `prg_to_thor()` replaces `translate_modelprg()` as the way to turn a compiled
  `model.prg` and its calibration csv into a solver-ready model.
  `translate_modelprg()` is kept as a thin wrapper, so existing scripts are
  unaffected, but it now runs the new engine.
* `translate_report()` prints what a translation did, and what it could not do.
* Five fixes over the previous implementation, each a failure mode on real
  compiler output:
  - `@elem()` is located by scanning balanced parentheses rather than by three
    shape-specific regexes, so nested forms such as `@elem(pk_sind(-1), 2019)`
    and anything with more than one operator are no longer left in place and
    passed to the solver as undefined variables.
  - its value is obtained by evaluating the inner expression against the
    calibration row, rather than by a switch over the four arithmetic
    operators, so any expression the compiler emits is handled.
  - occurrences are keyed on (expression, year) rather than on the variable
    name, so two `@elem` of the same variable at different years no longer
    collide onto one value.
  - the `rbind(elem_table_1, elem_table_2)` error on a model using lagged
    `@elem` but no plain ones (or the reverse) is gone.
  - EViews logical tests all go through one indicator rewrite. Previously three
    ThreeME-specific tests were pinned to a literal 1 or 0 and the generic
    rewrite only matched `<word><op><word><cmp><number>`, which misses shapes
    such as `(log(pe_senc)-log(p)>0.0)`.
* `prg_to_thor()` takes `base.year` as a required argument instead of defaulting
  to a global, takes an optional `first.year`, and can write the model straight
  out as a `.txt` via `out_file`. `translate_modelprg()` keeps the old defaults.
* Verified end to end on a compiled 3973-equation model: it builds, solves 30
  periods, and reproduces the calibration baseline to a median relative
  difference of 1.8e-13.

# ermeeth2 1.0.0

First version of `ermeeth2`, seeded from `ermeeth` 0.4.00.0 (branch `anissa-dev`).

## Packaging

* Package renamed to `ermeeth2`; all `system.file(..., package =)` calls and
  cross-references repointed.
* Documentation regenerated with roxygen2 8.1.0 (`Config/roxygen2/version`).
  The team should stay on 8.1.0.
* `Depends: R (>= 4.1.0)` — the package uses the native `|>` pipe.
* `tests/` now holds a `testthat` (3rd edition) skeleton. The scratch scripts
  and sample files that used to live there moved to `dev/`, which is excluded
  from the build.
* `data-raw/`, `documentation/` and `plop/` added to `.Rbuildignore`.
* Added a starter vignette.

## Fixes

* `install_dynamo()` unzipped `dynamo.zip`, which does not exist in `inst/`
  (the file is `dynamo_inst.zip`), so the call silently unzipped nothing. The
  shadowfile is now also looked up recursively, since the archive nests its
  contents under a `dynamo/` folder.
* 15 functions were called without being imported or namespaced and would have
  raised "could not find function" at runtime unless the caller happened to
  have the package attached: `rbindlist` (data.table); `createWorkbook`,
  `addWorksheet`, `writeData`, `saveWorkbook`, `loadWorkbook`,
  `removeWorksheet` (openxlsx); `read_lines` (readr); `compact`, `imap_dfr`,
  `map_chr`, `as_vector`, `quietly`, `is_empty` (purrr); `remove_rownames`
  (tibble, now added to `Imports`). This affected `run_simulations()`,
  `loadResults()`, `run_dynamo()`, `translate_modelprg()`, the sector plots
  and `get_from_shared_3me()`.
* Unescaped `%` in roxygen comments truncated the `\arguments` block of
  `simple_plot.Rd`, `table_macro.Rd`, `table_macro2.Rd` and
  `table_macro_double.Rd`, leaving those help pages malformed.
* Non-ASCII subscript characters in `table_reference.R` and
  `table_reference_new.R` replaced with `\U2080` escapes.
* `update_data_merge()`'s stub roxygen block completed (it is exported as of
  roxygen2 8.1.0).
* Startup message moved from `.onLoad()` to `.onAttach()`.

## Known issues carried over from `ermeeth`

See `ermeeth2_handoff.md`. In short: heavy reliance on globals
(`time_waypoints`, `endyear`, `shockyear`, `language`, `reference`,
`output_model_*`, `model_name_*`, `scenario_to_analyse`, `template_default`),
`color_scale` used as a default but defined nowhere, `confetti()` writing
palette objects into the global environment, `table_reference2()`'s `trad_base`
argument being ignored in favour of a global `label_tables`, and two
generations of the table/contrib/plot functions still coexisting.

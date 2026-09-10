# ermeeth2 1.6.0

* `run_simulations()` warns when a calibration script changes the value of an
  exogenous variable at the base year, naming the scenario and the variables.
  The baseline (`baseline_ch`) is compared with `calib.csv`, each shock
  (`shock_ch`) with the baseline calibration. The base year is the
  calibration, and the year every `@elem` of the model is read at, so a
  scenario is expected to leave it alone. The run goes on.
* Same warning for the years before the base year, there for every variable,
  endogenous or not: those years are never solved, only read as lags, so any
  change in them moves the results. (An endogenous variable changed at the
  base year is not reported: the base year is the first period solved, so it
  is recomputed.)

# ermeeth2 1.5.2

* `prg_to_thor()` reads `calib.csv` with `data.table::fread()` instead of
  `utils::read.csv()`, like the rest of the package. Same result, but
  `read.csv()` slows down with the number of columns: on FRA 29x33 (85,302
  columns) the read took 169 s of the 192 s the translation takes, against
  0.9 s now. Every R-solver run goes through it.

# ermeeth2 1.5.1

* Configuration addin, Files tab: `ENDOFLINE.mdl` is no longer shown in the
  calibration and model file lists, and is added at the end of both on save.
  It only closes the lists in the configuration file, so that commenting a
  file in or out never needs a thought for the trailing comma.
* A file list opened in the addin but left unchanged is no longer rewritten,
  so it keeps its comments and commented-out alternatives.

# ermeeth2 1.5.0

* New configuration options `baseline_scenario_folder` and
  `shock_scenario_folder`: the folders the calibration scripts are read from
  (`1_calib_<scenario_baseline>.R` for the first, the
  `2_calib_shock_<scenario>.R` and the automated shocks files for the
  second). Both default to `configuration/scenarii_calib`, so nothing changes
  for a configuration that does not set them; they are meant to name a
  subfolder of it. `readconfig()` builds `calib_baseline` and `calib_scenario`
  from them and `run_simulations()` sources the shocks from there. Every
  `configuration/config_input_*.R` in ThreeME_V4 has the two fields.
* The csv files a run writes for EViews (`calib_baseline.csv`,
  `calib_shock_<scenario>.csv`) stay in `configuration/scenarii_calib`, where
  `src/EViews/run.prg` reads them.
* New `calib_folder_default()` and `list_calib_folders()` (the folder and its
  subfolders). `create_calib()` says which folder option the configuration
  needs when it writes outside the default folder.
* Addins: the configuration addin sets the two folders in its Scenarios tab
  and lists the scenarios found in them; the calibration addin can write into
  a subfolder, creating it; the run addin checks the scenarios against the
  folders of the configuration it runs.

# ermeeth2 1.4.0

Solver options, following the changes in thortwo. Every
`configuration/config_input_*.R` in ThreeME_V4 is updated to match.

## R solver

* **The size cap is gone.** `eviews_checks()` no longer refuses the R solver
  to a `model.prg` above 500 KB, and `max_tresthor_capability` is removed from
  the configuration list. The cap was the limit of tresthor's dense solver;
  thortwo has none. Asking for EViews anywhere but on Windows now switches to
  the R solver whatever the size of the model, instead of aborting.
* `rcpp_option` is renamed `Rcpp`, default `TRUE`. `TRUE` is thortwo's
  compiled `sparse` backend, `FALSE` its pure R `sparse-r` one (it was
  `dense-r`, which thortwo replaced). A configuration file that still says
  `rcpp_option` is read as before. `eviews_checks()` returns `Rcpp`, and no
  longer forces it to `TRUE` when it falls back to R.
* New configuration options, passed to `thortwo::thor_model()` and
  `thortwo::thor_solve()`: `Rsolver_decompose`, `Rsolver_rtol`,
  `Rsolver_atol`, `Rsolver_max_iter`, `Rsolver_damping`, `Rsolver_verbose`.
  thortwo's `dense-cpp` backend and its `sequential` option are not offered.
* `R_model_solver()` builds the model through `thor_model()` on every call
  and relies on thortwo's cache: an unchanged model is loaded, an edited one
  is rebuilt. `recompile_model` maps onto `thor_model(recompile = )`. The
  function's own `themodel.rds` / `data_thor.rds` bookkeeping is removed, so
  `recompile_model = FALSE` can no longer solve a stale model. `model.prg` is
  now translated on every run, which the old `recompile_model = FALSE` path
  skipped.

## EViews solver

* New `eviews_solve_options()` writes the algorithm, the rounding digits and
  the maximum number of iterations into the `solve()` call of
  `src/EViews/solve.prg` (its `o`, `g` and `m` arguments), leaving the rest of
  the file alone. `eviews_model_solver()` calls it before running, with the
  new configuration options `eviews_algorithm` (`"broyden"`, `"newton"` or
  `"gauss-seidel"`), `eviews_digits` and `eviews_max_iter`.

## Configuration

* New `config_solver_defaults()`: the value of every solver option a
  configuration file does not set. `readconfig()` applies them, so older
  files still read.
* The configuration addin has two more tabs, *R solver* and *EViews solver*,
  each saying whether the configuration uses it. *Solver* keeps the choice of
  solver and what applies to both. `config_fields()` gains the sections
  `R solver` and `EViews solver`, the type `choice` and a `choices` column.

# ermeeth2 1.3.1

* `use.superlu` is removed: it was dead config (thortwo has no SuperLU path).
  `readconfig()` no longer reads it, `eviews_checks()` no longer returns it,
  and the config editor no longer offers it.

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

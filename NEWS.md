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

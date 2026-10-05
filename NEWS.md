# ermeeth2 1.0.3

* `readconfig()` reads an incomplete configuration input file. Nine options
  are compulsory (`config_required()`): `project_name`, `model_folder`,
  `scenario_baseline`, `scenario`, `baseyear`, `lastyear`, `lists_files`,
  `calib_files`, `model_files`. A file missing one stops with
  the names of those missing. Every other option has a default
  (`config_defaults()`), used when the file does not set it; the options left
  to their default are listed once, unless `quiet = TRUE`. A file can use an
  option it does not set (`firstyear = baseyear - max_lags`).
* `shockyear` is 2021 unless the file sets it. `firstyear` is
  `baseyear - max_lags` unless the file sets it, and an
  output configuration without `quartos_to_render` renders nothing.
* The configuration addin offers every option `readconfig()` knows, and a
  test holds the two in step. It gains `output_saved` (the aggregated
  databases to save), which it lacked, and shows the default of any option
  the file does not set.
* Fixed: `save_files_res` was forced to `TRUE` whatever the file said.
* Fixed: `scenario_name` and `shocks_nb` were empty in the configuration
  list since the scenario folders were added.

# ermeeth2 1.0.2

* The R solver prints how long each stage took, at the end of the solve:
  the translation of `model.prg`, the build and the compilation of the
  model, and the solve of each scenario.

      Timings: translation 19.6 s | build 38.4 s | compile 44.1 s
        solve: baseline 1 min 14 s | ct1 1 min 16 s

  New configuration option `Rsolver_timings` (default `TRUE`) switches it
  off. Build and compilation are told apart with a thortwo that reports
  them, and shown as one figure otherwise.

# ermeeth2 1.0.1

## Aggregation rules

* The aggregation rules are a csv file shipped with the package,
  `aggregation_rules.csv`, in place of the Excel workbook. One table holds
  both dimensions, told apart by a `sec_com` column (`sectors` or
  `commodities`), with the columns `var_root`, `sum`, `mean`,
  `weighted_mean` and `weight_var`. `read_aggregation_rules()` reads it.
* **The package table is the only copy.** ThreeME_V4 no longer carries one,
  and a `src/bridges/aggregation_rules.csv` or `.xlsx` left in a project is
  not read; a message says so. A set of rules under another name can still
  be passed to `aggregate_com_sec()`.
* Every variable root of the model has a rule. The 207 that had none, and
  were averaged with a warning, now have one. Two columns say where a rule
  stands: `manual_add` is 1 for a rule specified by a user and 0 for one
  filled in by default; `checked` is 0 until the rule has been reviewed.
* Twelve price rules asked for a weight that is never in the database
  (`PMGPD` by `MGPD`, for instance) and fell back to a mean on every run.
  They are now weighted by a volume that exists (`PMGPD` by `QD`) and
  marked `manual_add = 0`.
* The packaged rules also gain six sector rules that only the ThreeME_V4
  workbook had (`EMS`, `EMS_CI_HFC`, `EMS_CI_PFC`, `EMS_CI_SF6`, `EMS_MAT`,
  `EMS_Y`).

# ermeeth2 1.0.0

First version of `ermeeth2`, seeded from `ermeeth` 0.4.00.0 (branch
`anissa-dev`). What follows is what differs from `ermeeth`.

## Running the model in R: thortwo replaces tresthor

* `R_model_solver()` solves through `thortwo`. The model is built by
  `thortwo::thor_model()` on every run and thortwo's cache decides whether
  that means a rebuild: an unchanged model is loaded, an edited one is
  rebuilt. `recompile_model = TRUE` forces the rebuild.
* **No limit on the size of the model.** `eviews_checks()` no longer refuses
  the R solver to a large `model.prg`, and `max_tresthor_capability` is gone.
  Asking for EViews anywhere but on Windows switches to the R solver.
* When thortwo reports an error on an equation by its own id (`eq_2528`),
  the equation itself is printed. With `Rsolver_sequential = TRUE` that
  names the equation and the variable that cannot be determined.
* The variables the translation creates (`@elem` values, coefficients) are
  added to every scenario's database before solving.
* `aggregate_com_sec()` returns early when no aggregation is requested, so a
  model without a bridge file (the training model) runs.

## Model translation

* `prg_to_thor()` turns a compiled `model.prg` and its calibration csv into a
  solver-ready model. `translate_modelprg()` is kept as a wrapper around it.
  `translate_report()` prints what a translation did and could not do.
* `@elem()` is found by scanning balanced parentheses and valued by evaluating
  its expression against the calibration, so nested and multi-operator forms
  such as `@elem(pk_sind(-1), 2019)` are handled. Two `@elem` of the same
  variable at different years no longer collide.
* Every EViews logical test goes through one rewrite; none is pinned to a
  constant any more.
* `base.year` is a required argument. `first.year` and `out_file` (write the
  model as a `.txt`) are optional.
* `calib.csv` is read with `data.table::fread()`: under a second on FRA 29x33
  (85,302 columns), where `read.csv()` took 169 s.

## Configuration

Every `configuration/config_input_*.R` in ThreeME_V4 carries the options
below. A file that leaves one out gets its default.

* **R solver options:** `Rcpp` (`TRUE`, the default, for the compiled solver;
  `FALSE` for the pure R one, which needs no compiler), `Rsolver_decompose`,
  `Rsolver_sequential`, `Rsolver_reuse_jacobian`, `Rsolver_rtol`,
  `Rsolver_atol`, `Rsolver_max_iter`, `Rsolver_damping`, `Rsolver_verbose`. `Rcpp` was `rcpp_option` in `ermeeth`; the old name is
  still read. `use.superlu` is removed.
* **EViews solver options:** `eviews_algorithm` (`"broyden"`, `"newton"` or
  `"gauss-seidel"`), `eviews_digits`, `eviews_max_iter`.
  `eviews_solve_options()` writes them into the `solve()` call of
  `src/EViews/solve.prg` (its `o`, `g` and `m` arguments) before an EViews
  run.
* **Scenario folders:** `baseline_scenario_folder` and
  `shock_scenario_folder`, the folders the calibration scripts are read from.
  Both default to `configuration/scenarii_calib` and are meant to name a
  subfolder of it. The csv files written for EViews stay in
  `configuration/scenarii_calib`.
* `config_solver_defaults()` gives the default of every solver option;
  `calib_folder_default()` and `list_calib_folders()` the calibration folder
  and its subfolders.
* `config_edit()` changes one assignment of a configuration file at a time
  and leaves the comments and the rest of the file alone.
  `read_config_values()`, `list_configs()` and `config_fields()` read
  configurations and describe their fields.

## Checks during a run

* `run_simulations()` warns when a calibration script changes an exogenous
  variable at the base year, or any variable in the years before it, naming
  the scenario and the variables. The base year is the calibration, and the
  earlier years are only read as lags. The run goes on.

## Addins

* **New ThreeME calibration** (`calib_addin()`): writes a baseline or shock
  script from a template, in `scenarii_calib` or a subfolder.
  `create_baseline()` and `create_shock()` do the same from the console.
* **ThreeME configuration** (`config_addin()`): one tab per section,
  including an *R solver* and an *EViews solver* tab, and the scenario
  folders. `ENDOFLINE.mdl` is not shown in the file lists and is added on
  save. A file list left unchanged is not rewritten.
* **Run ThreeME simulations** (`run_addin()`): asks for the project name
  first, lists the output files it would overwrite, and checks that every
  scenario has its calibration script.
* **ThreeME viewer** (`threeme_viewer()`): plots and tables of a result
  database, with the code to reproduce them.

## Graphs, tables and the variable dictionary

* `simple_plot()` and `table_3me()` replace the earlier plot and table
  functions (`simpleplot()`, `table_macro()`, `table_macro2()`,
  `table_macro_double()`, `table_reference()`, `table_reference2()`). Both
  read their transformations from `threeme_transformations()`.
* `threeme_dictionary()` holds the labels, units and default transformations
  of the variables; `dict_labels()`, `dict_transformations()` and
  `dict_groups()` feed them to the plots and tables. `label()` is deprecated.

## Model documentation

* `model_doc()` writes the equations of the `.mdl` sources as Quarto or
  LaTeX, from its own parser. It replaces `teXdoc()` and `make_eq_qmd()`;
  `pegr` is no longer needed.

## Console output

* All console output goes through `cli`. It is a message (stderr), not
  stdout: `suppressMessages()` silences it, `capture.output()` does not.
  `crayon` is dropped from Imports.
* Diagnostic lists (uncalibrated variables, unidentified codes) are printed
  in full.
* Errors written as `stop(message("..."))` were raised empty. They now say
  what is wrong (`get_sec_com()`, `get_vars()`, the spline functions,
  `loadResults()`, `get_remote_file()`).

## Packaging

* Package renamed to `ermeeth2`; `system.file()` calls and cross-references
  repointed.
* Documentation generated with roxygen2 8.1.0, with markdown on. The team
  should stay on 8.1.0.
* `Depends: R (>= 4.1.0)`: the package code uses the native pipe (`|>`)
  throughout, in place of `%>%`.
* `dtplyr` is dropped from Imports; `tibble` and `cli` are added.
* `tests/` holds a `testthat` (3rd edition) suite. The scratch scripts that
  used to live there moved to `dev/`, which is excluded from the build, as
  are `data-raw/`, `documentation/`, `plop/` and the project notes.
* Startup message moved from `.onLoad()` to `.onAttach()`.

## Fixes carried from `ermeeth`

* `install_dynamo()` unzipped `dynamo.zip`, which does not exist in `inst/`
  (the file is `dynamo_inst.zip`), so it silently unzipped nothing.
* 15 functions were called without being imported or namespaced and failed
  with "could not find function" unless the caller had the package attached.
  This affected `run_simulations()`, `loadResults()`, `run_dynamo()`,
  `translate_modelprg()`, the sector plots and `get_from_shared_3me()`.
* Unescaped `%` in roxygen comments left several help pages malformed.

## Known issues carried over from `ermeeth`

See `ermeeth2_handoff.md` and the open items in `PLAN.md`. In short: heavy
reliance on globals in the legacy functions (`time_waypoints`, `endyear`,
`shockyear`, `language`, `reference`, `scenario_to_analyse`), `color_scale`
used as a default but defined nowhere, and `confetti()` writing palette
objects into the global environment.

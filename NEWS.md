# ermeeth2 1.2.1

- **`produce_quartos()` and `render_from_template()`**, for the reworked
  ThreeME_V4 templates:
  - Output format: a template's parameter list may hold `format = "html"`,
    `"revealjs"` or `"typst"` (PDF). Without it, the format follows the
    template's `pres` parameter: `revealjs` for the presentations, `html`
    otherwise. The output gets the matching extension.
  - `keep_qmd = TRUE` (an argument, or an entry of a template's parameter
    list) keeps the copy of the template written at the root of the project,
    `<template>_<project_name>_<date>.qmd`, with the values of the
    configuration as the defaults of its `params`. It renders on its own and
    can be edited to adjust a report.
  - The parameters are written into that copy (as `!expr` when they are not a
    single value, so named vectors and lists keep their names) instead of being
    passed through `execute_params`. `format` and `keep_qmd` are not passed
    to the template.
  - Fixed: `produce_quartos()` overwrote its `templates_selection`,
    `parameters` and `templates_path` arguments with the values of the global
    `config`, so they could not be given.
  - `yaml` added to Imports.

# ermeeth2 1.2.0

- **Endogenous / exogenous swaps** (R solver only), for fitting a target
  before a simulation: `Rsolver_swap = list(co2_target = list(endo =
  "EMS_CO2", exo = "RCO2TAX_VOL"))` in the configuration has the scenario
  `co2_target` solved with EMS_CO2 exogenous, at the path its shock sheet
  sets, and RCO2TAX_VOL endogenous. The baseline and the other scenarios
  use the model as it is. `endo` and `exo` take equal-length vectors to swap
  several pairs at once. The swapped model is made by `thortwo::thor_swap()`
  and cached like the main one. `readconfig()` stops on a malformed entry or
  with `Rsolver = FALSE`, and `R_model_solver()` on a scenario that is not run.

- **Parametric shocks**, for sensitivity analysis: one shock sheet run for
  several values of its parameters. They replace the automated shocks
  (`automated_shocks`), which never ran in ermeeth2 (every scenario was
  dropped, and the parameters only reached `run_simulations()` through the
  global environment).
  - A shock sheet declares its parameters and their defaults with
    `shock_parameters(ct_start = 100, ct_end = 200, ...)` and uses them as
    variables. It stays an ordinary shock script: listed in `scenario`, it
    runs with the defaults.
  - A parameter sheet `3_params_<shock>_<identifier>.R` holds the values to
    test, `shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 0.5, 1))`:
    every combination is a scenario, and the grid can be filtered before it
    ends the sheet. Several parameter sheets can use one shock sheet.
  - The configuration names the parameter sheets, never the values:
    `parametric_shocks = list(ct = c("redis", "traj"))`. They are read from
    `shock_params_folder`, the shock folder unless set. `scenario` is
    optional when `parametric_shocks` is set, and the classic scenarios still
    run alongside.
  - Each row is a scenario `<shock>_<identifier>_<n>` (`ct_redis_1`, ...).
    `readconfig()` reads the parameter sheets and stops, saying why, on a
    missing sheet, a shock sheet that declares no parameters, or a parameter
    it does not declare.
  - The values each parametric scenario ran with, the shock sheet's defaults
    included, are written next to the output databases as
    `<project_name>_params.csv`; `read_3me_params()` reads it back. A later
    run of the project without parametric shocks removes it.
  - `create_params()` writes a parameter sheet listing the parameters of its
    shock sheet at their defaults. `params_path()`, `params_file_name()` and
    `list_params()` follow the file convention.
  - In the calibration bubble, `calibration_environment()` also returns
    `parametric_grid`, the parametric scenarios and their values. With
    `scenario = "ct_redis_4"`, a shock sheet run line by line gets the values
    of that scenario from `shock_parameters()` instead of its defaults, and
    is told so; `use_parametric_scenario()` switches to another scenario, or
    back to the defaults, without building the bubble again.
  - Addins: the calibration addin writes parameter sheets; the configuration
    addin picks the parametric shocks and their folder, and creates the
    missing sheets; the run addin shows the scenarios of each parameter
    sheet with their values, to untick some for one run.
- `automated_shocks` is no longer read; a file that sets it to `TRUE` is told.
- `run_simulations()` runs every shock script before stopping, and lists each
  scenario that failed with its error. A script's result is its `shock_ch`
  rather than the value of its last line.

# ermeeth2 1.1.4

- New R solver option `Rsolver_continuation` (default `TRUE`), passed to
  `thortwo::thor_solve(continuation = )`. A period that does not solve from
  the previous year's solution is solved again with the exogenous variables
  moved there in steps, which is what a large carbon tax from one year to the
  next needs on ThreeME 29x33 (ct100 with 56.7 EUR/tCO2 in 2020, jumps of
  22.5 to 56.7 in 2021). Periods that solved before are solved exactly as
  before. `FALSE` solves each period once. In `readconfig()`, the
  configuration addin and `config_solver_defaults()`.
- Needs thortwo 0.9.4, which has continuation, and now says where to get it
  (`Remotes: OFCE/thortwo`), so that installing ermeeth2 from GitHub brings
  the right thortwo with it.

# ermeeth2 1.1.3

- The configuration addin offers, after a save, to update the `readconfig()`
  call of `Main.R` so that it reads the configuration files just saved. Only
  the arguments of the call are rewritten (internal `main_config_update()`).

# ermeeth2 1.1.2

- Removed `shiny_trial/`. The trial Shiny app now lives in its own repository,
  `threeme_webapp` (OFCE), where each model size ships as an R package built
  with `thortwo::thor_package()` instead of an `.rds` bundle.

# ermeeth2 1.1.1

- `prg_to_thor()` is more than twice as fast on a large model, with the same
  result: 8 s instead of 18 s on FRA 29x33 (26,439 equations). The `@elem` are
  substituted only in the equations that hold one, an `@elem` reads the
  calibration without searching its column names, and the endogenous variable
  of every equation is found in one pass (`prg_first_variable()`).

# ermeeth2 1.0.21

- `table_3me()`: the default look is a plain `gt` table (`theme = "none"`),
  as the tables of the `basic_results` report were before. The OFCE look is
  no longer the default; ask for it with `theme = "ofce"`.

# ermeeth2 1.0.20

- `simple_plot()` and `table_3me()`: new argument `force_short`, 25 by
  default. A variable whose dictionary label is longer than that many
  characters shows its code instead of the label; `force_short = 0` sets no
  limit. Labels given through `labels` are not concerned.

# ermeeth2 1.0.19

- `simple_plot()` and `table_3me()` take their labels from the ThreeME
  dictionary when `labels` is not given: the full label on a plot, the short
  one in a table (the full one where there is no short one). New arguments
  `lang` (`"en"` or `"fr"`, default from the `ermeeth2.lang` option) and
  `dict` (a project dictionary on top of the packaged one). A named `labels`
  still replaces the label of the codes it names; `labels = FALSE` shows the
  codes, as before this version.
- The code of an indexed variable is cut back to the root the dictionary
  knows: `CH_CIND` takes the label of `CH`, `F_K_SSER` that of `F` followed by
  `(K)`.
- The packaged dictionary goes from 34 to 591 variables: the 557 added come
  from the survey of the `.mdl` sources (`data-raw/dictionary_survey.csv`),
  with a full label in English and in French and no short one. The French is
  a translation that has not been reviewed.

# ermeeth2 1.0.18

- `R_model_solver()`: RStudio no longer opens a "Source Cpp" tab at the end of
  a simulation when the C++ compiler gave warnings on the model. The hooks
  RStudio sets on `Rcpp::sourceCpp()` are emptied during the solve and put
  back after it. A compilation that fails is reported as before.

# ermeeth2 1.0.17

- Configuration addin, Files tab: editing `lists_files`, `calib_files` or
  `model_files` no longer loses the comments of the list. It was rewritten as
  a plain vector; it is now edited line by line. A file taken out is commented
  out, a file added is uncommented when the list holds it as a commented-out
  alternative and written on a new line otherwise, and every other line is
  left as it is. Entries that are code (`lists_files`,
  `str_c("R_lists_", iso3, ...)`) stay code.
- New `config_files()`, to ask the same of `config_edit()`. A list that is not
  a `c()` call with one entry per line is still written afresh, with its
  commented lines kept below the files.

# ermeeth2 1.0.16

- `get_sec_com()`: finds the lists file on Linux. `readconfig()` stores `iso3`
  in lower case, so the default path was `R_lists_fra_c29_s33.mdl` for a file
  named `R_lists_FRA_c29_s33.mdl`: found on macOS and Windows, not on a
  case-sensitive file system, where every shock script calling it stopped.
- `get_sec_com(energy_commodities = FALSE)` no longer errors.

# ermeeth2 1.0.15

- `simple_plot()`: the y axis of a percentage (relative difference, growth
  rate) now shows the `digits` decimals asked for. It showed none with the
  default of 2, so that a deviation of a few tenths of a point was labelled
  `0%, 0%, 1%, 1%`.
- New folder `shiny_trial/`, outside the package build: a trial Shiny app that
  solves ThreeME v4 (FRA 8x8, 13x13 or 29x33, picked in the app) through thortwo for a carbon tax and
  a world demand shock set with sliders, with macroeconomic, sector and
  commodity results. Run it with `shiny::runApp("shiny_trial")`.

# ermeeth2 1.0.14

- R solver: the `ELEM_*` variables are no longer written to the results. They
  are the constants the translation creates for the `@elem()` of the model
  (3 128 of the 88 428 variables of a 28x32 run), not variables of ThreeME,
  and an EViews run never had them. The solver still uses them. To keep one,
  name it in `variables_to_keep`.
- The vignette and the help of `prg_to_thor()` now warn that a model variable
  must not be named `elem_<name>_<year>`: it would be overwritten by the
  constant of the same name.

# ermeeth2 1.0.13

- **The simulation results are now written as `.parquet` only.** The `.rds` is
  no longer written unless asked for: `run_simulations(output_format =
  c("rds", "parquet"))`, or the *rds* box of the run addin, which is now
  unticked. `save_3me_output()` follows the same default.
  Code that reads `data/output/<project>.rds` with `readRDS()` has to move to
  `read_3me_output(project_name)`, which reads whichever format is there.
- Without the `arrow` package the results are written as `.rds` instead, with
  a warning, rather than not at all.
- A run warns when it leaves behind an `.rds` (or `.parquet`) of an earlier
  run that it did not rewrite.
- `read_3me_output()` reads the most recent of the two files when both exist;
  it used to take the parquet even when only the `.rds` had been rewritten.
- `calibration_table()` reads the results through `read_3me_output()`.

# ermeeth2 1.0.12

- The four addins (viewer, calibration, configuration, run) now stop when their
  window is closed. Closing the window with its cross rather than with the
  addin's *Done* / *Cancel* button left the app running behind it: R stayed
  busy and had to be interrupted or restarted.

# ermeeth2 1.0.11

- `threeme_viewer()` opens on `GDP` alone. `Y` is selected only when the model
  has no `GDP`; before, both were selected.

# ermeeth2 1.0.10

- `threeme_viewer()` opens a full-size result at once. It now accepts a
  `.parquet` file and queries it instead of reading it: the list of variables
  comes from one small query, and only the rows of the variables selected are
  ever fetched. Pointed at an `.rds`, it uses the `.parquet` that
  `run_simulations()` wrote next to it, when there is one and it is not older
  than the `.rds`. On a 28x32 result (31 million rows, 88 000 variables) the
  data is ready in about 1 s, against 25 s to read the `.rds`. Needs the
  `arrow` package; without it, or without a parquet file, the `.rds` is read
  as before.
- `threeme_viewer(path)` no longer reads the file twice when it starts.

# ermeeth2 1.0.9

- EViews solver: a run no longer stops with `REAL() can only be applied to a
  'numeric', not a 'integer'` when the configuration sets `eviews_timeout` as
  an integer (`0L`). The timeout is now passed to EViews as a double.
- `config_addin()` writes whole numbers as `0`, `2019`, not `0L`, `2019L`. It
  was the addin that put the integer timeout in the file.

# ermeeth2 1.0.8

- `config_addin()`: the file lists of the Files tab now follow the Basics tab.
  After loading a configuration and changing its classification (or its
  country, or its base year), the lists and calibration files kept the names
  of the configuration as loaded -- `R_lists_FRA_c4_s4.mdl` after moving to
  `c28_s32` -- and editing any list then wrote those stale names into the
  file. The lists are now rebuilt from the configuration with the Basics as
  they stand, lines added by hand are kept, and a list that is not edited
  still keeps its code and comments.

# ermeeth2 1.0.7

- `threeme_viewer()` no longer freezes on a full-size model. The variable picker
  now searches server-side instead of sending every variable name to the browser
  (88 000 of them for a 28x32 result), and the plot and table are built from the
  rows of the selected variables only, so changing an option no longer rescans
  the whole result.

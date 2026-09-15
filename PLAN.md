# ermeeth2 — plan

Status key: `[ ]` todo · `[~]` in progress · `[x]` done

# Anissa's plan

## understanding the threeme v4 and ermeeth structure.

We are working on consolidating the ThreeME_V4 project. ThreeME_V4 is the main repo were one can get the structure of the model and the necessary tools to run it. The R functions to run and analyse the model are stored in the ermeeth2 package. We are currently building ermeeth2 as an overhaul of the original ermeeth package. The ThreeME_v4 dev_2026s2 branch is the test branch for ThreeME v4 as it should work with ermeeth2. 

## Goals

ermeeth2 package should contain properly documented functions with examples on a website to be built later that do the following :

   - the necessary functions to read and run threeme simulations 
      - reading configuration files (with checks)
      - installing/checking for dynamo
      - compiling the model
      - loading the calibration and compiling baseline and shock scenarios
      - running the simulations hrough eviews or R via tresthor , for now, other solutions to be added
   - the functions to analyse outputs and help create scenarios
      - tables simple and comparative tables 
      - simple plots accessible from one line of code with minimal compulsory arguments
      - better plots that follow ofce norms (and use the ofce themes)
      - dynamic plots using ggiraph
      - numeric calculations such as contributions to growth
      - data transformations (from wide to long and back)
      - loading excel sheets and filling in holes 
      - .. others ?
   - should store a dictionary of threeme variables so that codes are easily labeled in th graphs and tables 
   - should have a function akin to texdoc to translate mdl file into a quarto files with the equations registry. 

## Some guidelines

- graphs are all in ggplots, interactive graphs use ggiraph, ofce theme to be used as well as ofce::girafy() function when possible
- use new fonts used by ofce : Merriweather (for titles and such) and Arimo
- have some sort of colour themes in place and easily adjustable
- tables use `gt` package and themes present in ofce package. They should work nicely with and without the ofce theme. 
- data treatment is based n long format data output as described in https://ofce.github.io/ermeeth_project/qmd_ermeeth/data_full.html . so most data treatment and graphs and tables functions should work according to that structure. 

## data

In ThreeME variables are looked at in absolute levels (level) , relative difference to a scenario (most often the baseline), in growth rate, absolute difference to a scenario or in delta (for stuff like employment). we could also do index based 100 at a given year (usually the base year). 
The ThreeMe datasets contain multiple scenarios for one given model (Set of equations). If equations are changed, a new batch of simulations are ran

## graph function 

we like simple graphs (line) showing the mentioned data transformation, by default in relative difference. 
we also do contributions to difference or growth plots but we'll talk about this later.
simple_plot should produce graphs for a set of variables and should handle mutliple scenarios at once. geom_interactives should be added through small points and give some basic details in the tool tips : levels  (shock + baseline), growth, rate and relative difference and name of scenarios. 
we should be able to input a color palette. (default colour palettes, use the basic_results template found in threemev4 repo)

## table functions

we want to be able to table out one scenario or multiple. We want also to be able to compare scenarios contained in two different datafull data sets ( different version of the model).
Tables have variables in rows and in columns the time periods, by default t=0 t+1 t+2 t+5 t+10 and long term (last period available).
Tables should mention scenario in columns subheaders , and variables could be organised by group through a user input list variables should use variable codes, and if available the dictionary. if using sector or products variable, name and code of sector should also appear.
The type of transformation should appear (delta, percent ) in a column after the names.
we should have space for table captions. 
any percentage (relative diff, growth rate) are rounded at two decimal. levels also.
multiple  tranformations could be used in a same table, ie gdp in relative difference, employment in absolute diff, unemployment rate in points diff. 
so user input is variables , variables names trough a dictionary (or user input), type of transformation shown, time periods,  scenarios and database used (max 2 databases). so this should look like `scenarios` (implicit from database) and `scenarios_secondary_base` . database default is datafull.
You can look at what has been done before to know what we're looking for.
For now, dictionary hasnt been built yet so you can omit this (while planning for it), so user can input variable code as well as a prettier name.

## New user friendly approach

- we need a function that can create a skeleton of a baseline calib file and a shock calib file :
   create_baseline() takes into argument a character string (lower case letters and numbers , maybe underscores) and creates a blank baseline script with the bare minimum (loading OG calib, finishing on baseline_ch) with a couple of lines to show how its done (select year and an inconsequential variable (world demand and applying a neutral transformation (x1) ). it places the files in the corect folder and gives it the correct name. We can use a template in the inst folder.

 - 3 addins to be created 
   - one to create a calib_baseline or calib_shock based on the function above (user can specify whether baseline or shock and then give a name.) if we can write  script within the addin that would be cool.
   - another one to create the config input and output. each section of the config input is a tab, to is the output, so the user can fill in the different fields. config input and outpu lets the user choose the name and eventually modify an existing one. If a scenario doesnt exist, it creates the sheet. The config addins field should be where possible using drop down menus based on what's available in the folder
   - one addin to run the simulations, the user can select the configs and modify on the fly the scenarios in those as well as the project name. the project name modification is mentioned first, as a reminder that other wise results will override existing simulation database.
 
 
   
# Claude updates here 

## Now

- [x] ThreeME v4 runs on a small model — real output available to test against
      (2026-09-10).
- [x] Captured the minimodel run as `inst/extdata/minimodel.rds` (2026-09-14). All the
      tests run against it rather than synthetic data.
- [x] **Graph and table functions rebuilt** (2026-09-14). One `simple_plot()`, one
      `table_3me()`, both fed by a shared transformation registry. 76 tests pass.
  - `R/transformations.R` — `threeme_transformations()` is the single registry:
    `level`, `reldiff`, `diff`, `ppdiff`, `gr`, `index100`. Each declares its unit
    symbol, whether it needs `values_ref`, whether the baseline rows get dropped and
    whether it is a share. `threeme_transform()` applies them, and accepts a vector
    named by variable so one table can mix them.
  - `R/simple_plot.R` — multiple variables and scenarios; colour goes to the variable
    (or the scenario when there is only one variable), linetype to the other;
    `interactive = TRUE` adds ggiraph hover points whose tooltip carries scenario,
    level, baseline level, growth and relative difference, returned through
    `ofce::girafy()`.
  - `R/table_3me.R` — variables in rows, `t / t+1 / t+2 / t+5 / t+10 / Long term` in
    columns, scenarios as spanners, a unit column after the names, row groups from a
    named list, `caption`, 2 decimals, `gt` + `ofce::theme.gt_ofce()`. Passing
    `data_secondary` compares two model versions under a second spanner level.
  - `shock_year` is now **detected** from the first divergence from the reference
    instead of read from a global. Correctly finds 2021 on the minimodel.
- [x] Deleted the superseded functions and their `.Rd` pages: `simpleplot()`, the old
      `simple_plot()`, `table_macro()`, `table_macro2()`, `table_macro_double()`,
      `table_reference()`, `table_reference2()`, plus `flextable_theme()` and
      `selectseries()`, which had no callers left. Originals remain in
      `/Users/139804/Work/ermeeth/R/` if any of them is needed back.

- [x] **Showcase quarto** (2026-09-14): `documentation/graphs_and_tables.qmd`, rendered
      to `.html`. Runs entirely off the packaged fixture, so it reproduces without a
      simulation run. This is also what confirmed the ggiraph path: `ofce::girafy()`
      only returns a widget under html/interactive, so the tests alone could not.
- [x] **ThreeME viewer RStudio addin** (2026-09-14). `R/threeme_viewer.R`,
      registered in `inst/rstudio/addins.dcf`.
  - `threeme_viewer()` launches it; `threeme_viewer_app()` returns the bare
    `shiny.appobj`, which is what makes the server logic testable with
    `shiny::testServer()` instead of only by clicking.
  - Takes a `data_full` object, an `.rds` path, or nothing; lists the candidate
    ThreeME dataframes found in the global environment.
  - Exposes the arguments of both functions: scenarios, baseline, per-variable
    labels / transformations / groups, year range, palette, colour_by,
    interactive, horizons, shock year, decimals, theme, titles, caption, and the
    second database for a two-model comparison.
  - The Code tab prints the two calls it just ran, and *Insert code* drops them at
    the cursor in RStudio. A test round-trips that code: it must parse, and
    evaluating it must give back a ggplot and a `gt_tbl`.

- [x] **Model documentation rewritten, pegr dropped** (2026-09-14). `teXdoc()` +
      `make_eq_qmd()` are replaced by `model_doc()`, which parses the `.mdl` sources
      once and renders Quarto (default) or LaTeX from the same parsed document.
  - `R/mdl_parse.R` — tokenizer plus a precedence-climbing recursive descent parser
    for the `.mdl` language (`@over`, `if`, `on`/`where ... in ... \\`, `sum`, `d`,
    `log`, `exp`, `@elem`, indices, `{-1}` lags). `pegr` is gone from `Imports`.
  - `R/mdl_latex.R` — LaTeX renderer over the AST. Brackets come from the precedence
    table, not from the source. `mdl_symbols()` holds the name conventions as data,
    with `extra =` to extend them.
  - `R/model_doc.R` — `mdl_document()` returns the parsed document as a dataframe
    (one row per heading / prose / equation / exogenous variable, with `id` and
    `error`); `model_doc()` writes one self-contained `.qmd` (or `.tex`, or both)
    with every equation cross-referenceable as `@eq-<file>-<variable>` and a
    glossary that links to them.
  - On the full v4 model (10 sources + `exogenous.mdl`): **366/366 equations parse**
    against 349 before, the Quarto renders with 0 unresolved refs, and the LaTeX
    compiles to a PDF. The old pipeline dropped 17 equations silently, and its qmd
    half wrote only 1 of the 10 sections before erroring out.
  - Bugs fixed on the way: `dependentVar()` deleting every letter `d` (`delta` ->
    `elta`, `varDelta` in the workshop glossary); no operator precedence
    (`a / b * c` rendered as `a / (b * c)`); the hardcoded `explicit` list of ~10
    ThreeME equations (now the `overrides` argument); `saved.compiled <<-` writing
    to the global environment; writing outputs into `getwd()` before moving them.
    Parse failures now warn and the source is shown verbatim instead of vanishing.
  - `teXdoc()` warns and forwards to `model_doc(format = "tex")`; `make_eq_qmd()`
    errors with a pointer, since it cannot recover `.mdl` sources from `.tex`.

- [x] **Variable dictionary built** (2026-09-15). Packaged as the runtime source of
      truth so plots, tables and Quarto renders resolve labels offline and
      reproducibly; the website copy is generated from it, for discovery only.
  - `data-raw/dictionary.csv` is the file to edit — a CSV, not an xlsx, so changes
    diff and review in a PR. The old `label_database` was built from
    `tests/label.xlsx`, a file that is not in this repo, so it was unreproducible.
    `data-raw/dictionary.R` rebuilds `data/threeme_dictionary_data.rda`.
  - Schema: `code` (root code, unindexed), `label_en/_fr`, `label_short_en/_fr`,
    `unit`, `default_transformation`, `group`, `indexed_by`, `definition_en/_fr`.
    34 seed variables, carried over from `label_database` with French from
    `trad_database` (all 31 matched).
  - `threeme_dictionary()` layers three sources by code: packaged, then the
    `ermeeth2.dictionary` option, then the `dict` argument. That is what absorbs
    variables added to the model between package releases without a re-release.
  - `dict_labels()`, `dict_transformations()` and `dict_groups()` return vectors
    shaped for the `labels =`, `transformation =` and `variables =` arguments of
    `simple_plot()` / `table_3me()`, so a mixed-transformation table is one
    argument instead of a hand-written vector.
  - `dictionary_skeleton()` derives the variable list **and** `indexed_by` from the
    `.mdl` sources, so nobody types either. It warns about index names missing
    from `dictionary_index_map()`.
  - `mdl_variables()` (new, in `R/mdl_parse.R`) walks the whole AST. Collecting only
    dependent variables reported almost everything as unindexed: `CH[c]` is
    determined through `PCH[c] * CH[c] = ...` and most prices appear only on
    right-hand sides. `mdl_document()` also gained an `indices` column.
  - `label()` is deprecated, warns, and forwards to `dict_label()`. Its `data`
    argument is ignored, and the `language` / `label_database` globals are gone.
  - 69 new tests; 236 pass overall.


- [x] **`create_baseline()` / `create_shock()` and the three addins** (2026-09-15).
  - `R/create_calib.R` — `create_baseline()`, `create_shock()` and the shared
    `create_calib()` write a skeleton into `configuration/scenarii_calib/` under
    the name `readconfig()` expects. `calib_scenario_name()` holds the naming
    rule: a baseline gains the `baseline` prefix, because the path is built as
    `1_calib_<scenario_baseline>.R`, so a baseline called `ademe` has to become
    `baseline_ademe` for its own file to be found. `list_calibs()` reads them back.
  - Templates in `inst/templates/`. Both run as written and change nothing: they
    select a year and world demand (`dwd_c01`) and multiply by 1, ending on the
    `baseline_ch` / `shock_ch` object the pipeline reads.
  - **Addin 1, "New ThreeME calibration"** (`calib_addin()`): pick baseline or
    shock, name it, and write it. The script pane is editable, so a scenario can
    be written in the addin rather than in a second step.
  - **Addin 2, "ThreeME configuration"** (`config_addin()`): one tab per section
    — basics, scenarios, files, solver, output. Naming a scenario with no
    calibration file offers to create it, through the same template as addin 1.
  - **Addin 3, "Run ThreeME simulations"** (`run_addin()`): the project name is
    asked first and alone, and the addin lists by name the output files that name
    would overwrite. Scenario overrides apply to the run only — the configuration
    files on disk are not touched.
  - All three refuse to launch outside a ThreeME v4 project
    (`is_threeme_project()`), naming the folders that are missing, and show a
    banner if one is opened in a stray directory.
  - 84 new tests, driven through `shiny::testServer()`; 360 pass overall.

- [x] **Configuration editing is surgical, not regenerative** (2026-09-15).
      `R/config_edit.R` — `config_assignments()` finds top-level assignments and
      their line spans through the parser, and `config_set()` / `config_edit()`
      replace one value at a time. A config file is R code, not data: `calib_files`
      is threaded with `# ALL VERSIONS` comments and commented-out alternatives,
      and the solver section holds a live `if (use.superlu) Sys.setenv(...)` block.
      Regenerating from a template would have silently destroyed all of it.
      `config_edit()` re-parses before writing, so a bad edit fails there rather
      than at run time. `read_config_values()` takes `with =`, because an output
      configuration refers to `project_name` / `lastyear` / `scenario` from the
      input one and cannot be read alone.


### Next, in order

- [ ] **Fill in the calibration templates' realism.** They shock `dwd_c01` by
      name; on a classification without that commodity the skeleton runs but
      selects nothing. Deriving the first commodity from `get_sec_com()` would
      make them classification-agnostic.
- [ ] **Addin 2 rewrites file lists as plain vectors.** Editing `calib_files`,
      `model_files` or `lists_files` in the Files tab loses the commented-out
      alternatives in that one list (untouched lists keep them). Either leave
      those lists out of the addin, or carry the comments through.
- [ ] **Extend `dictionary_index_map()`.** Running the skeleton over the v4 sources
      leaves 23 index names unmapped, so variables carrying only those come out as
      unindexed: `bcl, ce, cea, ceb, cebb, cee, cm, cmo, ct, cth, ctt, DES, E, ecl,
      ecl2, f, ff, ghg, K, L, m, MAT, ss`. Most of the `c`-prefixed ones look like
      commodity subsets (`ce` = energy commodities) and `ss` like a sector subset,
      but that is a guess — needs someone who knows the sets.
- [ ] **Populate the dictionary beyond the 34 seed variables.** The v4 sources hold
      409 codes. Run `dictionary_skeleton(sources = ..., file = ...)` and fill in
      the label columns.
- [ ] **Sector / commodity code tables**, keyed by classification (`c29_s33`,
      `c4xs4`), seeded from `get_sec_com()`. `indexed_by` says *which* table a
      variable joins to; the tables themselves do not exist yet, so
      `simple_plot()` / `table_3me()` still cannot print "Output — Manufacturing
      (S02)" as the plan asks.
- [ ] **Wire the dictionary into `simple_plot()` / `table_3me()` defaults.** They
      take `labels =` and `transformation =` but do not consult the dictionary on
      their own yet.
- [ ] **Contribution graphs** — the deferred half of the plot work. `contrib` family
      still has two competing src versions plus a `contrib_calc` alias to settle.
- [ ] **Repoint the doc callers** at `model_doc()`: `CGE_in_R/results/quarto_templates/texdoc.qmd`
      and `iioa_R_workshop_2023/Main_ISIOA.R` call `teXdoc()` + `make_eq_qmd()`; the
      second of those now errors rather than warning.
- [ ] **Repoint the quartos**, which still call the deleted functions:
      `ThreeME_V4/results/quarto_templates/basic_results.qmd` (`simple_plot`,
      `table_macro2`, `table_reference2`), the `standard_shocks` templates
      (`table_macro`, `table_macro_double`, `table_reference`), and
      `ermeeth_project/qmd_cge_theory/` which calls `simpleplot()` **positionally** —
      those calls need rewriting, not just renaming.
- [ ] `table.output()` is a third, older table function (flextable, docx export, reads
      `loadResults()` output rather than long format). Left in place — decide whether
      it folds into `table_3me()` or stays as the docx path.
- [ ] Check the `simple_plot()` interactive path in a real quarto render.
      `ofce::girafy()` returns a plain ggplot outside html/interactive contexts, so
      the tests can only assert the layer and tooltip text, not the rendered widget.

### Snags worth remembering

- The `.mdl` prose comments are LaTeX, and contain inline maths. Anything that
  rewrites them (markdown <-> tex) has to skip `$...$` spans, or `$a*b$` becomes
  an `\emph{}`. `map_outside_math()` in `R/model_doc.R` is the guard.
- Bare `&` and `%` in `##` prose break a LaTeX build (`value & volume` ends a
  table cell). Escaped in `md_to_tex()`.
- `breqn` is needed for the `.tex` output (`tlmgr install breqn`).

- `ofce::theme_ofce()` **fails unless ggplot2 is attached** — it evaluates its own
  default arguments (`rel(0.5)`) in a frame where `rel` is not visible. Worked around
  by moving ggplot2 from `Imports` to `Depends`. The real fix is a PR to `ofce`
  importing `rel` from ggplot2.
- `as.data.frame()` on a `gt_tbl` returns **formatted strings**, not numbers. Compare
  against `format(round(x, 2), nsmall = 2)` in tests.

## Overhaul backlog

From handoff §4 (open bugs) and §5 (overhaul candidates):

- [ ] Remove `confetti(...) |> list2env(envir = globalenv())` from `table_macro`,
      `table_macro_double`, `table_reference` — they write into the user's global env.
- [ ] Replace the `if(exists(...))` global defaults (`time_waypoints`, `endyear`,
      `shockyear`, `language`, `reference`, `output_model_1/2`, `model_name_1/2`) with
      explicit arguments or a config object. Biggest obstacle to a testable package.
- [ ] `color_scale` is the default for the `scales` argument in three table functions but
      is not defined anywhere in the package — it comes from ThreeME config. Decide:
      vendor it, or make `scales` required.
- [ ] `table_reference2` — `trad_base` branch reads a global `label_tables` instead of the
      `trad_base` argument.
- [ ] Settle the two generations: `table_macro` / `table_macro2`, `table_reference` /
      `table_reference2`. Natural split is one function computing the dataframe, one
      formatting it.
- [ ] Settle `simpleplot` vs `simple_plot`. Check what the quartos and templates still
      call before dropping either.
- [x] `label()` / `trad()` globals: `label()` now forwards to `dict_label()`.
      `trad()` still reads the `language` global and is still the path for UI
      phrases (`trad_database`), which stay separate from variable labels.
- [ ] Settle the `contrib` family (two competing src versions plus a `contrib_calc` alias).
- [ ] Hardcoded default variable lists (`GDP`, `VA_SM`, `CH`, …) duplicated across table
      functions → one shared, overridable spec.
- [ ] Review the unreviewed src drift listed in handoff §6 (direction was never
      determined — this is a review, not a merge).

## Decided / out of scope

- Calibration export functions stay in `ThreeME_V4/src`.
- plotly plots are not integrated.

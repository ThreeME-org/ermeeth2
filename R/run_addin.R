## Addin 3: run the simulations.

#' Output databases an existing project name would overwrite
#'
#' The output file stem is the project name, so re-running under a name that has
#' already been used replaces that project's databases in place. This is what
#' the run addin checks before anything else.
#'
#' @param project_name the project name.
#' @param dir the output folder.
#'
#' @returns the paths that already exist.
#' @export
#'
#' @examples
#' project_output_files("nofit", dir = tempdir())
project_output_files <- function(project_name, dir = file.path("data", "output")) {
  if (!nzchar(project_name %||% "") || !dir.exists(dir)) return(character(0))
  ## Compared as plain strings rather than a pattern: a project name is free
  ## text and may hold regex metacharacters.
  wanted <- as.vector(outer(
    c("", "_com", "_sec", "_sec_com"),
    c(".rds", ".parquet"),
    function(suffix, ext) paste0(project_name, suffix, ext)
  ))
  found <- list.files(dir)
  file.path(dir, found[found %in% wanted])
}

#' Run ThreeME simulations
#'
#' @description An RStudio addin to launch a run. It asks for the project name
#'   first and on its own, because the project name is the output file stem: a
#'   run under a name already used replaces that project's databases in place,
#'   and there is no undo. The addin says plainly which files a given name would
#'   overwrite.
#'
#'   Below that, pick the input and output configurations, and adjust the
#'   baseline and shock scenarios for this run without editing the configuration
#'   on disk -- the overrides apply to the run only.
#'
#'   Only works inside a ThreeME v4 project: it compiles the model from `src/`
#'   and writes the output databases into `data/output`.
#'
#'   Launch it from the Addins menu ("Run ThreeME simulations"), or call this
#'   function.
#'
#' @param path the configuration folder.
#' @param output_dir where the output databases are written.
#' @param viewer where to open: `"dialog"`, `"browser"` or `"pane"`.
#'
#' @returns invisibly, a list with the `config` used and the `result` of
#'   [run_simulations()], or `NULL` if cancelled.
#' @export
#'
#' @examples \dontrun{
#' run_addin()
#' }
run_addin <- function(path = "configuration",
                      output_dir = file.path("data", "output"),
                      viewer = c("dialog", "browser", "pane")) {
  viewer <- match.arg(viewer)
  check_threeme_project(dirname(path), "The run addin")
  app <- run_addin_app(path = path, output_dir = output_dir)
  vw <- if (!rstudioapi::isAvailable()) {
    shiny::browserViewer()
  } else {
    switch(viewer,
           dialog  = shiny::dialogViewer("Run ThreeME simulations", width = 1100, height = 800),
           browser = shiny::browserViewer(),
           pane    = shiny::paneViewer())
  }
  shiny::runGadget(app, viewer = vw)
}

#' @rdname run_addin
#'
#' @returns `run_addin_app()` returns the `shiny.appobj` without launching it.
#' @export
run_addin_app <- function(path = "configuration",
                          output_dir = file.path("data", "output")) {

  for (pkg in c("shiny", "bslib", "rstudioapi")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The run addin needs the '", pkg, "' package. Install it first.")
    }
  }

  calib_dir <- file.path(path, "scenarii_calib")

  ui <- bslib::page_fluid(
    shiny::h3("Run ThreeME simulations"),
    project_banner(dirname(path)),

    ## Deliberately first, and deliberately alone: the project name decides
    ## which files the run replaces.
    bslib::card(
      bslib::card_header("1. Project name"),
      bslib::card_body(
        shiny::p(shiny::tags$b("The project name is the output file stem."),
                 " Running under a name already used overwrites that project's ",
                 "databases. Change it now if you want to keep them."),
        shiny::textInput("project_name", NULL, width = "360px"),
        shiny::uiOutput("overwrite_warning")
      )
    ),

    bslib::card(
      bslib::card_header("2. Configuration"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput("config_in", "Input configuration", choices = character(0)),
          shiny::selectInput("config_out", "Output configuration", choices = character(0))
        ),
        shiny::uiOutput("config_summary")
      )
    ),

    bslib::card(
      bslib::card_header("3. Scenarios for this run"),
      bslib::card_body(
        shiny::p("Overrides apply to this run only. The configuration files on ",
                 "disk are not changed."),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput("baseline", "Baseline", choices = character(0)),
          shiny::selectizeInput("scenarios", "Shock scenarios", choices = character(0),
                                multiple = TRUE)
        ),
        shiny::uiOutput("scenario_warning")
      )
    ),

    bslib::card(
      bslib::card_header("4. Run"),
      bslib::card_body(
        shiny::checkboxInput("to_excel", "Also export the scenarios to Excel", FALSE),
        shiny::checkboxGroupInput("formats", "Output formats",
                                  choices = c("rds", "parquet"),
                                  selected = c("rds", "parquet"), inline = TRUE),
        shiny::actionButton("run", "Run simulations", class = "btn-primary"),
        shiny::actionButton("cancel", "Cancel", class = "btn-outline-secondary"),
        shiny::hr(),
        shiny::verbatimTextOutput("log")
      )
    )
  )

  server <- function(input, output, session) {

    configs <- list_configs(path)
    calibs <- shiny::reactiveVal(list_calibs(calib_dir))

    shiny::observe({
      ins <- configs$name[configs$kind == "input"]
      outs <- configs$name[configs$kind == "output"]
      shiny::updateSelectInput(session, "config_in", choices = ins,
                               selected = if ("threeme" %in% ins) "threeme" else ins[1])
      shiny::updateSelectInput(session, "config_out", choices = outs,
                               selected = if ("threeme" %in% outs) "threeme" else outs[1])
    })

    ## The values the chosen input configuration holds.
    cfg_values <- shiny::reactive({
      nm <- input$config_in
      shiny::req(nzchar(nm %||% ""))
      f <- configs$file[configs$kind == "input" & configs$name == nm]
      shiny::req(length(f))
      tryCatch(read_config_values(f[[1]]), error = function(e) {
        shiny::showNotification(paste("Could not read that configuration:",
                                      conditionMessage(e)), type = "error")
        NULL
      })
    })

    ## The parameters the run will actually use. They live on the server, not
    ## in the controls: loading a configuration seeds them, and touching a
    ## control overrides it. Reading them straight off `input$` instead would
    ## mean the run silently used stale values whenever a control had not
    ## initialised yet.
    run_params <- shiny::reactiveValues(
      project_name = NULL, baseline = NULL, scenarios = NULL
    )

    ## Loading a configuration fills in the project name and the scenarios.
    shiny::observeEvent(cfg_values(), {
      v <- cfg_values()
      shiny::req(!is.null(v))
      cal <- calibs()
      run_params$project_name <- v$project_name %||% ""
      run_params$baseline     <- v$scenario_baseline
      run_params$scenarios    <- unname(v$scenario)

      shiny::updateTextInput(session, "project_name", value = run_params$project_name)
      shiny::updateSelectInput(
        session, "baseline",
        choices = unique(c(v$scenario_baseline, cal$name[cal$type == "baseline"])),
        selected = v$scenario_baseline)
      shiny::updateSelectizeInput(
        session, "scenarios",
        choices = unique(c(unname(v$scenario), cal$name[cal$type == "shock"])),
        selected = unname(v$scenario))
    })

    ## ignoreNULL = FALSE so that clearing every scenario really clears it,
    ## rather than falling back to whatever the configuration said; and
    ## ignoreInit = TRUE so these do not fire once with NULL at startup and
    ## wipe out what loading the configuration has just filled in.
    shiny::observeEvent(input$project_name, {
      run_params$project_name <- input$project_name
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    shiny::observeEvent(input$baseline, {
      run_params$baseline <- input$baseline
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    shiny::observeEvent(input$scenarios, {
      run_params$scenarios <- input$scenarios
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$overwrite_warning <- shiny::renderUI({
      nm <- run_params$project_name %||% ""
      if (!nzchar(nm)) return(shiny::helpText("Give the run a project name."))
      hit <- project_output_files(nm, dir = output_dir)
      if (!length(hit)) {
        return(shiny::div(class = "text-success", shiny::tags$small(
          "No existing output under this name. Nothing will be overwritten.")))
      }
      shiny::div(
        class = "alert alert-danger",
        shiny::tags$b(length(hit), " existing output file(s) will be overwritten:"),
        shiny::tags$ul(lapply(basename(hit), shiny::tags$li))
      )
    })

    output$config_summary <- shiny::renderUI({
      v <- cfg_values()
      if (is.null(v)) return(NULL)
      shiny::tags$small(shiny::HTML(paste0(
        "<b>", v$iso3, "</b> &middot; ", v$classification, " &middot; base year ",
        v$baseyear, " &middot; shock year ", v$shockyear, " &middot; to ",
        v$lastyear, " &middot; solver: ",
        if (isTRUE(v$Rsolver)) "R" else "EViews")))
    })

    missing_calibs <- shiny::reactive({
      cal <- calibs()
      miss <- setdiff(run_params$scenarios %||% character(0),
                      cal$name[cal$type == "shock"])
      base <- run_params$baseline %||% ""
      if (nzchar(base) && !base %in% cal$name[cal$type == "baseline"]) {
        miss <- c(miss, base)
      }
      miss
    })

    output$scenario_warning <- shiny::renderUI({
      miss <- missing_calibs()
      if (!length(miss)) return(NULL)
      shiny::div(class = "alert alert-warning",
                 shiny::tags$b("No calibration file for: "),
                 paste(miss, collapse = ", "),
                 shiny::br(),
                 shiny::tags$small("Create them with the ",
                                   shiny::tags$i("New ThreeME calibration"),
                                   " addin before running."))
    })

    log_text <- shiny::reactiveVal("")

    shiny::observeEvent(input$run, {
      v <- cfg_values()
      if (is.null(v)) return()
      if (!nzchar(run_params$project_name %||% "")) {
        shiny::showNotification("Give the run a project name.", type = "error"); return()
      }
      if (length(missing_calibs())) {
        shiny::showNotification(
          "Some scenarios have no calibration file. Create them first.",
          type = "error")
        return()
      }
      if (!length(run_params$scenarios)) {
        shiny::showNotification("Pick at least one shock scenario.", type = "error")
        return()
      }

      fi <- configs$file[configs$kind == "input" & configs$name == input$config_in]
      fo <- configs$file[configs$kind == "output" & configs$name == input$config_out]
      if (!length(fi) || !length(fo)) {
        shiny::showNotification("Pick both an input and an output configuration.",
                                type = "error")
        return()
      }

      ## The overrides are applied to a copy of the configuration, so a run
      ## under different settings never edits the files on disk.
      cfg <- tryCatch(readconfig(fi[[1]], fo[[1]]), error = function(e) {
        shiny::showNotification(paste("readconfig() failed:", conditionMessage(e)),
                                type = "error")
        NULL
      })
      if (is.null(cfg)) return()

      cfg$input$project_name <- run_params$project_name
      cfg$input$scenario_baseline <- tolower(run_params$baseline)
      cfg$input$scenario <- tolower(run_params$scenarios)
      cfg$input$scenario_name <- paste(cfg$input$scenario, cfg$input$iso3,
                                       sep = "_") |> tolower()
      cfg$input$shocks_nb <- length(cfg$input$scenario)
      cfg$input$calib_baseline <- calib_path(cfg$input$scenario_baseline,
                                             "baseline", path = calib_dir)
      cfg$input$calib_scenario <- vapply(
        cfg$input$scenario,
        function(s) calib_path(s, "shock", path = calib_dir),
        character(1), USE.NAMES = FALSE)

      log_text(paste0("Running ", cfg$input$project_name, ": ",
                      paste(cfg$input$scenario, collapse = ", "),
                      " on baseline ", cfg$input$scenario_baseline, " ...\n"))

      res <- tryCatch(
        run_simulations(configuration = cfg,
                        export_scenarii_to_excel = isTRUE(input$to_excel),
                        output_format = input$formats %||% c("rds", "parquet")),
        error = function(e) e)

      if (inherits(res, "error")) {
        log_text(paste0(log_text(), "FAILED: ", conditionMessage(res), "\n"))
        shiny::showNotification(paste("The run failed:", conditionMessage(res)),
                                type = "error", duration = NULL)
        return()
      }
      log_text(paste0(log_text(), "Done. ", nrow(res), " rows written to ",
                      output_dir, ".\n"))
      shiny::showNotification("Run finished.", type = "message")
      shiny::stopApp(invisible(list(config = cfg, result = res)))
    })

    output$log <- shiny::renderText(log_text())
    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(NULL)))
  }

  shiny::shinyApp(ui, server)
}

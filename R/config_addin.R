## Addin 2: edit the configuration input and output.

#' Deparse a value for a configuration file
#'
#' @param x the value.
#'
#' @returns R source text.
#' @keywords internal
config_code <- function(x) paste(deparse(x), collapse = "\n")

#' A named character vector as `c("a", "b") |> set_names(c("A", "B"))`
#'
#' The scenario entry carries display names alongside the codes, and that is how
#' the configuration files write it.
#'
#' @param x a named character vector.
#'
#' @returns R source text.
#' @keywords internal
scenario_code <- function(x) {
  if (!length(x)) return("c()")
  codes <- config_code(unname(x))
  nms <- names(x)
  if (is.null(nms) || !any(nzchar(nms))) return(codes)
  paste0(codes, " |> \n  set_names(", config_code(unname(nms)), ")")
}

#' ThreeME configuration editor
#'
#' @description An RStudio addin to fill in a ThreeME configuration. Each
#'   section of the input configuration is a tab -- basics, scenarios, file
#'   lists, solver -- and the output configuration is one more, listing the
#'   quartos to render.
#'
#'   Edits are **surgical**: only the assignments you change are rewritten, and
#'   every comment, commented-out alternative and live code block in the file is
#'   left byte-for-byte alone. A configuration file is R code rather than data,
#'   so regenerating it from a template would throw that away.
#'
#'   Naming a scenario the project has no calibration for offers to create it,
#'   through the same template [create_shock()] uses.
#'
#'   Only works inside a ThreeME v4 project: it reads and writes
#'   `configuration/config_input_*.R` and `config_output_*.R`.
#'
#'   Launch it from the Addins menu ("ThreeME configuration"), or call this
#'   function.
#'
#' @param path the configuration folder.
#' @param viewer where to open: `"dialog"`, `"browser"` or `"pane"`.
#'
#' @returns invisibly, a list of the files written.
#' @export
#'
#' @examples \dontrun{
#' config_addin()
#' }
config_addin <- function(path = "configuration",
                         viewer = c("dialog", "browser", "pane")) {
  viewer <- match.arg(viewer)
  check_threeme_project(dirname(path), "The configuration addin")
  app <- config_addin_app(path = path)
  vw <- if (!rstudioapi::isAvailable()) {
    shiny::browserViewer()
  } else {
    switch(viewer,
           dialog  = shiny::dialogViewer("ThreeME configuration", width = 1200, height = 820),
           browser = shiny::browserViewer(),
           pane    = shiny::paneViewer())
  }
  shiny::runGadget(app, viewer = vw)
}

#' @rdname config_addin
#'
#' @returns `config_addin_app()` returns the `shiny.appobj` without launching it.
#' @export
config_addin_app <- function(path = "configuration") {

  for (pkg in c("shiny", "bslib", "rstudioapi")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The configuration addin needs the '", pkg, "' package. Install it first.")
    }
  }

  fields <- config_fields()
  calib_dir <- file.path(path, "scenarii_calib")

  ## One input control per known field, built from config_fields().
  control <- function(row, value) {
    id <- paste0("f_", row$name)
    switch(row$type,
           bool   = shiny::checkboxInput(id, row$label, isTRUE(value)),
           number = shiny::numericInput(id, row$label,
                                        value = if (length(value)) value[[1]] else NA),
           shiny::textInput(id, row$label,
                            value = if (length(value)) as.character(value[[1]]) else ""))
  }

  ui <- bslib::page_sidebar(
    title = "ThreeME configuration",
    project_banner(dirname(path)),
    sidebar = bslib::sidebar(
      width = 330,
      shiny::selectInput("existing", "Start from", choices = c("(new)" = "")),
      shiny::textInput("name", "Configuration name", value = "threeme"),
      shiny::uiOutput("targets"),
      shiny::hr(),
      shiny::actionButton("save", "Save", class = "btn-primary"),
      shiny::actionButton("cancel", "Cancel", class = "btn-outline-secondary"),
      shiny::hr(),
      shiny::uiOutput("status")
    ),
    bslib::navset_tab(
      bslib::nav_panel("Basics",    shiny::uiOutput("tab_basics")),
      bslib::nav_panel("Scenarios", shiny::uiOutput("tab_scenarios")),
      bslib::nav_panel("Files",     shiny::uiOutput("tab_files")),
      bslib::nav_panel("Solver",    shiny::uiOutput("tab_solver")),
      bslib::nav_panel("Output",    shiny::uiOutput("tab_output"))
    )
  )

  server <- function(input, output, session) {

    configs <- shiny::reactiveVal(list_configs(path))
    values  <- shiny::reactiveVal(list())
    out_values <- shiny::reactiveVal(list())
    ## Held reactively so that creating a calibration invalidates the warning
    ## that pointed out it was missing.
    calibs  <- shiny::reactiveVal(list_calibs(calib_dir))

    shiny::observe({
      cf <- configs()
      ins <- cf$name[cf$kind == "input"]
      shiny::updateSelectInput(session, "existing",
                               choices = c("(new)" = "", stats::setNames(ins, ins)))
    })

    ## Loading an existing configuration prefills every control.
    shiny::observeEvent(input$existing, {
      if (!nzchar(input$existing %||% "")) {
        values(list()); out_values(list()); return()
      }
      cf <- configs()
      f <- cf$file[cf$kind == "input" & cf$name == input$existing]
      if (!length(f)) return()
      v <- tryCatch(read_config_values(f[[1]]), error = function(e) {
        shiny::showNotification(paste("Could not read that configuration:",
                                      conditionMessage(e)), type = "error")
        list()
      })
      values(v)
      shiny::updateTextInput(session, "name", value = input$existing)

      ## The output configuration refers to values the input one defines, so it
      ## has to be read with those in scope.
      fo <- cf$file[cf$kind == "output" & cf$name == input$existing]
      out_values(if (length(fo)) {
        tryCatch(read_config_values(fo[[1]], with = v), error = function(e) {
          shiny::showNotification(
            paste("Could not read the output configuration:", conditionMessage(e)),
            type = "warning")
          list()
        })
      } else list())
    })

    section_ui <- function(section) {
      shiny::renderUI({
        v <- values()
        rows <- fields[fields$section == section, , drop = FALSE]
        shiny::tagList(lapply(seq_len(nrow(rows)), function(i) {
          control(rows[i, ], v[[rows$name[i]]])
        }))
      })
    }
    output$tab_basics <- section_ui("Basics")
    output$tab_solver <- section_ui("Solver")

    output$tab_scenarios <- shiny::renderUI({
      v <- values()
      cal <- calibs()
      bases <- cal$name[cal$type == "baseline"]
      shocks <- cal$name[cal$type == "shock"]
      sc <- v$scenario
      shiny::tagList(
        shiny::selectInput("f_scenario_baseline", "Baseline",
                           choices = unique(c(v$scenario_baseline, bases)),
                           selected = v$scenario_baseline),
        shiny::selectizeInput("f_scenario", "Shock scenarios",
                              choices = unique(c(unname(sc), shocks)),
                              selected = unname(sc), multiple = TRUE,
                              options = list(create = TRUE)),
        shiny::helpText("Type a name that does not exist yet to create it."),
        shiny::textAreaInput(
          "f_scenario_names", "Display names, one per scenario, in order",
          value = paste(names(sc) %||% character(0), collapse = "\n"),
          height = "110px"),
        shiny::textAreaInput(
          "f_variables_to_keep", "Variables to keep (blank = all)",
          value = paste(v$variables_to_keep %||% character(0), collapse = "\n"),
          height = "110px"),
        shiny::uiOutput("missing_calibs")
      )
    })

    ## A scenario named in the configuration with no calibration file behind it
    ## is the classic way to get a run that dies halfway through.
    missing_calibs <- shiny::reactive({
      sel <- input$f_scenario
      if (!length(sel)) return(character(0))
      cal <- calibs()
      setdiff(sel, cal$name[cal$type == "shock"])
    })

    output$missing_calibs <- shiny::renderUI({
      miss <- missing_calibs()
      if (!length(miss)) return(NULL)
      shiny::div(
        class = "alert alert-warning",
        shiny::tags$b("No calibration file for: "),
        paste(miss, collapse = ", "),
        shiny::br(),
        shiny::actionButton("create_missing",
                            paste0("Create ", length(miss), " calibration script(s)"),
                            class = "btn-sm btn-warning")
      )
    })

    shiny::observeEvent(input$create_missing, {
      made <- character(0)
      for (nm in missing_calibs()) {
        ok <- tryCatch({
          create_calib(nm, type = "shock", path = calib_dir,
                       open = FALSE, quiet = TRUE)
          TRUE
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error"); FALSE
        })
        if (ok) made <- c(made, nm)
      }
      if (length(made)) {
        shiny::showNotification(
          paste0("Created ", paste(made, collapse = ", "),
                 ". Edit them before running."), type = "message")
        calibs(list_calibs(calib_dir))
      }
    })

    output$tab_files <- shiny::renderUI({
      v <- values()
      box <- function(id, label, val) {
        shiny::textAreaInput(id, label,
                             value = paste(val %||% character(0), collapse = "\n"),
                             width = "100%", height = "200px")
      }
      shiny::tagList(
        shiny::p("One file per line. These are rewritten as plain vectors, so ",
                 "the commented-out alternatives in the original file are lost ",
                 "for any list you edit here. Lists you leave alone keep them."),
        box("f_lists_files", "Lists files", v$lists_files),
        box("f_calib_files", "Calibration files", v$calib_files),
        box("f_model_files", "Model files", v$model_files)
      )
    })

    output$tab_output <- shiny::renderUI({
      ov <- out_values()
      qr <- ov$quartos_to_render
      if (!length(qr)) {
        return(shiny::p("No output configuration loaded. Pick an existing ",
                        "configuration to edit which quartos it renders."))
      }
      shiny::tagList(
        shiny::p("Which quartos are rendered after the run."),
        shiny::tagList(lapply(names(qr), function(n) {
          shiny::checkboxInput(paste0("q_", n), n, isTRUE(qr[[n]]))
        })),
        shiny::helpText("quartos_parameters is left untouched: it references ",
                        "values from the input configuration, and rewriting it ",
                        "would freeze them.")
      )
    })

    output$targets <- shiny::renderUI({
      nm <- input$name %||% ""
      if (!nzchar(nm)) return(shiny::helpText("Name the configuration."))
      fi <- file.path(path, paste0("config_input_", nm, ".R"))
      fo <- file.path(path, paste0("config_output_", nm, ".R"))
      shiny::div(
        shiny::tags$small("Input: "), shiny::tags$code(fi), shiny::br(),
        shiny::tags$small("Output: "), shiny::tags$code(fo),
        if (!file.exists(fi)) {
          shiny::div(class = "text-warning", shiny::tags$small(
            "That input file does not exist yet. Saving copies the ",
            "configuration you started from, then applies your changes."))
        }
      )
    })

    lines_of <- function(x) {
      if (is.null(x) || !nzchar(x)) return(character(0))
      v <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
      v[nzchar(v)]
    }

    ## Everything the user touched, as R source text, ready for config_edit().
    edits <- shiny::reactive({
      out <- list()
      for (i in seq_len(nrow(fields))) {
        nm <- fields$name[i]
        val <- input[[paste0("f_", nm)]]
        if (is.null(val)) next
        if (fields$type[i] == "number" && is.na(val)) next
        if (fields$type[i] == "text") val <- as.character(val)
        out[[nm]] <- I(config_code(val))
      }
      sel <- input$f_scenario
      if (length(sel)) {
        nms <- lines_of(input$f_scenario_names)
        sc <- if (length(nms) == length(sel)) stats::setNames(sel, nms) else sel
        out$scenario <- I(scenario_code(sc))
      }
      if (!is.null(input$f_scenario_baseline)) {
        out$scenario_baseline <- I(config_code(input$f_scenario_baseline))
      }
      if (!is.null(input$f_variables_to_keep)) {
        vk <- lines_of(input$f_variables_to_keep)
        out$variables_to_keep <- I(if (length(vk)) config_code(vk) else "c()")
      }
      for (nm in c("lists_files", "calib_files", "model_files")) {
        val <- input[[paste0("f_", nm)]]
        if (is.null(val)) next
        out[[nm]] <- I(config_code(lines_of(val)))
      }
      out
    })

    shiny::observeEvent(input$save, {
      nm <- input$name %||% ""
      if (!grepl("^[A-Za-z0-9._-]+$", nm)) {
        shiny::showNotification("The configuration name must be a plain file name.",
                                type = "error")
        return()
      }
      cf <- configs()
      src_in <- cf$file[cf$kind == "input" & cf$name == (input$existing %||% "")]
      tgt_in <- file.path(path, paste0("config_input_", nm, ".R"))

      ## Saving under a new name starts from the file we loaded, so the parts
      ## the addin does not model survive.
      if (!file.exists(tgt_in)) {
        if (!length(src_in)) {
          shiny::showNotification(
            "Pick an existing configuration to start from: the addin edits a ",
            "configuration, it does not write one from nothing.", type = "error")
          return()
        }
        file.copy(src_in[[1]], tgt_in)
      }
      ok <- tryCatch({ config_edit(tgt_in, edits()); TRUE },
                     error = function(e) { shiny::showNotification(
                       conditionMessage(e), type = "error"); FALSE })
      if (!isTRUE(ok)) return()

      written <- tgt_in
      ## The output configuration, when there is one to edit.
      src_out <- cf$file[cf$kind == "output" & cf$name == (input$existing %||% "")]
      tgt_out <- file.path(path, paste0("config_output_", nm, ".R"))
      qr <- out_values()$quartos_to_render
      if (length(qr)) {
        if (!file.exists(tgt_out) && length(src_out)) file.copy(src_out[[1]], tgt_out)
        if (file.exists(tgt_out)) {
          new_qr <- stats::setNames(
            lapply(names(qr), function(n) isTRUE(input[[paste0("q_", n)]])),
            names(qr))
          ok <- tryCatch({
            config_edit(tgt_out, list(quartos_to_render = I(config_code(new_qr))))
            TRUE
          }, error = function(e) { shiny::showNotification(
            conditionMessage(e), type = "error"); FALSE })
          if (isTRUE(ok)) written <- c(written, tgt_out)
        }
      }

      configs(list_configs(path))
      shiny::showNotification(paste("Wrote", paste(basename(written), collapse = ", ")),
                              type = "message")
      shiny::stopApp(invisible(list(files = written)))
    })

    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(NULL)))

    output$status <- shiny::renderUI({
      miss <- missing_calibs()
      if (length(miss)) {
        shiny::div(class = "text-warning", shiny::tags$small(
          length(miss), " scenario(s) have no calibration file."))
      } else {
        shiny::div(class = "text-success", shiny::tags$small(
          "Every scenario has a calibration file."))
      }
    })
  }

  shiny::shinyApp(ui, server)
}

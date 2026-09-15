## Addin 1: create a baseline or shock calibration script.

#' Read a template and fill its placeholders
#'
#' @param type `"baseline"` or `"shock"`.
#' @param title the header line.
#'
#' @returns the filled template as a single string.
#' @keywords internal
calib_template_text <- function(type, title) {
  f <- system.file("templates", paste0("calib_", type, ".R"), package = "ermeeth2")
  if (!nzchar(f)) stop("the ", type, " template is missing from the package.")
  body <- readLines(f, warn = FALSE)
  body <- gsub("{{title}}", if (nzchar(title)) title else type, body, fixed = TRUE)
  body <- gsub("{{date}}", format(Sys.Date()), body, fixed = TRUE)
  paste(body, collapse = "\n")
}

#' New ThreeME calibration
#'
#' @description An RStudio addin to create a baseline or shock calibration
#'   script. Pick the type, give it a name, and it writes the file into
#'   `configuration/scenarii_calib/` under the name [readconfig()] expects --
#'   `1_calib_<name>.R` or `2_calib_shock_<name>.R` -- so the configuration and
#'   the file cannot drift apart.
#'
#'   The script pane is editable, so the scenario can be written here rather
#'   than in a second step. It starts from a template that runs as written and
#'   changes nothing until edited.
#'
#'   Only works inside a ThreeME v4 project: it writes into
#'   `configuration/scenarii_calib/`, a path that only means anything there.
#'
#'   Launch it from the Addins menu ("New ThreeME calibration"), or call this
#'   function.
#'
#' @param path the `scenarii_calib` folder.
#' @param viewer where to open: `"dialog"`, `"browser"` or `"pane"`.
#'
#' @returns invisibly, the path written, or `NULL` if cancelled.
#' @export
#'
#' @examples \dontrun{
#' calib_addin()
#' }
calib_addin <- function(path = file.path("configuration", "scenarii_calib"),
                        viewer = c("dialog", "browser", "pane")) {
  viewer <- match.arg(viewer)
  check_threeme_project(dirname(dirname(path)), "The calibration addin")
  app <- calib_addin_app(path = path)
  vw <- if (!rstudioapi::isAvailable()) {
    shiny::browserViewer()
  } else {
    switch(viewer,
           dialog  = shiny::dialogViewer("New ThreeME calibration", width = 1000, height = 750),
           browser = shiny::browserViewer(),
           pane    = shiny::paneViewer())
  }
  shiny::runGadget(app, viewer = vw)
}

#' @rdname calib_addin
#'
#' @returns `calib_addin_app()` returns the `shiny.appobj` without launching it,
#'   which is what makes the server logic testable with [shiny::testServer()].
#' @export
calib_addin_app <- function(path = file.path("configuration", "scenarii_calib")) {

  for (pkg in c("shiny", "bslib", "rstudioapi")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The calibration addin needs the '", pkg, "' package. Install it first.")
    }
  }

  ui <- bslib::page_sidebar(
    title = "New ThreeME calibration",
    project_banner(dirname(dirname(path))),
    sidebar = bslib::sidebar(
      width = 340,
      shiny::radioButtons(
        "type", "Type",
        choices = c("Shock" = "shock", "Baseline" = "baseline"),
        selected = "shock"
      ),
      shiny::textInput("name", "Scenario name", placeholder = "e.g. ct2"),
      shiny::helpText("Lower case, starting with a letter. Letters, digits, ",
                      "`_` and `-`."),
      shiny::textInput("title", "Description",
                       placeholder = "e.g. 2 GDP points of carbon tax"),
      shiny::uiOutput("target"),
      shiny::hr(),
      shiny::checkboxInput("overwrite", "Overwrite if it already exists", FALSE),
      shiny::actionButton("reset", "Reset script to template",
                          class = "btn-outline-secondary btn-sm"),
      shiny::hr(),
      shiny::actionButton("create", "Create", class = "btn-primary"),
      shiny::actionButton("cancel", "Cancel", class = "btn-outline-secondary")
    ),
    bslib::navset_tab(
      bslib::nav_panel(
        "Script",
        shiny::p("Edit here if you like -- what is in this box is what gets written."),
        shiny::textAreaInput("script", NULL, value = "", width = "100%", height = "520px")
      ),
      bslib::nav_panel(
        "Existing",
        shiny::p("The calibrations this project already has."),
        shiny::tableOutput("existing")
      )
    )
  )

  server <- function(input, output, session) {

    calibs <- shiny::reactiveVal(list_calibs(path = path))

    ## The name as the configuration will carry it, or NULL when invalid.
    scenario <- shiny::reactive({
      tryCatch(calib_scenario_name(input$name, input$type), error = function(e) NULL)
    })

    ## The template for the current type and description.
    template_text <- shiny::reactive({
      calib_template_text(input$type %||% "shock", input$title %||% "")
    })

    ## What will actually be written: the box once the user has touched it,
    ## the template until then. Keeping this on the server rather than relying
    ## on the textarea alone is what makes the create step testable, and it
    ## means a create cannot write an empty file if the control has not
    ## initialised yet.
    script_text <- shiny::reactive({
      txt <- input$script
      if (is.null(txt) || !nzchar(trimws(txt))) template_text() else txt
    })

    ## Validation message, or the path that will be written.
    output$target <- shiny::renderUI({
      if (!nzchar(input$name %||% "")) {
        return(shiny::helpText("Give the scenario a name."))
      }
      msg <- tryCatch({
        calib_scenario_name(input$name, input$type)
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(msg)) {
        return(shiny::div(class = "text-danger", shiny::tags$small(msg)))
      }
      p <- calib_path(input$name, input$type, path = path)
      shiny::div(
        shiny::tags$small("Will write "), shiny::tags$code(p),
        if (file.exists(p)) {
          shiny::div(class = "text-warning",
                     shiny::tags$small("That file already exists."))
        }
      )
    })

    ## The template follows the type, and the header follows the description.
    ## Retyping the description should not throw away an edited script, so the
    ## reset is explicit rather than automatic.
    shiny::observeEvent(input$type, {
      shiny::updateTextAreaInput(session, "script", value = template_text())
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$reset, {
      shiny::updateTextAreaInput(session, "script", value = template_text())
    })

    output$existing <- shiny::renderTable({
      ex <- calibs()
      if (!nrow(ex)) return(data.frame(` ` = "No calibrations found.", check.names = FALSE))
      ex[, c("name", "type")]
    })

    shiny::observeEvent(input$create, {
      nm <- scenario()
      if (is.null(nm)) {
        shiny::showNotification("Fix the scenario name first.", type = "error")
        return()
      }
      target <- calib_path(input$name, input$type, path = path)
      if (!dir.exists(path)) {
        shiny::showNotification(
          paste0("No such folder: ", path,
                 ". Open the addin from a ThreeME project."), type = "error")
        return()
      }
      if (file.exists(target) && !isTRUE(input$overwrite)) {
        shiny::showNotification(
          paste0(target, " already exists. Tick overwrite, or pick another name."),
          type = "error")
        return()
      }
      writeLines(strsplit(script_text(), "\n", fixed = TRUE)[[1]], target)
      calibs(list_calibs(path = path))
      if (rstudioapi::isAvailable()) try(rstudioapi::navigateToFile(target), silent = TRUE)
      shiny::stopApp(invisible(list(file = target, name = nm, type = input$type)))
    })

    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(NULL)))
  }

  shiny::shinyApp(ui, server)
}

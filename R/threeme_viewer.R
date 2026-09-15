## ---------------------------------------------------------------------------
## Code generation helpers: the viewer shows the call it just ran, so that a
## session of clicking around ends with something to paste into a quarto.
## ---------------------------------------------------------------------------

## "a" or c("a", "b")
chr_vec_code <- function(x) {
  if (length(x) == 0) return("NULL")
  if (length(x) == 1) return(deparse(as.character(x)))
  paste0("c(", paste(vapply(x, function(v) deparse(as.character(v)), character(1)),
                     collapse = ", "), ")")
}

## c(GDP = "reldiff", UNR = "ppdiff", "level")
##
## An element with an empty name is emitted bare: that is how
## threeme_transform() reads the default for the variables not named, and
## `"" = "level"` would not parse.
named_vec_code <- function(x) {
  if (length(x) == 0) return("NULL")
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  parts <- vapply(seq_along(x), function(i) {
    value <- deparse(unname(x)[[i]])
    if (nzchar(nms[i])) paste0(backtick_if_needed(nms[i]), " = ", value) else value
  }, character(1))
  paste0("c(", paste(parts, collapse = ", "), ")")
}

## list(Activity = c("GDP", "CH"), Labour = "UNR")
named_list_code <- function(x) {
  parts <- paste0(vapply(names(x), backtick_if_needed, character(1)),
                  " = ", vapply(unname(x), chr_vec_code, character(1)))
  paste0("list(", paste(parts, collapse = ", "), ")")
}

backtick_if_needed <- function(nm) {
  if (identical(make.names(nm), nm)) nm else paste0("`", nm, "`")
}

## Assemble fn(arg = value, ...), dropping the arguments left at their default.
build_call <- function(fn, data_expr, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  lines <- paste0("  ", names(args), " = ", unlist(args))
  paste0(fn, "(\n  ", data_expr, ",\n", paste(lines, collapse = ",\n"), "\n)")
}

## Candidate ThreeME dataframes sitting in the global environment.
threeme_candidates <- function(env = globalenv()) {
  objs <- ls(env)
  keep <- vapply(objs, function(nm) {
    x <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    is.data.frame(x) &&
      all(c("year", "variable", "scenario", "values") %in% names(x))
  }, logical(1))
  objs[keep]
}

#' ThreeME viewer
#'
#' @description An RStudio addin to look at a simulation result interactively:
#'   point it at a `data_full` object or `.rds` file, tick the variables you
#'   want, and it draws the plot and builds the table, exposing the arguments of
#'   [simple_plot()] and [table_3me()] as controls.
#'
#'   The Code tab shows the two calls it just ran, so a session of clicking ends
#'   with something to paste into a quarto. Inside RStudio, the *Insert code*
#'   button drops them at the cursor.
#'
#'   Launch it from the Addins menu ("ThreeME viewer"), or call this function.
#'
#' @param data an optional starting point: a ThreeME long-format dataframe, or
#'   the path to an `.rds` file holding one. When `NULL` (default) the viewer
#'   opens on its data panel.
#' @param viewer where to open: `"dialog"` for an RStudio dialog, `"browser"`
#'   for the system browser, `"pane"` for the RStudio viewer pane. Outside
#'   RStudio this always falls back to the browser.
#'
#' @returns Invisibly, a list with the last `plot` and `table` built and the
#'   `code` that produced them.
#' @export
#'
#' @examples \dontrun{
#' threeme_viewer()
#' threeme_viewer(data_full)
#' threeme_viewer(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
#' }
threeme_viewer <- function(data = NULL, viewer = c("dialog", "browser", "pane")) {

  viewer <- match.arg(viewer)
  app <- threeme_viewer_app(data, data_expr = deparse(substitute(data)))

  vw <- if (!rstudioapi::isAvailable()) {
    shiny::browserViewer()
  } else {
    switch(viewer,
           dialog = shiny::dialogViewer("ThreeME viewer", width = 1200, height = 800),
           pane = shiny::paneViewer(minHeight = 600),
           browser = shiny::browserViewer())
  }

  shiny::runGadget(app, viewer = vw)
}

#' @rdname threeme_viewer
#'
#' @param data_expr character(1) how to refer to `data` in the generated code.
#'   Defaults to the expression `data` was passed as.
#'
#' @returns `threeme_viewer_app()` returns the `shiny.appobj` behind the viewer,
#'   without launching it. Useful for testing, or for serving the viewer
#'   outside RStudio.
#' @export
threeme_viewer_app <- function(data = NULL, data_expr = NULL) {

  for (pkg in c("shiny", "bslib", "rstudioapi")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("The ThreeME viewer needs the '", pkg, "' package. Install it first."))
    }
  }

  ## Accept a dataframe, a path, or nothing.
  start_data <- NULL
  start_path <- ""
  start_expr <- "data_full"
  if (is.character(data) && length(data) == 1) {
    start_path <- data
    start_data <- tryCatch(readRDS(data), error = function(e) NULL)
    start_expr <- paste0('readRDS("', data, '")')
  } else if (is.data.frame(data)) {
    start_data <- data
    if (!is.null(data_expr) && length(data_expr) == 1 && nzchar(data_expr) &&
        !identical(data_expr, "NULL")) {
      start_expr <- data_expr
    }
  }

  transformations <- names(threeme_transformations())

  ui <- bslib::page_sidebar(
    title = "ThreeME viewer",
    sidebar = bslib::sidebar(
      width = 340,
      bslib::accordion(
        id = "controls",
        open = c("data", "series"),

        bslib::accordion_panel(
          "Data", value = "data", icon = NULL,
          shiny::radioButtons(
            "source", NULL,
            choices = c("Global environment" = "env", "File (.rds)" = "file"),
            selected = if (nzchar(start_path)) "file" else "env",
            inline = TRUE
          ),
          shiny::conditionalPanel(
            "input.source == 'env'",
            shiny::selectInput("obj", "Object", choices = NULL)
          ),
          shiny::conditionalPanel(
            "input.source == 'file'",
            shiny::textInput("path", "Path", value = start_path),
            shiny::actionButton("browse", "Browse...", class = "btn-sm")
          ),
          shiny::uiOutput("data_status")
        ),

        bslib::accordion_panel(
          "Series", value = "series",
          shiny::selectInput("baseline", "Baseline scenario", choices = NULL),
          shiny::selectizeInput("scenarios", "Scenarios", choices = NULL,
                                multiple = TRUE),
          shiny::selectizeInput("variables", "Variables", choices = NULL,
                                multiple = TRUE),
          shiny::selectInput("transformation", "Transformation",
                             choices = transformations, selected = "reldiff")
        ),

        bslib::accordion_panel(
          "Variables: labels, units, groups", value = "vars",
          shiny::helpText(
            "A label renames the row or series. A transformation set here ",
            "overrides the default above, for the table only. A group name ",
            "turns the table rows into groups."
          ),
          shiny::uiOutput("var_controls")
        ),

        bslib::accordion_panel(
          "Plot options", value = "plot",
          shiny::sliderInput("years", "Years", min = 2000, max = 2100,
                             value = c(2000, 2100), sep = "", step = 1),
          shiny::selectInput("colour_by", "Colour by",
                             choices = c("automatic" = "", "variable", "scenario")),
          shiny::selectInput("palette_type", "Palette",
                             choices = c("distinct", "gradient", "ofce")),
          shiny::numericInput("base_year", "Base year (index100)", value = NA),
          shiny::numericInput("plot_digits", "Decimals", value = 2, min = 0, max = 6),
          shiny::numericInput("x_breaks", "X axis tick spacing", value = NA, min = 1),
          shiny::textInput("plot_title", "Title"),
          shiny::checkboxInput("interactive", "Interactive (ggiraph)", value = FALSE)
        ),

        bslib::accordion_panel(
          "Table options", value = "table",
          shiny::textInput("horizons", "Horizons", value = "0, 1, 2, 5, 10"),
          shiny::numericInput("shock_year", "Shock year (blank = detect)", value = NA),
          shiny::numericInput("end_year", "Last year (blank = end of data)", value = NA),
          shiny::numericInput("table_digits", "Decimals", value = 2, min = 0, max = 6),
          shiny::selectInput("theme", "Theme", choices = c("ofce", "none")),
          shiny::textInput("table_title", "Title"),
          shiny::textInput("table_subtitle", "Subtitle"),
          shiny::textInput("caption", "Caption")
        ),

        bslib::accordion_panel(
          "Compare two models", value = "compare",
          shiny::helpText("A second simulation, typically the same scenarios run ",
                          "after the equations changed."),
          shiny::textInput("path2", "Second file (.rds)"),
          shiny::actionButton("browse2", "Browse...", class = "btn-sm"),
          shiny::textInput("model_names", "Model names", value = "model 1, model 2")
        )
      ),
      shiny::div(
        class = "d-grid gap-2",
        shiny::actionButton("insert", "Insert code at cursor", class = "btn-sm"),
        shiny::actionButton("done", "Done", class = "btn-primary btn-sm")
      )
    ),

    bslib::navset_card_tab(
      bslib::nav_panel("Plot", shiny::uiOutput("plot_area")),
      bslib::nav_panel("Table", gt::gt_output("table")),
      bslib::nav_panel(
        "Code",
        shiny::verbatimTextOutput("code"),
        shiny::helpText("Copy this into your quarto, or use 'Insert code at cursor'.")
      )
    )
  )

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(plot = NULL, table = NULL, code = "")

    shiny::updateSelectInput(session, "obj", choices = threeme_candidates())

    ## --- data -------------------------------------------------------------

    browse_rds <- function(caption) {
      if (!rstudioapi::isAvailable()) return(NULL)
      tryCatch(
        rstudioapi::selectFile(caption = caption, filter = "R data (*.rds)"),
        error = function(e) NULL
      )
    }

    shiny::observeEvent(input$browse, {
      f <- browse_rds("Select a simulation result")
      if (!is.null(f)) shiny::updateTextInput(session, "path", value = f)
    })
    shiny::observeEvent(input$browse2, {
      f <- browse_rds("Select the second simulation result")
      if (!is.null(f)) shiny::updateTextInput(session, "path2", value = f)
    })

    dat <- shiny::reactive({
      if (identical(input$source, "file")) {
        shiny::req(nzchar(input$path %||% ""))
        if (!file.exists(input$path)) return(NULL)
        tryCatch(readRDS(input$path), error = function(e) NULL)
      } else {
        if (!is.null(start_data) && !isTruthy_chr(input$obj)) return(start_data)
        shiny::req(isTruthy_chr(input$obj))
        tryCatch(get(input$obj, envir = globalenv()), error = function(e) NULL)
      }
    })

    dat2 <- shiny::reactive({
      if (!isTruthy_chr(input$path2)) return(NULL)
      if (!file.exists(input$path2)) return(NULL)
      tryCatch(readRDS(input$path2), error = function(e) NULL)
    })

    data_expr <- shiny::reactive({
      if (identical(input$source, "file")) paste0('readRDS("', input$path, '")')
      else if (isTruthy_chr(input$obj)) input$obj
      else start_expr
    })

    ## What the data is missing, if anything - said out loud rather than
    ## failing further down with a cryptic error.
    output$data_status <- shiny::renderUI({
      d <- dat()
      if (is.null(d)) {
        return(shiny::div(class = "text-danger small",
                          "No data loaded. Pick an object or a readable .rds file."))
      }
      missing_cols <- setdiff(c("year", "variable", "scenario", "values"), names(d))
      if (length(missing_cols) > 0) {
        return(shiny::div(class = "text-danger small",
                          paste("Not ThreeME long format. Missing:",
                                paste(missing_cols, collapse = ", "))))
      }
      shiny::div(
        class = "text-success small",
        sprintf("%s rows, %s variables, %s scenarios, %s-%s",
                nrow(d), length(unique(d$variable)), length(unique(d$scenario)),
                min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))
      )
    })

    ## Fill the series controls from whatever data got loaded.
    shiny::observeEvent(dat(), {
      d <- dat()
      shiny::req(d, "scenario" %in% names(d))
      scen <- sort(unique(d$scenario))
      base_guess <- if ("baseline" %in% scen) "baseline" else scen[1]

      shiny::updateSelectInput(session, "baseline", choices = scen, selected = base_guess)
      shiny::updateSelectizeInput(session, "scenarios", choices = scen,
                                  selected = setdiff(scen, base_guess))
      vars <- sort(unique(d$variable))
      var_guess <- intersect(c("Y", "GDP"), vars)
      if (!length(var_guess)) var_guess <- utils::head(vars, 3)
      shiny::updateSelectizeInput(session, "variables", choices = vars,
                                  selected = var_guess)
      shiny::updateSliderInput(
        session, "years",
        min = min(d$year, na.rm = TRUE), max = max(d$year, na.rm = TRUE),
        value = c(min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))
      )
    })

    ## --- per-variable controls -------------------------------------------

    output$var_controls <- shiny::renderUI({
      vars <- input$variables
      if (length(vars) == 0) {
        return(shiny::helpText("Select some variables first."))
      }
      lapply(vars, function(v) {
        id <- make.names(v)
        shiny::div(
          class = "border-bottom pb-2 mb-2",
          shiny::strong(v),
          shiny::textInput(paste0("label_", id), "Label", value = ""),
          shiny::selectInput(paste0("tr_", id), "Transformation (table)",
                             choices = c("(default)" = "", transformations)),
          shiny::textInput(paste0("group_", id), "Group", value = "")
        )
      })
    })

    labels_vec <- shiny::reactive({
      vars <- input$variables
      out <- vapply(vars, function(v) input[[paste0("label_", make.names(v))]] %||% "",
                    character(1))
      out <- out[nzchar(out)]
      out
    })

    ## A named vector only for the variables actually overridden, plus the
    ## default as an unnamed element - which is what threeme_transform() wants.
    transformation_arg <- shiny::reactive({
      vars <- input$variables
      over <- vapply(vars, function(v) input[[paste0("tr_", make.names(v))]] %||% "",
                     character(1))
      over <- over[nzchar(over)]
      if (length(over) == 0) return(input$transformation)
      c(over, stats::setNames(input$transformation, ""))
    })

    ## Groups, in the order the variables were selected.
    variables_arg <- shiny::reactive({
      vars <- input$variables
      grp <- vapply(vars, function(v) input[[paste0("group_", make.names(v))]] %||% "",
                    character(1))
      if (!any(nzchar(grp))) return(vars)
      grp[!nzchar(grp)] <- "Other"
      split(vars, factor(grp, levels = unique(grp)))
    })

    model_names <- shiny::reactive({
      nm <- trimws(strsplit(input$model_names %||% "", ",")[[1]])
      nm <- nm[nzchar(nm)]
      if (length(nm) < 2) c("model 1", "model 2") else nm[1:2]
    })

    horizons <- shiny::reactive({
      h <- suppressWarnings(as.numeric(trimws(strsplit(input$horizons %||% "", ",")[[1]])))
      h <- h[!is.na(h)]
      if (length(h) == 0) c(0, 1, 2, 5, 10) else h
    })

    ## --- outputs ----------------------------------------------------------

    the_plot <- shiny::reactive({
      d <- dat()
      shiny::req(d, length(input$variables) > 0)
      p <- try(simple_plot(
        d,
        variables = input$variables,
        scenarios = input$scenarios,
        transformation = input$transformation,
        labels = if (length(labels_vec())) labels_vec() else NULL,
        startyear = input$years[1],
        endyear = input$years[2],
        base_year = if (is.na(input$base_year)) NULL else input$base_year,
        name_baseline = input$baseline,
        palette = threeme_palette(keys = input$variables, type = input$palette_type),
        colour_by = if (nzchar(input$colour_by %||% "")) input$colour_by else NULL,
        title = if (nzchar(input$plot_title %||% "")) input$plot_title else NULL,
        digits = input$plot_digits,
        x_breaks = if (is.na(input$x_breaks)) NULL else input$x_breaks,
        interactive = input$interactive
      ), silent = TRUE)
      rv$plot <- p
      p
    })

    output$plot_area <- shiny::renderUI({
      if (isTRUE(input$interactive)) ggiraph::girafeOutput("giraph", height = "540px")
      else shiny::plotOutput("plot", height = "540px")
    })

    output$plot <- shiny::renderPlot({
      p <- the_plot()
      if (inherits(p, "try-error")) {
        shiny::validate(shiny::need(FALSE, as.character(p)))
      }
      p
    })

    output$giraph <- ggiraph::renderGirafe({
      p <- the_plot()
      if (inherits(p, "try-error")) {
        shiny::validate(shiny::need(FALSE, as.character(p)))
      }
      if (inherits(p, "girafe")) p else ggiraph::girafe(ggobj = p)
    })

    output$table <- gt::render_gt({
      d <- dat()
      shiny::req(d, length(input$variables) > 0)
      tb <- try(table_3me(
        d,
        variables = variables_arg(),
        transformation = transformation_arg(),
        scenarios = input$scenarios,
        data_secondary = dat2(),
        model_names = model_names(),
        horizons = horizons(),
        shock_year = if (is.na(input$shock_year)) NULL else input$shock_year,
        end_year = if (is.na(input$end_year)) NULL else input$end_year,
        labels = if (length(labels_vec())) labels_vec() else NULL,
        name_baseline = input$baseline,
        base_year = if (is.na(input$base_year)) NULL else input$base_year,
        digits = input$table_digits,
        title = if (nzchar(input$table_title %||% "")) input$table_title else NULL,
        subtitle = if (nzchar(input$table_subtitle %||% "")) input$table_subtitle else NULL,
        caption = if (nzchar(input$caption %||% "")) input$caption else NULL,
        theme = input$theme
      ), silent = TRUE)
      if (inherits(tb, "try-error")) {
        shiny::validate(shiny::need(FALSE, as.character(tb)))
      }
      rv$table <- tb
      tb
    })

    the_code <- shiny::reactive({
      shiny::req(length(input$variables) > 0)
      de <- data_expr()

      tr <- transformation_arg()
      tr_code <- if (length(tr) == 1 && is.null(names(tr))) deparse(tr) else named_vec_code(tr)
      vars <- variables_arg()
      vars_code <- if (is.list(vars)) named_list_code(vars) else chr_vec_code(vars)

      plot_args <- list(
        variables = chr_vec_code(input$variables),
        scenarios = if (length(input$scenarios)) chr_vec_code(input$scenarios) else NULL,
        transformation = deparse(input$transformation),
        labels = if (length(labels_vec())) named_vec_code(labels_vec()) else NULL,
        startyear = input$years[1],
        endyear = input$years[2],
        base_year = if (is.na(input$base_year)) NULL else input$base_year,
        name_baseline = deparse(input$baseline),
        colour_by = if (nzchar(input$colour_by %||% "")) deparse(input$colour_by) else NULL,
        title = if (nzchar(input$plot_title %||% "")) deparse(input$plot_title) else NULL,
        digits = input$plot_digits,
        x_breaks = if (is.na(input$x_breaks)) NULL else input$x_breaks,
        interactive = if (isTRUE(input$interactive)) "TRUE" else NULL
      )

      table_args <- list(
        variables = vars_code,
        transformation = tr_code,
        scenarios = if (length(input$scenarios)) chr_vec_code(input$scenarios) else NULL,
        model_names = if (!is.null(dat2())) chr_vec_code(model_names()) else NULL,
        horizons = chr_vec_code_num(horizons()),
        shock_year = if (is.na(input$shock_year)) NULL else input$shock_year,
        end_year = if (is.na(input$end_year)) NULL else input$end_year,
        labels = if (length(labels_vec())) named_vec_code(labels_vec()) else NULL,
        name_baseline = deparse(input$baseline),
        digits = input$table_digits,
        title = if (nzchar(input$table_title %||% "")) deparse(input$table_title) else NULL,
        subtitle = if (nzchar(input$table_subtitle %||% "")) deparse(input$table_subtitle) else NULL,
        caption = if (nzchar(input$caption %||% "")) deparse(input$caption) else NULL,
        theme = deparse(input$theme)
      )
      if (!is.null(dat2())) {
        table_args <- append(
          table_args,
          list(data_secondary = paste0('readRDS("', input$path2, '")')),
          after = 2
        )
      }

      code <- paste0(
        "library(ermeeth2)\n\n",
        build_call("simple_plot", de, plot_args),
        "\n\n",
        build_call("table_3me", de, table_args),
        "\n"
      )
      rv$code <- code
      code
    })

    output$code <- shiny::renderText(the_code())

    shiny::observeEvent(input$insert, {
      code <- tryCatch(the_code(), error = function(e) "")
      if (!nzchar(code)) return(NULL)
      if (rstudioapi::isAvailable()) {
        rstudioapi::insertText(text = code)
        shiny::showNotification("Code inserted at cursor.", type = "message")
      } else {
        shiny::showNotification("Only available inside RStudio.", type = "warning")
      }
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(list(
        plot = rv$plot, table = rv$table, code = rv$code
      )))
    })
  }

  shiny::shinyApp(ui, server)
}

## Numeric vectors in generated code, without the quotes chr_vec_code adds.
chr_vec_code_num <- function(x) {
  if (length(x) == 1) return(as.character(x))
  paste0("c(", paste(x, collapse = ", "), ")")
}

isTruthy_chr <- function(x) !is.null(x) && length(x) == 1 && nzchar(x)

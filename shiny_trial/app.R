## ThreeME solving app: a trial.
##
##   shiny::runApp("shiny_trial")
##
## Solves ThreeME v4 for France in R through thortwo, for a carbon tax and/or a
## world demand shock sized with a slider, and shows the basic macroeconomic
## results and the sector and commodity ones. The size of the model is picked
## in the app: 8x8, 13x13 or 29x33 (commodities x sectors).
## Nothing is imported: each compiled model and its calibration come from a
## model_bundle_<classification>.rds, written by build_bundle.R.
##
## The app opens on 29x33; to open on another one:
##   Sys.setenv(THREEME_CLASSIFICATION = "c13_s13")
##
## Not part of the package (see .Rbuildignore).

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
})
if (requireNamespace("ermeeth2", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ermeeth2))
} else {
  ## not installed: use the sources this folder sits in
  pkgload::load_all("..", quiet = TRUE)
}

## ---- the models ---------------------------------------------------------------

## One bundle per classification, picked in the app. Those found next to app.R
## are offered, smallest first.
classifications <- sub("^model_bundle_(.*)\\.rds$", "\\1", list.files(pattern = "^model_bundle_.*\\.rds$"))
size_of <- function(cl) as.numeric(sub("^c(\\d+)_s(\\d+)$", "\\1", cl))
classifications <- classifications[order(size_of(classifications))]
stopifnot(length(classifications) > 0)
names(classifications) <- sub("^c(\\d+)_s(\\d+)$", "\\1 commodities x \\2 sectors", classifications)
default_classification <- Sys.getenv("THREEME_CLASSIFICATION", "c29_s33")
if (!default_classification %in% classifications) default_classification <- classifications[[1]]

gdp_parts <- c("GDP", "CH", "I", "G", "X", "M")
## for the codes the dictionary does not know yet
extra_labels <- c(PX = "Export prices", PM = "Import prices", PY = "Production prices",
                  PCH = "Consumer prices", W = "Wages", F_L = "Employment")

#' Everything the app needs to know about one classification
#'
#' Read once per R process and kept: the bundle of the 29x33 model takes a
#' couple of seconds to read.
setups <- new.env()
get_setup <- function(classification) {
  if (!is.null(setups[[classification]])) return(setups[[classification]])
  bundle <- readRDS(paste0("model_bundle_", classification, ".rds"))
  calib  <- bundle$data
  years  <- bundle$years

  ## World demand is shocked through its shifter, one per commodity:
  ## WD[c] = WD_bis[c] * DWD[c]
  dwd_vars <- paste0("dwd_", bundle$commodities)
  stopifnot(all(c("rco2tax_vol", "gdp", dwd_vars) %in% names(calib)))

  ## The carbon tax (RCO2TAX_VOL) is in constant euros per tonne of CO2. Its
  ## ex-ante revenue is the tax times the emissions it applies to, as in
  ## 2_calib_shock_ct1.R of ThreeME_V4, which goes the other way round (from 1
  ## point of GDP to a tax rate).
  tax_basis <- with(calib, ems_ci_co2 + ems_mat_co2 + ems_y_co2 + ems_ch_co2)
  at_shock  <- which(calib$year == years$shock)

  table_groups <- list(
    Activity = gdp_parts,
    Labour   = c("F_L", "UNR", "W"),
    Prices   = c("PCH", "PY", "PX", "PM"),
    `Balances and emissions` = c("RBAL_TRADE_VAL", "RBAL_G_PRIM_VAL", "RSAV_H_VAL", "EMS")
  ) |>
    map(~ intersect(.x, toupper(names(calib)))) |> compact()
  main_vars  <- unlist(table_groups, use.names = FALSE)
  var_labels <- dict_labels(main_vars)
  unlabelled <- intersect(names(extra_labels), names(var_labels)[var_labels == names(var_labels)])
  var_labels[unlabelled] <- extra_labels[unlabelled]

  setups[[classification]] <- list(
    classification = classification,
    bundle = bundle, calib = calib, years = years,
    dwd_vars = dwd_vars,
    gdp_points_per_euro = 100 * tax_basis[at_shock] / calib$gdp[at_shock],
    table_groups = table_groups, main_vars = main_vars, var_labels = var_labels,
    all_vars = sort(toupper(setdiff(c(bundle$endo, bundle$exo), bundle$solver_only))),
    sc_names = list(
      sector    = with(bundle$names_sectors, setNames(name, tolower(code))),
      commodity = with(bundle$names_commodities, setNames(name, tolower(code)))),
    sc_codes = list(sector = bundle$sectors, commodity = bundle$commodities)
  )
}

## The years are those of build_bundle.R, the same for every classification:
## the sliders are laid out on them once.
years <- get_setup(default_classification)$years

## ---- the two shocks ----------------------------------------------------------

#' The calibration with the shocks applied from the shock year on
#' @param m a setup, from get_setup()
#' @param carbon_tax euros per tonne of CO2, added to the baseline rate
#' @param world_demand permanent change of world demand, in %
apply_shocks <- function(m, carbon_tax = 0, world_demand = 0) {
  data <- m$calib
  after <- data$year >= m$years$shock
  data$rco2tax_vol[after] <- data$rco2tax_vol[after] + carbon_tax
  for (v in m$dwd_vars) data[[v]][after] <- data[[v]][after] * (1 + world_demand / 100)
  data
}

scenario_name <- function(carbon_tax, world_demand) {
  paste(c(if (carbon_tax != 0) sprintf("CO2 tax %g €/t", carbon_tax),
          if (world_demand != 0) sprintf("World demand %+g%%", world_demand)),
        collapse = " & ")
}

## ---- the solver --------------------------------------------------------------

## A model is built on its first solve and kept for every session of this
## process. thortwo caches the build on disk: the C++ compilation happens once
## per machine (about 20 s at 13x13, 1 min 30 at 29x33), a few seconds
## afterwards.
solver <- new.env()

get_model <- function(m) {
  cl <- m$classification
  if (!is.null(solver[[cl]])) return(solver[[cl]]$model)
  build <- function(backend) {
    thortwo::thor_model(
      name = paste0("threeme_shiny_trial_", cl),
      endogenous = m$bundle$endo, exogenous = m$bundle$exo,
      coefficients = m$bundle$coef, equations = m$bundle$equations,
      backend = backend, decompose = TRUE, verbose = FALSE, timings = FALSE)
  }
  ## compiled if the machine can, pure R otherwise (about 15 times slower)
  backend <- "sparse"
  model <- tryCatch(build("sparse"), error = function(e) {
    backend <<- "sparse-r"
    build("sparse-r")
  })
  solver[[cl]] <- list(model = model, backend = backend)
  model
}

solve_threeme <- function(m, data) {
  solved <- thortwo::thor_solve(
    get_model(m), from = m$years$base, to = m$years$last, data = data,
    index_time = "year", rtol = 1e-6, atol = 1e-7,
    verbose = FALSE, timings = FALSE)
  ## the elem_* columns only exist for the solver
  solved[, setdiff(names(solved), m$bundle$solver_only), drop = FALSE]
}

## ---- results -----------------------------------------------------------------

#' The ThreeME long format (as longer_data() gives it) for some variables
#' @param solved named list of solved databases, `baseline` among them
long_results <- function(solved, variables = NULL) {
  cols <- if (is.null(variables)) setdiff(names(solved$baseline), "year") else tolower(variables)
  solved |>
    imap(~ .x[.x$year >= years$base, c("year", cols), drop = FALSE] |>
           pivot_longer(-year, names_to = "variable", values_to = .y)) |>
    reduce(full_join, by = c("year", "variable")) |>
    mutate(variable = toupper(variable),
           sector = NA_character_, commodity = NA_character_) |>
    longer_data()
}

## Fills of the contribution bars, as in basic_results.qmd
contrib_pal <- c(CH = "#E9967A", I = "#FFEC8B", G = "#9BCD9B", X = "#87CEFA", M = "#CD5C5C")

## ---- sectors and commodities ---------------------------------------------------

## What the sector_results.qmd template of ThreeME_V4 shows: a root, then one
## variable per sector (Y_SAGR) or per commodity (CH_CAGR).
sc_indicators <- list(
  Sectors = c("Production" = "sector|Y", "Value added" = "sector|VA",
              "Employment" = "sector|F_L", "Investment" = "sector|IA",
              "Production price" = "sector|PY", "Energy consumption" = "sector|F_E",
              "GHG emissions" = "sector|EMS"),
  Commodities = c("Household consumption" = "commodity|CH",
                  "Consumer price" = "commodity|PCH",
                  "Exports" = "commodity|X", "Imports" = "commodity|M")
)
as_girafe <- function(p) {
  if (inherits(p, "girafe")) return(p)
  ggiraph::girafe(ggobj = p, width_svg = 6, height_svg = 4)
}

## ---- UI ----------------------------------------------------------------------

plot_card <- function(title, id, ...) {
  card(full_screen = TRUE, card_header(title), ggiraph::girafeOutput(id, height = "auto"), ...)
}

ui <- page_sidebar(
  title = "ThreeME v4 — France, solved in R",
  theme = bs_theme(version = 5, base_font = font_google("Arimo", local = FALSE),
                   heading_font = font_google("Merriweather", local = FALSE)),
  fillable = FALSE,

  sidebar = sidebar(
    width = 340,
    selectInput("classification", "Model size", choices = classifications,
                selected = default_classification),
    helpText(textOutput("model_size", inline = TRUE)),
    hr(),
    h6("Shock, from ", years$shock, " on"),
    sliderInput("carbon_tax", "Carbon tax (€ per tonne of CO2)",
                min = 0, max = 300, value = 70, step = 5),
    helpText(textOutput("carbon_tax_gdp", inline = TRUE)),
    sliderInput("world_demand", "World demand (permanent change, %)",
                min = -10, max = 10, value = 0, step = 0.5),
    div(class = "d-grid gap-2",
        actionButton("solve", "Solve", class = "btn-primary", icon = icon("play")),
        actionButton("clear", "Remove all scenarios", class = "btn-sm btn-outline-secondary")),
    helpText("Each solve adds a scenario; they are compared in the graphs and tables."),
    hr(),
    selectInput("focus", "Scenario detailed", choices = character(0)),
    sliderInput("endyear", "Last year shown", min = years$shock + 5, max = years$last,
                value = years$last, step = 1, sep = ""),
    hr(),
    downloadButton("download", "Results (.rds, long format)", class = "btn-sm"),
    helpText(textOutput("solver_status"))
  ),

  uiOutput("empty"),

  conditionalPanel(
    "output.has_results",
    layout_columns(
      fill = FALSE,
      value_box("GDP", textOutput("vb_gdp"), textOutput("vb_gdp_sub"), theme = "primary"),
      value_box("Employment", textOutput("vb_emp"), textOutput("vb_emp_sub"), theme = "secondary"),
      value_box("Unemployment rate", textOutput("vb_unr"), textOutput("vb_unr_sub"), theme = "secondary"),
      value_box("GHG emissions", textOutput("vb_ems"), textOutput("vb_ems_sub"), theme = "success")
    ),
    navset_card_tab(
      nav_panel(
        "GDP",
        layout_columns(
          col_widths = c(6, 6),
          plot_card("GDP, relative difference to the baseline", "p_gdp_rel"),
          plot_card("GDP, level", "p_gdp_level"))
      ),
      nav_panel(
        "GDP components",
        layout_columns(
          col_widths = c(6, 6),
          plot_card("Domestic components", "p_domestic"),
          plot_card("Foreign trade", "p_trade")),
        card(full_screen = TRUE,
             card_header("Contributions to the relative change in GDP (points)"),
             plotOutput("p_contrib", height = "380px"))
      ),
      nav_panel(
        "Labour, prices, emissions",
        layout_columns(
          col_widths = c(6, 6),
          plot_card("Employment", "p_emp"),
          plot_card("Unemployment rate (points)", "p_unr"),
          plot_card("Prices and wages", "p_prices"),
          plot_card("GHG emissions", "p_ems"))
      ),
      nav_panel(
        "Sectors and commodities",
        layout_columns(
          col_widths = c(5, 7), fill = FALSE,
          selectInput("sc_indicator", "Shown", choices = sc_indicators, width = "100%"),
          sliderInput("sc_year", "Year of the ranking", min = years$shock, max = years$last,
                      value = years$shock + 9, step = 1, sep = "", width = "100%")),
        card(full_screen = TRUE,
             card_header(textOutput("sc_rank_title", inline = TRUE)),
             ggiraph::girafeOutput("p_sc_rank", height = "auto")),
        card(full_screen = TRUE,
             card_header("Over time, relative difference to the baseline ",
                         "(grey: the other sectors or commodities)"),
             plotOutput("p_sc_time", height = "auto"))
      ),
      nav_panel(
        "Tables",
        h6("Difference from the baseline"),
        gt::gt_output("t_diff"),
        h6(class = "mt-4", "Growth rates, scenario detailed"),
        gt::gt_output("t_growth")
      ),
      nav_panel(
        "Any variable",
        layout_columns(
          col_widths = c(8, 4), fill = FALSE,
          selectizeInput("explore_vars", "Variables", choices = NULL, multiple = TRUE,
                         width = "100%",
                         options = list(placeholder = "Type a code: VA_SCXX, CH_CDEX, PCH...")),
          selectInput("explore_tr", "Shown as",
                      c("Relative difference (%)" = "reldiff", "Level" = "level",
                        "Difference" = "diff", "Difference in points" = "ppdiff",
                        "Growth rate" = "gr"))),
        div(style = "max-width: 820px;",
            ggiraph::girafeOutput("p_explore", height = "auto"))
      ),
      nav_panel("Scenarios", tableOutput("scenarios"), verbatimTextOutput("about"))
    )
  )
)

## ---- server ------------------------------------------------------------------

server <- function(input, output, session) {

  ## Results are kept per classification: going to another size and back does
  ## not lose the scenarios solved.
  rv <- reactiveValues(solved = list(), runs = list())

  m <- reactive({
    req(input$classification)
    get_setup(input$classification)
  })
  solved  <- reactive(rv$solved[[input$classification]] %||% list())
  runs    <- reactive(rv$runs[[input$classification]])
  backend <- reactive({
    runs()
    solver[[input$classification]]$backend
  })

  output$model_size <- renderText({
    sprintf("%s equations. A scenario takes %s, after a first build of the model.",
            format(length(m()$bundle$equations), big.mark = " "),
            switch(input$classification, c29_s33 = "about a minute",
                   "a second or two"))
  })
  observeEvent(input$classification, {
    updateSelectInput(session, "focus", choices = runs()$Scenario %||% character(0),
                      selected = tail(runs()$Scenario, 1))
    updateSelectizeInput(session, "explore_vars", choices = m()$all_vars,
                         selected = character(0), server = TRUE)
  })

  output$carbon_tax_gdp <- renderText({
    sprintf("Ex-ante revenue: %.2f points of GDP (on %s emissions)",
            input$carbon_tax * m()$gdp_points_per_euro, years$shock)
  })

  observeEvent(input$solve, {
    name <- scenario_name(input$carbon_tax, input$world_demand)
    if (!nzchar(name)) {
      showNotification("Both sliders are at zero: nothing to solve.", type = "warning")
      return()
    }
    cl <- input$classification
    m <- m()
    shocked <- apply_shocks(m, input$carbon_tax, input$world_demand)

    result <- tryCatch(
      withProgress(message = "ThreeME", value = 0, {
        if (is.null(solver[[cl]])) {
          setProgress(0.1, detail = "building the model (up to 1 min 30 the first time on this machine)")
          get_model(m)
        }
        solved <- solved()
        if (is.null(solved$baseline)) {
          setProgress(0.4, detail = "solving the baseline")
          solved$baseline <- solve_threeme(m, m$calib)
        }
        setProgress(0.7, detail = paste("solving", name))
        elapsed <- system.time(solved[[name]] <- solve_threeme(m, shocked))[["elapsed"]]
        list(solved = solved, elapsed = elapsed)
      }),
      error = function(e) {
        showNotification(paste("The solver failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      })
    if (is.null(result)) return()

    rv$solved[[cl]] <- result$solved
    rv$runs[[cl]] <- runs()[runs()$Scenario != name, ] |>
      rbind(data.frame(
        Scenario = name,
        `Carbon tax (EUR/tCO2)` = input$carbon_tax,
        `Carbon tax (ex-ante points of GDP)` = round(input$carbon_tax * m$gdp_points_per_euro, 2),
        `World demand (%)` = input$world_demand,
        `Solve time (s)` = round(result$elapsed, 2),
        check.names = FALSE))
    updateSelectInput(session, "focus", choices = rv$runs[[cl]]$Scenario, selected = name)
  })

  ## of the model size shown; the baseline is kept, it does not change
  observeEvent(input$clear, {
    rv$solved[[input$classification]] <- solved()["baseline"] |> compact()
    rv$runs[[input$classification]] <- NULL
    updateSelectInput(session, "focus", choices = character(0))
  })

  scenarios <- reactive(setdiff(names(solved()), "baseline"))
  has_results <- reactive(length(scenarios()) > 0)
  output$has_results <- reactive(has_results())
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  output$empty <- renderUI({
    if (has_results()) return(NULL)
    card(card_body(
      h5("Set the shock on the left, then press Solve."),
      p("The model is ThreeME v4 for France with ", length(m()$bundle$commodities),
        " commodities and ", length(m()$bundle$sectors), " sectors (",
        length(m()$bundle$equations), " equations). It is solved here, in R, from ",
        years$base, " to ", years$last, ": nothing has to be imported."),
      p("The first solve of a model size on a machine builds and compiles it: about 20 seconds ",
        "at 13x13, a minute and a half at 29x33. After that a scenario takes about a second at ",
        "13x13 and a little under a minute at 29x33, plus the baseline on the first solve.")))
  })

  focus <- reactive({
    req(has_results())
    if (isTruthy(input$focus) && input$focus %in% scenarios()) input$focus else tail(scenarios(), 1)
  })

  data_main <- reactive({
    req(has_results())
    long_results(solved(), m()$main_vars)
  })

  scen_pal <- reactive(c(baseline = "thistle",
                         threeme_palette(keys = scenarios(), type = "gradient")))

  ## One plot of the package, as a girafe widget
  draw <- function(variables, scenarios, transformation = "reldiff", data = data_main(), ...) {
    simple_plot(data, variables = variables, scenarios = scenarios,
                transformation = transformation, labels = m()$var_labels,
                startyear = years$shock - 1, endyear = input$endyear,
                interactive = TRUE, ...) |>
      as_girafe()
  }

  output$p_gdp_rel   <- ggiraph::renderGirafe(draw("GDP", scenarios(), palette = scen_pal()))
  output$p_gdp_level <- ggiraph::renderGirafe(
    draw("GDP", c("baseline", scenarios()), "level", palette = scen_pal()))
  output$p_domestic  <- ggiraph::renderGirafe(
    draw(c("GDP", "CH", "I", "G"), focus(), title = focus()))
  output$p_trade     <- ggiraph::renderGirafe(draw(c("X", "M"), focus(), title = focus()))
  output$p_emp       <- ggiraph::renderGirafe(draw("F_L", scenarios(), palette = scen_pal()))
  output$p_unr       <- ggiraph::renderGirafe(draw("UNR", scenarios(), "ppdiff", palette = scen_pal()))
  output$p_prices    <- ggiraph::renderGirafe(
    draw(c("PCH", "PY", "W"), focus(), title = focus()))
  output$p_ems       <- ggiraph::renderGirafe(draw("EMS", scenarios(), palette = scen_pal()))

  ## Contributions to the relative deviation of GDP: the change of each
  ## component over baseline GDP, imports counted negatively.
  output$p_contrib <- renderPlot({
    parts <- setdiff(gdp_parts, "GDP")
    d <- data_main() |>
      filter(scenario == focus(), variable %in% gdp_parts,
             year >= years$shock - 1, year <= input$endyear)
    gdp <- d |> filter(variable == "GDP") |>
      transmute(year, gdp_ref = values_ref, total = 100 * (values / values_ref - 1))
    contrib <- d |> filter(variable %in% parts) |>
      left_join(gdp, by = "year") |>
      mutate(contribution = 100 * (values - values_ref) / gdp_ref *
               ifelse(variable == "M", -1, 1),
             variable = factor(variable, parts, m()$var_labels[parts]))
    ggplot(contrib, aes(year, contribution)) +
      geom_col(aes(fill = variable), width = 0.75) +
      geom_line(data = gdp, aes(y = total, linetype = "GDP"), linewidth = 0.9) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      scale_fill_manual(values = unname(contrib_pal[parts])) +
      labs(x = NULL, y = NULL, fill = NULL, linetype = NULL, subtitle = focus()) +
      ofce::theme_ofce() +
      theme(legend.position = "bottom")
  }, res = 96)

  output$t_diff <- gt::render_gt({
    table_3me(data_main(), variables = m()$table_groups,
              transformation = dict_transformations(m()$main_vars),
              scenarios = scenarios(), labels = m()$var_labels,
              shock_year = years$shock, end_year = input$endyear)
  })
  output$t_growth <- gt::render_gt({
    table_3me(data_main(), variables = gdp_parts, transformation = "gr",
              scenarios = c("baseline", focus()), labels = m()$var_labels,
              shock_year = years$shock, end_year = input$endyear)
  })

  ## --- sectors and commodities ---
  ## Relative difference to the baseline of one variable per sector or commodity
  data_sc <- reactive({
    req(has_results(), input$sc_indicator)
    what <- strsplit(input$sc_indicator, "|", fixed = TRUE)[[1]]
    codes <- m()$sc_codes[[what[1]]]
    names_of <- m()$sc_names[[what[1]]]
    vars <- paste0(tolower(what[2]), "_", codes)
    here <- vars %in% names(m()$calib)
    req(any(here))
    long_results(solved(), vars[here]) |>
      filter(scenario != "baseline", year >= years$shock - 1, year <= input$endyear) |>
      mutate(code = sub("^.*_", "", tolower(variable)),
             name = coalesce(unname(names_of[code]), code),
             ## a sector that does not produce has no relative difference
             rel = ifelse(abs(values_ref) > 1e-9, values / values_ref - 1, NA_real_)) |>
      filter(!is.na(rel))
  })
  sc_label <- reactive({
    all <- unlist(unname(sc_indicators))
    names(all)[all == input$sc_indicator]
  })
  output$sc_rank_title <- renderText({
    sprintf("%s in %s, relative difference to the baseline: %s",
            sc_label(), min(input$sc_year, input$endyear), focus())
  })

  output$p_sc_rank <- ggiraph::renderGirafe({
    d <- data_sc() |>
      filter(scenario == focus(), year == min(input$sc_year, input$endyear)) |>
      mutate(name = reorder(name, rel),
             tip = sprintf("%s\n%+.2f %%\nlevel: %s (baseline %s)", name, 100 * rel,
                           format(round(values, 1), big.mark = " "),
                           format(round(values_ref, 1), big.mark = " ")))
    p <- ggplot(d, aes(rel, name)) +
      ggiraph::geom_col_interactive(aes(tooltip = tip, data_id = code),
                                    fill = scen_pal()[[focus()]], width = 0.7) +
      geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey30") +
      scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
      labs(x = NULL, y = NULL) +
      ofce::theme_ofce() +
      theme(panel.grid.major.y = element_blank())
    ggiraph::girafe(ggobj = p, width_svg = 9, height_svg = 0.6 + 0.2 * nrow(d))
  })

  output$p_sc_time <- renderPlot({
    d <- data_sc() |> mutate(panel = stringr::str_wrap(name, 22))
    behind <- d |> filter(scenario == focus()) |> select(year, rel, other = code)
    ggplot(d, aes(year, rel)) +
      geom_line(data = behind, aes(group = other), colour = "grey80", linewidth = 0.25) +
      geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
      geom_line(aes(colour = scenario), linewidth = 0.85) +
      facet_wrap(~panel, ncol = 6) +
      scale_y_continuous(labels = scales::label_percent()) +
      scale_colour_manual(values = scen_pal(), name = NULL) +
      labs(x = NULL, y = NULL) +
      ofce::theme_ofce() +
      theme(legend.position = "bottom", strip.text = element_text(size = 8),
            panel.spacing.x = grid::unit(1.2, "lines"))
  }, res = 96, height = function() {
    ## six panels a row
    dimension <- strsplit(input$sc_indicator %||% "sector|Y", "|", fixed = TRUE)[[1]][1]
    130 + 150 * ceiling(length(m()$sc_codes[[dimension]]) / 6)
  })

  ## --- headline figures, scenario detailed, last year shown ---
  headline <- reactive({
    d <- data_main() |> filter(scenario == focus(), year == input$endyear)
    setNames(as.list(d$values), d$variable) |>
      c(ref = list(setNames(as.list(d$values_ref), d$variable)))
  })
  pct <- function(v) sprintf("%+.2f %%", 100 * (headline()[[v]] / headline()$ref[[v]] - 1))
  when <- reactive(paste0("in ", input$endyear, ", ", focus()))
  output$vb_gdp <- renderText(pct("GDP"))
  output$vb_emp <- renderText(pct("F_L"))
  output$vb_unr <- renderText(sprintf("%+.2f pts", 100 * (headline()$UNR - headline()$ref$UNR)))
  output$vb_ems <- renderText(pct("EMS"))
  output$vb_gdp_sub <- output$vb_emp_sub <- output$vb_unr_sub <- output$vb_ems_sub <-
    renderText(when())

  ## --- any variable of the model ---
  output$p_explore <- ggiraph::renderGirafe({
    req(has_results(), input$explore_vars, all(input$explore_vars %in% m()$all_vars))
    shown <- if (input$explore_tr %in% c("level", "gr")) c("baseline", scenarios()) else scenarios()
    draw(input$explore_vars, shown, input$explore_tr,
         data = long_results(solved(), input$explore_vars))
  })

  output$scenarios <- renderTable(runs(), digits = 2)
  output$about <- renderText({
    bundle <- m()$bundle
    i <- bundle$info
    paste0("Model: ThreeME_V4 ", i$threeme_branch, " @ ", i$threeme_commit,
           " (", i$model_folder, ", ", i$iso3, " ", i$classification, "), compiled ",
           format(i$built, "%Y-%m-%d"), "\n",
           length(bundle$equations), " equations, ", length(bundle$exo), " exogenous variables\n",
           "Baseline: steady state (the calibration, unchanged)\n",
           "Solver: thortwo ", utils::packageVersion("thortwo"), ", backend ", backend())
  })
  output$solver_status <- renderText({
    if (is.null(runs())) return("")
    sprintf("Last solve: %.1f s (%s backend)", tail(runs()$`Solve time (s)`, 1),
            if (identical(backend(), "sparse")) "compiled" else "pure R")
  })

  output$download <- downloadHandler(
    filename = function() paste0("threeme_shiny_trial_", input$classification, ".rds"),
    content = function(file) {
      req(has_results())
      saveRDS(long_results(solved()), file)
    })
}

shinyApp(ui, server)

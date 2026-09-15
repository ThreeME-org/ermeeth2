#' Aggregate commodities and sectors into broader groups to reduce the database post-simulation
#'
#' @description Commodity and sector codes are read off each variable name and
#'   replaced by the group they belong to in the bridge, and the variables
#'   sharing a group are combined according to the aggregation rules table
#'   (`sum`, `mean` or `weighted_mean`).
#'
#'   Codes are recognised by matching the suffixes of a variable name against
#'   the codes listed in the bridge, peeling them off from the right one at a
#'   time. A variable may therefore carry several commodity codes (as
#'   `ES_NRJ_CBIO_CCOA_SAGR` does) and all of them are aggregated.
#'
#'   A weighted mean is `sum(value * weight) / sum(weight)` within each
#'   (group, year, scenario), the weight variable being the rule's `weight_var`
#'   carrying the same code suffix as the variable it weights. It falls back to
#'   a simple mean when the total weight of a group is zero, or when none of the
#'   variables sharing a root has its weight variable in the database. Members
#'   whose own weight variable is missing drop out of the weighted average.
#'
#' @param data ThreeME result database, in wide form (one column per scenario).
#' @param scenarios scenarios existing
#' @param agg_s_table which aggregation rules to use for sectors. Default is "aggregation_rules" to use the ones from aggregation_rules.xlsx
#' @param agg_c_table which aggregation rules to use for commodities. Default is "aggregation_rules" to use the ones from aggregation_rules.xlsx
#' @param by_com Boolean. Whether to aggregate by commodities. Default is TRUE
#' @param by_sec Boolean. Whether to aggregate by commodities. Default is TRUE
#' @param bridge_com Commodities bridge file  indicating how to aggregate by commodities
#' @param bridge_sec Sectors bridge file  indicating how to aggregate by sectors
#' @param exception_s_c Codes that must never be treated as a sector or
#'   commodity code, even if the bridge lists them.
#' @param detailed.warnings Boolean. Whether to list the variable roots concerned by each warning.
#'
#' @return A list of four data.tables: the input data, the commodity-aggregated
#'   database, the sector-aggregated database and the database aggregated on
#'   both dimensions. Entries not requested are `NULL`.
#'
#' @keywords internal
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv2
#' @importFrom purrr safely
#' @importFrom data.table as.data.table data.table rbindlist set setnames fifelse
#' @import dplyr stringr
#'
#' @export
#'
aggregate_com_sec <- function(data = data_full,
                              scenarios = c("baseline", scenario |> unname()),
                              agg_s_table = "aggregation_rules",
                              agg_c_table = "aggregation_rules",
                              by_com = TRUE,
                              by_sec = TRUE,
                              bridge_com = bridge_commodities,
                              bridge_sec = bridge_sectors,
                              exception_s_c = c("CONS", "CONT"),
                              detailed.warnings = TRUE) {

  variable <- NULL
  year <- NULL
  sec_com <- NULL
  code <- NULL
  super <- NULL
  root <- NULL
  c_in <- NULL
  c_out <- NULL
  s_code <- NULL
  super_c_in <- NULL
  super_c_out <- NULL
  super_s <- NULL
  has_c <- NULL
  has_s <- NULL
  has_w <- NULL
  any_w <- NULL
  stem <- NULL
  up <- NULL
  code_suffix <- NULL
  root_com <- NULL
  root_sec <- NULL
  root_com_sec <- NULL
  weight_var <- NULL
  weighted_mean <- NULL
  wname <- NULL
  var_root <- NULL
  i.super <- NULL
  .SD <- NULL

  og_data <- data.table::as.data.table(data)[, c("variable", "year", scenarios), with = FALSE]

  # Retrieve right version of aggregation rules

  if(agg_s_table == "aggregation_rules" & agg_s_table == "aggregation_rules"){

    if(file.exists(file.path("src","bridges", paste0(agg_s_table, ".xlsx")))){
      agg_s_table <- readxl::read_excel(file.path("src","bridges", paste0(agg_s_table, ".xlsx")), sheet = "sectors")
      agg_c_table <- readxl::read_excel(file.path("src","bridges", paste0(agg_c_table, ".xlsx")), sheet = "commodities")
    }else{
      # If the aggregation rules are not changed, the program will not pass by get_remote_file()
      agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "sectors")
      agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "commodities")
    }

  }else{
    if(file.exists(file.path("src","bridges", paste0(agg_s_table, ".csv")))){
      agg_s_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_s_table, ".csv"))) %>%
        dplyr::filter(sec_com == "sectors") %>% dplyr::select(-sec_com)
    }else{
      safe_get_remote_file <- purrr::safely(get_remote_file)
      downloader <- safe_get_remote_file(object = paste0(agg_s_table, ".csv"),
                                         destination.folder = file.path("src","bridges"))
      if(!is.null(downloader$error)){ # If the file cannot be downloaded
        message_warning("Aggregation rule cannot be downloaded, using default rule instead.")
        agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "sectors")
      }else if(downloader$result){
        agg_s_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_s_table, ".csv"))) %>%
          dplyr::filter(sec_com == "sectors") %>% dplyr::select(-sec_com)
      }else{ # If the file does not exist on the remote
        message_warning("Aggregation rule does not exist, using default rule instead.")
        agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "sectors")
      }
    }

    if(file.exists(file.path("src","bridges", paste0(agg_c_table, ".csv")))){
      agg_c_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_c_table, ".csv"))) %>%
        dplyr::filter(sec_com == "commodities") %>% dplyr::select(-sec_com)
    }else{
      safe_get_remote_file <- purrr::safely(get_remote_file)
      downloader <- safe_get_remote_file(object = paste0(agg_c_table, ".csv"),
                                         destination.folder = file.path("src","bridges"))

      if(!is.null(downloader$error)){ # If the file cannot be downloaded
        message_warning("Aggregation rule cannot be downloaded, using default rule instead.")
        agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "commodities")
      }else if(downloader$result){
        agg_c_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_c_table, ".csv"))) %>%
          dplyr::filter(sec_com == "commodities") %>% dplyr::select(-sec_com)
      }else{ # If the file does not exist on the remote
        message_warning("Aggregation rule does not exist, using default rule instead.")
        agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth2"), sheet = "commodities")
      }
    }
  }

  ## ------------------------------------------------------------------
  ## 1. Codes are taken from the bridge, not guessed from a name pattern.
  ## ------------------------------------------------------------------
  cmap <- data.table::data.table(
    code  = toupper(unlist(bridge_com, use.names = FALSE)),
    super = toupper(rep(names(bridge_com), lengths(bridge_com))))
  smap <- data.table::data.table(
    code  = toupper(unlist(bridge_sec, use.names = FALSE)),
    super = toupper(rep(names(bridge_sec), lengths(bridge_sec))))

  if (length(exception_s_c) > 0) {
    cmap <- cmap[!code %in% toupper(exception_s_c)]
    smap <- smap[!code %in% toupper(exception_s_c)]
  }
  if (nrow(cmap) == 0 || nrow(smap) == 0) {
    stop("The bridge contains no usable commodity or sector code.")
  }

  Cpat <- paste0("(?:", paste(cmap$code, collapse = "|"), ")")
  Spat <- paste0("(?:", paste(smap$code, collapse = "|"), ")")

  ## Everything below is computed once per unique variable name, not per row.
  V <- data.table::data.table(variable = unique(og_data$variable))
  V[, up := toupper(variable)]

  ## peel the suffixes off from the right, one known code at a time
  V[, s_code := stringr::str_extract(up, paste0("_(", Spat, ")$"), group = 1)]
  V[, stem   := stringr::str_remove(up, paste0("_", Spat, "$"))]
  V[, c_in   := stringr::str_extract(stem, paste0("_(", Cpat, ")$"), group = 1)]
  V[, stem   := stringr::str_remove(stem, paste0("_", Cpat, "$"))]
  V[, c_out  := stringr::str_extract(stem, paste0("_(", Cpat, ")$"), group = 1)]
  V[, root   := stringr::str_remove(stem, paste0("_", Cpat, "$"))]
  V[, stem := NULL]

  V[cmap, on = c(c_in  = "code"), super_c_in  := i.super]
  V[cmap, on = c(c_out = "code"), super_c_out := i.super]
  V[smap, on = c(s_code = "code"), super_s    := i.super]

  V[, has_c := !is.na(c_in)]
  V[, has_s := !is.na(s_code)]

  ## rebuild names keeping the original token order root_[Cout]_[Cin]_[S]
  jn <- function(...) {
    p <- lapply(list(...), function(x) ifelse(is.na(x), "", paste0("_", x)))
    sub("^_", "", do.call(paste0, p))
  }
  V[, root_com     := jn(root, super_c_out, super_c_in, s_code)]
  V[, root_sec     := jn(root, c_out,       c_in,       super_s)]
  V[, root_com_sec := jn(root, super_c_out, super_c_in, super_s)]
  ## the weight variable is the same name with the root swapped for weight_var
  V[, code_suffix  := jn(c_out, c_in, s_code)]

  ## ------------------------------------------------------------------
  ## 2. Attach the aggregation rules, at variable level.
  ## ------------------------------------------------------------------
  all_vars_up <- unique(toupper(og_data$variable))

  prep <- function(sub, rules, kind) {
    x <- merge(sub, data.table::as.data.table(rules),
               by.x = "root", by.y = "var_root", all.x = TRUE, sort = FALSE)
    x[, wname := ifelse(is.na(weight_var), NA_character_,
                        paste0(weight_var, ifelse(code_suffix == "", "", paste0("_", code_suffix))))]

    ## no rule at all -> simple mean
    miss <- x[is.na(sum) & is.na(mean) & is.na(weighted_mean), unique(root)]
    if (length(miss) > 0) {
      message_warning(sprintf(
        "[%s] %d variable roots have no assigned aggregation method, simple mean will be used. To change this, add them to the aggregation rules table.",
        kind, length(miss)))
      if (detailed.warnings) { cat(miss, sep = "  "); cat("\n") }
      x[is.na(sum) & is.na(mean) & is.na(weighted_mean),
        `:=`(sum = 0, mean = 1, weighted_mean = 0)]
    }

    ## A weighted mean needs its weight variable in the database. The decision is
    ## taken per root, never per variable: splitting one group between the
    ## weighted and the mean branch would emit that group twice, each row
    ## averaging only part of its members.
    x[, has_w := !is.na(wname) & toupper(wname) %in% all_vars_up]
    bad <- x[weighted_mean == 1, list(any_w = any(has_w)), by = root][any_w == FALSE, root]
    if (length(bad) > 0) {
      message_warning(sprintf(
        "[%s] %d variable roots ask for a weighted mean but none of their weight variables is in the database, simple mean will be used.",
        kind, length(bad)))
      if (detailed.warnings) { cat(bad, sep = "  "); cat("\n") }
      x[root %in% bad, `:=`(sum = 0, mean = 1, weighted_mean = 0, wname = NA_character_)]
    }
    x[, wname := ifelse(has_w, wname, NA_character_)]
    x
  }

  ## ------------------------------------------------------------------
  ## 3. Aggregation.
  ## ------------------------------------------------------------------
  grp_sum  <- function(x, g) x[, lapply(.SD, sum),  .SDcols = scenarios, by = c(g, "year")]
  grp_mean <- function(x, g) x[, lapply(.SD, mean), .SDcols = scenarios, by = c(g, "year")]

  ## weighted mean: sum(value * weight) / sum(weight) within (group, year, scenario)
  grp_wmean <- function(x, g) {
    if (nrow(x) == 0) return(NULL)
    wc <- paste0("..w.", scenarios)
    pc <- paste0("..p.", scenarios)
    w <- og_data[variable %in% unique(x$wname)]
    data.table::setnames(w, c("variable", scenarios), c("wname", wc))
    y <- merge(x, w, by = c("wname", "year"), all.x = TRUE, sort = FALSE)
    for (i in seq_along(scenarios)) {
      data.table::set(y, j = pc[i], value = y[[scenarios[i]]] * y[[wc[i]]])
    }
    num <- y[, lapply(.SD, sum, na.rm = TRUE), .SDcols = pc,        by = c(g, "year")]
    den <- y[, lapply(.SD, sum, na.rm = TRUE), .SDcols = wc,        by = c(g, "year")]
    mn  <- y[, lapply(.SD, mean),              .SDcols = scenarios, by = c(g, "year")]
    out <- merge(merge(mn, num, by = c(g, "year"), sort = FALSE),
                 den, by = c(g, "year"), sort = FALSE)
    for (i in seq_along(scenarios)) {
      d <- out[[wc[i]]]
      data.table::set(out, j = scenarios[i],
                      value = data.table::fifelse(!is.na(d) & d != 0, out[[pc[i]]] / d, out[[scenarios[i]]]))
    }
    out[, c(g, "year", scenarios), with = FALSE]
  }

  run <- function(vinfo, gcol) {
    keys <- vinfo[, c("variable", gcol, "sum", "mean", "weighted_mean", "wname"), with = FALSE]
    d <- merge(og_data, keys, by = "variable", sort = FALSE)
    parts <- list(grp_sum(d[sum == 1], gcol),
                  grp_mean(d[mean == 1], gcol),
                  grp_wmean(d[weighted_mean == 1], gcol))
    out <- data.table::rbindlist(Filter(Negate(is.null), parts), use.names = TRUE)
    data.table::setnames(out, gcol, "variable")
    out
  }

  ## variables that carry no code at all travel through untouched
  pass  <- function(vsub) og_data[variable %in% vsub$variable, c("variable", "year", scenarios), with = FALSE]
  plain <- V[has_c == FALSE & has_s == FALSE]

  com_agg_data <- NULL
  sec_agg_data <- NULL
  com_sec_agg_data <- NULL

  if (by_com) {
    vc <- prep(V[has_c == TRUE], agg_c_table, "commodities")
    com_agg_data <- data.table::rbindlist(
      list(run(vc, "root_com"),
           pass(V[has_s == TRUE & has_c == FALSE]),   # sector-only variables pass through
           pass(plain)), use.names = TRUE)
  }

  if (by_sec) {
    vs <- prep(V[has_s == TRUE], agg_s_table, "sectors")
    sec_agg_data <- data.table::rbindlist(
      list(run(vs, "root_sec"),
           pass(V[has_c == TRUE & has_s == FALSE]),   # commodity-only variables pass through
           pass(plain)), use.names = TRUE)
  }

  if (by_com && by_sec) {
    ## Both dimensions at once, from the raw data: aggregating by commodity and
    ## then by sector would re-weight already weighted values.
    rules_cs <- rbind(data.table::as.data.table(agg_c_table),
                      data.table::as.data.table(agg_s_table)[!var_root %in% agg_c_table$var_root])
    vcs <- prep(V[has_c == TRUE | has_s == TRUE], rules_cs, "commodities and sectors")
    com_sec_agg_data <- data.table::rbindlist(
      list(run(vcs, "root_com_sec"), pass(plain)), use.names = TRUE)
  }

  list(og_data, com_agg_data, sec_agg_data, com_sec_agg_data)
}

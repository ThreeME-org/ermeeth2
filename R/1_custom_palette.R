# source("tests/bridge_c28_s32.R")
# source("tests/codenames_c28_s32.R")
# subgroups <- bridge_sectors
# name_subgroups = names_sectors
# palette_base = NULL

#' custom_palette
#'
#' @param n numeric(1) A number of colors to pick
#' @param bridge_group If not null, ThreeME bridge to be loaded takes precedent over n
#' @param palette_base If not null, a vector of colors to based the palette
#'
#' @return a vector of colors (hex code)
#' @export
#' @import colorspace
#' @importFrom purrr map set_names
#' @examples
#' custom.palette(n = 6)
custom.palette <- function(n = NULL,
                           bridge_group = NULL,
                           palette_base = NULL){

  if (is.null(bridge_group) & is.null(n)){
    stop(message = " Pleeeease, specifiy at least a number n or a brudge (ex:bridge_sectors) .\n")
  }

  if (!is.null(bridge_group) & !is.null(n)){
    cat("Droping n and retaining bridge option .\n")
  }


  if (!is.null(bridge_group) ){
    n <- length(bridge_group)
  }


  if (is.null(palette_base)){
    # Palette of OFCE diverging colors. Base to differentiate sectors
    pal_base <- c("#B61615",
                  "#017FC2",
                  "#2E7437",
                  "#F9B000",
                  "#EA5B0C",
                  "#9473AB",
                  "#D3A170",
                  "#DA2311",
                  "#662483",
                  "#C0087F",
                  "#7A9E1A",
                  "#009FE3",
                  "#009FE3",
                  "#846A3B",
                  "#25378D",
                  "#568E2F",
                  "#1D1D1B",
                  "#FBBA00",
                  "#E30613")
  } else {
    pal_base <- palette_base
  }

  if(length(pal_base) < n){
    multiplier <- trunc(n/length(pal_base) )
    pal_base <- c(pal_base,rep(pal_base,multiplier))
  }

  if (!is.null(bridge_group)){
    pal_group <- purrr::set_names(pal_base[1:length(bridge_group)],names(bridge_group))
    brd <- purrr::set_names(names(bridge_group)) %>%
      purrr::map(~list(subgroup = bridge_group[[.x]], base_col = pal_group[.x]))



    # Option to get the full set of sectors/commodities (before bridging)

     colors_subgroup <- function(base_colour, n_subgroup, names= NULL){
       if (n_subgroup == 1){
         pal_out <- base_colour
       }else {
         col_hcl <- colorspace::coords(as( colorspace::hex2RGB(base_colour, gamma = FALSE), "polarLAB"))
         pal_out <- colorspace::sequential_hcl(n = n_subgroup +1,
                                             h = col_hcl[3],
                                             c = c(col_hcl[2], NA, NA),
                                             #l = c(20, 70),
                                             l = c(col_hcl[1]-25,
                                                   col_hcl[1] + 25),
                                             power = 1.2 )
        pal_out <- pal_out[1:n_subgroup]
       }

       names(pal_out) = names
       pal_out
     }


     pal_subgroup <-  names(bridge_group)  %>%
       map(~colors_subgroup(brd[[.x]]$base_col, length(brd[[.x]]$subgroup), brd[[.x]]$subgroup )) %>%
       reduce(c)

     pal_all_groups <- c(pal_subgroup,pal_group)

       # Option to get a named palette by group and subgroup
     return(pal_all_groups)
     }else {
       # Option by default to get a palette of n colors
    return(pal_base[seq(1,n)])
  }
}




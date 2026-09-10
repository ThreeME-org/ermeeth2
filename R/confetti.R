#' Create a theme for tables and plots
#'
#' @param lines Vector of colours for the graph lines
#' @param background One colour for table background
#' @param text One colour for texts
#'
#' @returns a list of colours, named for the tables and graphs
#' @export
#'
#' @examples
#' palette_a <- confetti()
confetti <- function(lines = c("#00496FFF","#BF812DFF","#0F85A0FF","#7D4F73FF","#87AFD1FF","lightgoldenrod2"),
                     background = "whitesmoke",
                     text = "grey30"){
  return(
    confetti = list(
      #Set table colors
      table_body = "white",
      table_header = background,
      table_footer = background,
      table_striped = background,
      table_header_text = text,
      table_footer_text = text,
      table_body_text = text,

      #Set table borderline colors
      border_inner_header = "grey83",
      border_inner_body = "grey83",
      border_outer_all = "grey40",

      #Set graphics colors
      plot_header = background,
      plot_header_text = text,
      lines_palette = lines
    )
  )
}

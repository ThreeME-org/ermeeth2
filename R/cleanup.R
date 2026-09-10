#' Remove superseded rendered reports
#'
#' @description Scans the quarto render folder and deletes the older
#'   timestamped copies of each report, keeping only the most recent file for
#'   every report root name.
#'
#' @param render_dir character. Path to the folder holding the rendered html
#'   files. Defaults to `results/quarto_render`.
#'
#' @returns Called for its side effect of deleting files. Returns the list of
#'   `file.remove()` results, one element per deleted file.
#' @export
cleanup_output <- function(
    render_dir = file.path("results","quarto_render")
    ){

  nom_fichier <- NULL
  root <- NULL
  most_recent <- NULL

  html_list <- list.files(render_dir, "\\.html$")

  treatment_base <- data.frame(nom_fichier = html_list) |>
    mutate(root = str_remove(nom_fichier, "_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}\\.html$"),
           date = str_extract(nom_fichier, "_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}\\.html$")) |>
    arrange(desc(date)) |>
    group_by(root) |>
    mutate(most_recent = ifelse(date == max(date, na.rm = TRUE),1,0)) |>
    ungroup() |> filter(most_recent == 0)

  files_to_remove <- file.path(render_dir,treatment_base$nom_fichier)

  files_to_remove |> map(~file.remove(.x))




}

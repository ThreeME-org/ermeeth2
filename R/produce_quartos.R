#' Render the configured quarto reports
#'
#' @description Renders each selected quarto template with its parameters via
#'   [render_from_template()], and optionally opens the results in a browser.
#'
#' @param Show logical. Whether to open each rendered html when it is done.
#' @param templates_selection character vector. Names of the templates to
#'   render, without the `.qmd` extension.
#' @param parameters list. Quarto parameters, one element per template.
#' @param templates_path character. Folder holding the quarto templates.
#' @param output_path character. Folder to write the rendered reports to.
#' @param project_name character. Project name, used to build output filenames.
#'
#' @returns Called for its side effect of rendering the reports.
#' @export
produce_quartos<- function(Show = TRUE,
                templates_selection = config$output$quartos_to_render,
                parameters = config$output$quartos_parameters,
                templates_path = file.path("results","quarto_templates"),
                output_path = file.path("results","quarto_render"),
                project_name = config$input$project_name ){
  
  templates_selection = config$output$quartos_to_render
  parameters = config$output$quartos_parameters
  templates_path = file.path("results","quarto_templates")
  
  quartos_to_produce <- names(templates_selection[which(templates_selection[]==TRUE)])
  
  quartos_infos <- parameters[quartos_to_produce]

  output_names <- purrr::set_names( str_c(project_name,"_",quartos_to_produce,"_",format(Sys.time(), "%Y-%m-%d_%H-%M") ),
                                    quartos_to_produce)
  # browser()
  imap(quartos_infos,
      ~render_from_template(quarto_to_use = str_c(.y,".qmd"),
                             quartos_folder = templates_path,
                             interim_file_name = str_c(output_names[.y],".qmd") ,
                             output_file_name = output_names[.y],
                             quarto_parameters_list =.x,
                             browse = FALSE)
      )
    
    if(Show){
      str_c(output_names,".html") |> map(~browseURL(file.path(output_path,.x)))

    }

}

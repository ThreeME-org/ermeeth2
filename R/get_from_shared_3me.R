## Function to download and get sample data from the public data repository

#' Standard ThreeME scenario labels
#'
#' @return named vector containing labels for each scenario
#' @export
#'
threeme_scenario_labels <- function(){

  c("expg1" = "Increase in public expenditure by 1% of GDP",
    "rrsc1" = "Reduction of employers' social contributions by 1% of ex ante GDP",
    "vat1" = "VAT reduction by 1% of ex ante GDP",
    "inct1" = "Household income tax cut by 1% of ex ante GDP",
    "exr10" = "Permanent depreciation of 10% of the euro",
    "wd1" = "Permanent increase of 1% in global demand",
    "ff10" = "Increase in fossil fuel prices by 10%",
    "ct1" = "Increase in carbon tax by 1% of GDP",
    "prodk1" = "Increase in capital productivity by 1%",
    "prode1" = "Increase in energy productivity by 1%",
    "prodl1" = "Increase in labor productivity by 1%",
    "tpf1" = "Increase in total productivity by 1%"
  )

}


#' File structure of the shared threeme data
#'
#' @param url location of the tree.txt file
#'
#' @return dataframe with folder contents
#' @export
#' @import dplyr stringr
#' @importFrom purrr reduce
#' @importFrom tools file_path_sans_ext
#'
get_file_structure <- function(url = "https://raw.githubusercontent.com/ThreeME-org/shared_3me_data/main/tree.txt"){

  files <- NULL
  V1 <- NULL
  V2 <- NULL
  V3 <- NULL
  V4 <- NULL
  object <- NULL

  tree <- data.frame(files = readLines(url) ) |>
    filter(grepl("^resources",files)) |>
    mutate(files = str_remove(files,"^resources/"))

  tree_map <- stringr::str_split(tree$files, pattern = "/") |>
    reduce(rbind) |> as.data.frame() |> tibble::remove_rownames() |>
    rename(root = V1, type = V2, subtype = V3 , object = V4) |>
    mutate(object.net = tools::file_path_sans_ext(object)  ) |> mutate(unit = 1)



}

#' Get a file from the threeme share repo
#'
#' @param object name of the file with or without extension
#' @param root root folder name (lvl 1)
#' @param type type folder name (lvl 2)
#' @param subtype subtype folder name (lvl 3)
#' @param destination.folder path where to download
#' @param repo repo url
#'
#' @return download file if successful
#' @export
#'
#' @import dplyr stringr
#' @importFrom purrr reduce
#' @importFrom tools file_path_sans_ext
#' @importFrom glue glue
#'
#'
#'
get_remote_file <- function(object = NULL, root = NULL, type = NULL, subtype = NULL ,
                            destination.folder = getwd(),
                            repo = "https://raw.githubusercontent.com/ThreeME-org/shared_3me_data/main/resources"){


  ### TEST ARGUMENTS
  # object = "ff10"
  # root = NULL
  # type = NULL
  # subtype = NULL
  # destination.folder = getwd()
  # repo = "https://raw.githubusercontent.com/ThreeME-org/shared_3me_data/main/resources"
  ###
  success <- FALSE

  if(is.null(object)){
    stop(message = message_not_ok("Argument 'object' has not been specified. What are you looking to download?"))}

  ## functions
  search_base <- function(data, subfolder = "plop"){

    if(nrow(data) == 0){
      message_not_ok(glue::glue("Could not find subfolder {subfolder}"))
      error <- TRUE
    }else{
      error <- FALSE
    }

    return <- error
  }

  search_object <- function(object_to_look = object.test, data){
    object_short <- tools::file_path_sans_ext(object_to_look)

    object_search <- data |> filter(object == object_to_look)

    if(nrow(object_search)==0){
      message_warning(glue::glue("Could not find {object_to_look} in the specified folders. Searching for similarly named files with a different extension."))

      object_search <- data |> filter(object.net == object_short)
    }

    if(nrow(object_search)==0){
      ## error
    out <- NULL

    }
    if(nrow(object_search)>=1){
      ## multiple files found
      out <- str_c(object_search$root, object_search$type,object_search$subtype,object_search$object, sep = "/")
    }
    return <- out


  }


  ## initialisation
  root.test = root
  type.test = type
  subtype.test = subtype
  object.test = object
  object_short <- tools::file_path_sans_ext(object.test)


  available_data <- searched_data <- get_file_structure()


  if(!is.null(root)){
    rooted_data <- available_data |> filter (root == root.test)
    root_error <- search_base(rooted_data,root.test)
    if(root_error){rooted_data <- available_data}
  }else{rooted_data <- available_data}
  if(!is.null(type)){
    typed_data <- rooted_data |> filter (type == type.test)
    type_error <- search_base(typed_data,type.test)
    if(type_error){typed_data <- rooted_data}
  }else{typed_data <- rooted_data}
  if(!is.null(subtype)){
    subtyped_data <- typed_data |> filter (subtype == subtype.test)
    subtype_error <- search_base(subtyped_data,subtype.test)
    if(subtype_error){subtyped_data <- typed_data}
  }else{subtyped_data <- typed_data}


  folder_search <- TRUE
  object_search <- search_object(object.test,subtyped_data)

  if(is.null(object_search)){
    object_search <- search_object(object.test,available_data)
    folder_search <- FALSE
  }

  if(is.null(object_search)){
  message_not_ok(glue::glue("No file named '{object_short}' has been found"))
  }else{
    if(length(object_search)>1){
      message_warning(glue::glue("Multiple files like '{object_short}' have been found:\n"))
      cat(object_search,sep = "\n")
      cat("\nPlease refine your search to narrow it down to one of those files to download.\n")
    }else{
      if(folder_search){
        extramessage <- " in another folder than what was specified"
        }else{
        extramessage <- ""}
      message_main_step(glue::glue("A file such as '{object_short}' has been found{extramessage} with the following path: \n {object_search}"))

      dl.url = file.path(repo,object_search)
      dest.url = file.path(destination.folder, basename(dl.url))
    cat(glue::glue("\nDownloading {dl.url} to {dest.url}\n"))

    cat("\n")

    download.file(url = dl.url,
                  destfile = dest.url)
    success <- TRUE
      }

  }


return <- success

}

#' Pre-programmed get_remote_file function for data
#'
#' @param ... arguments to be passed on to `get_remote_file` function
#'
#' @return downloads file
#' @export
#'
get_sample_data  <- function(...){
  get_remote_file(..., destination.folder = file.path("data","output"), root = "data")

}

#' Pre-programmed get_remote_file function for qmd templates
#'
#' @param ... arguments to be passed on to get_remote_file function
#'
#' @return downloads file
#' @export
#'
get_qmd_template  <- function(...){
  get_remote_file(..., destination.folder = file.path("results","quarto_templates"), root = "quarto")
}


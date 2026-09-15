
#' Check a tar.gz package installation and local availability of new version
#'
#' @param name_package Character string. Name of the package to check
#'
#' @returns package installation if a newer version of the tar.gz is found locally
#' @export
#'
#' @importFrom glue glue
#' @import dplyr stringr
#'
install_local <- function(name_package){

  Package <- NULL
  Version <- NULL

  if(name_package %in% installed.packages()){
    version_installed <- as.data.frame(installed.packages()) |>
      dplyr::filter(Package == name_package ) |>
      dplyr::select(Version) |> unlist()

    cat(glue::glue("{name_package} is installed, version: {version_installed} \n

             "))

    ## Checking for newer sources
    source_packages <- list.files("src",pattern = glue::glue("^{name_package}.+gz$")  )


    if(length(source_packages)>0){

      latest <- max(source_packages)

      version_available  <- latest |> stringr::str_extract("_(\\d+\\.)+") |>
        stringr::str_remove("_") |> stringr::str_remove("\\.$")

      if(version_available>version_installed){

        cat(glue::glue("A more recent version of {name_package} package has been found: {version_available},. Installing this new version...\n"))

        install.packages(file.path("src",latest), repos = NULL, type = "source")

        cat(glue::glue("Version {version_available} of {name_package} package has been installed.\n"))
      }
    }


  }else{
    source_package <- list.files("src/",pattern = glue::glue ("^{name_package}.+gz$") )

    if(length(source_package)>0){

      latest <- max(source_package)
      install.packages(file.path("src",latest), repos = NULL, type = "source")

    }else{cat(glue::glue(":( Could not find {name_package} source package to install. :( \n"))}

  }
}



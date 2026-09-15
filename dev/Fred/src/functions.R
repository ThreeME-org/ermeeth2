
# Define required packages
required_packages <- c("data.table", "lemon", "tidyverse", "extrafont", "scales", "readxl",
                       "colorspace", "ggpubr", "svglite", "magrittr", "png", "remotes","glue", "flextable",
                       'Deriv', 'splitstackshape', 'gsubfn', 'cointReg',"RcppArmadillo","Rcpp",
                       "devtools","eurostat","rdbnomics","rmarkdown","officer","shiny","ggh4x","openxlsx")

# Define required "house" packages in git

required_packages_git<- c("ofce")


# Extract not installed packages
not_installed     <- required_packages[!(required_packages     %in% installed.packages()[ , "Package"])]
not_installed_git <- required_packages_git[!(required_packages_git %in% installed.packages()[ , "Package"])]

# Install missing packages
if(length(not_installed)>0) {install.packages(not_installed)}
library(tidyverse)

if(length(not_installed_git)>0){
  if("ofce" %in% rownames(installed.packages()) == FALSE){
    devtools::install_github("OFCE/ofce")
  }
  
}

# # install tresthor
# if("tresthor" %in% installed.packages() == FALSE){
#   install.packages("src/tresthor_1.0.2.tgz", repos = NULL, type = "source")  
# }

# install ermeeth
if("ermeeth" %in% installed.packages()){
  version_installed <- as.data.frame(installed.packages()) %>% filter(Package == "ermeeth" ) %>%
    select(Version) %>% unlist()
  
  cat(paste0("ermeeth is installed, version: ",version_installed,"\n"))
  
  ## Checking for newer sources 
  source_packages <- list.files("src",pattern = "^ermeeth.+gz$") 
  
  
  if(length(source_packages)>0){
    
    latest <- max(source_packages)
    
    version_available  <- latest %>% str_extract("_(\\d+\\.){3}") %>%
      str_remove("_") %>% str_remove("\\.$") 
    
    if(version_available>version_installed){
      
      cat(paste0("A more recent version of ermeeth package has been found: ",version_available,". Installing this new version...\n"))
      
      install.packages(file.path("src",latest), repos = NULL, type = "source")
      
      cat(paste0("Version ",version_available," of ermeeth package has been installed.\n"))
    }
  }
  
  
}else{ 
  source_package <- list.files("src/",pattern = "^ermeeth.+gz$")
  
  if(length(source_package)>0){
    
    latest <- max(source_package) 
    install.packages(file.path("src",latest), repos = NULL, type = "source") 
    
  }else{cat(":( Could not find ermeeth source package to install. :( \n")}
  
}




# Load packages
# NB: "require" is equivalent to "library" but is designed for use inside other functions.
purrr::map(c(required_packages,required_packages_git,"ermeeth"),~require(.x,character.only = TRUE))


# Loading all functions
purrr::map(list.files("src/functions_src/"),~source(file.path("src","functions_src",.x)))

# rm(list=ls())

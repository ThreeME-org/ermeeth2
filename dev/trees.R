

basic_tree <- function(id =  sample(month.name,1), elements = sample(LETTERS,sample(1:4,1))){


  spaces = str_replace_all(id,"."," ")
  separator = str_c("  \n",spaces,"   |\n",spaces,"   +-------* ")

  last_bit = paste(elements, collapse = separator)

  first_bit = paste0(" ",id, " -+-------* ")

  tree = str_c(first_bit,last_bit)

  cat(tree)

  return<- tree

}




get_tree_id<- function(tree){
  str_extract(tree,"^\\s*([A-Za-z0-9\\s_]+)", group = 1) |> str_remove("\\s*$")
}

get_tree_end_nodes <- function(tree){
  str_count(tree,"\\*")

}



combine_basic_trees <- function(subtrees= c(tree_1,tree_2,tree_3),
                                id_tree = sample(LETTERS,sample(1:8,1)) |> str_c(collapse = "")
){

  tree_ids <- get_tree_id(subtrees)
  subtrees_nodes <- get_tree_end_nodes(subtrees)




  first_bit = paste0(" ",id_tree, " -+----·")
  bar_position <- str_locate(first_bit,"\\+") |> max()

  new_space <- str_replace_all(first_bit,"."," ")
  new_space2 <-str_replace_all(first_bit,"."," ")

  substr(new_space2,bar_position,bar_position) <- "|"


  spaces = str_replace_all(id_tree,"."," ")

  separator = str_c(" \n",spaces,"   |\n",spaces,"   +----·")

  ## modifying subtrees
  ### non first nodes should be shifted right
  new_subtrees <- subtrees |> str_replace_all(
    pattern = "\\n(\\s+[\\+\\|])",
    replacement = str_c("\n",new_space2,"\\1")
  )

  new_subtrees[length(subtrees)]<- subtrees[length(subtrees)] |> str_replace_all(
    pattern = "\\n(\\s+[\\+\\|])",
    replacement = str_c("\n",new_space,"\\1")
  )




  last_bit = paste(new_subtrees, collapse = separator)


  tree = str_c(first_bit,last_bit)

  cat(tree)

  return<-tree

}


## shared threeme tree

sample_data <- get_file_structure()

map_data <- sample_data |>
  mutate(
    modenode = ifelse(model.dupe,NA,model) ,
    subtypenode = ifelse(subtype.dupe,NA,subtype),
    objectnode = object
  ) |>
  group_by(model,subtype) |>
  mutate(finaltree = cur_group_id())

## Algo to sort model equations

library(tidyverse)
## Data ThreeME

#
calib <- read.csv("tests/calibthreeme44.csv")
model_dynamo <- readLines("tests/modelthreeme44.prg")
# model_dynamo <- readLines("tests/model.prg")
# calib <- read.csv("tests/calib.csv")
##

var_list <- names(calib)


model <- model_dynamo |> str_replace("a_3ME.append"," ") |>
  ##remove @elem
  str_replace_all("@elem\\((.+?),\\s\\d{4}\\)" , "@@") |>
  ##remove dlog
  str_replace_all("(\\s|\\()(d\\(log\\()" , "\\1@@@") |>
  ##remove log
  str_replace_all("(\\s|\\()(log\\()" , "\\1@@@@@") |>
  ##remove d
  str_replace_all("(\\s|\\()(d\\()+" , "\\1@@@@") |>
  ##remove lagged variables
  str_replace_all("\\w+\\(\\-\\d+\\)" , "@@@@@") |>
  ##remove scientific numbers
  str_replace_all("(\\s|\\(|\\+|-)(\\d+e)" , "@@@@@@") |>
  ##remove numbers
  str_replace_all("(\\s|\\(|\\)|\\+|-)(\\d+(\\.\\d+)?)" , " @@@@@@ ") |>
  ##remove signs
  str_replace_all("(\\s|\\(|\\)|\\+|-|/|\\*|<|>|<=|>=|\\^)" , " @ ") |>
  ##start placeholder
  str_replace_all("^","&&")|>
  ##remove @
  str_replace_all("@"," ") |>
  str_replace_all("\\s+"," ") |>
  str_remove("&&\\s")



writeLines(model, con = "tests/test_regex.prg")

model_frame <- data.frame(nb = 1:length(model_dynamo) , og = model ) |>

mutate(lhs = og |> str_remove_all("\\s=(\\s.+$)?"),
       rhs = og |> str_remove_all("^.+\\s=\\s")

       )
## Step 1  : figure out the endogenous variables
### The endogenous variables are the first of the left handside variables

lhs <- model_frame$nb |> purrr::set_names() |>
  map(~str_split(model_frame$lhs[.x], pattern = "\\s") |> as.vector( ) |> unlist()|>
        str_remove_all("\\s+")
        )
lhs_decompo <- lhs |> map(~list(first_var = .x[1] ,
                                other_vars = .x[-1]
                                ))

rhs <- model_frame$nb |> purrr::set_names() |>
  map(~str_split(model_frame$rhs[.x], pattern = "\\s") |> as.vector( ) |> unlist()|>
        str_remove_all("\\s+")
  )



endogenous_variables_list <- lhs_decompo |> map(~.x[["first_var"]])
model_frame_2 <- data.frame(nb = names(endogenous_variables_list), endo = unlist(endogenous_variables_list))

endogenous_variables <- unlist(endogenous_variables_list)
exogenous_variables <-var_list[!var_list %in% model_frame_2$endo]

## get all variables per equation except the endogenous
var_list_compendium <- model_frame$nb |> purrr::set_names() |>
  imap(~list(other_vars = c(lhs_decompo[[.x]]$other_vars, rhs[[.x]]) ,
            first_var = lhs_decompo[[.x]]$first_var )) |>
  ## remove in each vector empty character strings, 1st endogenous variable, exogenous variable
  imap(~list(
    other_vars = .x[["other_vars"]][!.x[["other_vars"]] %in% c(.x[["first_var"]],"",exogenous_variables )] ,
    first_var = .x[["first_var"]],
    rank = 0,
    nb = .y))  |>
  set_names(model_frame_2$endo)

## Equation rank loop


count_rank_0 <- function(tibble = var_list_compendium){
  plop <- tibble |> mutate(test = ifelse(rank == 0 , 1, 0))
  count = sum(plop$test)
  count
}

tibble_list <- var_list_compendium |> reduce(rbind)  |> as.tibble() |>
  mutate(stage = NA)

og_tibble <- tibble_list

var_used <- c()
i <- 1
dp <- 1

## Prologue
while(dp > 0){
  old_count <- count_rank_0(tibble_list)

  tibble_list2 <- tibble_list |>
    mutate(length = map(other_vars, ~length(.x)) |> unlist() )  |>
    mutate(stage = ifelse(length == 0 & rank == 0, "prologue" , stage) |> unlist()) |>
    mutate(rank = ifelse(length == 0 & rank == 0, i , rank))


  new_var_used <- tibble_list2 |> select(first_var, rank) |> filter(rank == i ) |>
    select(first_var) |> unlist()

  var_used <- c(var_used , new_var_used) |> unique()

  new_count <- count_rank_0(tibble_list2)

  tibble_list <- tibble_list2 |> mutate(
    other_vars = map(other_vars, ~.x[!.x %in% var_used] )
    )

  dp = old_count - new_count
  i = i +1

}

tibble_prologue = tibble_list

#Epilogue

## count how many times an endogenous variable appears among non classified equations (rank 0)

tibble_0 = tibble_prologue |>  filter(rank ==0)
leftover_endo <- reduce(tibble_0$first_var, c)
tibble_list <- tibble_prologue


j = length(leftover_endo)
de = 1

while(de > 0){
  old_count <- count_rank_0(tibble_list)

  tibble_0 <- tibble_list |>  filter(rank == 0)
  called_vars <- reduce(tibble_0$other_vars, c)

  tibble_list3 <- tibble_list |>
    mutate(repetition_first_var = map(first_var, ~sum(called_vars == .x)) |> unlist() ) |>
    mutate(stage = ifelse(repetition_first_var == 0 & rank == 0, "epilogue" , stage) |> unlist() ) |>
    mutate( rank =  ifelse(repetition_first_var == 0 & rank == 0, j , rank))


  tibble_list <- tibble_list3

  new_count <- count_rank_0(tibble_list)

  de <- old_count - new_count
  j <- j - 1
}

## Algo de separation des blocs simultanés
## A faire - extraire les rank 0, enlever endo des prologue et epilogue , make a matrix
##

## Réorganisation des equations

final_count <- data.frame(nb = unlist(tibble_list$nb) |> as.numeric() ,
                          rank = unlist(tibble_list$rank) ,
                          stage = unlist(tibble_list$stage)
                          ) |>
  mutate(stage = ifelse(is.na(stage), "heart" , stage),
         rank = ifelse(stage == "heart" , max(rank[which(stage == "prologue")]) + 1 ,rank)
         )  |>
  arrange(rank) |>

  mutate(unit = 1,
         new_rank = cumsum(unit)

  )

heart_size <- length(final_count$stage[which(final_count$stage == "heart")])
prologue_size <- length(final_count$stage[which(final_count$stage == "prologue")])
epilogue_size <- length(final_count$stage[which(final_count$stage == "epilogue")])
new_mdl <- model_dynamo[final_count$nb]

writeLines(new_mdl, "ordered_model.prg")


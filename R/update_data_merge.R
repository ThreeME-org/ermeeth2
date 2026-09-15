#' update_data_merge
#'
#' @description Combine two databases that may have variables in common. The
#'   common variables are replaced by their values from the new database.
#'
#' @param old_data data.frame to update
#' @param new_data new data.frame holding the updated values
#' @param by_join_var character vector of the columns to join on. Default is "year"
#'
#' @returns a data.frame with the columns of `old_data` updated with the values
#'   of `new_data`. If the two data.frames do not have the same number of rows,
#'   `old_data` is returned unchanged.
#' @export
#'
update_data_merge <- function(old_data , new_data, by_join_var = "year"){
## Add check that same length
## add foolproof with differing length

## add check that by join var is common

if(nrow(old_data) == nrow(new_data) ){

var_to_update = setdiff(names(new_data), by_join_var)

res <- old_data %>% select(-any_of(var_to_update) ) %>%
full_join(new_data, by = by_join_var)



}else{
    cat("Function doesnt work yet with data.frames of differing length")
    res <- old_data
}

return(res)

}

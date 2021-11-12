## Daily Logic Check functions---------------------------------------------



#' Number of missing values for each variable in the given list
#'
#' List the number of missing values for each variable in the given list of
#' variables in a tidy format.
#'
#' @importFrom purrr map
#' @importFrom dplyr rename
#' @importFrom broom tidy
#'
#' @param variable_names_vec a vector of variable names. For each of these
#'                           variables, the function calculates the number
#'                           of missing values present in their list of
#'                           values.
#'
#' @param data underlying dataset from which the variable names are
#'             picked.
#'
#' @examples
#' count_missing_val(variable_names_vec = c("a", "b"), data = data)
#'
#' @return
#'
#' @export

count_missing_val <- function(variable_names_vec, data){
  dummy_data[, variable_names_vec] %>%
    purrr::map(function(x) sum(is.na(x))) %>%
    unlist() %>%
    broom::tidy() %>%
    dplyr::rename("variable_name" = "names", "num_missing_values" = "x")
}

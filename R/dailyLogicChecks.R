## Daily Logic Check functions---------------------------------------------



#' Number of missing values for each variable in the given list
#'
#' List the number of missing values for each variable in the given list of
#' variables in a tidy format.
#'
#' @importFrom purrr map
#' @importFrom dplyr rename as_tibble
#' @importFrom tibble is_tibble
#' @importFrom broom tidy
#'
#' @param df dataset(\code{tibble}/\code{data.frame}) object from which the
#' variable names are chosen.
#'
#' @param variable_names_vec a vector of variable names. For each of these
#'                           variables, the function calculates the number
#'                           of missing values present in their list of
#'                           values. If you want to see missing values for
#'                           all variables (columns) in the dataset, set
#'                           \code{variable_names_vec = "all"}. Also, the
#'                           default for this argument is "all".
#'
#'
#'
#' @examples
#' count_missing_values(df = dataObj, variable_names_vec = c("a", "b"))
#' count_missing_values(df = dataObj, variable_names_vec = "all")
#' count_missing_values(df = dataObj) # uses default \code{variable_names_vec = "all"}
#'
#' @return
#'
#' @export


count_missing_values <- function(df, variable_names_vec = "all"){

  # "df" object should exist and should be a tibble/dataframe
  if((envnames::get_obj_name(df) %in% ls()) &&
     (tibble::is_tibble(df) || is.data.frame(df))){

    df <- dplyr::as_tibble(df)

    # view missing values for all variables (columns) in the df
    if(variable_names_vec == "all"){

      return(df %>%
        purrr::map(function(x) sum(is.na(x))) %>%
        unlist() %>%
        broom::tidy() %>%
        dplyr::rename("variable_name" = "names", "num_missing_values" = "x"))

    # view missing values only for selected variables in the "variable_names_vec"
    # argument
    } else if (sum(variable_names_vec %in% colnames(df)) ==
               length(variable_names_vec)) {

      return(df[, variable_names_vec] %>%
        purrr::map(function(x) sum(is.na(x))) %>%
        unlist() %>%
        broom::tidy() %>%
        dplyr::rename("variable_name" = "names", "num_missing_values" = "x"))

    # check if atleast one of the variable (column) names entered in the "variable_names_vec"
    # argument does not exist in the underlying dataset entered in "df".
    } else {
      stop("All the variable (columns) names present in the vector 'varibale_names_vec' should
         exist in the underlying dataset entered into 'data'. Please make sure that
         this is the case, before proceeding.")
    }

  } else {
    stop("Either the object doesn't exist or it isn't a tibble/dataframe. Please make
         sure that the object exists in the current environment and also make
         sure that it is of class: 'data.frame'/'tbl'/'tbl_df'. To coerce an object to
         to tibble you can use: 'as_tibble' function.")
  }

}




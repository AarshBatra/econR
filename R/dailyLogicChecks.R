## Daily Logic Check functions---------------------------------------------



#' Count missing values for each variable in the given list
#'
#' List the number of missing values for each variable in the given list of
#' variables in a tidy format.
#'
#' @importFrom envnames get_obj_name
#' @importFrom purrr map
#' @importFrom dplyr rename as_tibble
#' @importFrom tibble is_tibble
#' @importFrom broom tidy
#'
#' @param df dataset(\code{tibble}/\code{data.frame}) object from which the
#'           variable names are chosen.
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
#' @return a tibble containing the list of column (variable) names
#' (for the underlying dataset \code{df}) and for each of those column names,
#'  it shows the number of values that are missing in that column. It also
#'  shows a column that displays the percentage (of total missing values)
#'  of values missing in each column.
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
        dplyr::rename("variable_name" = "names", "num_missing_values" = "x") %>%
        dplyr::mutate(total_values = nrow(df),
                      percent_missing_val = (num_missing_values/total_values) * 100))

    # view missing values only for selected variables in the "variable_names_vec"
    # argument
    } else if (sum(variable_names_vec %in% colnames(df)) ==
               length(variable_names_vec)) {

      return(df[, variable_names_vec] %>%
        purrr::map(function(x) sum(is.na(x))) %>%
        unlist() %>%
        broom::tidy() %>%
        dplyr::rename("variable_name" = "names", "num_missing_values" = "x") %>%
        dplyr::mutate(total_values = nrow(df),
                      percent_missing_values = (num_missing_values/total_values) * 100))


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

#======================================


#' Count duplicates for a given (single or combination) of column(s).
#'
#' Counts the total number of duplicates for a given (single or combination)
#' of columns. "Combination" refers to the case, in which there is more than
#' one column, which in combination uniquely identify the dataset. In dplyr
#' terminology, we are using more than one column to group our variables (
#' using the \code{group_by} family of functions.).
#'
#' @importFrom envnames get_obj_name
#' @importFrom tibble is_tibble
#' @importFrom dplyr group_by summarise ungroup mutate
#'
#' @param df dataset(\code{tibble}/\code{data.frame}) object from which the
#'           \code{uniq_identifier_col} is chosen.
#'
#' @param uniq_identifier_col a character vector of column name(s) that uniquely
#'                            identifies the dataset. In here, \code{tidyselect}
#'                            can be used to select columns. See examples below.
#'
#'
#' @examples
#' count_duplicates(df = dataObj, uniq_identifier_col = c("ID"))
#'
#' count_duplicates(df = dataObj, uniq_identifier_col = c("ID", "Name"))
#'
#' count_duplicates(df = dataObj, uniq_identifier_col
#' = tidyselect::contains("abc")) # tidyselection used to select columns.
#'
#' @return a tibble containing the the variable(s) contained in
#'         \code{uniq_identifier_col} and their corresponding duplicate count and
#'         percentage (of total duplicates in data). If there is more than one variable in
#'          \code{uniq_identifier_col}, the tibble shows the duplicate count (and
#'          percentages) for those joint variables.
#'
#' @export

count_duplicates <- function(df, uniq_identifier_col) {

  if((envnames::get_obj_name(df) %in% ls()) &&
     (tibble::is_tibble(df) || is.data.frame(df))){

      return(df %>%
        dplyr::group_by(across(uniq_identifier_col)) %>%
        dplyr::summarise(num_duplicates = n() - 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(tot_dup_entire_data = sum(num_duplicates),
      percent_of_tot_dup = (num_duplicates/sum(num_duplicates)) * 100))


  } else {
    stop("Either the object doesn't exist or it isn't a tibble/dataframe. Please make
         sure that the object exists in the current environment and also make
         sure that it is of class: 'data.frame'/'tbl'/'tbl_df'. To coerce an object to
         to tibble you can use: 'as_tibble' function.")
  }

}

#' Display duplicate surveys data
#'
#' This function displays the duplicate surveys, given a unique identifier
#' (which could be a single variable, or a combination of variables). In
#' case, its a combination of variables (that uniquely identifies the
#' underlying dataset in \code{df}), then this function will show
#' duplicate surveys for that joint (combination) unique identifier variable.
#'
#' @importFrom envnames get_obj_name
#' @importFrom tibble is_tibble
#' @importFrom dplyr group_by summarise ungroup mutate select filter
#' @importFrom tidyselect everything
#'
#' @inheritParams count_duplicates
#'
#' @examples
#'
#' count_duplicates(df = dataObj, uniq_identifier_col = c("ID"))
#'
#' count_duplicates(df = dataObj, uniq_identifier_col = c("ID", "Name"))
#'
#' count_duplicates(df = dataObj, uniq_identifier_col =
#' tidyselect::contains("abc")) # tidyselection used to select columns.
#'
#' @return a tibble that displays the duplicates for the given unique identifiers
#'         in \code{uniq_identifier_col}.
#'
#' @note If for example, \code{surveyor_id} is the unique identifier col,
#'       and after running \code{display_duplicates} function, we find
#'       that, there are 5 rows with \code{surveyor_id} = 122. Now,
#'       the output tibble from this function  should be interpreted
#'       as follows: 4 of 5 of the \code{surveyor_id} = 122 are duplicates,
#'       1 of 5 is original.
#'
#' @export

display_duplicates <- function(df, uniq_identifier_col){
  if((envnames::get_obj_name(df) %in% ls()) &&
     (tibble::is_tibble(df) || is.data.frame(df))){

    uniq_identifier_col_df <- df %>%
      econR::count_duplicates(uniq_identifier_col) %>%
      dplyr::select(uniq_identifier_col)

    for(i in 1:length(uniq_identifier_col)){
      df <- df %>%
        dplyr::filter(!!as.symbol(uniq_identifier_col[i]) %in%
      as.numeric(unlist(uniq_identifier_col_df[, uniq_identifier_col[i]])))
    }

    return(df %>%
             dplyr::select(uniq_identifier_col, tidyselect::everything()) %>%
      dplyr::arrange(!!as.symbol(uniq_identifier_col[1])))

  } else {

    stop("Either the object doesn't exist or it isn't a tibble/dataframe. Please make
         sure that the object exists in the current environment and also make
         sure that it is of class: 'data.frame'/'tbl'/'tbl_df'. To coerce an object to
         to tibble you can use: 'as_tibble' function.")

  }
}

#' List common column names in two different data sets
#'
#' Find common column names in 2 different data sets. E.g. In doing field surveys,
#' checking for variables with same name in master tracking list
#' & field tracking list. It's a good habit to check for such
#' common variable names to avoid losing necessary variables from using data.
#'
#' @importFrom envnames get_obj_name
#' @importFrom tibble is_tibble
#' @importFrom lubridate intersect
#'
#' @param df1 First dataset. This should be in either of the following formats:
#'            \code{data.frame/tbl/tbl_df}.
#'
#' @param df2 Second dataset. This should be in either of the following formats:
#'            \code{data.frame/tbl/tbl_df}.
#'
#' @examples
#' list_common_columns(df1 = dataObj1, df2 = dataObj2)
#'
#' @return a character vector of column names that are shared by \code{df1} and
#'         \code{df2}
#'
#' @export

list_common_columns <- function(df1, df2) {
  if(((envnames::get_obj_name(df1) %in% ls()) &&
     (tibble::is_tibble(df1) || is.data.frame(df1))) &&
     ((envnames::get_obj_name(df2) %in% ls()) &&
      (tibble::is_tibble(df2) || is.data.frame(df2)))){

    return(lubridate::intersect(colnames(df1), colnames(df2)))


  } else {
    stop("Either the objects (df1, df2, or both) doesn't exist or it isn't a tibble/dataframe. Please make
         sure that the objects exist in the current environment and also make
         sure that they are of class: 'data.frame'/'tbl'/'tbl_df'. To coerce an object to
         to tibble you can use: 'as_tibble' function.")
  }
}

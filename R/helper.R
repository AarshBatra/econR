## Helper functions--------------------------------------------------------

# Libraries----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(ggplot2)
library(stringr)
library(skimr)
library(purrr)
library(forcats)
library(foreign)
library(haven)
library(dplyr)
library(broom)
library(glue)
library(VIM)
library(outliers)
library(ggplot2)
library(scales)
library(grid)
library(RColorBrewer)
library(psych)
library(lubridate)

# converting backward slashes to forward slashes in the path names-------------

#' converting backward slashes to forward slashes in the path names
#'
#' When we construct relative path names in R using functions like
#' \code{normalizePath()}, R outputs path names containing "backslashes", but
#' we require path names containing forward slashes. \code{correct_path} \
#' function takes in a path name with backward slashes and converts it into a
#' path name with forward slashes.
#'
#' @importFrom stringr str_replace_all
#' @param backward_slash_path backward slash path
#' @return \code{forward_slash_path} path with forward slashes
#'
#' @examples
#' correct_path(backward_slash_path = "backward slash path")
#'
#' @export
#'

rectify_path <- function(backward_slash_path){
  forward_slash_path <- str_replace_all(backward_slash_path, "\\\\", "/")
  forward_slash_path
}

# end of function

#======================#

# creating a absolute file path for reading in files

#' This functions creates a absolute file path with forward slashes. It
#' takes in as its argument a relative path to the data file you want to
#' read. From that relative path it creates a absolute path to that data
#' file that is unique to the local file system. This helps avoid file path
#'errors that people run into when they read each others code.
#'
#' @param relPathToDataFile relative path to the data file (using forward slashes)
#' @return \code{abs_path} absolute path to data file (with forward slashes)
#'
#' @examples
#' abs_path <- create_abs_path(relPathToDataFile = "path/to/data.csv")
#'
#' @export

create_abs_path <- function(relPathToDataFile){
  abs_path <- file.path(rectify_path(normalizePath(".")), relPathToDataFile)
  abs_path
}

# end of function


#=======================#




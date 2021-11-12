# testing temporary script-----------------------------------------------------

# read in testing data

dummy_data <- read.dta(create_abs_path("data-raw/dummy_main.dta"))
skimr::skim(dummy_data)


# Let us consider the variables which are unique and you want to run the missing checks for.
# Suppose we consider "surveyor_id" and "name"

variable_names <- c("surveyor_id", "name")      # you can put as many variables as you want
foo <- dummy_data[, variable_names] %>%
  purrr::map(function(x) sum(is.na(x))) %>%
  unlist() %>%
  broom::tidy() %>%
  rename("variable_name" = "names", "num_missing_values" = "x")



sapply(dummy_data[,variable_names], function(x) sum(is.na(x)))

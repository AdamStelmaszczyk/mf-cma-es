library(here)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)

# solver_name :: str
# dimension :: integer
# result_storage_dir :: str

load_eigenvalues <- function(solver_name, dimension, result_storage_dir) {
  result_file <- here::here(result_storage_dir, stringr::str_glue("experimentum-crucis-{solver_name}-{dimension}-d.csv"))
  readr::read_csv(result_file, col_types=readr::cols(
    t = readr::col_integer(),
    decision_variable = readr::col_factor(),
    eigenvalue = readr::col_double()
  ))
}

# eigen_values :: list[double]

eigenlog_to_dataframe <- function(eigen_values) {
  data.frame(eigen_values) %>%
    tidyr::pivot_longer(cols=starts_with("X"), names_to = "decision_variable") %>%
    dplyr::transmute(
      decision_variable = as.factor(readr::parse_number(decision_variable)),
      eigenvalue = value
    ) %>%
    dplyr::group_by(decision_variable) %>%
    dplyr::mutate(t = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(decision_variable)
}

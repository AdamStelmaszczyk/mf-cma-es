library(ggplot2)
library(stringr)
library(purrr)
library(dplyr)
library(scales)

source(here::here("src", "common.R"))

# result_storage_dir :: str
# solvers :: list[name :: str, timestamp :: ISO8601Timestamp]
# problems :: list[name :: str]
# dims :: list[integer]

read_exp_data <- function(result_storage_dir, solvers, problems, dims) {
  grid <- expand.grid(s = solvers, p = problems, d = dims)
  dataset <- purrr::pmap(grid, function(s, p, d) {
    tryCatch({
    result_file <- stringr::str_glue("{result_storage_dir}/{s$name}-{p$name}-{d}D_{s$timestamp}.csv")
    readr::read_csv(result_file, col_types=readr::cols()) |>
      dplyr::mutate(t = 1:dplyr::n(), dim = d, problem = p$name)
    }, error = function(err) {
      print(stringr::str_glue("Failed to load convergence data: solver [{s$name}], dimension [{d}], problem [{p$name}]"))
      return()
    }
  )
  }) |> purrr::reduce(dplyr::bind_rows)
  return(dataset)
}

# dataset :: data.frame[t :: int | sofar_best :: double | curr_best :: double | solver  :: str | factor]
# y_var :: expression 
# problem_name :: str
# dimension :: integer
# trans :: double -> double

plot_convergence <- function(dataset, y_var, problem_name, dimension, trans = base::identity) {
  y_var <- enquo(y_var)
  dataset |>
    dplyr::slice(which(dplyr::row_number() %% 100 == 1)) |>
    dplyr::filter(problem == problem_name, dim == dimension) |>
    ggplot2::ggplot(aes(x = t, y = trans(!!y_var), col = solver, linetype = solver)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(aes(shape = solver), size=4.5) +
    ggplot2::scale_y_continuous(trans = scales::log10_trans(),
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    make_ew_theme() + 
    ggplot2::xlab("Generation") + 
    ggplot2::ylab("log10(q(x))") +
    ggplot2::ggtitle(stringr::str_glue("Problem: {problem_name}, Dimension: {dimension}"))

}

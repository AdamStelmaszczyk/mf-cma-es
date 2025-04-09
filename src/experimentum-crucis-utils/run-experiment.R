library(here)

source(here::here("src", "problems.R"))
source(here::here("src", "experimentum-crucis-utils", "data-utils.R"))

# solver :: OptimizationSolver: (list[double], list[double] -> double, double, double, list[key :: str -> value :: any]) -> cma_es.result
# starting_point :: list[double]
# control :: list[key :: str -> value :: any]

do_experiment <- function(solver, starting_point, control) {
  result <- solver(par=starting_point, fn=f_weighted_sphere, lower=-100, upper=100, control = control)
  return(result$diagnostic$eigen)
}

# solvers :: list[list[solver :: OptimizationSolver, name :: str]]
# starting_point_generator :: integer -> double
# dimension :: integer
# control :: list[key :: str -> value :: any]
# result_storage_dir :: str
# rng_seed :: integer

run_experimentum_crucis <- function(solvers, starting_point_generator, dimension, control, result_storage_dir, rng_seed) {
  if (!dir.exists(result_storage_dir)) {
    dir.create(result_storage_dir, recursive=TRUE)
  }
  for (s in solvers) {
    set.seed(rng_seed)
    x0 <- starting_point_generator(dimension)
    eigen_values <- do_experiment(s$solver, x0, control) %>% eigenlog_to_dataframe
    output_path <- here::here(result_storage_dir, stringr::str_glue("experimentum-crucis-{s$name}-{dimension}-d.csv"))
    readr::write_csv(eigen_values, output_path)
  }
}



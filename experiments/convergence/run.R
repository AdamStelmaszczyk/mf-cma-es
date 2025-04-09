library(here)
library(stringr)
library(parallel)

source(here::here("src", "solvers", "nm-cma-es-vectorized.R"))
source(here::here("src", "solvers", "vanilla-cma-es.R"))

source(here::here("src", "problems.R"))
source(here::here("src", "convergence-utils", "run-experiment.R"))
source(here::here("src", "common.R"))

RNG_SEEDS_FILE = here::here("experiments", "convergence", "rng-seeds.txt")

main <- function() {
  logger::log_layout(layout_glue_colors)
  start_timestamp <- make_now_iso8601_timestamp()
  result_storage_dir <- here::here("experiments", "convergence", "data")

  logging_control = list(
    "diag.sigma"=FALSE,
    "diag.value"=TRUE,
    "diag.bestVal"=TRUE
  )

  termination_control = list(
    "terminate.stopfitness" = FALSE,
    "terminate.std_dev_tol" = FALSE,
    "terminate.cov_mat_cond" = FALSE,
    "terminate.maxiter" = TRUE
  )

  common_control = c(
    logging_control,
    termination_control,
    list("do_flatland_escape" = FALSE, "do_hsig" = FALSE, "maxiter" = 2500)
  )
  
  repetition_number <- 30
  dimensions <- c(30)
  rng_seeds = readr::read_lines(RNG_SEEDS_FILE)

  # Strategy responsible for generating a starting point for an optimizer
  starting_point_generator <- \(dim) {
    runif(dim, -0.2, 0.8)
  }

  problems <- list(list("name" = "weighted-sphere", fn = f_weighted_sphere))

  exp_spec_factory <- \(window_size) {
    window_control = list("window" = \(dim) { window_size })
    return(list(
      "name" = stringr::str_glue("nm-cma-es-vectorized-w-{window_size}"),
      "solver" = nm_cma_es_vectorized,
      "control" = c(common_control, window_control),
      "timestamp" = start_timestamp
    ))
  }

  solvers <- c(
    purrr::map(c(10, 30, 60, 80), exp_spec_factory),
    list(list("name" = "vanilla-cma-es", "solver" = vanilla_cma_es, "control" = common_control, "timestamp" = start_timestamp))
  )

  concurrency_level = floor(parallel::detectCores() * 0.9)
  run_experiments(
    solvers,
    problems,
    repetition_number,
    dimensions,
    starting_point_generator,
    result_storage_dir,
    start_timestamp,
    rng_seeds,
    concurrency_level
  )
}

main()

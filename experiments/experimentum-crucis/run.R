source(here::here("src", "solvers", "vanilla-cma-es.R"))
source(here::here("src", "solvers", "nm-cma-es-vectorized.R"))

source(here::here("src", "experimentum-crucis-utils", "run-experiment.R"))
source(here::here("src", "experimentum-crucis-utils", "data-utils.R"))
source(here::here("src", "experimentum-crucis-utils", "plot.R"))

main <- function() {
  result_storage_dir <- here::here("experiments", "experimentum-crucis", "data")

  # Strategy responsible for generating a starting point for an optimizer
  starting_point_generator <- \(dim) {
    runif(dim, -0.2, 0.8)
  }

  logging_control = list(
    "diag.eigen" = TRUE
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
    list(
      "do_flatland_escape" = FALSE,
      "do_hsig" = FALSE,
      "maxiter" = 2500,
      "sigma_updater" = "sigma_identity"
    )
  )

  solvers <- list(
    # VANILLA_CMA_ES (fixed sigma)
    list("name" = "vanilla-cma-es", "solver" = vanilla_cma_es),
    # NM-CMA-ES-VECTORIZED (fixed sigma)
    list("name" = "nm-cma-es-vectorized-w-linear", "solver" = nm_cma_es_vectorized)
  )

  dimension <- 30
  rng_seed <- 420

  run_experimentum_crucis(
    solvers,
    starting_point_generator,
    dimension,
    common_control,
    result_storage_dir,
    rng_seed
  )
}

main()


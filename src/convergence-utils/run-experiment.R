library(logger)
library(readr)
library(here)
library(stringr)
library(purrr)
library(doParallel)
library(foreach)

source(here::here("src", "common.R"))

# results_df :: data.frame[sofar_best :: double, cur_best :: double, duration :: double]

aggr_results <- function(results_df) {
  reps <- length(results_df)
  logger::log_debug("Experiments results will be averaged over [{reps}] repetitions.")
  purrr::reduce(results_df, function(acc, x) {
    acc$cur_best <- (acc$cur_best + x$cur_best)
    acc$sofar_best <- (acc$sofar_best + x$sofar_best)
    acc$duration <- (acc$duration + x$duration)
    return(acc)
  }) |>
    dplyr::mutate(
      cur_best = cur_best / reps,
      sofar_best = sofar_best / reps,
      duration = duration / reps,
    )
}

# solvers :: list[name :: str, solver :: OptimizationSolver]
# problems :: list[name :: str, problem :: list[double] -> double]
# starting_point :: double

run_solver <- function(solver, problem, starting_point) {
  tryCatch({
    dim = length(starting_point)
    start = Sys.time()
    result <- solver$solver(starting_point, problem$fn, control=solver$control)
    elapsed = as.numeric(Sys.time() - start, unit="secs")

    df <- tibble::tibble(
      t = seq_along(length(result$diagnostic$value[,1])),
      cur_best = result$diagnostic$value[,1],
      sofar_best = result$diagnostic$bestVal[,1],
      solver = solver$name,
      duration = elapsed
    )
    return(list("solver_result" = df, "duration" = elapsed, "termination_msg" = result$message, "best_value" = result$value, "n.evals" = result$n.evals))
  }, error = function(e) {
      logger::log_error("Execution of experiment for solver [{solver$name}] on problem [{problem$name}/D{dim}] failed. Error: {e}.")
  })
}

# solvers :: list[name :: str, solver :: OptimizationSolver]
# problems :: list[name :: str, problem :: list[double] -> double]
# dim :: integer
# result_storage_dir :: str
# rep_num :: integer
# rng_seeds :: list[integer]
# starting_point_generator :: double -> double

run_repeated_experiment <- function(solver, problem, dim, rep_num, rng_seeds, starting_point_generator) {
  results <- list()
  logger::log_info("Numerical experiments for solver [{solver$name}] will be repeated [{rep_num}] times for problem [{problem$name}] with dimensionality [{dim}].") # nolint
  for (r in 1:rep_num) {
    logger::log_info("[s: {solver$name}, d: {dim}, rng: {rng_seeds[r]}] Starting running [{r}] repetition for problem [{problem$name}].")
    set.seed(rng_seeds[r]) 
    starting_point <- starting_point_generator(dim)
    result <- run_solver(solver, problem, starting_point)
    results[[r]] <- result$solver_result
    logger::log_info(
      "[s: {solver$name}, d: {dim}, p: {problem$name}, rep: {r}, rng: {rng_seeds[r]}] Duration: [{round(result$duration, 3)}s]. Termination condition: [{result$termination_msg}]. Best value: [{result$best_value}]. Counteval: [{result$n.evals}]"
    )
  }
  return(results)
}

# solvers :: list[list[name :: str, solver :: OptimizationSolver]]
# problems :: list[list[name :: str, problem :: list[double] -> double]]
# repetition_number :: integer
# dimensions :: list[integer]
# starting_point_generator :: double -> double
# result_storage_dir :: str
# start_timestamp :: ISO8601Timestamp
# rng_seeds :: list[integer]
# concurrency_level :: integer

do_run_experiment <- function(solvers, problems, result_storage_dir, exp_spec, rng_seeds, starting_point_generator, start_timestamp) {
  ignore <- foreach::foreach(solver = solvers) %dopar% {
    tryCatch({
      for (problem in problems) {
        results <- run_repeated_experiment(solver, problem, exp_spec$dim, exp_spec$rep, rng_seeds, starting_point_generator)
        output_path <- here::here(result_storage_dir, stringr::str_glue("{solver$name}-{problem$name}-{exp_spec$dim}D_{start_timestamp}.csv"))
        aggregated <- aggr_results(results)
        readr::write_csv(aggregated, output_path)
      }
    }, error = function(e) {
      logger::log_error(
        "[d: {exp_spec$dim}, rep: {exp_spec$rep}, rng: {exp_spec$rng_seed}] Execution of experiment for solver [{solver$name}] failed."
      )
    })
  }
}

# solvers :: list[list[name :: str, solver :: OptimizationSolver]]
# problems :: list[list[name :: str, problem :: list[double] -> double]
# repetition_number :: integer
# dimensions :: list[double]
# starting_point_generator :: double -> double
# result_storage_dir :: str
# start_timestamp :: ISO8601Timestamp
# rng_seeds :: list[integer]
# concurrency_level :: integer

run_experiments <- function(
  solvers,
  problems,
  repetition_number,
  dimensions,
  starting_point_generator,
  result_storage_dir,
  start_timestamp,
  rng_seeds,
  concurrency_level = 1
) {
  if (!dir.exists(result_storage_dir)) {
    logger::log_info("Creating result storage directory [{result_storage_dir}]")
    dir.create(result_storage_dir, recursive=TRUE)
  }
  doParallel::registerDoParallel(cores=concurrency_level)
  experiment_grid <- expand.grid(dim=dimensions, rep=repetition_number)
  logger::log_info("Experiment results will be stored in [{result_storage_dir}].")
  ignore <- purrr::pmap(experiment_grid, \(dim, rep, rng_seed) {
    do_run_experiment(solvers, problems, result_storage_dir, list("dim"=dim, "rep"=rep), rng_seeds, starting_point_generator, start_timestamp)
  })
  logger::log_info("Successfuly finished all planned numerial experiments.")
}

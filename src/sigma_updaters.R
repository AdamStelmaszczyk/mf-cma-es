library(rlang)
library(here)

source(here::here("src", "solvers", "utils.R"))

# sigma :: float
# env :: solver environment (a key-value store for each named object defined in a solver scope)
sigma_identity <- function(sigma, env = caller_env()) {
  return(sigma)
}

# Cumulative Step-size Adaptation
# DOI: https://doi.org/10.48550/arXiv.1604.00772
# sigma :: float
# env :: solver environment (a key-value store for each named object defined in a solver scope)
csa <- function(sigma, env = caller_env()) {
  return(sigma * exp((norm(env$ps)/env$chiN - 1)*env$cs/env$damps))
}

# Previous Population Midpoint Fitness 
# DOI: 10.1109/CEC45853.2021.9504829 
# sigma :: float
# env :: solver environment (a key-value store for each named object defined in a solver scope)
ppmf <- function(sigma, env = caller_env()) {
  # log current generation value 
  env$prev_midpoint_fitness <- env$midpoint_fitness
  # Procedure 3, line 1 (p. 3)
  mean_point <- matrix(rowMeans(env$vx), nrow=nrow(env$vx), ncol=1)
  # Procedure 3, line 2 (p. 3)
  env$midpoint_fitness <- apply(mean_point, 2, \(x) { env$fn(x) })
  env$counteval <- env$counteval + 1
  # Procedure 3, line 3 (p. 3)
  p_succ = length(which(env$arfitness < env$prev_midpoint_fitness))/env$lambda 
  # Procedure 3, line 4 (p. 3)
  return(sigma * exp(env$damps_ppmf * (p_succ - env$p_target_ppmf) / (1 - env$p_target_ppmf)))
}

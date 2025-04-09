library(here)
library(logger)

source(here::here("src", "sigma_updaters.R"))
source(here::here("src", "solvers", "utils.R"))


# Implementation of MF-CMA-ES 
# This implementation is based on the {cmaes} package.
# Source code: https://github.com/cran/cmaes/blob/master/R/cmaes.R
# CRAN package: https://cran.r-project.org/web/packages/cmaes/index.html

# par :: list[double
# fn :: list[double] -> double
# lower :: double
# upper :: double
# control :: list[key :: str -> value :: any]

nm_cma_es_vectorized <- function(par, fn, ..., lower = -100, upper = 100, control=list()) {
  xmean <- par
  N <- length(xmean)

  ## Termination conditions:
  terminate.stopfitness <- controlParam("terminate.stopfitness", TRUE, as.logical)
  terminate.maxiter <- controlParam("terminate.maxiter", TRUE, as.logical)

  ## Hacks:
  do_flatland_escape <- controlParam("do_flatland_escape", TRUE, as.logical)
  
  ## Parameters:
  trace       <- controlParam("trace", FALSE, as.logical)
  fnscale     <- controlParam("fnscale", 1, as.numeric)
  stopfitness <- controlParam("stopfitness", 1e-8, as.numeric)
  budget      <- controlParam("budget", 10000*N, as.numeric)  
  sigma       <- controlParam("sigma", 1.0, as.numeric)
  keep.best   <- controlParam("keep.best", TRUE, as.logical)
  vectorized  <- controlParam("vectorized", FALSE, as.logical)

  ## Logging options:
  log.all    <- controlParam("diag", FALSE, as.logical)
  log.sigma  <- controlParam("diag.sigma", log.all, as.logical)
  log.eigen  <- controlParam("diag.eigen", log.all, as.logical)
  log.value  <- controlParam("diag.value", log.all, as.logical)
  log.pop    <- controlParam("diag.pop", log.all, as.logical)
  log.bestVal<- controlParam("diag.bestVal", log.all, as.logical)
  
  ## Strategy parameter setting (defaults as recommended by Nicolas Hansen):
  lambda      <- controlParam("lambda", \(dim) { 4 * N }, as.function)(N)
  maxiter     <- controlParam("maxiter", round(budget/lambda), as.numeric)
  mu          <- controlParam("mu", floor(lambda/2), as.numeric)
  weights     <- controlParam("weights", rep(1, mu), as.numeric)
  weights     <- weights/sum(weights)
  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2), as.numeric)
  cc          <- controlParam("ccum", 4/(N+4), as.numeric)
  cs          <- controlParam("cs", (mueff+2)/(N+mueff+3), as.numeric)
  mucov       <- controlParam("ccov.mu", mueff, as.numeric)
  ccov        <- controlParam("ccov.1",
                              (1/mucov) * 2/(N+1.4)^2
                              + (1-1/mucov) * ((2*mucov-1)/((N+2)^2+2*mucov)), as.numeric)
  # Step-size update policy 
  sigma_updater <- get(controlParam("sigma_updater", "sigma_identity", identity))

  # NM-CMA-ES specific parameters
  c_mu <- ccov * (1 - 1/mucov) # default value? 
  c_1 <- ccov - c_mu # default value?

  damps       <- controlParam("damps",
                              1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + cs, as.numeric)
  window      <- controlParam("window", \(dim) { floor(1.4*dim) + 20 }, as.function)(N)

  print("Running vectorized NM-CMA-ES with window")
  print(window)

  # PPMF hyper-parameters
  # see 10.1109/CEC45853.2021.9504829 
  # section III, p. 3 for default values rationale
  damps_ppmf <- controlParam("damps_ppmf", 2.0, as.numeric)
  p_target_ppmf = controlParam("p_target_ppmf", 0.1, as.numeric)


  ## Bookkeeping variables for the best solution found so far:
  best.fit <- Inf
  best.par <- NULL
  
  ## Preallocate logging structures:
  if (log.sigma)
    sigma.log <- numeric(maxiter)
  if (log.eigen)
    eigen.log <- matrix(0, nrow=maxiter, ncol=N)
  if (log.value)
    value.log <- matrix(0, nrow=maxiter, ncol=mu)
  if (log.pop)
    pop.log <- array(0, c(N, mu, maxiter))
  if(log.bestVal)
    bestVal.log <-  matrix(0, nrow=0, ncol=1)
  
  ## Initialize dynamic (internal) strategy parameters and constants
  pc <- rep(0.0, N)
  
  iter <- 0L      ## Number of iterations
  counteval <- 0L ## Number of function evaluations
  cviol <- 0L     ## Number of constraint violations
  msg <- NULL     ## Reason for terminating
  nm <- names(par) ## Names of parameters
  
  t <- 1:maxiter
  decay_table <- (1 - ccov)^((t - 1) / 2)

  p_history <- matrix(0, nrow = N, ncol = window)
  p_index <- function(t) {
    (t - 1) %% window + 1
  }
  d_history <- matrix(0, nrow = N, ncol = window * mu)
  d_range <- function(t) {
    start <- (p_index(t) - 1) * mu + 1
    end <- start + mu - 1
    start:end
  }
  shift <- function(v, n) {
    len = length(v)
    n <- n %% len
    c(tail(v, n), head(v, len - n))
  }

  # Runtime parameters of PPMF
  prev_midpoint_fitness <- sign(stopfitness) * Inf
  midpoint_fitness <- sign(stopfitness) * Inf

  while (counteval < budget) {
    iter <- iter + 1L
    
    ## Generate new population:
    # Difference vectors in columns
    t <- iter

    decay <- decay_table[window:1]
    decay <- shift(decay, t - 1)
    decay_rep <- rep(decay, each = mu)
    w <- rep(sqrt(weights), each = window)

    r_mu <- matrix(rnorm(window * mu * lambda), nrow = window * mu, ncol = lambda)
    inner_sum <- d_history %*% (r_mu * decay_rep * w)
    rank_mu <- sqrt(c_mu) * inner_sum

    r_1 <- matrix(rnorm(window * lambda), nrow = window, ncol = lambda)
    rank_1 <- sqrt(c_1) * p_history %*% (r_1 * decay)

    outer_sum <- rank_mu + rank_1
    if (t <= window) {
      last_decay <- decay_table[t]
    } else {
      last_decay <- sqrt((1 - (ccov))^(t - 1) + ((1 - ccov)^window) * (1 - (ccov))^(t - window - 1))
    }
    r_last <- matrix(rnorm(N * lambda), nrow = N, ncol = lambda)
    last_term = last_decay * r_last
    d <- outer_sum + last_term
    arx <- xmean + sigma * d


    vx <- ifelse(arx > lower, ifelse(arx < upper, arx, upper), lower)
    if (!is.null(nm))
      rownames(vx) <- nm
    pen <- 1 + colSums((arx - vx)^2)
    pen[!is.finite(pen)] <- .Machine$double.xmax / 2
    cviol <- cviol + sum(pen > 1)
    
    if (vectorized) {
      y <- fn(vx, ...) * fnscale
    } else {
      y <- apply(vx, 2, function(x) fn(x, ...) * fnscale)
    }
    counteval <- counteval + lambda
    
    if (!keep.best) {
      best.fit <- Inf
      best.par <- NULL
    }

    arfitness <- y * pen
    valid <- pen <= 1
    if (any(valid)) {
      wb <- which.min(y[valid])
      if (y[valid][wb] < best.fit) {
        best.fit <- y[valid][wb]
        best.par <- arx[,valid,drop=FALSE][,wb]
      }
    }
    
    ## Order fitness:
    arindex <- order(arfitness)
    arfitness <- arfitness[arindex]
    aripop <- arindex[1:mu]
    selx <- arx[,aripop]
    xmean <- drop(selx %*% weights)
    seld <- d[,aripop]
    dmean <- drop(seld %*% weights)

    ## Save selected x value:
    if (log.pop) pop.log[,,iter] <- selx
    if (log.value) value.log[iter,] <- arfitness[aripop]
    if (log.sigma) sigma.log[iter] <- sigma
    if (log.bestVal) bestVal.log <- rbind(bestVal.log,min(suppressWarnings(min(bestVal.log)), min(arfitness)))
    
    ## Cumulation: Update evolutionary paths
    pc <- (1-cc)*pc + sqrt(cc*(2-cc)*mueff) * dmean

    d_history[, d_range(t)] <- d[, aripop]
    p_history[, p_index(t)] <- pc

    ## Adapt step size sigma:
    sigma <- sigma_updater(sigma)


    ## Experimentum Crucis
    if (log.eigen) {
      emp_cov_mat <- cov(t(arx))
      e <- eigen(emp_cov_mat, symmetric=TRUE)
      eigen.log[iter,] <- rev(sort(e$values))
    }

    # Stop conditions
    if (terminate.stopfitness && (arfitness[1] <= stopfitness * fnscale)) {
      msg <- "Stop fitness reached."
      break
    }

    if (terminate.maxiter && (iter >= maxiter)) {
      msg <- "Exceeded maximal number of iteration"
      break
    }

    if(do_flatland_escape) {
     if (arfitness[1] == arfitness[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) {
       sigma <- sigma * exp(0.2+cs/damps);
     if (trace)
       message("Flat fitness function. Increasing sigma.")
     }
    }

    if (trace) {
      message(sprintf("Iteration %i of %i: current fitness %f",
                      iter, maxiter, arfitness[1] * fnscale))
    }
  }
  cnt <- c(`function`=as.integer(counteval), gradient=NA)
  
  log <- list()
  ## Subset lognostic data to only include those iterations which
  ## where actually performed.
  if (log.value) log$value <- value.log[1:iter,]
  if (log.sigma) log$sigma <- sigma.log[1:iter]
  if (log.eigen) log$eigen <- eigen.log[1:iter,]
  if (log.pop)   log$pop   <- pop.log[,,1:iter]
  if (log.bestVal) log$bestVal <- bestVal.log
  
  ## Drop names from value object
  names(best.fit) <- NULL
  res <- list(par=best.par,
              value=best.fit / fnscale,
              counts=cnt,
              n.evals=counteval,
              convergence=ifelse(iter >= maxiter, 1L, 0L),
              message=msg,
              constr.violations=cviol,
              diagnostic=log
  )
  class(res) <- "cma_es.result"
  return(res)
}

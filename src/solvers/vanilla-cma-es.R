library(here)

source(here::here("src", "sigma_updaters.R"))
source(here::here("src", "solvers", "utils.R"))

# Implementation of VANILLA-CMA-ES
# This implementation is based on the {cmaes} package.
# Source code: https://github.com/cran/cmaes/blob/master/R/cmaes.R
# CRAN package: https://cran.r-project.org/web/packages/cmaes/index.html

# par :: list[double
# fn :: list[double] -> double
# lower :: double
# upper :: double
# control :: list[key :: str -> value :: any]

vanilla_cma_es <- function(par, fn, ..., lower = -100, upper = 100, control=list()) {  
  xmean <- par
  N <- length(xmean)

  ## Termination conditions:
  terminate.stopfitness <- controlParam("terminate.stopfitness", TRUE, as.logical)
  terminate.std_dev_tol <- controlParam("terminate.std_dev_tol", TRUE, as.logical)
  terminate.cov_mat_cond <- controlParam("terminate.cov_mat_cond", TRUE, as.logical)
  terminate.maxiter <- controlParam("terminate.maxiter", TRUE, as.logical)

  ## Hacks:
  do_flatland_escape <- controlParam("do_flatland_escape", TRUE, as.logical)
  do_hsig <- controlParam("do_hsig", TRUE, as.logical)
  
  ## Parameters:
  trace       <- controlParam("trace", FALSE, as.logical)
  fnscale     <- controlParam("fnscale", 1, as.numeric)
  stopfitness <- controlParam("stopfitness", 1e-8, as.numeric)
  budget      <- controlParam("budget", 10000*N, as.numeric)                     ## The maximum number of fitness function calls
  sigma       <- controlParam("sigma", 1, as.numeric)
  sc_tolx     <- controlParam("stop.tolx", 1e-12 * sigma, as.numeric) ## Undocumented stop criterion
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
  lambda      <- controlParam("lambda", 4 * N, as.numeric)
  maxiter     <- controlParam("maxiter", round(budget/lambda), as.numeric)
  mu          <- controlParam("mu", floor(lambda/2), as.numeric)
  weights     <- controlParam("weights", rep(1, mu), as.numeric)
  weights     <- weights/sum(weights)

  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2), as.numeric)
  cc          <- controlParam("ccum", 4/(N+4), as.numeric)
  mucov       <- controlParam("ccov.mu", mueff, as.numeric)
  ccov        <- controlParam("ccov.1",
                              (1/mucov) * 2/(N+1.4)^2
                              + (1-1/mucov) * ((2*mucov-1)/((N+2)^2+2*mucov)), as.numeric)
  # Step-size update policy 
  sigma_updater <- get(controlParam("sigma_updater", "sigma_identity", identity))

  # CSA hyper-parameters
  cs          <- controlParam("cs", (mueff+2)/(N+mueff+3), as.numeric)
  damps       <- controlParam("damps",
                              1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + cs, as.numeric)

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
  ps <- rep(0.0, N)
  B <- diag(N)
  D <- diag(N)
  BD <- B %*% D
  C <- BD %*% t(BD)
  
  chiN <- sqrt(N) * (1-1/(4*N)+1/(21*N^2))
  
  iter <- 0L      ## Number of iterations
  counteval <- 0L ## Number of function evaluations
  cviol <- 0L     ## Number of constraint violations
  msg <- NULL     ## Reason for terminating
  nm <- names(par) ## Names of parameters
  
  ## Preallocate work arrays:
  arx <-  replicate(lambda, runif(N,lower,upper))
  arfitness <- apply(arx, 2, function(x) fn(x, ...) * fnscale)

  # Runtime parameters of PPMF
  prev_midpoint_fitness <- sign(stopfitness) * Inf
  midpoint_fitness <- sign(stopfitness) * Inf

  counteval <- counteval + lambda
  while (counteval < budget) {
     iter <- iter + 1L
    
    if (!keep.best) {
      best.fit <- Inf
      best.par <- NULL
    }
    if (log.sigma)
      sigma.log[iter] <- sigma
    
    if (log.bestVal) 
      bestVal.log <- rbind(bestVal.log,min(suppressWarnings(min(bestVal.log)), min(arfitness)))
    
    ## Generate new population:
    arz <- matrix(rnorm(N*lambda), ncol=lambda)
    arx <- xmean + sigma * (BD %*% arz)
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
    selz <- arz[,aripop]
    zmean <- drop(selz %*% weights)
    
    ## Save selected x value:
    if (log.pop) pop.log[,,iter] <- selx
    if (log.value) value.log[iter,] <- sort(arfitness[aripop])
    
    ## Cumulation: Update evolutionary paths
    ps <- (1-cs)*ps + sqrt(cs*(2-cs)*mueff) * (B %*% zmean)

    hsig <- if (do_hsig) {
      drop((norm(ps)/sqrt(1-(1-cs)^(2*counteval/lambda))/chiN) < (1.4 + 2/(N+1)))
    } else { 
      1
    }
    pc <- (1-cc)*pc + hsig * sqrt(cc*(2-cc)*mueff) * drop(BD %*% zmean)
    
    ## Adapt Covariance Matrix:
    BDz <- BD %*% selz
    C <- (1-ccov) * C + ccov * (1/mucov) *
      (pc %o% pc + (1-hsig) * cc*(2-cc) * C) +
      ccov * (1-1/mucov) * BDz %*% diag(weights) %*% t(BDz)
    
    ## Adapt step size sigma:
    sigma <- sigma_updater(sigma)
    
    e <- eigen(C, symmetric=TRUE)
    if (log.eigen) {
      eigen.log[iter,] <- rev(sort(e$values))
    }
    
    if (terminate.cov_mat_cond && (!all(e$values >= sqrt(.Machine$double.eps) * abs(e$values[1])))) {      
      msg <- "Covariance matrix 'C' is numerically not positive definite."
      break
    }
    
    B <- e$vectors
    D <- diag(sqrt(e$values), length(e$values))
    BD <- B %*% D
    
    ## break if fit:
    if (terminate.stopfitness && (arfitness[1] <= stopfitness * fnscale)) {
      msg <- "Stop fitness reached."
      break
    }
    
    ## Check stop conditions:
    ## Condition 1 (sd < tolx in all directions):
    if (terminate.std_dev_tol && (all(D < sc_tolx) && all(sigma * pc < sc_tolx))) {
      msg <- "All standard deviations smaller than tolerance."
      break
    }

    if (terminate.maxiter && (iter >= maxiter)) {
      msg <- "Exceeded maximal number of iteration"
      break
    }
    
    ## Escape from flat-land:
    if (do_flatland_escape) {
      if (arfitness[1] == arfitness[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
        sigma <- sigma * exp(0.2+cs/damps);
      if (trace)
        message("Flat fitness function. Increasing sigma.")
      }
    }

    if (trace)
      message(sprintf("Iteration %i of %i: current fitness %f",
                      iter, maxiter, arfitness[1] * fnscale))
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

  if (is.null(msg)) {
    msg <- "Budget exhausted"
  }
  
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

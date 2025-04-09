library(rlang)

# x :: double

norm <- function(x) {
  drop(sqrt(crossprod(x)))
}

# name :: str
# default_value :: any
# type_factory :: as.numeric | as.logical | as.function | as.character | ...
# solver_env :: caller environment
 
controlParam <- function(name, default_value, type_factory, solver_env = rlang::caller_env()) {
  control_value <- solver_env$control[[name]]
  if (is.null(control_value)) {
    return(default_value)
  }

  if (is.numeric(control_value) && identical(type_factory, as.function)) {
    return(function(...) { as.numeric(control_value)})
  }

  return(type_factory(control_value))
}


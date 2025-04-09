library(cecb)
library(here)


download_cec_data <- function(dimensions) {
  for (d in dimensions) {
    ignore <- cecs::cec2017(1, rep(0, d))
  }
}



main <- function() {
  download_cec_data(c(10, 30, 50))
  cecb::run_benchmark(here::here("experiments", "benchmarks", "specification.yml"))
}

main()

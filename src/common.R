library(ggplot2)

# () -> ISO 8601 timestamp

make_now_iso8601_timestamp <- function() {
  now <- Sys.time()
  return(format(as.POSIXct(now, tz = "UTC"), "%Y%m%dT%H%M%S"))
}

# () -> ggplot2::theme object
 
make_ew_theme <- function() {
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 15, face = "bold"),
    axis.text = ggplot2::element_text(size = 15, face = "bold"),
    legend.text = ggplot2::element_text(size = 15, face = "bold"),
    legend.title = ggplot2::element_text(size = 15, face = "bold"),
    title = ggplot2::element_text(size = 20, face = "bold")
  )
}


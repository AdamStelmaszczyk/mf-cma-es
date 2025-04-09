source(here::here("src", "common.R"))
library(scales)

# eigen_df :: data.frame[t :: integer, eigenvalue :: double, decision_variable :: str]
# solver_name :: str
# dimension :: integer

plot_eigenvalues <- function(eigen_df, solver_name, dimension) {
 eigen_df %>%
  ggplot2::ggplot(ggplot2::aes(x = t, y = eigenvalue, col = decision_variable)) +
  ggplot2::geom_line(ggplot2::aes(group = decision_variable), size=0.5) +
  ggplot2::scale_y_continuous(trans = scales::log10_trans(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  make_ew_theme() + 
  ggplot2::scale_color_grey() +
  ggplot2::xlab("Generation") + 
  ggplot2::ylab("Eigenvalue") +
  ggplot2::ggtitle(stringr::str_glue("Solver: {solver_name}, Dimension: {dimension}"))
}

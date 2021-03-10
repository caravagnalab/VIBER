#' Plot the clustering assignments.
#'
#' @description This functions runs for every pair of dimensions
#' in the data the function \code{plot_2D}.
#'
#' @param x An object of class vb_bmm.
#' @param ... Default extra paramaters.
#'
#' @return A list of plots, that can be assembled in a unique figure
#' with other packages like \code{ggpubr}.
#'
#' @exportS3Method plot vb_bmm
#' @export plot.vb_bmm
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot(fit_mvbmm_example)
plot.vb_bmm <- function(x, ...)
{
  stopifnot(inherits(x, "vb_bmm"))

  samples = colnames(x$x %>% select(-cluster.Binomial))

  cm = combn(samples, 2)
  apply(
    cm,
    2,
    function(w) plot_2D(x, d1 = w[1], d2 = w[2], ...)
  )
}

my_ggplot_theme = function(cex = 1)
{
  theme_light(base_size = 10 * cex) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.3 * cex, "cm"),
      panel.background = element_rect(fill = 'white')
    )
}

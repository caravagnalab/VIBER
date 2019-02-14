#' Plot the clustering assignments.
#'
#' @description This functions runs for every pair of dimensions
#' in the data the function \code{plot_2D}.
#'
#' @param x An object of class vb_bmm.
#' @param ...
#'
#' @return A list of plots, that can be assembled in a unique figure
#' with other packages like \code{ggpubr}.
#' @export
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

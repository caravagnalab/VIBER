#' Plot 2D and 1D fits altogether.
#'
#' @description
#'
#' Performs a joint plot in which both the clustering scatter and the
#' marginal plots are repreoduced. This function uses the \code{plot_2D} and
#' \code{plot_1D} functions to create all the required plots, and the ellipsis
#' is used to forward parameters to both functions.
#'
#' @param x An object of class vb_bmm.
#' @param ... Extra parameters for \code{plot_2D} and \code{plot_1D} functions.
#'
#' @return A figure assembled with \code{ggpubr}.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot_fits(fit_mvbmm_example)
#'
#' \donotrun{
#'
#' require(dplyr)
#'
#' colors_clusters = fit_mvbmm_example$labels %>% unique %>% pull()
#' colors_clusters_samples = ggsci::pal_lancet()(colors_clusters %>% length())
#'
#' names(colors_clusters_samples) = colors_clusters
#'
#' plot_fits(fit_mvbmm_example, colors = colors_clusters_samples)
#' }
plot_fits = function(x, ...)
{
  stopifnot(inherits(x, "vb_bmm"))

  sample_ids = x$x %>% colnames()
  sample_ids = sample_ids[-1]

  # 2D cluster plot, arranged
  plotlist = plot(x, ...)

  by_row = ceiling(sqrt(plotlist %>% length()))
  if(by_row > sample_ids %>% length()) by_row = sample_ids %>% length()

  plot2d = ggpubr::ggarrange(
    plotlist = plotlist,
    nrow = by_row
  )

  # 1D cluster plot
  plot1d = plot_1D(x, ...)

  # Arrange
  ggpubr::ggarrange(
    plot1d,
    plot2d,
    ncol = 2,
    nrow = 1
  )

}




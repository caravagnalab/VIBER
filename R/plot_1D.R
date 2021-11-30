#' Plot the clustering assignments for marginal data.
#'
#' @description
#'
#' Plot a histogram of successes over trials, coloured by clustering assignments and
#' with a facet for each sample.
#'
#' @param x An object of class vb_bmm.
#' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#'
#' plot_1D(fit_mvbmm_example)
plot_1D = function(
  x,
  colors = NA)
{
  stopifnot(inherits(x, "vb_bmm"))

  sample_ids = x$x %>% colnames()
  sample_ids = sample_ids[-1]

  F_data = (x$x[-1]/x$y[-1]) %>%
    bind_cols(x$x[1]) %>%
    reshape2::melt()

  ns = sample_ids %>% length()
  by_row = ceiling(sqrt(ns * (ns - 1) / 2))
  if(by_row > sample_ids %>% length()) by_row = sample_ids %>% length()

  myp = ggplot(F_data) +
    geom_histogram(aes(x = value, fill = cluster.Binomial), binwidth = 0.01) +
    facet_wrap(~variable, nrow =  by_row) +
    xlim(0.01,1.01) +
    my_ggplot_theme() +
    guides(fill = guide_legend("Cluster"))

  if(!all(is.na(colors)))
    myp = myp + scale_fill_manual(values = colors)

  return(myp)
}


#' Plot the ELBO
#'
#' @param x An object of class 'vb_bmm'.
#' @param cex Cex of the plot.
#'
#' @return A plot of the ELBO.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot_ELBO(fit_mvbmm_example)
plot_ELBO = function(x, cex = 1)
{
  stopifnot(inherits(x, "vb_bmm"))

  ELBO = tibble(step = 1:length(x$ELBO), ELBO = x$ELBO)

  ggplot(ELBO, aes(step, ELBO)) +
    geom_line(color = 'steelblue') +
    geom_point(size = 1.5 * cex) +
    my_ggplot_theme(cex) +
    labs(
      title = bquote(bold("Status: ") ~ .(x$status)),
      subtitle = bquote(.(x$fit_type)~'with'~epsilon==.(x$epsilon_conv))
    )
}

#' Plot the Binomial peaks of the mixture.
#'
#' @param x An object of class 'vb_bmm'.
#' @param cex Cex of the plot.
#' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#'
#' @return A plot of the Binomial peaks per dimension.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot_peaks(fit_mvbmm_example)
plot_peaks = function(x, cex = 1, colors = NA)
{
  stopifnot(inherits(x, "vb_bmm"))

  peaks = reshape2::melt(x$theta_k)
  colnames(peaks) = c('Dimension', 'cluster', 'peak')

  prop = tibble(cluster = names(x$pi_k), proportion = x$pi_k)
  prop$proportion = round(prop$proportion, 2)
  prop$proportion = paste0('Mix. prop. ', prop$proportion, '%')

  p = ggplot(peaks, aes(x = Dimension, y = peak, ymax = peak, ymin = 0, color = cluster)) +
    geom_linerange() +
    geom_point() +
    my_ggplot_theme(cex) +
    facet_wrap(~cluster, nrow = 1) +
    ylim(0, 1) +
    labs(
      title = bquote(bold("Binomial peaks"))
    ) +
    geom_text(data = prop,
              aes(label = proportion, x = 1, y = 1),
              inherit.aes = FALSE,
              hjust = 0,
              size = 2.5 * cex
              )


  return(add_color_pl(x, p, colors))
}

#' Plot the mixing proportions of the mixture.
#'
#' @param x An object of class 'vb_bmm'.
#' @param cex Cex of the plot.
#' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#'
#' @return A plot of the mixing proportions of the mixture.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot_mixing_proportions(fit_mvbmm_example)
plot_mixing_proportions = function(x, cex = 1, colors = NA)
{
  stopifnot(inherits(x, "vb_bmm"))

  prop = tibble(cluster = names(x$pi_k), proportion = x$pi_k)

  p = ggplot(prop, aes(x = cluster, y = proportion, fill = cluster)) +
    geom_bar(stat = 'identity') +
    my_ggplot_theme(cex) +
    # facet_wrap(~cluster, nrow = 1) +
    ylim(0, 1) +
    labs(
      title = bquote(bold("Mixing proportions"))
    )

  return(p)
}

#' Plot the latent variables of the mixture.
#'
#' @param x An object of class 'vb_bmm'.
#' @param cex Cex of the plot.
#'
#' @return A plot of the latent variables of the mixture.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' plot_latent_variables(fit_mvbmm_example)
plot_latent_variables = function(x, cex = 1)
{
  stopifnot(inherits(x, "vb_bmm"))

  # Valuse for the ordering
  lv = x$r_nk %>%
    as_tibble()

  lv$uncertainty = 1 - apply(lv, 1, max)
  lv$Cluss_ass = x$labels$cluster.Binomial
  lv$mutation = rownames(x$r_nk)

  lv = lv %>% arrange(Cluss_ass, uncertainty)

  mutation_ordering = lv$mutation

  # Reshape
  lv = reshape2::melt(x$r_nk) %>% as_tibble()
  colnames(lv) = c('Point', "Cluster", "Value")

  lv$Point = factor(lv$Point, levels = mutation_ordering)


  ggplot(lv, aes(x = Cluster, y = Point, fill = Value)) +
    geom_raster() +
    scale_fill_distiller(palette = 'YlGnBu', direction = 1, limits = c(0, 1)) +
    my_ggplot_theme(cex) +
    theme(
      # axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      title = bquote(bold("Latent variables"))
    ) +
    guides(
      fill = guide_colorbar("", barwidth = 8)
    )

  # Data
  # cn = colnames(x$x)[-1]1
  #
  # vals = lapply(cn, function(n) x$x[, n]/x$y[, n])
  # vals = Reduce(cbind, vals)
  #
  # colnames(vals) = cn
  # vals$y = paste0('x', 1:nrow(vals))
  #
  # vals = reshape2::melt(vals[order(x$labels$cluster.Binomial), ], id = 'y')
  # colnames(vals) = c( "Y", 'Dimension', "Success probability")
  #
  # dt = ggplot(vals, aes(x = Dimension, y = Y, fill = `Success probability`)) +
  #   geom_raster() +
  #   scale_fill_gradient(low = 'orange', high = 'red') +
  #   theme_light(base_size = 8 * cex) +
  #   theme(
  #     legend.position = "bottom",
  #     legend.key.size = unit(.3 * cex, "cm"),
  #     legend.text = element_text(size = 8 * cex),
  #     # axis.title.y = element_blank(),
  #     axis.text.y = element_blank(),
  #     axis.ticks.y = element_blank()
  #   ) +
  #   labs(
  #     title = bquote(bold("Latent variables"))
  #   )

  # pl
}



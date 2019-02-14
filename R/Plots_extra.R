#' Plot the ELBO
#'
#' @param x An object of class 'vb_bmm'.
#' @param cex Cex of the plot.
#'
#' @return A plot of the ELBO.
#' @export
#'
#' @examples
#' TODO
plot_ELBO = function(x, cex = 1)
{
  stopifnot(inherits(x, "vb_bmm"))

  ELBO = tibble(step = 1:length(x$ELBO), ELBO = x$ELBO)

  ggplot(ELBO, aes(step, ELBO)) +
    geom_line(color = 'steelblue') +
    geom_point(size = 1.5 * cex) +
    theme_light(base_size = 8 * cex) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.3 * cex, "cm"),
      legend.text = element_text(size = 8 * cex)
    ) +
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
#' TODO
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
    theme_light(base_size = 8 * cex) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.3 * cex, "cm"),
      legend.text = element_text(size = 8 * cex)
    ) +
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
#' @return A plot of the of the mixing proportions of the mixture.
#' @export
#'
#' @examples
#' TODO
plot_mixing_proportions = function(x, cex = 1, colors = NA)
{
  stopifnot(inherits(x, "vb_bmm"))

  prop = tibble(cluster = names(x$pi_k), proportion = x$pi_k)

  p = ggplot(prop, aes(x = cluster, y = proportion, fill = cluster)) +
    geom_bar(stat = 'identity') +
    theme_light(base_size = 8 * cex) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.3 * cex, "cm"),
      legend.text = element_text(size = 8 * cex)
    ) +
    # facet_wrap(~cluster, nrow = 1) +
    ylim(0, 1) +
    labs(
      title = bquote(bold("Mixing proportions"))
    )

  return(add_fill_pl(x, p, colors))
}


#' #' Plot the mixing proportions of the mixture, per dimension.
#' #'
#' #' @param x An object of class 'vb_bmm'.
#' #' @param cex Cex of the plot.
#' #' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#' #'
#' #' @return A plot of the of the mixing proportions of the mixture, per dimension.
#' #' @export
#' #'
#' #' @examples
#' #' TODO
#' plot_mixing_proportions_per_dimension = function(x, cex = 1, colors = NA)
#' {
#'   stopifnot(inherits(x, "vb_bmm"))
#'
#'   # Per sample
#'   samples = colnames(x$x %>% select(-cluster.Binomial))
#'   prop.samples = lapply(
#'     samples,
#'     function(s)
#'     {
#'       p = bind_cols(get_2D_points(x, s, s)[, 1],
#'                     cl = x$labels) %>%
#'         filter(!!s > 0)
#'
#'       n = nrow(p)
#'       table(p %>% pull(cluster.Binomial))/n
#'     })
#'
#'   prop.samples = Reduce(bind_rows, prop.samples)
#'   prop.samples$sample = samples
#'
#'   prop.samples = reshape2::melt(prop.samples)
#'   colnames(prop.samples) = c('sample', 'cluster', 'proportion')
#'
#'   p = ggplot(prop.samples, aes(x = cluster, y = proportion, fill = cluster)) +
#'     geom_bar(stat = 'identity') +
#'     theme_light(base_size = 8 * cex) +
#'     theme(
#'       legend.position = "bottom",
#'       legend.key.size = unit(.3 * cex, "cm"),
#'       legend.text = element_text(size = 8 * cex)
#'     ) +
#'     facet_wrap(~sample, nrow = 1) +
#'     ylim(0, 1) +
#'     labs(
#'       title = bquote(bold("Mixing proportions per dimension"))
#'     )
#'
#'   if(!is.na(colors)){
#'     wh_col = unique(x$x$cluster.Binomial)
#'     stopifnot(all(wh_col %in% names(colors)))
#'
#'     p = p + scale_fill_manual(values = colors)
#'   }
#'
#'   p
#' }
#'


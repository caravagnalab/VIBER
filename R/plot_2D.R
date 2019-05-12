#' 2D clusters plot.
#'
#' @description Plot the fit for two dimensions, one against the other. Colour each point
#' (defined as the ratio of sucesses over trail) according to the clustering assignments.
#'
#'
#' @param x An object of class 'vb_bmm'.
#' @param d1 The name of the dimension to plot (x-axis).
#' @param d2 The name of the dimension to plot (y-axis).
#' @param cex Cex of the points and the overall plot.
#' @param alpha Alpha of the points.
#' @param downsample Maximun number of points to plot. Default is \code{Inf}, no downsampling.
#' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#'
#' @return
#'
#' @export
#'
#' @examples
#' TODO
plot_2D = function(x,
                   d1,
                   d2,
                   cex = 1,
                   alpha = 0.3,
                   downsample = Inf,
                   colors = NA)
{
  data = get_2D_points(x, d1, d2)
  data$cluster.Binomial = x$labels$cluster.Binomial

  # downsample data if too many
  caption = ''
  if(!is.null(downsample) & nrow(data) > downsample)
  {
    stopifnot(is.numeric(downsample))

    data = data[sample(1:nrow(data), downsample), , drop = FALSE]

    caption = paste0(caption, "Downsampled (N = ", downsample, ')')
  }

  # Ggplot object
  p = ggplot(data = data,
               aes(
                 x = eval(parse(text = d1)),
                 y = eval(parse(text = d2)),
                 color = factor(cluster.Binomial)
               )) +
    geom_point(alpha = alpha, size = 1 * cex) +
    labs(
      title = bquote(bold(.(d1)) ~ "vs" ~ bold(.(d2))),
      caption = caption,
      x = d1,
      y = d2
    ) +
    guides(color = guide_legend(title = 'Cluster', override.aes = list(alpha = 1))) +
    theme_light(base_size = 8 * cex) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.3 * cex, "cm"),
      legend.text = element_text(size = 8 * cex)
    ) +
    geom_vline(xintercept = 0,
               colour = "darkgray",
               size = .3) +
    geom_hline(yintercept = 0,
               colour = "darkgray",
               size = .3)

  return(add_color_pl(x, p, colors))
}


get_2D_points = function(x, d1, d2)
{
  # Data
  d1.nv = x$x[, d1] %>% pull()
  d1.dp = x$y[, d1] %>% pull()

  d2.nv = x$x[, d2] %>% pull()
  d2.dp = x$y[, d2] %>% pull()

  data = cbind(d1.nv/d1.dp, d2.nv/d2.dp) %>% as_tibble()
  colnames(data) = c(d1, d2)

  data
}

# modify ggplot colour
add_color_pl =  function(x, pl, colors)
{
  if(!is.vector(colors) | any(is.na(colors))) return(pl)

  # clusters in x
  wh_col = unique(x$x$cluster.Binomial)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_color_manual(values = colors)
}

# modify ggplot fill
add_fill_pl =  function(x, pl, colors)
{
  if(!is.vector(colors) | any(is.na(colors))) return(pl)

  # clusters in x
  wh_col = unique(x$x$cluster.Binomial)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_fill_manual(values = colors)
}




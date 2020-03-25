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
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' 
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

  # Fits
  points = x$theta_k[c(d1, d2), unique(data$cluster.Binomial[!is.na(data$cluster.Binomial)]), drop = F]
  # points = x$theta_k[c(d1, d2), unique(data$cluster.Binomial), drop = FALSE]
  points = data.frame(t(points))
  points$cluster.Binomial = rownames(points)

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
               colour = factor(cluster.Binomial)
             )) +
    geom_point(alpha = alpha, size = 1 * cex) +
    labs(
      title = bquote(bold(.(d1)) ~ "vs" ~ bold(.(d2))),
      caption = caption,
      x = d1,
      y = d2
    ) +
    guides(color = guide_legend(title = 'Cluster', override.aes = list(alpha = 1))) +
    VIBER:::my_ggplot_theme(cex) +
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
               size = .3) +
    geom_point(
      data = points,
      aes(
        x = eval(parse(text = d1)),
        y = eval(parse(text = d2)),
        fill = cluster.Binomial
      ),
      alpha = 1,
      size = 3 * cex,
      pch = 22,
      color = 'black'
    ) +
    guides(fill = FALSE)

  return(VIBER:::add_fill_pl(x, VIBER:::add_color_pl(x, p, colors), colors))
}

plot_2D_trials = function(x,
                   d1,
                   d2,
                   cutoff = 10,
                   cex = 1,
                   alpha = 0.3,
                   downsample = Inf)
{
  data = get_2D_points(x, d1, d2)
  trials = get_2D_trials(x, d1, d2)
  trials$class = apply(trials,
                       1,
                       function(w) sum(c(w[1] > cutoff, w[2] > cutoff)))
  trials$class = 2 - trials$class

  # downsample data if too many
  caption = ''
  if(!is.null(downsample) & nrow(data) > downsample)
  {
    stopifnot(is.numeric(downsample))

    data = data[sample(1:nrow(data), downsample), , drop = FALSE]

    caption = paste0(caption, "Downsampled (N = ", downsample, ')')
  }

  # Ggplot object
  ggplot(data = bind_cols(data, trials),
             aes(
               x = eval(parse(text = d1)),
               y = eval(parse(text = d2)),
               colour = paste(class)
               )) +
    geom_point(alpha = alpha, size = 1 * cex) +
    scale_color_manual(values = c(`0`='forestgreen', `1`='orange', `2`='red')) +
    labs(
      title = bquote(bold(.(d1)) ~ "vs" ~ bold(.(d2)) ~ ": trials below cutoff"~.(cutoff)),
      caption = caption,
      x = d1,
      y = d2
    ) +
    guides(color = guide_legend(title = 'Trials', override.aes = list(alpha = 1))) +
    my_ggplot_theme(cex) +
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

get_2D_trials = function(x, d1, d2)
{
  # Data
  d1.dp = x$y[, d1] %>% pull()
  d2.dp = x$y[, d2] %>% pull()

  data = cbind(d1.dp, d2.dp) %>% as_tibble()
  colnames(data) = c(d1, d2)

  data
}

# modify ggplot colour
add_color_pl =  function(x, pl, colors)
{
  if(!is.vector(colors) | any(is.na(colors))) return(pl)

  # clusters in x
  cl = x$x$cluster.Binomial[!is.na(x$x$cluster.Binomial), ]
  wh_col = unique(cl)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_color_manual(values = colors)
}

# modify ggplot fill
add_fill_pl =  function(x, pl, colors)
{
  if(!is.vector(colors) | any(is.na(colors))) return(pl)

  # clusters in x
  cl = x$x$cluster.Binomial[!is.na(x$x$cluster.Binomial), ]
  wh_col = unique(cl)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_fill_manual(values = colors)
}




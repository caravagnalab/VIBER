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
#' @param cut_zeros If \code{TRUE}, remove points that are 0 in both dimensions.
#' @param binning_b This parameter goes with \code{binning_n}. If both \code{NA},
#' no binning is computed. If a real valuem points are binned in a grid of size
#' \code{binning_b} by \code{binning_b}.
#' @param binning_n This parameter goes with \code{binning_b}. If both \code{NA},
#' no binnig is computed. If an integer value points are binned in a grid of size
#'defined by \code{binning_b}, and up to \code{binning_n} points are sampled
#' per bin according to each cluster whose points map inside the bin.
#' @param downsample Maximun number of points to plot. Default is \code{Inf}, no downsampling.
#' @param colors Optional vector of colors, default (\code{NA}) are \code{ggplot} colors.
#' @param bin_params If \code{TRUE} it adds also a point to show the fit Binomial success probabilities.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'
plot_2D = function(x,
                   d1,
                   d2,
                   cex = 1,
                   alpha = 0.3,
                   cut_zeroes = TRUE,
                   binning_n = NA,
                   binning_b = NA,
                   downsample = Inf,
                   colors = NA,
                   bin_params = FALSE)
{
  data = VIBER:::get_2D_points(x, d1, d2)
  data[['cluster.Binomial']] = x$labels$cluster.Binomial

  # Cutting zeroes
  if(cut_zeroes) {
    data = data[!(data[[d1]] == 0 & data[[d2]] == 0), , drop = FALSE]

    if(nrow(data) == 0) {
      warning(d1, ' vs ', d2, ' curring 0s there are no points, returning empty plot')
      return(ggplot() + geom_blank())
    }
  }

  # Binning
  if(!is.na(binning_n) & !is.na(binning_b))
  {
    stopifnot(is.numeric(binning_n))
    stopifnot(is.numeric(binning_b))
    stopifnot(binning_n > 0)
    stopifnot(binning_b > 0 & binning_b < 1)

    # Squares of size binning_n x binning_n
    data$b1 = round(data[[d1]]/binning_n)
    data$b2 = round(data[[d2]]/binning_b)

    # Sample per cluster up to binning_n points
    data = data %>%
      dplyr::group_by(b1, b2, cluster.Binomial) %>%
      dplyr::sample_n(min(n(), binning_n))
  }

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
    guides(fill = 'none')

  if(bin_params)
  {
    p = p +
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
      )
  }

  return(add_fill_pl(x, add_color_pl(x, p, colors), colors))
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
  cl = x$x %>% pull(1)
  cl = cl[!is.na(unlist(cl))]

  wh_col = unique(cl)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_color_manual(values = colors)
}

# modify ggplot fill
add_fill_pl =  function(x, pl, colors)
{
  if(!is.vector(colors) | any(is.na(colors))) return(pl)

  # clusters in x
  # clusters in x
  cl = x$x %>% pull(cluster.Binomial)
  cl = cl[!is.na(cl)]

  wh_col = unique(cl)
  stopifnot(all(wh_col %in% names(colors)))

  pl + scale_fill_manual(values = colors)
}




#' Plot the clusters
#'
#' @param x An object of class vb_bmm.
#' @param ...
#'
#' @return Nothing, just print to screen.
#' @export
#'
#' @examples
#' TODO
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

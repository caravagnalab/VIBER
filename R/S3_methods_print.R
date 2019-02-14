############## S3 functions for the object

#' Print to screen the object
#'
#' @param x An object of class vb_bmm.
#' @param ...
#'
#' @return Nothing, just print to screen.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' print(fit_mvbmm_example)
summary.vb_bmm <- function(x, ...) {
  print.vb_bmm(x, ...)
}

#' Print to screen the object
#'
#' @param x An object of class vb_bmm.
#'
#' @return Nothing, just print to screen.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' print(fit_mvbmm_example)
print.vb_bmm <- function(x, ...)
{
  stopifnot(inherits(x, "vb_bmm"))

  pioHdr(attributes(x)$modelname)

  pio::pioStr("\n Points", paste0("N = ",  x$N))
  pio::pioStr("\nSamples", paste0("W = ",  nrow(x$theta_k)))
  pio::pioStr("\n\n Status",  paste0(x$status, ' (', length(x$ELBO), ' steps; eps. ', x$epsilon_conv, ') with ', x$fit_type))

  sor_p = sort(x$pi_k, decreasing = TRUE)
  sor_p = names(sor_p)

  pio::pioStr("\n\nBinomial parameters (2 digits rounded)", '\n')

  pio::pioDisp(round(x$theta_k[, sor_p], 2))

  pio::pioStr("\nProportions (sorted)", '', suffix = '\n')

  pio::pioDisp(x$pi_k[sor_p])
}

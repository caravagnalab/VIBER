############## S3 functions for the object

#' Print to screen the object
#'
#' @param object An object of class vb_bmm.
#' @param ... Default extra paramaters.
#'
#' @return Nothing, just print to screen.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' print(fit_mvbmm_example)
summary.vb_bmm <- function(object, ...) {
  print.vb_bmm(object, ...)
}

#' Print to screen the object
#'
#' @param object An object of class vb_bmm.
#' @param ... Default extra paramaters.
#' @return Nothing, just print to screen.
#' @export
#'
#' @examples
#' data(fit_mvbmm_example)
#' print(fit_mvbmm_example)
print.vb_bmm <- function(object, ...)
{
  x = object
  stopifnot(inherits(x, "vb_bmm"))

  # pioHdr(attributes(x)$modelname)
  # 
  # pio::pioStr("\n    Points", paste0("N = ",  x$N))
  # pio::pioStr("\nDimensions", paste0("W = ",  nrow(x$theta_k)))
  # pio::pioStr("\n       Fit",
  #             paste0(x$status,
  #                    ' (', length(x$ELBO), ' steps; eps. ', x$epsilon_conv, ') with ',
  #                    x$fit_type))
  # 
  # sor_p = sort(x$pi_k, decreasing = TRUE)
  # sor_p = names(sor_p)
  # 
  # pio::pioStr("\n\nBinomial parameters (2 digits rounded)", '\n')
  # 
  # pio::pioDisp(round(x$theta_k[, sor_p], 2))
  # 
  # pio::pioStr("\nProportions (ordered)", '', suffix = '\n')
  # 
  # pio::pioDisp(x$pi_k[sor_p])
  
  cli::cli_rule(
    paste(
      crayon::bgYellow(crayon::black("[ VIBER ] {.value {attributes(x)$modelname}}")),
      'n = {.value {x$N}} (w = {.value {nrow(x$theta_k)}} dimensions). Fit with {.field k = {x$K}} clusters.'
    )
  )
  
      
  # pio::pioHdr(attributes(x)$modelname)
  
  # cli::cli_alert_info(
  #   'n = {.value {x$N}} points with w = {.value {nrow(x$theta_k)}} dimensions. Fit {.value {ifelse(x$status, crayon::green("converged"), crayon::red("interupted"))}} in {.value {length(x$ELBO)}} steps, and \u03B5 = {.value {x$epsilon_conv}}.'
  # )
  
  sor_p = sort(x$pi_k, decreasing = TRUE)
  sor_p = names(sor_p)
  
  pi = round(x$pi_k[sor_p], digits = 2) * 100
  pi = pi[pi > 0]
  pi_label = paste0(pi, '% [', yellow(names(pi)), ']')
  cli::cli_li(
    ' Clusters: \u03C0 = {.value {pi_label}}, with \u03C0 > 0.'
  )
  
  bin = round(x$theta_k[, sor_p], 2)
  bin = bin[, names(pi), drop = FALSE]
  
  bin_label = apply(bin, 2, function(x) paste0('<', paste0(x, collapse = ', '), '>'))
  bin_label = paste0(bin_label, ' [', yellow(names(bin_label)), ']')
  
  cli::cli_li(
    'Binomials: \u03B8 = {.value {bin_label}}.'
  )
  
  cli::cli_end()
  
  cli::cli_alert_info(
    'Score(s): ELBO = {.value {round(x$ELBO[length(x$ELBO)], 3)}}. Fit {.value {ifelse(x$status == "CONVERGED", crayon::green("converged"), crayon::red(x$status))}} in {.value {length(x$ELBO)}} steps, \u03B5 = {.value {x$epsilon_conv}}.'
  )
  
  
}

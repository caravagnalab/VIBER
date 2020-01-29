#' Variational fit for Binomial mixtures
#'
#' @description
#'
#' Variational fit for a semi-parametric Dirichelt mixture of Binomial
#' distributions. The fit convergency can be monitored through the ELBO,
#' can be run either sequentially (single core) or in parallel. You need
#' to provide an upper bound on the number of clusters that you want to
#' obtain, through parameters \code{K}. You can explicit the Dirichlet
#' prior for the concentration of the mixture (\code{alpha_0}), as well as
#' the hyperparmeters of the Beta priors for each mixture component.
#'
#' @param x A matrix where each column is a dimension of the multivariate Binomial,
#' and each row is an input point. Values of this matrix represent the number of
#' successes of independent Bernoulli trials. This matrix and \code{y} should have
#' the same dimension.
#' @param y A matrix where each column is a dimension of the multivariate Binomial,
#' and each row is an input point. Values of this matrix represent the number of
#' attempts of independent Bernoulli trials. This matrix and \code{x} should have
#' the same dimension.
#' @param K The maximum number of clusters returned, it should be lower than the
#' number of rows of \code{x} and \code{y}. Default is \code{K = 10}; lower values
#' speed up convergence.
#' @param alpha_0 The concentration parameter of the Dirichlet mixture. The default
#' is a stringent fit with \code{alpha = 1e-6}.
#' @param a_0 Prior Beta hyperparameter. If this values is a scalar than all the
#' mixture components have the same prior. The default is scalar  \code{a_0 = 1}.
#' @param b_0 Prior Beta hyperparameter. If this values is a scalar than all the
#' mixture components have the same prior. The default is scalar  \code{b_0 = 1}.
#' @param max_iter Maximum number of fit iterations. The fit is interrupted when
#' this number of iterations is performed. Default \code{max_iter = 5000}
#' @param epsilon_conv Epsilon to measure convergence (ELBO absolute difference).
#' @param samples Number of fits computed by the algorithm. Only the best fit is
#' returned. This value must be greater or equal than 1.
#' @param q_init Initialization of the q-distribution to compute the approximation
#' of the posterior distributions. This can be set in three different waysL
#' equal to the prior (\code{q_init = 'prior'}), via kmeans clustering
#' (\code{q_init = 'kmeans'}) and capturing points which are private to each
#' dimension (\code{q_init = 'private'}). The default is equal to the prior.
#' @param trace If true the trace computed during the fit is returned (this allows
#' to check fits a posterirori, make animations etc.). Default is \code{FALSE}; this
#' feature can slow down quite substantially the fit.
#'
#' @return An object of class \code{vb_bmm} which contains S3 methods to extract
#' the fit, plots the results, compute summary statistics etc.
#'
#' @export
#'
#' @import pio
#' @import easypar
#' @import tidyverse
#' @import crayon
#' @import reshape2
#' @import cli
#'
#' @examples
#' data(mvbmm_example)
#' f = variational_fit(mvbmm_example$successes, mvbmm_example$trials)
#' print(f)
variational_fit = function(x,
                           y,
                           K = 10,
                           alpha_0 = 1e-6,
                           a_0 = 1,
                           b_0 = 1,
                           max_iter = 5000,
                           epsilon_conv = 1e-10,
                           samples = 10,
                           q_init = 'prior',
                           trace = FALSE,
                           description = "My VIBER model")
{
  best = NULL

  pioHdr('VIBER - variational fit')

  # Stop if errors on the input
  var_check_input(x,
                  y,
                  K,
                  alpha_0,
                  a_0,
                  b_0,
                  max_iter,
                  epsilon_conv,
                  samples,
                  q_init)

  TIME = as.POSIXct(Sys.time(), format = "%H:%M:%S")

  # # Console header
  # cat(bold("\n\tINPUT"))
  # 
  # pioStr("\n  Points", paste0('N = ', nrow(x)))
  # pioStr("\nClusters", paste0('K = ', K), suffix = "(max)\n")
  # 
  # pioStr("\nDirichlet", paste0('alpha = ', alpha_0), suffix = "(conc.)")
  # pioStr("\n     Beta", paste0('a0 = ', a_0, '; b0 =', b_0), suffix = "(shape)\n")
  # pioStr("\n     Beta (posterior)", q_init)
  # 
  # pioStr(
  #   "\n Optimize", paste0('epsilon = ', epsilon_conv, '; steps =', max_iter, '; r = ', samples),
  #   suffix = '\n\n'
  # )
  # Console header
  cli::cli_alert_info(
    "Input n = {.value {nrow(x)}}, with k < {.value {K}}. Dirichlet concentration {.field \u03B1 = {alpha_0}}."
    )
  
  cli::cli_alert_info(
    "Beta (a_0, b_0) = ({.value {a_0}}, {.value {b_0}}); q_i = {.value {q_init}}. Optimise: \u03B5 = {.value {epsilon_conv}} or {.value {max_iter}} steps, r = {.value {samples}} starts."
  )
  
  # Fits are obtained using the easypar package
  # which allows easy parallelization of R functions
  #
  # https://github.com/caravagn/easypar
  #
  fits = easypar::run(
    FUN = function(i) {
      vb_bmm_MV(
        x_NV = x,
        x_DP = y,
        K = K,
        alpha_0 = alpha_0,
        a_0 = a_0,
        b_0 = b_0,
        max_iter = max_iter,
        epsilon_conv = epsilon_conv,
        save_trace = trace,
        q_init = q_init,
        description = description
      )
    },
    PARAMS = lapply(1:samples, list),
    packages = c("crayon", "pio", "VIBER", "tidyverse"),
    export = ls(globalenv(), all.names = TRUE),
    cores.ratio = .8,
    parallel = TRUE,
    cache = NULL
  )

  # Polish errors if any
  nerrs = numErrors(fits)
  if(nerrs == samples) {

    lapply(fits, function(w) print(w$message))

    stop("All task returned errors, not fit available.")
  }

  fits = filterErrors(fits)

  # Model selection is done as usual by
  # maximizing ELBO values. Only the best
  # model is returned.
  best = NULL
  best_ELBO = -Inf

  for (i in seq_along(fits))
  {
    obj = fits[[i]]

    if (max(obj$ELBO) > best_ELBO) {
      best = obj
      best_ELBO = max(obj$ELBO)
    }
  }

  # Print some output
  TIME = difftime(as.POSIXct(Sys.time(), format = "%H:%M:%S"), TIME, units = "mins")
  
  cat('\n')
  
  cli::cli_alert_success(
    paste(bold("VIBER fit"), 'completed in',
          round(TIME, 2),
          'mins (status: {.value {ifelse(best$status == "CONVERGED", crayon::green("converged"), crayon::red(best$status))}})'))
  
  cat('\n')
  
  print(best)

  best$CPU_time = TIME

  return(best)
}

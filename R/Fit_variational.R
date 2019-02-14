# Fit Variational Dirichelt Mixture Model with Binomial data, this is the function
# which runs the actual fit
vb_bmm_MV <-
  function(x_NV,
           x_DP,
           K,
           alpha_0,
           a_0,
           b_0,
           max_iter,
           epsilon_conv,
           save_trace,
           q_init
  )
  {
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # log sum exp trick for having numeric stability   #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

    # Use the
    log_sum_exp <- function(x) {
      # Computes log(sum(exp(x))
      offset <- max(x)
      s <- log(sum(exp(x - offset))) + offset
      i <- which(!is.finite(s))
      if (length(i) > 0) {
        s[i] <- offset
      }
      return(s)
    }

    Sn = as.matrix(x_NV)
    Tn = as.matrix(x_DP)
    TminusSn = Tn - Sn

    # =-=-=-=-=-=-=-=-=-=-= #
    # Auxiliary variables   #
    # =-=-=-=-=-=-=-=-=-=-= #

    N = nrow(Sn)             # Number of observations
    W = ncol(Sn)             # Number of dimensions
    L = -Inf                 # ELBO values

    cluster_names = paste0("C", 1:K)
    sample_names = paste0('__x_', 1:N)
    dimensions_names = colnames(x_NV)

    rownames(Sn) = rownames(Tn) = rownames(TminusSn) = sample_names

    # Latent variables
    r_nk = log_r_nk = log_lambda_nk = matrix(0, nrow = N, ncol = K)
    rownames(r_nk) = rownames(log_r_nk) = rownames(log_lambda_nk) = sample_names
    colnames(r_nk) = colnames(log_r_nk) = colnames(log_lambda_nk) = cluster_names

    log_pi = rep(0, K)
    names(log_pi) = cluster_names

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Bayesian priors from the hyperparameters   #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

    # Dirichlet [alpha_0 ... alpha_0]
    alpha_0 = rep(alpha_0, K)
    names(alpha_0) = cluster_names

    # Beta (a_0, b_0) with small randomness (~10e-3) to render different components
    a_0_scalar = a_0
    a_0 = rep(a_0, K * W) + runif(K * W) / 100
    b_0 = rep(b_0, K * W) + runif(K * W) / 100

    # .. we turn these into a KxW matrix
    a_0 = matrix(a_0, ncol = K)
    b_0 = matrix(b_0, ncol = K)

    rownames(a_0) = rownames(b_0) = dimensions_names
    colnames(a_0) = colnames(b_0) = cluster_names

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Posterior approximations, different ways to initialize the q   #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Init: variational distribution q (via `q_init`)
    # - random : q_init = to the prior's value
    # - kmeans : via kmeans clustering
    alpha = alpha_0

    if(q_init == 'prior')
    {
      a = a_0
      b = b_0
    }

    if(q_init == 'kmeans') # Special case: kmeans initialization
    {
      # get parameters from kmeans
      kmeans_params = initial_condition_Binomial_kmeans(x_NV, x_DP, K, a_0_scalar)

      a = kmeans_params$a
      b = kmeans_params$b
    }

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Normalization constants for the priors, required for the ELBO  #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # ln Cn -- log of Binomial data
    # ln C(alpha_0) -- log of the Dirichlet
    # ln Beta(a0, b0) -- log of K Beta
    log_Cn = lchoose(Tn, Sn)
    log_C_alpha_0 = lgamma(sum(alpha_0)) - sum(lgamma(alpha_0))
    log_beta_ab_0 = lbeta(a, b)

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Other extra variables that we report in the fit  #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Computation status to be updated in the end
    status = ''
    fit_type = 'Variational'

    # The trace of the fit
    fit_trace = NULL

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Minimization of the KL divergence between the posterior and its approximution  #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    i = 1
    repeat {

      # =-=-=-=-=-=-=-=-=- #
      # Variational E-Step #
      # =-=-=-=-=-=-=-=-=- #

      # Digamma values for the current Beta distributions
      # that we need in the E-Step
      dga = digamma(a)      # Psi(a)
      dgb = digamma(b)      # Psi(b)
      dgab = digamma(a + b) # Psi(a+b)

      # Update expectations
      log_pi = digamma(alpha) - digamma(sum(alpha)) # ln \pi -- Dirichlet entropy
      log_theta = dga - dgab                        # ln \theta
      log_1min_theta = dgb - dgab                   # ln 1-\theta

      # Log of lambda terms
      for (k in 1:K)
        log_lambda_nk[, k] <-
        rowSums(log_pi[k] + sapply(1:W, function(w)
          Sn[, w] * log_theta[w, k] + TminusSn[, w] * log_1min_theta[w, k]))

      # Calculate probabilities using the logSumExp trick for numerical stability
      Z        <- apply(log_lambda_nk, 1, log_sum_exp)
      log_r_nk <- log_lambda_nk - Z              # log of r_nk
      r_nk     <- apply(log_r_nk, 2, exp)        # r_nk

      # Save this entry in the trace
      if(save_trace) {
        fit_trace = bind_rows(fit_trace,
                              tibble(
                                cluster.Binomial = latent_vars_hard_assignments(lv = list(`z_nk` = r_nk, `pi` = alpha_0)),
                                step = i))
      }

      # =-=-=-=-=-=-=-=-=- #
      # Variational M-Step #
      # =-=-=-=-=-=-=-=-=- #

      # Auxiliary quantities - s_star and t_star as in the notes
      N_k <- colSums(r_nk) + 1e-10
      s_star = sapply(1:K,
                      function(i) {
                        sapply(1:W, function(w)
                          return(Sn[, w] %*% r_nk[, i]))
                      })

      t_star =  sapply(1:K,
                       function(i) {
                         sapply(1:W, function(w)
                           return(Tn[, w] %*% r_nk[, i]))
                       })

      t_star = matrix(t_star, ncol = ncol(a))
      s_star = matrix(s_star, ncol = ncol(a))

      colnames(t_star) = colnames(s_star) = colnames(a)
      rownames(t_star) = rownames(s_star) = rownames(a)

      # Update equations
      alpha = alpha_0 + N_k       # Dirichlet
      a = a_0 + s_star            # Beta
      b = b_0 + t_star - s_star   # Beta

      # Compute posterior's expected values of model's parameter
      pi_k <-
        (alpha_0 + N_k) / (K * alpha_0 + N)  # mixing proportions
      theta_k = a / (a + b)                  # Binomial parameter (mean of the posterior)

      # =-=-=-=-=-=-=-=-=- #
      # Variational ELBO   #
      # =-=-=-=-=-=-=-=-=- #
      #
      # Updated values for the new posterior estimates
      dga = digamma(a)        # Psi(a)
      dgb = digamma(b)        # Psi(b)
      dgab = digamma(a + b)   # Psi(a+b)
      log_beta_ab = lbeta(a, b)  # Beta(a,b)

      # These are the quantities for the ELBO equation
      hat_digamma_ab = dga - dgb
      hat_digamma_ba = dgb - dgab
      log_rho = digamma(alpha) - digamma(sum(alpha))

      eta_k = lapply(1:W,
                     function(w)
                       log_beta_ab[w, ] - log_beta_ab_0[w, ] + (a_0[w, ] - a[w, ]) * dga[w, ] + (b_0[w, ] - b[w, ]) * dgb[w, ] + (a[w, ] - a_0[w, ] + b[w, ] - b_0[w, ]) * dgab[w, ])
      eta_k = Reduce(rbind, eta_k)
      eta_k = matrix(eta_k, ncol = K)

      rownames(eta_k) = rownames(a)
      colnames(eta_k) = names(pi_k)

      # Dirichlet normalization constant -- for numerical stability in log format
      log_C_alpha = lgamma(sum(alpha)) - sum(lgamma(alpha))

      # The data likelihood
      my_dotprodfun = function(k) {
        dmdotprod = sapply(1:W, function(w)
          Sn[, w] * hat_digamma_ab[w, k] + Tn[, w] * hat_digamma_ba[w, k] - log_Cn[, w] + log_rho[k] - log_r_nk[, k])
        dmdotprod = rowSums(dmdotprod)

        return(r_nk[, k] %*% dmdotprod)
      }

      # The actual ELBO --  rounded to the 9th digit
      ELBO = log_C_alpha_0 - log_C_alpha + sum(sapply(1:K, my_dotprodfun)) + (alpha_0 - alpha) %*% (log_rho) + sum(colSums(eta_k))
      ELBO = round(ELBO, 9)

      if(is.na(ELBO)) stop("ELBO is NA at step #", i)

      L = c(L, ELBO)

      # =-=-=-=-=-=-=-=-=- #
      # Checks convergency #
      # =-=-=-=-=-=-=-=-=- #
      if(i > 1)
      {
        ELBO_diff = abs(L[i] - L[i - 1])

        # Decreasing ELBO, it should not happen -- notified iff diff > 1
        if (L[i] < L[i - 1] & ELBO_diff > 1)
          warning("ELBO decreased by ", ELBO_diff ,"at step #", i)

        # Convergence by epsilon_conv
        if(ELBO_diff < epsilon_conv)
        {
          status = 'CONVERGED'
          break;
        }

        # Cnvergence by max_iter
        if (!is.na(max_iter) & i == max_iter)
        {
          status = 'INTERRUPTED'
          break;
        }
      }

      i = i + 1
    }

    flush.console()

    # =-=-=-=-=-=-=-=-=-=-=-=-=- #
    # Hard clustering assignment #
    # =-=-=-=-=-=-=-=-=-=-=-=-=- #

    labels = tibble(
      cluster.Binomial = latent_vars_hard_assignments(lv = list(`z_nk` = r_nk, `pi` = pi_k)))

    # add the label to each point
    x_DP = bind_cols(
      labels, x_DP %>% as_tibble())

    x_NV = bind_cols(
      labels, x_NV %>% as_tibble())


    if(save_trace) {
      fit_trace = bind_rows(fit_trace,
                            tibble(
                              cluster.Binomial = labels,
                              step = i))
    }


    obj <-
      structure(
        list(
          x = x_NV,
          y = x_DP,
          K = K,
          N = N,
          pi_k = pi_k,
          theta_k = theta_k,
          alpha = alpha,
          r_nk = r_nk,
          labels = labels,
          a = a,
          b = b,
          a_0 = a_0,
          b_0 = b_0,
          alpha_0 = alpha_0,
          epsilon_conv = epsilon_conv,
          status = status,
          fit_type = fit_type,
          ELBO = L,
          fit_trace = fit_trace
        ),
        class = "vb_bmm",
        call = match.call(),
        modelname = "mvbmm - Variational fit for Binomial mixtures"
      )

    return(obj)
  }

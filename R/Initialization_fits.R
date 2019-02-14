initial_condition_kmeans = function(x_NV, x_DP, K, a_0)
{
  # Data
  XVAF = (x_NV)/(x_DP) %>% as_tibble()
  colnames(XVAF) = colnames(x_NV)
  XVAF[is.na(XVAF)] = 0

  W = ncol(XVAF)

  # Kmeans with K clusters
  km_cl = kmeans(XVAF, centers = K, nstart = 25)
  XVAF$cluster.kmeans = km_cl$cluster

  # Beta means as Gaussian means
  beta_mu = km_cl$centers[, 1:W]

  # Beta variances from posterior assignments
  beta_var = XVAF %>%
    reshape2::melt(id = 'cluster.kmeans') %>% as_tibble() %>%
    arrange(cluster.kmeans) %>%
    group_by(cluster.kmeans, variable) %>% summarise(value = var(value))  %>%
    spread(variable, value) %>%
    ungroup() %>%
    select(-cluster.kmeans)

  a_0 = rep(a_0, K * W) + runif(K * W) / 100
  a_0 = matrix(a_0, ncol = K)

  a = b = a_0

  beta_mu = t(beta_mu)
  beta_var = t(beta_var)

  for(i in 1:nrow(beta_mu))
  {
    for(j in 1:ncol(beta_mu))
    {
      a[i,j] = mobster:::.estBetaParams(beta_mu[i,j], beta_var[i,j])$a
      b[i,j] = mobster:::.estBetaParams(beta_mu[i,j], beta_var[i,j])$b
    }
  }

  extr = function(x){
    if(is.na(x)) return(runif(1) / 100)
    x
  }
  a = apply(a, c(1,2), extr)
  b = apply(b, c(1,2), extr)

  return(list(a=a, b=b, X = XVAF %>% as_tibble()))
}


# initial_condition_prv_clusters = function(x_NV, x_DP)
# {
#   # reconstruct thw raw VAF profile for each sample
#   vaf = x_NV/x_DP
#   colnames(XVAF) = colnames(x_NV)
#   vaf[is.na(vaf)] = 0
#
#   ret_vaf = vaf %>% as_tibble()
#   ret_vaf$private.cluster = NA
#
#   W = ncol(vaf)
#
#   # Private clusters have only one positive entry in the vaf
#   prv = apply(vaf, 1, function(w) sum(w>0)) == 1
#   ret_vaf$private.cluster = apply(ret_vaf, 1,
#                                   function(w)
#                                   {
#                                     pos_entries = as.numeric(w[1:W]) > 0
#
#                                     if (sum(pos_entries) > 1)
#                                       return(NA)
#                                     else
#                                       return(colnames(ret_vaf)[which(pos_entries)])
#                                   })
#
#   prv = vaf[prv, ]
#
#
#   # Make a list of params
#   prv = lapply(1:ncol(prv), function(w) unlist(prv[prv[, w] > 0, w]))
#   names(prv) = fitgp$samples
#
#   # Where we have the 'a' and the "b", for each mean and variance value
#   prv_beta_params = rbind(
#     sapply(prv, mean),
#     sapply(prv, var))
#
#   prv_size = sapply(prv, length)
#
#   # Assemble everything into a dataframe
#   prv_beta_params = apply(prv_beta_params, 2, function(w) data.frame(mobster:::.estBetaParams(w[1], w[2])))
#   prv_beta_params = Reduce(rbind, prv_beta_params)
#   rownames(prv_beta_params) = names(prv)
#
#   prv_beta_params = cbind(prv_beta_params, n = prv_size)
#
#   # Now, we create the matrix of a and b parameters for the Beta where each dimension has
#   # pdf concentrated on 0 (b >> a), and the private parameters per cluster
#
#   a_Beta = b_Beta = matrix(NA, ncol = nrow(prv_beta_params), nrow = W)
#   rownames(a_Beta) = rownames(b_Beta) = rownames(prv_beta_params)
#
#   a_Beta = apply(a_Beta, c(1,2), function(w) 1+runif(1))
#   b_Beta = apply(a_Beta, c(1,2), function(w) 30+runif(1))
#
#
#   for(i in 1:nrow(prv_beta_params)) {
#     # Get a and b for a single dimension
#     a_Beta[i, i] = prv_beta_params[i, 'a']
#     b_Beta[i, i] = prv_beta_params[i, 'b']
#   }
#
#   return(list(a=a_Beta,b=b_Beta, X=ret_vaf))
# }

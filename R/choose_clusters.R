#' Filter clusters heuristic
#'
#' @description This removes the clusters from a fit object. There are 2
#' conditions that can be used to filter out clusters: 1) the cluster size
#' (mixing proportion), which one can require to be above a certain cutoff,
#' and 2) the Binomial peak, whichi one can ask to be above a certain value
#' in at least a certain number of dimensions. Output clusters will
#' be renamed by size (C1 will be larger etc.), and the latent variables and
#' hard clustering assignments will be updated accordingly.
#'
#' @param x An object of class 'vb_bmm'.
#' @param binomial_cutoff Minimum Binomial success probability.
#' @param dimensions_cutoff Minimum number of dimensions where we want to detect a Binomial component.
#' @param pi_cutoff Minimum size of the mixture component.
#' @param re_assign If \code{TRUE}, point assigned to a cluster that is filtered our, are re-assigned
#' from the density function (without updating the parameters). Otherwise, clustering assignments for
#' those points are returned as \code{NA}.
#'
#' @return An object of class 'vb_bmm'.
#' @export
#'
#' @import dplyr
#'
#' @examples
#' data(fit_mvbmm_example)
#' choose_clusters(fit_mvbmm_example)
choose_clusters = function(x, binomial_cutoff = 0.05, dimensions_cutoff = 1, pi_cutoff = 0.02, re_assign = FALSE)
{
  # pio::pioTit(paste0("Selecting Binomial clusters (F1,2-heuristic)."))
  # pio::pioStr("\nF1.       Cluster size", pi_cutoff)
  # pio::pioStr("\nF2. Num. of dimensions", dimensions_cutoff)
  # pio::pioStr("\nF2. Min. Binomial peak", binomial_cutoff, suffix = '\n')

  original_K = length(x$pi_k)

  if(original_K == 1) return(x)

  # CLuster size
  tab_clusters = data.frame(cluster = names(x$pi_k), pi = x$pi_k, stringsAsFactors = F) %>% as_tibble()

  # Number of dimensions above binomial_cutoff
  nom_C = round(x$theta_k, 4)
  nom_C = data.frame(nom_C)

  nom_C[nom_C <= binomial_cutoff] = 0
  nom_C[nom_C > 0] = 1

  nom_C = apply(nom_C, 2, sum)
  nom_C = nom_C[tab_clusters$cluster]

  tab_clusters$Above_cut = nom_C

  # Apply filters
  tab_clusters$F1 = tab_clusters$pi > pi_cutoff
  tab_clusters$F2 = tab_clusters$Above_cut > dimensions_cutoff

  tab_clusters = tab_clusters %>%
    mutate(accepted = F1 & F2) %>%
    arrange(desc(accepted))

  # Report easy cases
  if(sum(tab_clusters$accepted) == 0) stop("All clusters filtered out, this seems an error.")
  if(sum(tab_clusters$accepted) == x$K) return(x)

  # partitions of clusters
  detect.clones = tab_clusters %>% filter(accepted)
  rejected.clones = tab_clusters %>% filter(!accepted)

  idx_to_remove = which((x$labels %>% pull) %in% rejected.clones$cluster)

  K = nrow(detect.clones)
  K.rj = nrow(rejected.clones)

  # Mapping old labels to new ones
  tab_clusters$new.labels = paste0('C', 1:nrow(tab_clusters))

  # print(tab_clusters)
  mapping = tab_clusters$new.labels
  names(mapping) = tab_clusters$cluster

  # rename all entries with reference to the labels
  x$labels$cluster.Binomial = mapping[x$labels$cluster.Binomial]
  x$x$cluster.Binomial = mapping[x$x$cluster.Binomial]
  x$y$cluster.Binomial = mapping[x$y$cluster.Binomial]

  names(x$alpha) = names(x$pi_k) = names(x$alpha_0) = mapping[names(x$alpha_0)]

  colnames(x$a) = colnames(x$b) = colnames(x$a_0) = colnames(x$b_0) =
    colnames(x$r_nk) = colnames(x$theta_k) =
    mapping[colnames(x$theta_k)]

  # now drop useless clusters and re-normalize the variables
  detect.clones = tab_clusters %>% filter(accepted) %>% pull(new.labels)

  x$alpha = x$alpha[detect.clones]
  x$pi_k = x$pi_k[detect.clones]
  x$alpha_0 = x$alpha_0[detect.clones]
  x$alpha = x$alpha[detect.clones]

  x$a = x$a[, detect.clones, drop = FALSE]
  x$b = x$b[, detect.clones, drop = FALSE]
  x$a_0 = x$a_0[, detect.clones, drop = FALSE]
  x$b_0 = x$b_0[, detect.clones, drop = FALSE]
  x$r_nk = x$r_nk[, detect.clones, drop = FALSE]
  x$theta_k = x$theta_k[, detect.clones, drop = FALSE]

  # renormalize latent variables
  C = rowSums(x$r_nk)
  for (i in 1:nrow(x$r_nk))
    x$r_nk[i, ] = x$r_nk[i, ] / C[i]

  # Now force NA values for those points that used to belong to clusters that have been cancelled
  if(!re_assign) {
    x$r_nk[idx_to_remove, ] = NA
  }

  # renormalize mixing proportions to recompute clusters
  C = sum(x$pi_k)
  x$pi_k = x$pi_k / C

  # recompute clustering assignments..
  labels = data.frame(
    cluster.Binomial = VIBER:::latent_vars_hard_assignments(lv = list(`z_nk` = x$r_nk, `pi` = x$pi_k)),
    stringsAsFactors = F) %>% as_tibble()

  # Cluster counts determine the new pi
  pi_counts = labels[[1]] %>% table
  x$pi_k = pi_counts/sum(pi_counts)

  x$labels = labels
  x$x$cluster.Binomial = labels %>% pull(cluster.Binomial)
  x$y$cluster.Binomial = labels %>% pull(cluster.Binomial)

  # Update num of clusters
  x$K = K

  # Report to console
  new_k =  length(x$pi_k)

  if(new_k != original_K)
    cli::cli_alert_success(
      "Reduced to k = {.value {new_k}} (from {.value {original_K}}) selecting VIBER cluster(s) with \u03C0 > {.value {pi_cutoff}}, and Binomial p > {.value {binomial_cutoff}} in w > {.value {dimensions_cutoff}} dimension(s)."
    )

  if(!re_assign & sum(is.na(x$x$cluster.Binomial)) > 0)
    cli::cli_alert_success(
      "{.value {sum(is.na(x$x$cluster.Binomial))}} points are now not assigned (cluster = NA) because their cluster has been removed."
    )

  x
}



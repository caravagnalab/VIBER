latent_vars_hard_assignments = function(lv)
{
  # Cluster labels of each data point. Each data point is assigned to the cluster
  # with the highest posterior responsibility. Hard assignment.
  unlist(apply(lv$z_nk, 1,
               function(x) {
                 names(lv$pi)[which(x == max(x, na.rm = TRUE))[1]]
               }))

}

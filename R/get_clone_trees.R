#' Return clone trees from the fit.
#'
#' @description This function uses the output fit of VIBER
#' to create a call to \code{ctree} (\url{https://caravagn.github.io/ctree/}),
#' a package to create clone trees for cancer evolution models.
#'
#' Creation of a clone tree requires annotations that are not usually
#' necessary for just a plain  VIBER analyses. These annotations report the status of \code{driver}
#' and \code{gene} for each one of the input datapoints; the annotations should
#' have been passed when calling the \code{variational_fit} function, and stored
#' inside the \code{data} field of the VIBER object.
#'
#' The clonal cluster is estimated from the cluster with the highest parameter
#' values in most of the input dimensions (Binomial peaks).
#'
#' The output is the result of calling the constructor \code{ctree::cetrees}
#' on the input clustering results \code{x}.
#'
#' @param x A VIBER fit.
#' @param ... Extra parameters passed to the constructor \code{ctree::cetrees}, which
#' affect the sampling of the trees.
#'
#' @return The output of the constructor \code{ctree::cetrees}.
#'
#' @export
#'
#' @import ctree
#'
#' @examples
#'
#'

#' data(mvbmm_example)
#'
#' # We create annotation data assigning dummy names
#' # and picking 10 events to be drivers (randomly chosen)
#' data_annotations = data.frame(
#'  gene = paste0("G", 1:nrow(mvbmm_example$trials)),
#'  driver = FALSE
#'  )
#'
#' data_annotations$driver[sample(1:nrow(data_annotations), 10)] = TRUE
#'
#' # Compared to the main variational_fit, we use the same call but add data
#' f = variational_fit(mvbmm_example$successes, mvbmm_example$trials, data = data_annotations)
#' print(f)
#'
#' trees = get_clone_trees(f)
#'
#' ctree:::print.ctree(trees[[1]])
#' ctree::plot.ctree(trees[[1]])
get_clone_trees = function(x, ...)
{
  if(all(is.null(x$data)))
    stop("Your input object should have a data field; recreate the VIBER input.")

  if(!all(c('driver', 'gene') %in% colnames(x$data)))
    stop("Your data should have a logical 'driver' and 'gene' column to annotate driver events, cannot build a ctree otherwise.")

  stopifnot(inherits(x, 'vb_bmm'))

  # Patient ID
  patientID = ifelse(is.null(x$description), "VIBER dataset", x$description)
  patientID = gsub(pattern = ' ', replacement = '_', patientID)

  pi = x$pi_k[((x$N * x$pi_k) %>% round) > 0]
  theta = x$theta_k[ , names(pi), drop = T]

  # Get clusters table: cluster and fit
  cluster_table = data.frame(cluster = colnames(theta), stringsAsFactors = FALSE) %>% as_tibble()
  cluster_table = bind_cols(cluster_table, t(theta) %>% as_tibble)

  # Cluster size
  cluster_table$nMuts = table(x$labels)[cluster_table$cluster] %>% as.vector()

  # Clonality status - maximum fit is the clonal
  clonal_cluster = apply(theta, 1, which.max)
  clonal_cluster = colnames(theta)[clonal_cluster]
  clonal_cluster = which.max(table(clonal_cluster)) %>% names

  cli::cli_text("Estimated clonal cluster {.value {clonal_cluster}} from VIBER fit.")

  cluster_table$is.clonal = FALSE
  cluster_table$is.clonal[cluster_table$cluster %in% clonal_cluster] = TRUE

  # Detect presence/ absence of drivers
  x$data$cluster = paste(unlist(x$labels))

  drivers_collapse = x$data %>%
    dplyr::filter(driver) %>%
    pull(cluster) %>%
    unique

  cluster_table$is.driver = FALSE
  cluster_table$is.driver[which(cluster_table$cluster %in% drivers_collapse)] = TRUE

  cli::cli_text("Found {.value {sum(cluster_table$is.driver)}} driver event(s) in VIBER fits.")

  # Create drivers table
  cx = x$x %>% dplyr::select(-starts_with('cluster'))
  cy = x$y %>% dplyr::select(-starts_with('cluster'))

  vaf_table = cx/cy

  drivers_table = x$data %>%
    as_tibble() %>%
    dplyr::filter(driver) %>%
    dplyr::rename(variantID = gene, is.driver = driver) %>%
    dplyr::mutate(patientID = patientID)
  drivers_table = bind_cols(drivers_table, vaf_table[which(x$data$driver), , drop = F])

  drivers_table$is.clonal = FALSE
  drivers_table$is.clonal[which(
    drivers_table$cluster == cluster_table %>% dplyr::filter(is.clonal) %>% dplyr::pull(cluster)
  )] = TRUE

  drivers_table = drivers_table %>%
    dplyr::select(patientID, variantID, is.driver, is.clonal, cluster, colnames(cx), dplyr::everything())

  trees = ctree::ctrees(
    CCF_clusters =  cluster_table,
    drivers = drivers_table,
    samples = colnames(cx),
    patient = patientID,
    ...
  )

  return(trees)
}


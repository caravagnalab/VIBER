#' Rename VIBER clusters.
#'
#' @description From an object fit, clusters (all or some) can be renamed.
#' This changes the names inside the VIBER object, affecting plots and all
#' other outputs. This function can be used after clusters have been given
#' some interpretation, e.g., cluster "C1" is the "Clonal" cluster for instance.
#'
#'
#' @param x A VIBER fit object.
#' @param new_labels A named vector representing a new names map. For instance
#' \code{c(`C1`=`Clonal`, `C2` = `Subclonal`)} renames two clusters "C1" and "C2"
#' as "Clonal" and "Clonal" respectively.
#'
#' @return A VIBER fit object with renamed clusters.
#' @export
#'
#' @examples
#' data(mvbmm_example)
#' x = variational_fit(mvbmm_example$successes, mvbmm_example$trials)
#' print(x)
#'
#' x_renamed = rename_clusters(x, c(`C1`=`Clonal`, `C2` = `Subclonal`))
#' print(x_renamed)
rename_clusters = function(x, new_labels){

  stopifnot(inherits(x, "vb_bmm"))

  # what we rename, let's just make some nice reports to screen
  original_clusters = x$pi_k %>% names()

  cli::cli_h1("Renaming VIBER clusters {.field {original_clusters}} for the input object.")
  cat("\n")
  x %>% print()

  no_renamed = setdiff(original_clusters, new_labels %>% names())
  if(length(no_renamed) > 0) cli::cli_alert_warning("Cluster {.field {no_renamed}} are not going to be renamed.")

  unused_names = setdiff(new_labels %>% names(), original_clusters)
  if(length(unused_names) > 0) cli::cli_alert_warning("Names {.field {unused_names}} are not going to be used because does not appear in the input object.")

  actual_renaming = intersect(original_clusters, new_labels %>% names())
  str = sapply(actual_renaming, function(a) paste("\t", a, "->", new_labels[a])) %>%
    paste(collapse = '\n')

  cli::cli_h3("Renaming map")
  cat("\n")
  str %>% cat
  cat("\n")

  # Renaming process - we add what we don't rename to make renaming trivial
  names(no_renamed) = no_renamed
  new_labels = c(new_labels, no_renamed)

  x$x$cluster.Binomial = new_labels[x$x$cluster.Binomial]
  x$y$cluster.Binomial = new_labels[x$y$cluster.Binomial]
  x$labels$cluster.Binomial = new_labels[x$labels$cluster.Binomial]

  names(x$pi_k) = new_labels[names(x$pi_k)]
  names(x$alpha) = new_labels[names(x$alpha)]
  names(x$alpha_0) = new_labels[names(x$alpha_0)]

  colnames(x$theta_k) = new_labels[colnames(x$theta_k)]
  colnames(x$r_nk) = new_labels[colnames(x$r_nk)]
  colnames(x$a) = new_labels[colnames(x$a)]
  colnames(x$b) = new_labels[colnames(x$b)]
  colnames(x$a_0) = new_labels[colnames(x$a_0)]
  colnames(x$b_0) = new_labels[colnames(x$b_0)]

  cli::cli_h3("Renamed VIBER object")
  cat("\n")
  x %>% print()

  return(x)
}



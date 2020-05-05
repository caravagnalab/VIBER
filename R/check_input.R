# Error checking inputs
var_check_input = function(x,
                           y,
                           data,
                           K,
                           alpha_0,
                           a_0,
                           b_0,
                           max_iter,
                           epsilon_conv,
                           samples,
                           q_init)
{
  # x and y
  stopifnot(is.matrix(x) | is.data.frame(x))
  stopifnot(is.matrix(y) | is.data.frame(y))
  stopifnot(nrow(x) == nrow(y) & ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y) & ncol(x) == ncol(y))

  stopifnot(all(apply(
    x, FUN = is.numeric, MARGIN = c(1, 2)
  )))

  stopifnot(all(apply(
    y, FUN = is.numeric, MARGIN = c(1, 2)
  )))

  stopifnot(all(x <= y))

  stopifnot(all(colnames(x) == colnames(y)))
  
  # extra data
  if(!all(is.null(data))) 
    stopifnot(nrow(data) == nrow(x))
  
  # other params
  stopifnot(samples >= 1)

  stopifnot(K > 1 & K <= nrow(x))

  stopifnot(alpha_0 > 0)

  stopifnot(all(a_0 > 0) & all(b_0 > 0))

  stopifnot(max_iter > 0)
  stopifnot(epsilon_conv > 0)
  stopifnot(!is.na(epsilon_conv))

  stopifnot(
    q_init %in%
      c('prior', 'kmeans', 'private')
  )
}

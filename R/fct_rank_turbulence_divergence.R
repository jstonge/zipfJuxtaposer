#' calculate disjoint  set
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calc_disjoint <- function(N1, N2) { 
  stopifnot(is.integer(N1))
  stopifnot(is.integer(N2))
  
  return( 1 / (N2 + N1/2) )
  }

#' rank_turbulence_divergence 
#'
#' @description normalization is computational such that disjoint version gives 1
#' 
#'
#' @return Returns per type divergence values for rank turbulence divergence. alpha >= 0 and may be specified as Inf
#'
#' @noRd
rank_turbulence_divergence <- function(mixedelements, alpha) {
  inv_r1 = mixedelements[[1]]$ranks ^ -1
  inv_r2 = mixedelements[[2]]$ranks ^ -1
  
  testthat::expect_gte(alpha, 0)
  
  if (alpha == Inf) {
    divergence_elements = purrr::map2_dbl(inv_r1, inv_r2, ~max(c(.x, .y)))
    divergence_elements[inv_r1 == inv_r2] = 0
  } else if (alpha == 0) {
    x_max <- purrr::map2_dbl(1 / inv_r1, 1 / inv_r2, ~max(.x, .y))
    x_min <- purrr::map2_dbl(1 / inv_r1, 1 / inv_r2, ~min(.x, .y))
    divergence_elements <- log(x_max / x_min ) 
  } else {
    divergence_elements = (alpha+1)/alpha*(abs(inv_r1^alpha - inv_r2^alpha))^(1. / (alpha+1))
  }
  
  c1 <- mixedelements[[1]]$counts
  c2 <- mixedelements[[2]]$counts
  
  # treat as disjoint
  indices1 = which(c1 > 0)
  indices2 = which(c2 > 0)
  N1 = length(indices1)
  N2 = length(indices2)
  
  # ranks for system's elements
  inv_r1_disjoint <- calc_disjoint(N1, N2) 
  inv_r2_disjoint <- calc_disjoint(N2, N1)
  
  if (alpha == Inf) {
    normalization <- sum(inv_r1[indices1]) + sum(inv_r2[indices2])
  }
  else if (alpha == 0) {
    normalization <- sum(abs(log(inv_r1[indices1] / inv_r2_disjoint))) + 
      sum(abs(log(inv_r2[indices2] / inv_r1_disjoint)))
  }
  else {
    normalization <- (alpha+1)/alpha * sum((abs(inv_r1[indices1]^alpha - inv_r2_disjoint^alpha))^(1. / (alpha+1))) + 
    (alpha+1)/alpha * sum((abs(inv_r1_disjoint^alpha - inv_r2[indices2]^alpha))^(1. / (alpha+1)))
  }
  
  divergence_elements <-  divergence_elements / normalization
  
  return(list(divergence_elements=divergence_elements, normalization=normalization))
}

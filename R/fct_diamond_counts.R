#' Counts ranks 
#'
#' @description 
#' 
#' Extract the value for the diamond plot. 
#' Here we use rank_turbulence_divergence. 
#' We might offer more possibilities in the future.
#'
#' @return list
#'
#' @noRd
diamond_counts <- function(mixedelements, alpha) {
  types <- probs <- ranks <- NULL
  rank_turbulence <- rank_turbulence_divergence(mixedelements, alpha)
  deltas <- rank_turbulence$divergence_elements
  divergence_score <- sum(deltas)
  
  sorted_div <- matlab_sort(deltas,rev=TRUE)
  indices_deltas <- sorted_div$orig_idx

  deltas <- deltas[indices_deltas]
  
  # %% re-sort mixedelements
  mixedelements[[1]]$types  <- mixedelements[[1]][indices_deltas, types]
  mixedelements[[1]]$counts <- mixedelements[[1]][indices_deltas, counts]
  mixedelements[[1]]$ranks  <- mixedelements[[1]][indices_deltas, ranks]
  # 
  if ("probs" %in% names(mixedelements[[1]])) {
    mixedelements[[1]]$probs <- mixedelements[[1]][indices_deltas, probs]
  }

  # mixedelements[[2]]$types <-  mixedelements[[2]]$types[indices_deltas]
  mixedelements[[2]]$counts <- mixedelements[[2]][indices_deltas, counts]
  mixedelements[[2]]$ranks  <-  mixedelements[[2]][indices_deltas, ranks]
  if ("probs" %in% names(mixedelements[[2]])) {
    mixedelements[[2]]$probs <-  mixedelements[[2]][indices_deltas, probs]
  }

  deltas_loss <- deltas
  deltas_gain <- deltas

  deltas_loss[which(mixedelements[[1]]$ranks > mixedelements[[2]]$ranks)] = -1
  deltas_gain[which(mixedelements[[1]]$ranks < mixedelements[[2]]$ranks)] = -1

  c1 <- mixedelements[[1]]$counts
  c2 <- mixedelements[[2]]$counts

  minlog10 = 0 # %% always for ranks
  maxlog10 = rank_maxlog10(mixedelements)

  # %% for too small data sets
  if (maxlog10 < 1) {
    maxlog10 <- 1
  }
  
  CELL_LENGTH = 1/15
  Ncells = floor(maxlog10/CELL_LENGTH) + 1

  x1_indices = 1 + floor(log10(mixedelements[[1]]$ranks)/CELL_LENGTH)
  x2_indices = 1 + floor(log10(mixedelements[[2]]$ranks)/CELL_LENGTH)
  
  counts <- matrix(0, Ncells, Ncells)
  for (i in 1:length(mixedelements[[1]]$ranks)) {
    counts[x1_indices[i], x2_indices[i]] <- counts[x1_indices[i], x2_indices[i]] + 1
  }

  return(list(counts=data.table::data.table(counts), div_score=divergence_score, 
              deltas=deltas, max_delta_loss=max(deltas_loss)))
}

#' Binning names
#' 
#' @description 
#' 
#' Functions group by elem and bin, then find max and take 
#' a sample from possible the furthest points from axis. 
#' Importantly, our binning process is based on which 
#' coordinate on the axis (ax) 
#' 
#' @return data.table
#'
#' @noRd
#'
#' @importFrom data.table := .I
bin_names <- function(DT, sys, nb_bins=20) {
  elem <- dist <- bins <- ax <- NULL
  sel_sys <- DT[which(elem == sys)]
  sel_sys[,("bins") := cut(ax, breaks=nb_bins)]
  sel_sys[sel_sys[, .I[which.max(dist)], by=list(bins, elem)]$V1]
}

#' Combine two systems 
#'
#' @description Combine two elem objects that have types and counts as colnames.
#'
#' @return data.table
#'
#' @noRd
#' 
#' @importFrom data.table data.table
combine_distributions <- function(elem1, elem2) {
  probs <- counts <- totalunique <- NULL
  union_types <- union(elem1$types, elem2$types)
  N = length(union_types)
  
  mixedelem <- list(data.table(types=union_types), data.table(types=union_types))
  
  enum_list <- list(elem1, elem2)
  for (i in seq_along(enum_list)) {
    # Where do types in system 1 ends up in system 2? e.g. James went 1 -> 18.
    indices <- match(mixedelem[[i]]$types, enum_list[[i]]$types, nomatch = 0) # like ismember()
    # Indices of types in mixed elem found exclusively in one of the 2 sets, e.g.
    # James is found and is now index=1 
    newindices <- which(mixedelem[[i]]$types %in% enum_list[[i]]$types) 
    
    # Tricky part": 
    mixedelem[[i]][, ("counts") := as.vector(matrix(0,nrow=N))]
    mixedelem[[i]][newindices, counts := enum_list[[i]][indices[newindices], counts]]

    mixedelem[[i]][, ("ranks") := rank(-mixedelem[[i]]$counts)]
    
    if ("probs" %in% names(elem1)) {
      mixedelem[[i]][, ("probs") := as.vector(matrix(0,nrow=N))]
      mixedelem[[i]][newindices, probs := enum_list[[i]][indices[newindices], probs]]
    }
    
    if ("totalunique" %in% names(elem1)) {
      mixedelem[[i]][, totalunique := unique(enum_list[[i]]$totalunique)]
    }
    
  }
  return(mixedelem)
}

#' Euclidean distance d(p,q)
#'
#' @description Calculate euclidean distance
#' 
#'
#' @return double
#'
#' @examples
#' 
#' p_df <- data.frame(x1=1:5, y1=1:5)
#' q_df <- data.frame(x2=5, y2=2)
#' lapply(1:nrow(p_df), ~eucl_dist(p_df[.x,], q_df))
#'
#' @noRd
eucl_dist <- function(p,q){
  sqrt( (q$x2 - p$x1)^2 + (q$y2 - p$y1)^2 )
}

#' Get diagonal 
#'
#' @description Get diagonal of diamond plot matrix from mixedelements
#' 
#'
#' @return data.table
#'
#' @noRd
get_diag <- function(mixedelements) {
  maxlog10 = rank_maxlog10(mixedelements)
  CELL_LENGTH = 1/15
  Ncells = floor(maxlog10/CELL_LENGTH) + 1
  return( data.table::data.table(x2=1:Ncells, y2=1:Ncells) )
  
}

#' Get labs for diamond plot
#'
#' @description  Get labels based on counts rounded on the order of 10s
#' 
#'
#' @return float
#'
#' @noRd
get_labs <- function(meta_diamond) {
  max_count <- max(meta_diamond$max_counts)
  log10_lab = c(10^0)
  i = 1
  while (log10_lab[length(log10_lab)] < max_count) { 
    log10_lab[[i+1]] <- 10^i
    i = i + 1
  }
  return(log10_lab)
}

#' labeled input
#'
#' @description Used for shiny
#'
#' @return div
#'
#' @noRd
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}

#' Log counts tbl
#'
#' @description 
#' 
#'
#' @return double
#'
#' @noRd
logged_rank <- function(counts_long) {
  maxcountslog10 <- ceiling(log10(max(counts_long$value)))
  counts_long$value <- ifelse(test = counts_long$value > 0, 
                              yes = 0.0 + 1 * log10(counts_long$value/maxcountslog10),
                              no = NA)
  return(counts_long[!is.na(counts_long$value),])
}

#' Sort matlab style
#'
#' @description 
#' 
#' Inspired by matlab, this functions keep track of the original indices of an array after sorting.
#' Returns both the sorted vector `v` and the original indices.
#'
#' @return list with new value and original index
#'
#' @noRd
#'
#'@examples 
#'
#' A <- c(5, 4, 1, 2, 3)
#' ([1, 2, 3, 4, 5], [3, 4, 5, 2, 1])
matlab_sort <- function(A, rev=FALSE) {
  DT <- data.table(data.frame(A), keep.rownames = TRUE)
  if (rev == TRUE) {
    sorted_DT <- DT[order(-A)]
  } else {
    sorted_DT <- DT[order(A)]
  }
  return(list("value" = sorted_DT$A, "orig_idx" = as.integer(sorted_DT$rn)))
}


#' Get perpendicular coord on diagonal from arbitrary coord
#'
#' @description 
#' 
#' Takes a coordinate p, and find the coordinate q on the diagonal
#' that is perpendicular. Function expects coordinate p to be
#' a tibble or named list with columns x1 y1, and similarly for 
#' p but with columns x2 y2.
#'
#' @return A (round down) integer on the diagonal
#'
#' @noRd
get_perp_coord <- function(p,q) {
  y2 = x2 = q$x2[nrow(q)]
  y1 = x1 = q$x2[1]
  upper_k <- ((y2 - y1) * (p$x1 - x1) - (x2 - x1) * (p$y1 - y1))
  k <-  upper_k / ((y2 - y1)^2 + (x2 - x1)^2)
  return( as.integer( floor(p$x1 - k * (y2-y1)) ) )
}

#' Prepare counts from mixedelem in a long format
#' 
#' @description 
#' 
#' Prep counts data in the form of a tibble in the long format, and transposed
#'
#' @return list
#'
#' @noRd
tidy_count_mixedelem <- function(mixedelements, alpha = Inf) {
  y1 <- NULL
  dc <- diamond_counts(mixedelements, alpha)
  counts_t <- data.table::data.table(t(dc$counts))
  counts_t[, ("x1") := 1:nrow(counts_t)]
  counts_long <- data.table::melt(counts_t, id.vars = "x1", variable.name = "y1")
  counts_long[, y1 := as.integer(gsub("V", "", y1))]
  return(list(counts_long=counts_long, divergence_score=dc$div_score, 
              deltas=dc$deltas, max_delta_loss=dc$max_delta_loss))
}

#' Max log10 of rank in mixedelements
#'
#' @description 
#' 
#' Get maximum of log10 ranks from both systems, then round up
#'
#' @return integer
#'
#' @noRd
rank_maxlog10 <- function(mixedelements) {
  c(max(mixedelements[[1]]$ranks), max(mixedelements[[2]]$ranks)) |>
    log10() |> max() |> ceiling()
}

#' Round counts based on nearest order of 10
#'
#' @description We use a cutoff of log10(5) to round up or down.
#'
#' @return list of floats
#'
#' @noRd
round_log10 <- function(x) {
  order_magnitude <- nchar(substring(x, first=2))
  log_x <- log10(x)
  ifelse(log_x >= glue::glue("{order_magnitude}.69897"), 10^ceiling(log_x), 10^floor(log_x) )
}



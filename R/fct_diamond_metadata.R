#' Getting the metadata for the diamond plot 
#'
#' @description Extract metadata for the diamond plot from mixedlement
#'
#' @return data.table
#'
#' @noRd
#' 
#' @importFrom data.table data.table fifelse
#' @importFrom foreach %dopar% foreach
metadata_diamond <- function(mixedelements) {
  # Setup
  options(cores=parallel::detectCores()-1)
  axis_coords <- get_diag(mixedelements)
  CELL_LENGTH <-  1/15
  
  x1_indices = 1 + floor(log10(mixedelements[[1]]$ranks)/CELL_LENGTH)
  y1_indices = 1 + floor(log10(mixedelements[[2]]$ranks)/CELL_LENGTH)
  
  # 1.
  all_tuples <- data.table(y1=x1_indices, x1=y1_indices)
  #     taking the transposed  ^^^^^^^        ^^^^^^
  #     by inversing x and y cols.
  
  # 2.
  # Which system c(x1,y1) belongs to -> is x2 (on diag) at same y gt x1?
  # If so, then c(x1,y1) belongs to the right system, else left.
  # Below works because all_tuples$y1 == axis_coords$x2[y1]
  coords_sys <- fifelse(all_tuples$y1 - all_tuples$x1 >= 0, "right", "left")
  
  # 3.
  all_types <- mixedelements[[2]]$types
  
  # 4.
  unique_tuples <- unique(all_tuples)
  doParallel::registerDoParallel()
  i <- NULL # make it a visible binding
  coord_perp <- foreach(i=1:nrow(unique_tuples), .combine = rbind) %dopar% {
    get_perp_coord(unique_tuples[i,], axis_coords)
  }
  
  doParallel::registerDoParallel()
  dist <- foreach(i=1:nrow(unique_tuples), .combine=rbind) %dopar% {
    eucl_dist(unique_tuples[i,], axis_coords[coord_perp[i],])
  }
  
  all_dists_from_axis <- merge(x = all_tuples, 
                               y = data.table(unique_tuples, dist=dist[,1], ax=coord_perp[,1]), 
                               by = c("y1", "x1"), sort = FALSE)

  # 5. max (rounded) counts
  max_counts <- purrr::map2_dbl(mixedelements[[1]]$counts, mixedelements[[2]]$counts, max)
  rank_counts_map <- data.table(r1 = x1_indices, c1 = mixedelements[[1]]$counts,
                                r2 = y1_indices, c2 = mixedelements[[2]]$counts,
                                max_counts = round_log10(max_counts))
  
  testthat::expect_equal(length(unique(coords_sys)), 2)
  
  return(data.table(elem=coords_sys, types=all_types, all_dists_from_axis, rank_counts_map))
  
}


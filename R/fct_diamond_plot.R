#' diamond_plot
#'
#' @description Diamond plot, as found in https://arxiv.org/abs/2002.09770
#'
#' @return ggplot
#'
#' @noRd
#'
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_polygon geom_text geom_tile
#' @importFrom grid viewport unit
#' @importFrom glue glue
#' @importFrom rlang .data
diamond_plot <- function(mixedelements, alpha, system_names) {
  tcm <- tidy_count_mixedelem(mixedelements, alpha)
  tidy_counts <- tcm$counts_long
  meta_diamond <- metadata_diamond(mixedelements)
  meta_diamond$deltas <- tcm$deltas
  
  # plot settings
  dim_mat <- max(tidy_counts$y1)
  triangle1 <- data.frame(x = c(0, 0, dim_mat), y = c(0, dim_mat, dim_mat))
  triangle2 <- data.frame(x = c(0, dim_mat, dim_mat), y = c(0, 0, dim_mat))
  rotation <- 45 * 5

  # logged rank
  tidy_counts_log <- logged_rank(tidy_counts)

  # Binning names
  NB_BINS <- floor(dim_mat / 2.5)
  sel_names_r <- bin_names(meta_diamond, "right", nb_bins = NB_BINS)
  sel_names_l <- bin_names(meta_diamond, "left", nb_bins = NB_BINS)
  sel_names <- rbind(sel_names_l, sel_names_r)
  DELTAMIN_TEXT_COLOR <- 0.35
  sel_names$deltas <- sel_names$deltas / tcm$max_delta_loss
  sel_names$deltas <- (1 - sel_names$deltas) * (1 - DELTAMIN_TEXT_COLOR)

  # annotation names left and right
  anno_names_right <- geom_text(
    data = sel_names[sel_names$elem == "right", ],
    aes(.data$x1, .data$y1, label = .data$types, alpha = .data$deltas),
    nudge_x = -2.8, nudge_y = 3, angle = (-1 * rotation),
    show.legend = FALSE
  )

  anno_names_left <- geom_text(
    data = sel_names[sel_names$elem == "left", ],
    aes(x = .data$x1, y = .data$y1, label = .data$types, alpha = .data$deltas),
    nudge_x = 2.8, nudge_y = -3, angle = (-1 * rotation),
    show.legend = FALSE
  )


  # Actual plotting ---------------------------------------------------------


  base_p <- ggplot(tidy_counts, aes(.data$x1, .data$y1)) +
    geom_tile(data = meta_diamond, aes(.data$x1, .data$y1, color = .data$c1), alpha = 0., size = 0.) +
    geom_polygon(data = triangle1, aes(.data$x, .data$y), alpha = 0.2, fill = "#FECC5D") +
    geom_polygon(data = triangle2, aes(.data$x, .data$y), alpha = 0.2, fill = "#9E75B7") +
    geom_tile(data = tidy_counts_log, aes(fill = .data$value), color = "black", show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 76), breaks = scales::breaks_width(15)) +
    ggplot2::scale_y_continuous(limits = c(0, 76), breaks = scales::breaks_width(15)) +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1)

  # update labs { ranks -> counts }
  log10_labs <- get_labs(meta_diamond)
  log10_labs_padded <- log10_labs[c(NA, 1:length(log10_labs), NA)]

  current_ax <- ggplot2::ggplot_build(base_p)$layout$panel_params[[1]]$x.sec$breaks
  if (length(current_ax) != length(log10_labs_padded)) {
    log10_labs <- c(log10_labs, max(log10_labs) * 10)
    log10_labs_padded <- log10_labs[c(NA, 1:length(log10_labs), NA)]
  }
  new_labs <- data.frame(old_labs = current_ax, new_labs = log10_labs_padded)

  mylabels <- function(x) {
    sprintf("%.0f", new_labs[new_labs$old_labs %in% x, ]$new_labs)
  }

  decorated_p <- base_p +
    anno_names_right +
    anno_names_left +
    # Add colorbar by hiding a new tile layer
    # geom_tile(data=meta_diamond, aes(.data$x1, .data$y1, color = .data$c1), alpha = 0., size = 0.) +
    ggplot2::scale_color_viridis_c(option = "magma", direction = -1) +
    ggplot2::guides(color = ggplot2::guide_colourbar(
      barwidth = 0.5, barheight = 8,
      reverse = TRUE, nbin = 10, raster = FALSE,
      title = "Counts per cell", title.position = "left",
      title.theme = element_text(angle = 89.), title.hjust = 0.5,
      frame.colour = "black", label.hjust = 0.3, ticks = FALSE
    )) +
    # Adjust scale based on mylabels function, which map old labs to log10 labs
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(15), position = "top", labels = mylabels) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(15), position = "right", labels = mylabels) +
    # themes
    ggplot2::theme_light(base_rect_size = 0) +
    labs(
      x = glue("<<<  less abundant                 more abundant  >>> \n\n rank r \n\n for \n\n {system_names[2]}"),
      y = glue("<<<  less abundant                  more abundant  >>> \n\n rank r \n\n for \n\n {system_names[1]}"),
      caption = latex2exp::TeX(glue("$D_{{\\infty}}^R(\\Omega_1||\\Omega_2)$ = {round(tcm$divergence_score, 3)}"))
    ) +
    theme(
      text = element_text(angle = (-1 * rotation)),
      panel.grid.minor = ggplot2::element_line(colour = "grey", size = 0.3),
      axis.title.x = element_text(angle = 180, hjust = 0.5, size = 12, lineheight = 0.4),
      axis.title.y.right = element_text(angle = 90, hjust = 0.5, size = 12, lineheight = 0.4),
      aspect.ratio = 1,
      legend.position = c(-0.1, 0.55),
      plot.caption = element_text(hjust = 1, vjust = 0.5)
    )

  # decorated_p
  print(decorated_p, vp = viewport(
    angle = rotation,
    width = unit(0.75, "npc"),
    height = unit(0.75, "npc")
  ))
}

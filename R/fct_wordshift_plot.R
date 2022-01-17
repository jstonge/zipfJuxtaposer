#' wordshift_plot
#'
#' @description Plot wordshit using shifterator
#'
#' @return ggplot
#'
#' @noRd
shifterator_plot <- function(mixedelements, alpha, type_shift, system_names) {
  type2freq1 <- mixedelements[[1]][, c("types", "counts")]
  type2freq2 <- mixedelements[[2]][, c("types", "counts")]

  if (alpha == Inf) {
    # diamond plot accepts Inf but not wordshift, apparently.
    # We change the value to 5 if this happens.
    alpha <- 5
  }

  word_shift <- switch(type_shift,
    "Proportion" = shifterator::proportion_shift(type2freq1, type2freq2),
    "Shannon" = shifterator::entropy_shift(type2freq1, type2freq2, base = 2, alpha = 1),
    "Tsallis" = shifterator::entropy_shift(type2freq1, type2freq2, base = 2, alpha = alpha),
    "KLD" = shifterator::kldivergence_shift(type2freq1, type2freq2, base = 2),
    "JSD" = shifterator::jsdivergence_shift(type2freq1, type2freq2, weight_1 = 0.5, weight_2 = 0.5, base = 2, alpha = alpha)
  )

  # word_shift <- shifterator::jsdivergence_shift(type2freq1, type2freq2, weight_1=0.5, weight_2=0.5, base=2, alpha=alpha)
  # word_shift <- shifterator::entropy_shift(type2freq1, type2freq2, base=2, alpha=1)
  # system_names <- c("a","b")

  p <- shifterator::get_shift_graphs(word_shift, system_names, detailed = FALSE)

  # Deconstruct the plot to fix a bug
  max_score <- max(abs(c(p[[1]]$data$value, p[[2]]$data$type2shift_score))) * 1.25

  p[[1]] <- p[[1]] +
    ggplot2::coord_cartesian(ylim = c(-max_score, max_score)) +
    ggplot2::coord_flip(ylim = c(-max_score, max_score))

  # Weird stuff happening here to make sure ylim is fixed
  p[[2]] <- p[[2]] +
    ggplot2::theme_light() +
    ggplot2::coord_cartesian(ylim = c(-max_score, max_score)) +
    ggplot2::coord_flip(ylim = c(-max_score, max_score))

  p[[3]] <- p[[3]] +
    ggplot2::annotation_logticks(alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::xlab(NULL) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(expression(sum(delta * Phi[tau])))) +
    ggplot2::ggtitle(NULL) +
    ggplot2::theme_light()


  p[[3]] <- patchwork::inset_element(p[[3]],
    left = 0.01, bottom = 0.005,
    right = 0.25, top = 0.4
  )

  p$labels$title <- "Text Size"
  p[[4]] <- patchwork::inset_element(p[[4]], left = 0.75, right = 0.95, bottom = 0.01, top = 0.2)
  p[[4]]$data$text_names <- c("left", "right")

  return(p)
}
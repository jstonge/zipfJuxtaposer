#' wordshift_plot 
#'
#' @description Plot wordshit using shifterator
#'
#' @return ggplot
#'
#' @noRd
shifterator_plot <- function(mixedelements, alpha, type_shift, system_names) {
  type2freq1 <- mixedelements[[1]][,c("types", "counts")]
  type2freq2 <- mixedelements[[2]][,c("types", "counts")]

  if (alpha == Inf) {
    # diamond plot accepts Inf but not wordshift, apparently.
    # We change the value to 5 if this happens.
    alpha <- 5
  }
  
  word_shift = switch(
    type_shift,
    "Proportion" = shifterator::proportion_shift(type2freq1, type2freq2),
    "Shannon" = shifterator::entropy_shift(type2freq1, type2freq2, base=2, alpha=1),
    "Tsallis" = shifterator::entropy_shift(type2freq1, type2freq2, base=2, alpha=alpha),
    "KLD" = shifterator::kldivergence_shift(type2freq1, type2freq2, base=2),
    "JSD" = shifterator::jsdivergence_shift(type2freq1, type2freq2, weight_1=0.5, weight_2=0.5, base=2, alpha=alpha)
  )

  p <- shifterator::get_shift_graphs(word_shift, system_names, detailed = FALSE)
  
  # Deconstruct the plot to make the inset plots
  # Ideally that would be a feature in shifterator
  text_size_plot <- p$plot_env$text_size_plot + ggplot2::ggtitle("Text Size") 
  text_size_plot$data$text_names <- c("left", "right")
  text_size_plot_inset <- patchwork::inset_element(
    text_size_plot, left = 0.7, bottom = 0.07, right = 0.9,top = 0.2
    )
  
  get_cumulative_inset <- function(p) {
    p <- p + 
      ggplot2::annotation_logticks(alpha = 0.7)  +
      ggplot2::coord_flip() +
      ggplot2::xlab(NULL) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(expression(sum(delta * Phi [tau])))) +
      ggplot2::ggtitle(NULL) +
      ggplot2::theme_light()
    
    patchwork::inset_element(p, left = 0.01, bottom = 0.005, right= 0.25, top=0.4)
  }
  
  if ((type_shift == "Proportion") || (type_shift == "KLD") || (type_shift == "JSD")) {
    total_plot <- p$patches$plots[[1]]
    main_plot <- p$patches$plots[[2]] + ggplot2::theme_light()
    cummulative_inset <- get_cumulative_inset(p$patches$plots[[3]])
  
    layout <- 'A#
               BCD'
    
    patchwork::wrap_plots(A=total_plot, B=main_plot, C=cummulative_inset,
                          D = text_size_plot_inset, heights = c(0.05, 0.95))
    
  } else if ((type_shift == "Shannon") || (type_shift == "Tsallis")) {
    div_plot   <- p$patches$plots[[3]] 
    total_plot <- p$patches$plots[[1]] + ggplot2::ggtitle(NULL)
    main_plot  <- p$patches$plots[[2]] + ggplot2::theme_light()
    cummulative_inset <- get_cumulative_inset(p$patches$plots[[4]])
  
    layout <- 'A#
               B#
               CDE'
    
    patchwork::wrap_plots(A=div_plot, B=total_plot, C=main_plot, D=cummulative_inset,
                          E = text_size_plot_inset, heights = c(0.05, 0.05, 0.9))
    
  }
}
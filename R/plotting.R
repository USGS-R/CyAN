#' Plot bivariate data
#'
#' Plot data retrieved using \code{get_bivariate}
#'
#' @param plot_data bivariate data to plot - should be collected
#' before the plotting function
#'
#' @param log_1,log_2 logicals indicating whether to use a log
#' scale for parameter 1 and parameter 2, respectively
#'
#' @param method_highlight a method to be highlighted - points analyzed
#' with this method will appear in red instead of black
#'
#' @param flagged_results a vector of flagged results - could be
#' obtained using the \code{find_flagged} function. These results will
#' be plotted with an x symbol rather than a filled circle
#'
#' @param range_1,range_2 numeric vectors specifiying the range for
#' values in parameter 1 and parameter 2, respectively. The minimum
#' should be given first, followed by the maximum.
#'
#' @importFrom magrittr %>%
#'
#' @export

plot_bivariate <- function(plot_data,
                           log_1 = FALSE, log_2 = FALSE,
                           method_highlight = NULL, flagged_results = NULL,
                           range_1 = NULL, range_2 = NULL) {

  VALUE.1 <- VALUE.2 <- highlight <- is_flagged <- ".dplyr.var"

  if(!all(is.logical(c(log_1, log_2))))
    stop("log_1 and log_2 should be TRUE/FALSE")

  if(!is.null(flagged_results)) {
    if(!is.numeric(flagged_results))
      stop("flagged_results should be a numeric vector")
  }

  if(!is.null(range_1)) {
    if(!is.numeric(range_1))
      stop("range_1 should be numeric")
    if(length(range_1) != 2)
      stop("range_1 should be length 2")
  }
  if(!is.null(range_2)) {
    if(!is.numeric(range_2))
      stop("range_2 should be numeric")
    if(length(range_1) != 2)
      stop("range_2 should be length 2")
  }

  if(nrow(plot_data > 10000)) {
    warning("plot_data contains more than 10,000 rows and will be truncated")
    plot_data <- utils::head(plot_data, 10000)
  }

  if(is.null(method_highlight)) {
    plot_data$highlight <- FALSE
  } else {
    plot_data$highlight <-
      plot_data$METHOD_ID.1 == method_highlight |
      plot_data$METHOD_ID.2 == method_highlight
  }

  if(is.null(flagged_results)) {
    plot_data$is_flagged <- FALSE
  } else {
    plot_data$is_flagged <-
      plot_data$RESULT_ID.1 %in% flagged_results |
      plot_data$RESULT_ID.2 %in% flagged_results
  }

  parameter_1_label <- plot_data$PARAMETER_NAME.1[1]
  parameter_2_label <- plot_data$PARAMETER_NAME.2[1]

  plot_data$VALUE.1 <- as.numeric(plot_data$VALUE.1)
  plot_data$VALUE.2 <- as.numeric(plot_data$VALUE.2)

  plot <- ggplot2::ggplot(plot_data,
                 ggplot2::aes(x = VALUE.1, y = VALUE.2,
                              color = highlight, shape = is_flagged)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour='grey60'),
                   panel.grid.minor = ggplot2::element_line(colour='grey60', linetype = "dashed")) +
    ggplot2::geom_point(size=1.5) +
    ggplot2::scale_color_manual(guide=FALSE, values=c("black", "red1")) +
    ggplot2::scale_shape_manual(guide=FALSE, values=c(19, 4)) +
    ggplot2::xlab(parameter_1_label) + ggplot2::ylab(parameter_2_label)

  if(log_1) {
    plot <- plot + ggplot2::scale_x_log10(limits = range_1)
  } else {
    plot <- plot + ggplot2::scale_x_continuous(limits = range_1)
  }

  if(log_2) {
    plot <- plot + ggplot2::scale_y_log10(limits = range_2)
  } else {
    plot <- plot + ggplot2::scale_y_continuous(limits = range_2)
  }

  return(plot)

}

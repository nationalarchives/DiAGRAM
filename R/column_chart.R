#' Column chart
#' 
#' Creates the standard column chart used in many tabs with set colours and themeing
#' 
#' @param data data to be displayed
#' @param xlabel character, x axis label for the plot
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar xlab ylab geom_hline stat_summary geom_text
#' @importFrom ggplot2 position_stack theme_light theme element_blank element_text
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom stats reorder
#' @importFrom rlang .data
#' @export
column_chart = function(data, xlabel) {
  # hack for build check warning on non exported globals
  ..y.. = NULL
  data %>%
  dplyr::mutate(utility = .data$Intellectual_Control + .data$Renderability) %>%
    tidyr::pivot_longer(c(.data$Intellectual_Control, .data$Renderability), names_to = "node") %>%
    ggplot2::ggplot(
      ggplot2::aes(x = stats::reorder(.data$name, -.data$value), fill = .data$node, y = .data$value*50)
    ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::xlab(xlabel) + ggplot2::ylab("Score") +
    ggplot2::geom_hline(yintercept = 0.1013*50, linetype = "dashed", color = "black") +
    ggplot2::geom_hline(yintercept = 1.4255*50, linetype = "dashed", color = "black") +
    ggplot2::stat_summary(
      fun.y = sum, ggplot2::aes(label = format(round(..y.., 0), nsmall = 0), group = .data$name),
      geom = "text", size = 7, fontface = "bold", vjust = -0.25
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = format(round(.data$value*50, 0), nsmall = 0)), size = 5, colour = "white",
      fontface = "bold", position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(), text = ggplot2::element_text(size = 20),
      legend.position="top", legend.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(values = c("#FF6E3A", "#8400CD")) #colour blind scheme
}
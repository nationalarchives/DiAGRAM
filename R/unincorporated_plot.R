#
#
# ## 5 L1796
# tidyr::pivot_longer(c(.data$Intellectual_Control, .data$Renderability), names_to = "policy") %>%
#   dplyr::mutate(value = ifelse(policy == "Renderability", .data$value*a/(a + b)*100, .data$value*b/(a + b)*100)) %>%
#   ggplot2::ggplot(
#     ggplot2::aes(x = stats::reorder(.data$name, -.data$value), fill = .data$policy, y = .data$value)
#   ) +
#   ggplot2::geom_bar(position = "stack", stat = "identity") +
#   ggplot2::xlab("Policy") + ggplot2::ylab("Score") +
#   ggplot2::geom_hline(yintercept = 0.1013*50, linetype = "dashed", color = "black") +
#   ggplot2::geom_hline(yintercept = 1.4255*50, linetype = "dashed", color = "black") +
#   ggplot2::stat_summary(
#     fun.y = sum, ggplot2::aes(label = format(round(..y.., 0), nsmall = 0), group = .data$name),
#     geom = "text", size = 7, fontface = "bold", vjust = -0.25
#   ) +
#   ggplot2::geom_text(
#     ggplot2::aes(label = format(round(.data$value, 0), nsmall = 0)), size = 5, colour = "white",
#     fontface = "bold", position = ggplot2::position_stack(vjust = 0.5)
#   ) +
#   ggplot2::theme_light() +
#   ggplot2::theme(
#     panel.border = ggplot2::element_blank(), text = ggplot2::element_text(size = 20),
#     legend.position="top", legend.title = ggplot2::element_blank()
#   ) +
#   ggplot2::scale_fill_manual(values = c("#FF6E3A", "#8400CD")) #colour blind scheme

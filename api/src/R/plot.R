#' Write PNGs to temporary file for sending via the API
#'
#' @param data raw list data as parsed from JSON
write_temp_png = function(data) {
  path = tempfile(fileext = ".png")
  g = bar_chart(data)
  ggplot2::ggsave(path, g)
  path
}

#' Prepare data to be plotted as a bar chart
#'
#' @param data raw list data as parsed from JSON
prepare_bar_chart_data = function(data) {
  df = dplyr::bind_rows(data)
  df = dplyr::distinct(
    df,
    .data$model_name, .data$scenario,
    .data$intellectual_control, .data$renderability
  )
  tidyr::pivot_longer(
    df,
    c(.data$intellectual_control, .data$renderability),
    names_to = "metric",
    values_to = "value"
  )
}

#' Create a bar chart plot from data
#'
#' @param data raw list data as parsed from JSON
bar_chart = function(data) {
  df = prepare_bar_chart_data(data)
  df = dplyr::rename(df, Model = .data$model_name)
  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$value, fill = .data$metric,
    y = .data$scenario
  )) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(0.5, preserve = "single"),
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$value, x = .data$value + 5),
      position = ggplot2::position_dodge2(0.9, preserve = "single"),
      size = 7
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::scale_y_discrete(drop = TRUE) +
    ggplot2::scale_fill_manual(
      values = c(
        "intellectual_control" = "#FF6E3A",
        "renderability" = "#8400CD"
      ),
      labels = c(
        "intellectual_control" = "Intellectual Control",
        "renderability" = "Renderability"
      )
    ) +
    ggplot2::labs(
      y = "Scenario",
      x = "Score",
      fill = "Score Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text.x = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 12)
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$Model),
      ncol = 1,
      labeller = ggplot2::label_both,
      scales = "free"
    )
}

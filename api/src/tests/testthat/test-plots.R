cases = c("simple_scenario", "simple_scenario2", "simple", "advanced")

purrr::walk(cases, ~{
  req = test_req_plot(.x)$postBody
  parsed = jsonlite::parse_json(req)

  test_that(glue::glue("{.x}: chart creation data"), {
    plot_data = prepare_bar_chart_data(parsed)
    expect_s3_class(plot_data, "data.frame")
    expect_named(
      plot_data, c("model_name", "scenario", "metric", "value"),
      ignore.order = TRUE
    )
    expect_true(is.numeric(plot_data$value))
  })

  test_that(glue::glue("{.x}: chart creation"), {
    res = bar_chart(parsed)
    expect_s3_class(res, "gg")
  })
})

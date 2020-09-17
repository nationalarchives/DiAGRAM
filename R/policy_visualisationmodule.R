#' Create bar chart for visualisation module
#'
#' @param policy_data Model and policy data. See inst/table_experiment.R for temp data
#' @importFrom tidyr replace_na pivot_longer
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous labs coord_flip facet_wrap
#' @importFrom dplyr mutate
#' @export
policy_bar_chart = function(policy_data){

  ## Convert NA to baseline
  policy_data = policy_data %>%
    tidyr::replace_na(list(Policy = "baseline"))

  ## Reorder factors for chart
  policy_data = policy_data %>%
    dplyr::mutate(
      policy = forcats::fct_relevel(.data$Policy, sort),
      policy = forcats::fct_relevel(.data$Policy, "baseline", after = 0),
      policy = forcats::fct_rev(.data$Policy)
    )

  ## Restructure data for facetting
  policy_data = policy_data %>%
    tidyr::pivot_longer(
      cols = c(.data$`Intellectual Control`, .data$Renderability),
      names_to = "Metric",
      values_to = "Score"
    ) %>%
    dplyr::mutate(
      Metric = factor(.data$Metric)
    )

  ## Create plot - needs sprucing up
  plot = policy_data %>%
    ggplot2::ggplot(ggplot2::aes(x = Policy,
                        y = Score,
                        fill = Metric,
                        text = Notes )) + #ifelse(!is.na(Notes), Notes, NULL))) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Policy",
                  y = "Score",
                  fill = "Score Type") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Model,
                        ncol = 1,
                        scales = "free_y",
                        labeller = ggplot2::label_both) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") # ignored by ggplotly, I know it is really annoying

  plotly::ggplotly(plot)

}



#' policy_visualisation module ui
#'
#' @param id shiny id necessary for module
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @export
policy_visualisation_module_ui = function(id){
  ns = NS(id) # no lint (excluded from lint for jrshinyapp template)
  tagList(
    shinydashboard::box(
      width = 12,
      plotly::plotlyOutput(ns("policy_bar_chart"))
    ),
    model_table_module_ui(ns('bar-select'))
  )
}

#' policy_visualisation module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
#' @importFrom plotly renderPlotly
policy_visualisation_module_server = function(input, output, session, model_data, model, scoring_funcs){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  # model_data() is reactive

  selection = callModule(model_table_module_server, 'bar-select', data = model_data, model = model, selection = "multiple", show_policy = TRUE, scoring_funcs = scoring_funcs)
  vis_data = shiny::reactive({
    req(nrow(model_data()) > 0)
    intermediate = model_data()
    intermediate = dplyr::bind_cols(intermediate, purrr::map_dfr(intermediate$response, ~{
      score_model(model, format_responses(.x), scoring_funcs) %>% unlist
    }))
    df = intermediate %>%
      dplyr::select(.data$model, .data$policy, "Intellectual Control" = .data$Intellectual_Control, .data$Renderability, .data$notes, .data$response) %>%
      dplyr::rename_with(stringr::str_to_title) %>%
      dplyr::mutate_if(is.numeric, ~ round(.x, 2))
    df[selection(), ]
  })

  observe({
    print("vis data")
    print(vis_data())
  })

  output$policy_bar_chart = plotly::renderPlotly({
    policy_bar_chart(vis_data())
  })
}

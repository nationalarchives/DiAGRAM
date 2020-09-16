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
    tidyr::replace_na(list(policy = "baseline"))

  ## Reorder factors for chart
  policy_data = policy_data %>%
    dplyr::mutate(
      policy = forcats::fct_relevel(policy, sort),
      policy = forcats::fct_relevel(policy, "baseline", after = 0),
      policy = forcats::fct_rev(policy)
    )

  ## Restructure data for facetting
  policy_data = policy_data %>%
    tidyr::pivot_longer(
      cols = c(Intellectual_Control, Renderability),
      names_to = "type",
      values_to = "score"
    )

  ## Create plot - needs sprucing up
  plot = policy_data %>%
    ggplot2::ggplot(aes(x = policy,
                        y = score,
                        fill = type)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Policy",
                  y = "Score",
                  fill = "Score Type") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ model,
                        ncol = 1,
                        scales = "free_y") +
    ggplot2::theme(legend.position = "bottom") # ignored by ggplotly

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
    plotly::plotlyOutput(ns("policy_bar_chart"))
  )
}

#' policy_visualisation module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
#' @importFrom plotly renderPlotly
policy_visualisation_module_server = function(input, output, session, model_data){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  # model_data() is reactive
  output$policy_bar_chart = plotly::renderPlotly({
    policy_bar_chart(model_data())
  })
}

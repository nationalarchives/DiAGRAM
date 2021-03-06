#' Create bar chart for visualisation module
#'
#' @param policy_data Model and policy data. See inst/table_experiment.R for temp data
#' @importFrom tidyr replace_na pivot_longer
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous labs coord_flip facet_wrap
#' @importFrom dplyr mutate
#' @export
policy_bar_chart = function(policy_data){

  plot = policy_bar_gg(policy_data)
  p = plotly::ggplotly(plot, tooltip = "text")
  # tidy up the extra hover text created by plotly
  p$x$data = purrr::map(p$x$data, function(x){
    x$hoverinfo = if(all(x$x == 100)) "skip" else "text"
    x
  })
  # tidy up facet labels
  # p$x$layout$annotations = purrr::map(
  #   p$x$layout$annotations, function(x) {
  #     if(stringr::str_detect(x$text, "^Model")){
  #       x$x = 0.1
  #       x$font$size = 2*x$font$size
  #     }
  #     x
  #   }
  # )
  p %>% plotly::layout(legend = list(orientation = "h", y = -0.1))
}


#' policy bar gg
#'
#' Creates a nice bar chart using ggplot with fixed colours for policies and
#' models
#'
#' @param policy_data a tibble of model/scenario data
#' @export
policy_bar_gg = function(policy_data){
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
      Metric = factor(.data$Metric),
      Score = round(Score*100),
      Total = 100
    )

  # create nice hover text
  policy_data = policy_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover = glue::glue(
      "<b>{Model}</b>
<b>Scenario: </b>{policy}
<b>Metric: </b>{Metric}
<b>Score: </b>{Score}%"
    ))

  plot = policy_data %>%
    ggplot2::ggplot(ggplot2::aes(x =  Policy,
                                 y = Score,
                                 fill = Metric)) +
    ggplot2::geom_col(ggplot2::aes(x = Policy, fill = Metric, y = Total),
                      alpha = 0.2,
                      position = ggplot2::position_dodge2(0.5, preserve = "single")) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(0.5, preserve = "single"), aes(text = hover )) +

    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = "Policy",
                  y = "Score",
                  fill = "Score Type") +
    xlab(NULL) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Model,
                        ncol = 1,
                        scales = "free_y",
                        labeller = ggplot2::label_both) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   strip.text.x = ggplot2::element_text(size = 8)) + # ignored by ggplotly, I know it is really annoying
    scale_fill_manual(values = c("Intellectual Control" = "#FF6E3A", "Renderability" = "#8400CD"))
  plot
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
    shiny::h3("View Results"),
   # shiny::h4("Your results"),
    shiny::p("Your result has been calculated from your answers to the questions combined with the data in the underlying network. This shows you the probability of successfully preserving your records in terms of the two main digital preservation outcomes: ",strong("Renderability")," and ",strong("Intellectual Control.")),
    shiny::p("For example, if your score for Intellectual Control is 21, this means that out of a 100 files you are likely to have full knowledge of the material content, provenance and conditions of use for 21 of them. If your score for Renderability is 45 this means that out of a 100 files you would be able to provide a sufficiently useful representation of 45 of the original files."),
    shiny::p("If you would like to know more about how your results were calculated see 'Learn about Diagram'."),
    shiny::p("To save your results to view offline or upload later go to 'Download a report'."),
    shiny::p("In order to give you an idea of where your score puts your archive compared to others we have provided two reference models which will appear in the table below alongside your own models. See 'Using the reference models' to learn more about what they can tell you."),
    shinydashboard::box(
      width = 12,
      title = "Select the models and scenarios to visualise",
      model_table_module_ui(ns('bar-select'))
    ),
    shinydashboard::box(
      width = 12,
      title="Your results",
      plotly::plotlyOutput(ns("policy_bar_chart"))
    ),
    shiny::h4("How do I improve my results?"),
    shiny::p("To improve your results, you have to improve your answers to the questions, which you can do by creating scenarios."),
    shiny::p("Some of your answers may not be things you can change, for instance the type of digital objects in your collection, so bear in mind this may limit the maximum score you can get. It is also impossible to get 100, because no archive will ever be risk free!"),
    shiny::p("The risk variables interact in a complex way, so you may wish to review the network first to identify which areas you’d like to focus on. You can see a diagram of the network on Learn about DiAGRAM.")
  )
}

#' policy_visualisation module server
#'
#' server side logic for the visualisation of model/scenario data
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
#' @importFrom plotly renderPlotly
policy_visualisation_module_server = function(input, output, session, model_data, model, scoring_funcs, question_data){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  # model_data() is reactive

  selection = callModule(model_table_module_server, 'bar-select', data = model_data, model = model, selection = "multiple", show_policy = TRUE, scoring_funcs = scoring_funcs, question_data = question_data)
  vis_data = shiny::reactive({
    req(nrow(model_data()) > 0)
    intermediate = model_data()
    df = format_vis_data(intermediate, model, scoring_funcs)
    df[selection$selected(), ]
  })

  output$policy_bar_chart = plotly::renderPlotly({
    validate(
      need(nrow(vis_data()) > 0,
           "No policies currently selected.")
    )
      policy_bar_chart(vis_data())
  })
  return(selection$data)
}

format_vis_data = function(intermediate, model, scoring_funcs) {
  intermediate = tryCatch(
    {dplyr::bind_cols(intermediate, purrr::map_dfr(intermediate$response, ~{
      score_model(model, format_responses(.x), scoring_funcs) %>% unlist
    }))}, error = function(e) browser())
  intermediate %>%
    dplyr::select(.data$model, .data$policy, "Intellectual Control" = .data$Intellectual_Control, .data$Renderability, .data$notes, .data$response) %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::mutate_if(is.numeric, ~ round(.x, 2))
}

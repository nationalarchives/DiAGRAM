# nodes which are from responses

generate_example_reponse = function() {
  min_max = function(x){
    100*x/sum(x)
  }
  example_responses = list(
    rpois(1,25), rpois(1,40), min_max(c(rpois(3,33))),
    rpois(1,50), min_max(rpois(3,c(10,20,70))), min_max(rpois(3,c(20,50,30))),
    rpois(1,10), rpois(1,10), rpois(1,3)
  )
  example_responses %>% setNames(.user_nodes)
}

format_responses = function(values) {
  tibble::tibble(node = names(values), response = values)
}

# model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))
# default_responses = model[.user_nodes] %>% purrr::map(~{
#   if(length(.x$prob) == 2) as.vector(.x$prob[1])*100 else as.numeric(.x$prob)*100
# })
# default_clone = default_responses
#
# default_data = tibble::tibble(node = user_nodes, response = default_responses)
# default_score = score_model(model, format_responses(default_responses))

format_score = function( scores, name = "model"){
  dplyr::bind_cols(Name = name, tibble::as_tibble(scores))
}

# library(shiny)
# library(plotly)
#
# ui = fluidPage(
#   sliderInput("val", label = "Technical_Skills", min = 0, max = 100, value = default_responses[[1]], step = 1),
#   br(),
#   tableOutput("plot")
# )
#
# server = function(input, output, session) {
#   output$plot = renderTable({
#     default_clone[[1]] = input$val
#     format_score(score_model(model, format_responses(default_clone)))
#   })
# }
#
# shiny::shinyApp(ui, server)

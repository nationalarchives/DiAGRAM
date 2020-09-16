#' Run the diagram app
#'
#' Runs the diagram app using the partial function from purrr.
#'
#' @param n Number of questions, defaults to 9
#' @importFrom purrr partial
#' @importFrom shiny shinyApp
#' @export
run_app = function(question_data, default_response, model){
  app_ui_partial = purrr::partial(diagramNAT::app_ui, question_data = question_data, default_response = default_response)
  app_server_partial = purrr::partial(diagramNAT::app_server, question_data = question_data, default_response = default_response, model = model)


  shiny::shinyApp(ui = app_ui_partial, server = app_server_partial)
}

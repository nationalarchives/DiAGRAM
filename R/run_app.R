#' Run the diagram app
#'
#' Runs the diagram app using the partial function from purrr.
#'
#' @param n Number of questions, defaults to 9
#' @importFrom purrr partial
#' @importFrom shiny shinyApp
#' @export
run_app = function(n = 9){
  app_ui_partial = purrr::partial(diagram::app_ui, nquestions = n)
  shiny::shinyApp(ui = app_ui_partial, server = diagram::app_server)
}

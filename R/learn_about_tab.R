#' learn about/definitions tab
#'
#' Creates the learn about page from the learn_about.md file in the inst/text_content file
#'
#' @return a shiny tag list object
definitions_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      title = NULL,
      width = 12,
      includeMarkdown(system.file('text_content/learn_about.md', package = "diagramNAT"))
    ))
}

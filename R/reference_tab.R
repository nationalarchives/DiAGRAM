#' understand reference models tab
#'
#' Creates the understand reference models page from the ref_models.md file in the inst/text_content file
#'
#' @return a shiny tag list object
reference_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      shiny::includeMarkdown(system.file("text_content", "ref_models.md", package = "diagramNAT"))
    ) #,
   # shinydashboard::box(
   #   title = "Reference models", width = 12,
   #   shiny::div(
   #     "table and graph of reference models only here"
   #   ) #,
      #model_table_module_ui(ns('table')
   #   )
  )
}

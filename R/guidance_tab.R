guidance_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      shiny::includeMarkdown(system.file("text_content", "guidance.md", package = "diagramNAT"))
    ),
    shinydashboard::box(
      width = 12,
      shiny::includeMarkdown(system.file("text_content", "guidance_extra.md", package = "diagramNAT"))
    )
  )
}

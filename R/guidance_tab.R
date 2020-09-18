guidance_tab = function() {
  shinydashboard::box(
    width = 12,
    shiny::includeMarkdown(system.file("text_content", "guidance.md", package = "diagramNAT"))
  )
}

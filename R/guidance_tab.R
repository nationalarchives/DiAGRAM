guidance_tab = function() {
  shinydashboard::box(
    shiny::includeMarkdown(system.file("text_content", "guidance.md", package = "diagramNAT"))
  )
}

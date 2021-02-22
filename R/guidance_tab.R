guidance_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      shiny::includeMarkdown(system.file("text_content", "guidance.md", package = "diagramNAT"))
    ),
    shinydashboard::box(
      width = 12,
      shiny::includeMarkdown(system.file("text_content", "guidance_extra.md", package = "diagramNAT"))
    ),
    shinydashboard::box(
      width = 12,
      shiny::h4("Video of DiAGRAM in use"),
      shiny::p("This is a recording of a live demonstration of how to use the tool. At the time of recording it was not anticipated that this would be shared as part of the DiAGRAM guidance, so please do not expect it to be a copmrehensive walkthrough guide. However, we hope you may find it useful."),
      shiny::tags$video(id="video", type = "video/mp4",src = "Diagramdemo_Paper_Archive_EDIT_1-2-2021.mp4", controls = "controls")
    )
  )
}

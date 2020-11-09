#' home tab
#'
#' Function generates the home page/landing oage for the diagram app, grabbing assets from the inst
#' file from the package.
#'
#' @return a shiny taglist object
home_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      title = NULL,
      width = 12,
      shiny::h2(
        "DiAGRAM - The ",
        shiny::tags$b("Di", .noWS="outside"), "gital ",
        shiny::tags$b("A", .noWS="outside"), "rchiving ",
        shiny::tags$b("G", .noWS="outside"), "raphical ",
        shiny::tags$b("R", .noWS="outside"), "isk ",
        shiny::tags$b("A", .noWS="outside"), "ssessment ",
        shiny::tags$b("M", .noWS="outside"), "odel",
        align="center",
        class = "main-title"
      ),
      shiny::div(shiny::img(src = "www/diagram_logo_transparent.png", width = "400px"), style = "text-align:center"),
      shiny::h3("Version 0.9.7 (Prototype)", align="center"), #update in June
      shiny::includeMarkdown(system.file("text_content/home_content.md", package = "diagramNAT")),
      shiny::fluidRow(
        shiny::column(
          width=12,
          shinydashboard::box(
            title="DiAGRAM structure",
            collapsible=TRUE,
            width=NULL,
            shiny::img(
              src="www/DiAGRAM-Roboto.png",
              style="max-width:100%;max-height:500px;"
            )
          )
        )
      ),
      shiny::br(),
      shiny::div(
        class = "logos",
        shiny::img(
          src="www/TNA - SQUARE LOGO POSITIVE.png",
          height=100,
          width=100
        ),
        shiny::img(
          src="www/university_of_warwick_logo_detail.png",
          height=80,
          width=120
        ),
        shiny::img(
          src="www/TNLHLF_Colour_Logo_English_RGB_0_0.jpg",
          height=80,
          width=216
        ),
        shiny::img(src="www/UKRI_EPSR_Council-Logo_Horiz-RGB.png", height=75)
      )
    )
  )
}

#' app_ui
#'
#' Defines the user interface for the DIAGRAM application. This content is a heavily revised
#' version of a script built for DiAGRAM by the University of Warwick and The National
#' Archive. Stephen James Krol, Monash University, Melbourne, stephen.james.krol@gmail.com
#'
#' @param req Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @param question_data The question data, a list, likely parsed from a YAML file that specifies the questions to be
#' displayed in the application
#' @param default_response The default values, used to initialise the model
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard tabItems tabItem box
#' @importFrom shinydashboardPlus dashboardPagePlus
#' @importFrom shinydashboardPlus dashboardFooter
#' @importFrom shiny icon fluidRow h3 strong p h2 tags a br div actionButton img h1 column
#' @importFrom shiny selectInput uiOutput tableOutput fileInput textInput HTML h5 checkboxGroupInput
#' @importFrom shiny h4 radioButtons htmlOutput sliderInput downloadButton addResourcePath
#' @importFrom rintrojs introjsUI
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinysky hotable
#' @importFrom DT dataTableOutput
#' @importFrom plotly plotlyOutput
#' @export
app_ui = function(req, question_data, default_response) {

  # create main dashboard page
  shiny::addResourcePath(
    "www", system.file("assets/www", package = "diagramNAT")
  )
  shiny::tagList(
    # shinya11y::use_tota11y(),
    shiny::tags$head(shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "www/ui.css"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "www/questions.css"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://use.typekit.net/jwz4fne.css"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,400;0,600;1,400;1,600&family=Roboto+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "www/branding.css"
    )),
    shinydashboardPlus::dashboardPagePlus(
      skin="purple",
      # Add header and title to Dashboard
      header = shinydashboard::dashboardHeader(
        title="DiAGRAM"
      ),
      # Add dashboard sidebar
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebarMenu",
          shinydashboard::menuItem(
            "Home page", tabName = "Home"
          ),
          shinydashboard::menuItem(
            "How to use the tool", tabName = "how-to"
          ),
          shinydashboard::menuItem(
            "Create a model", tabName = "model"
          ),
          shinydashboard::menuItem(
            "Create a scenario", tabName = "scenario"
          ),
          shinydashboard::menuItem(
            "View results", tabName = "visualise"
          ),
          shinydashboard::menuItem(
            "Download a report", tabName = "report"
          ),
          shinydashboard::menuItem(
            "Upload a previous model", tabName = "save"
          ),
          shinydashboard::menuItem(
            "Learn about DiAGRAM", tabName = "definitions"
          ),
          shinydashboard::menuItem(
              "Advanced customisation", tabName = "advanced"
          ),
          shinydashboard::menuItem(
            "Glossary", tabName = "glossary"
          )
        )
      ),
      body = shinydashboard::dashboardBody(
        shiny::includeScript(system.file("assets", "js", "close_warning.js",
                                         package = "diagramNAT")),
        shinyjs::useShinyjs(),
        shinyalert::useShinyalert(),
        id = "dashboardBody",
        # dev_banner_module_ui('dev-banner'),
        shinydashboard::tabItems(
          # id = "menu-select",
          shinydashboard::tabItem(
            tabName = "Home",
            home_tab()
          ),
          shinydashboard::tabItem(
            tabName = "how-to",
            guidance_tab()
          ),
          shinydashboard::tabItem(
            tabName = "definitions",
            definitions_tab()
          ),
          shinydashboard::tabItem(
            tabName = "model",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                questions_module_ui('model-questions', question_data, default_response)
              )#,
              # shinydashboard::box(
              #   width = 12,
              #   shiny::div(
              #     id = "no-model-container",
              #     "There are currently no models defined in this session"
              #   )
              # ),
            ),
            # shinyjs::hidden(
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  shiny::div(
                    id = "model-table-container",
                    model_table_module_ui("model_table")
                  )
                )
              )
            # )
          ),
          shinydashboard::tabItem(
            tabName = "scenario",
            shiny::fluidRow(
              shinydashboard::box(
                title = NULL, width = 12,
                policy_creation_module_ui('policy-questions')
              )
            )

          ),
          shinydashboard::tabItem(
            tabName = "visualise",
            shiny::fluidRow(
              # width = 12, #NULL
              shinydashboard::box(
                width = 12,
                policy_visualisation_module_ui('bar'),
              )
              # model_table_module_ui('bar-select')
            )
          ),
          shinydashboard::tabItem(
            tabName = "save",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
            shiny::fileInput("upload", label = "Upload data", accept = ".json")#,
            # shiny::downloadButton("download"),
            # model_table_module_ui("save_table")
              ))
          ),
          shinydashboard::tabItem(
            tabName = "report",
            shiny::fluidRow(
              report_tab_module_ui('report')
            )
          ),
          shinydashboard::tabItem(
            tabName = "advanced",
            shiny::fluidRow(
              advanced_tab_module_ui('adv')
            )
          ),
          shinydashboard::tabItem(
            tabName = "glossary",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                shiny::includeMarkdown(system.file("text_content", "glossary.md",
                                               package = "diagramNAT"))
              )
            )
          )
        )
      ),
      footer = shinydashboardPlus::dashboardFooter(
        shiny::includeMarkdown(system.file("text_content", "footer.md",
                                           package = "diagramNAT"))
      ),
      sidebar_fullCollapse = TRUE
    )
  )
}

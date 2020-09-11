#' app_ui
#'
#' Defines the user interface for the DIAGRAM application. This content is a heavily revised
#' version of a script built for DiAGRAM by the University of Warwick and The National
#' Archive. Stephen James Krol, Monash University, Melbourne, stephen.james.krol@gmail.com
#'
#' @param req Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @param nquestions integer defining the number of questions for the model creation step,
#'     typically generated from a call to `read_csv("setup_questions.csv") %>% nrow()`
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard tabItems tabItem box
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
app_ui = function(req, nquestions = 9){
  # create main dashboard page
  shiny::addResourcePath(
    "www", system.file("assets/www", package = "diagramNAT")
  )
  shiny::tagList(
    shiny::tags$head(shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "www/ui.css"
    )),
    shinydashboard::dashboardPage(
      skin="purple",
      # Add header and title to Dashboard
      shinydashboard::dashboardHeader(
        title="DiAGRAM"
      ),
      # Add dashboard sidebar
      shinydashboard::dashboardSidebar(
        # Create sidebar menu
        shinydashboard::sidebarMenu(
          id="sidebarMenu",
          # Create home page
          shinydashboard::menuItem(
            "Home",
            tabName="Home",
            icon=shiny::icon("home")
          ),
          # Create Network Info Page
          shinydashboard::menuItem(
            "Definitions",
            tabName="Node_definitions",
            icon=shiny::icon("info")
          ),
          # Customise model tab
          shinydashboard::menuItem(
            "1. Create your model",
            tabName="CustomiseModel",
            icon=shiny::icon("user-edit")
          ),
          # Sensitivity Page
          shinydashboard::menuItem(
            "Recommendations",
            tabName = "Sensitivity",
            icon=shiny::icon("clipboard")
          ),
          # Simple View Page
          shinydashboard::menuItem(
            "2. Compare policies",
            tabName = "CustomiseNode",
            icon=shiny::icon("chart-bar")
          ),
          # Create Report Tab
          shinydashboard::menuItem(
            "3. Report",
            tabName="Report",
            icon=shiny::icon("book")
          ),
          # Advanced Page
          shinydashboard::menuItem(
            "Advanced customisation",
            tabName = "AdvancedCustomiseNode",
            icon=shiny::icon("project-diagram")
          )
        )
      ),
      # Create Dashboard body
      shinydashboard::dashboardBody(
        id="dashboardBody",
        # include introjs UI
        rintrojs::introjsUI(),
        # create content for tabs
        dev_banner_module_ui("dev-banner"),
        shinydashboard::tabItems(
          # Home Tab
          shinydashboard::tabItem(
            tabName="Home",
            home_tab()
          ),

          # Network Tab
          shinydashboard::tabItem(
            tabName="Node_definitions",
            shinyalert::useShinyalert(),
            definitions_tab()
          ),
          # Model Tab
          shinydashboard::tabItem(
            tabName="CustomiseModel",
            shinyalert::useShinyalert(),
            model_tab()
          ),

          # TODO:sid - change policyTab identifier to the most appropriate (once decided)
          shinydashboard::tabItem(
            tabName="CustomiseNode",
            shinyalert::useShinyalert(),
            shiny::h1("Create and compare different policies"),
            shiny::br(),
            shiny::div(
              shiny::selectInput(
                "customModelSelection",
                shiny::h3("Select model"),
                choices = "Default"
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::selectInput(
                  "customOaisEntitySelection",
                  shiny::h5("Select OAIS Function Entity"),
                  choices="None",
                  multiple = TRUE
                ),
                shinydashboard::box(
                  title="Nodes checklist",
                  width=NULL,
                  shiny::checkboxGroupInput(
                    "policyTabNodesChecklist",
                    label=NULL,
                    choices=character(0)
                  )
                )
              ),
              shiny::column(
                width=4,
                shinyjs::useShinyjs(),
                shinydashboard::box(
                  id="StateNodeSliderBox",
                  title=NULL,
                  width=NULL,
                  shiny::div(id="nodeSliderPlaceholder", shiny::h4('No nodes selected')),
                  shiny::uiOutput("policyTabNodesSlider"),
                  shiny::actionButton("SimpleViewPolicyPrevious", label = "Previous"),
                  shiny::actionButton("SimpleViewPolicyNext", label = "Next"),
                  shiny::br()
                ),
                shinydashboard::box(
                  id="SimpleViewPolicyAddBox",
                  title=NULL,
                  width=NULL,
                  shiny::textInput("SimpleViewPolicyName", label = shiny::h4("Enter policy name"), value = ""),
                  shiny::actionButton("SimpleViewAddPolicy", "Add policy")
                ),
                shiny::div(
                  id = "simple-policy-reset-container",
                  shiny::actionButton('SimplePolicyReset', 'Reset model')
                )
              ),
              shiny::column(
                width=5,
                shiny::plotOutput("policyTabUtilityScorePlot"),
                shiny::br(),
                shinydashboard::box(
                  id="RemovePolicyBox",
                  width=NULL,
                  title="Remove policy",
                  shiny::selectInput(
                    "policyTabPolicyRemove",
                    shiny::h5("Select policy to remove"),
                    choices=""
                  ),
                  shiny::div(
                    id = "remove-policy-container",
                    shiny::actionButton('RemovePolicy', 'Remove')
                  )
                )
              )
            )),

          # Policy Tab
          shinydashboard::tabItem(
            tabName="AdvancedCustomiseNode",
            shinyalert::useShinyalert(),
            shiny::h1("Advanced model customisation"),
            shiny::br(),
            shiny::fluidRow(
              width=8,
              shiny::fluidRow(
                shiny::column(
                  width=12,
                  # Plot the bayesian network
                  shinydashboard::box(
                    title="DiAGRAM structure",
                    collapsible=TRUE,
                    width=NULL,
                    shiny::plotOutput("netPlot")
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width=4,
                  shinydashboard::box(
                    width=NULL,
                    shiny::selectInput(
                      "model_version",
                      shiny::h3("Select Model"),
                      choices="Default"
                    ),
                    shiny::selectInput(
                      "nodeProbTable",
                      shiny::h4("Select Node"),
                      choices=c("nodes loading")
                    ),
                    shiny::actionButton('networkReset','Reset model')
                  )
                ),
                shiny::column(
                  width=8,
                  shinydashboard::box(
                    title="Probability table",
                    width=NULL,
                    shiny::column(
                      width=4,
                      shiny::radioButtons(
                        "probtabltype",
                        "Table Type",
                        choices=c(
                          "Independent Probability Table",
                          "Conditional Probability Table"
                        )
                      ),
                      shiny::actionButton("updateProb", "Add changes")
                    ),
                    shiny::column(
                      width=8,
                      shinysky::hotable("probabilityTable")
                    )

                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width=4,
                  shinydashboard::box(
                    title="Changed nodes",
                    width=NULL,
                    collapsible=TRUE,
                    shiny::tags$ul(
                      shiny::uiOutput("ChangeNodes")
                    )
                  ),
                  shinydashboard::box(
                    title="Save model",
                    width=NULL,
                    collapsible=TRUE,
                    shiny::textInput(
                      "policyName",
                      label="Modified model name:",
                      value=""
                    ),
                    shiny::actionButton("addPolicy", "Add as policy"),
                    shiny::actionButton("addModelAdvanced", "Add as custom model")
                  )
                ),
                shiny::column(
                  width=8,
                  shinydashboard::box(
                    title="Node probability",
                    width=NULL,
                    collapsible=TRUE,
                    shiny::plotOutput("nodeProbability")
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width=6,
                  shinydashboard::box(
                    title="Policy comparison",
                    width=NULL,
                    shiny::plotOutput("PolicyComparison")
                  )
                ),
                shiny::column(
                  width=6,
                  shinydashboard::box(
                    title="Model comparison",
                    width=NULL,
                    shiny::plotOutput("BaseUtilityComparison")
                  )
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName="Report",
            shiny::h1("Report"),
            shiny::br(),
            shiny::div(
              shiny::selectInput(
                "reportTabModelSelection",
                shiny::h3("Select model"),
                choices ="Default"
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width=6,
                shinydashboard::box(
                  title="Summary",
                  width=NULL,
                  shiny::htmlOutput("ReportTabSummaryText")
                ),
                shinydashboard::box(
                  title="Score function weightings",
                  width=NULL,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  shiny::sliderInput(
                    inputId="RenderabilityWeighting",
                    label="Renderability weighting",
                    min=0,
                    max=1,
                    value=1,
                    step=0.1
                  ),
                  shiny::sliderInput(
                    inputId="IntellectualWeighting",
                    label="Intellectual control weighting",
                    min=0,
                    max=1,
                    value=1,
                    step=0.1
                  )
                ),
                shinydashboard::box(
                  title="Downloads",
                  width=NULL,
                  "Select what you would like to download:",
                  shiny::br(),
                  shiny::br(),
                  shiny::checkboxGroupInput(
                    "downloadOptions",
                    NULL,
                    choices=c("The plot", "The model", "A policy")
                  ),
                  shiny::br(),
                  shiny::selectInput(
                    "ReportTabPolicySelection",
                    shiny::h5("Select policy to download"),
                    choices="Base"
                  ),
                  shiny::br(),
                  shiny::downloadButton("reportTabDownloadBtn", "Download")

                ),
              ),
              shiny::column(
                width=6,
                shinydashboard::box(
                  title="Comparing policies",
                  width=NULL,
                  shiny::plotOutput("ReportTabUtilityComparisonPlot")
                ) #,
                #box(id="RemovePolicyBoxReport",
                #    width=NULL,
                #    title="Remove policy",
                #    selectInput("reportTabPolicyRemove",
                #                h5("Select policy to remove"),
                #                choices=""),
                #    tags$style(HTML('#RemovePolicyReport{background-color:red}')),
                #    tags$style(HTML('#RemovePolicyReport{color:white}')),
                #    div(actionButton('RemovePolicyReport', 'Remove'), style="float:right")
                #)
              )
            )
          ),
          shinydashboard::tabItem(
            tabName="Sensitivity",
            shiny::h1("Recommendations"),
            shiny::br(),
            shiny::div(
              shiny::selectInput(
                "sensTabModelSelection",
                shiny::h3("Select model"),
                choices ="Default"
              )
            ),
            shiny::fluidRow(
              shinyjs::useShinyjs(),
              # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
              # note that "A" needs to be replaced with plotly source string if used
              shinyjs::extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-A', 'null'); }"),
              shiny::column(
                width=12,
                shinydashboard::box(
                  title="Visualisation of potential policy changes",
                  width=NULL,
                  shiny::h5(
                    "This plot shows how changing your answers to the input questions will impact the score for renderability
                  (on the x axis) and intellectual control (on the y axis). Changes that improve the score will appear as
                  points above and to the right of the origin (where the axes meet) and changes that decrease the score
                  will be to the bottom and left."
                  ),
                  shiny::h5("Note: All input nodes are considered changable here but some may not be in your control."),
                  shiny::br(),
                  plotly::plotlyOutput("SensitivityPlot")
                ),
                # box(
                #   title="Selected Node",
                #   width=NULL,
                #   textOutput("clickevent")
                # ),
                shinydashboard::box(
                  title="Summary table of potential policy changes",
                  width=NULL,
                  shiny::h5("Note: All input nodes are considered changable here but some may not be in your control."),
                  shiny::br(),
                  DT::dataTableOutput("SensitivityTable")
                )
              )
            )
          )
        )
      )
    )
  )
}


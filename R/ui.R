
# Following script defines components displayed on the webpage.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com

library(shiny)
library(rintrojs)
library(networkD3)
library(shinydashboard)
library(gRbase)
library(gRain)
library(shinysky)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(tidyverse)
library(plotly)
library(DT)
library(V8)

options(repos = BiocManager::repositories())
nquestions <- read_csv("setup_questions.csv") %>% nrow()

#' app_ui
#' 
#' Defines the user interface for the DIAGRAM application
#' 
#' @param req Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard tabItems tabItem box
#' @importFrom shiny icon fluidRow h3 strong p h2 tags a br div actionButton img h1 column
#' @importFrom shiny selectInput uiOutput tableOutput fileInput textInput HTML h5 checkboxGroupInput
#' @importFrom shiny h4 radioButtons htmlOutput sliderInput downloadButton
#' @importFrom rintrojs introjsUI
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinysky hotable
#' @importFrom DT dataTableOutput
app_ui = function(req){
  # create main dashboard page
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
      shinydashboard::tabItems(
        # Home Tab
        shinydashboard::tabItem(
          tabName="Home",
          shiny::fluidRow(
            # Welcome box
            shinydashboard::box(
              title = NULL,
              width = 12,
              background="orange",
              shiny::h3(
                shiny::strong("Important note: This model is still in development")
              ),
              shiny::p(
                "There will be further user interface changes and additional functionality added as the 
                project progresses. Any feedback to inform the future development would be welcome 
                - please send your comments to a member of the project team.")
              ),
            shinydashboard::box(
              title = NULL,
              width = 12,
              shiny::h2(
                "DiAGRAM - The ",
                shiny::tags$b("Di", .noWS="outside"), "gital ",
                shiny::tags$b("A", .noWS="outside"), "rchiving ",
                shiny::tags$b("G", .noWS="outside"), "raphical",
                shiny::tags$b("R", .noWS="outside"), "isk ",
                shiny::tags$b("A", .noWS="outside"), "ssessment ",
                shiny::tags$b("M",.noWS="outside"), "odel",
                align="center"
              ),
              shiny::h3("Version 0.9.7 (Prototype)", align="center"), #update in June   
              shiny::br(),
              shiny::p(
                "This is the Digital Archiving Graphical Risk Assessment Model (DiAGRAM) built by the ",
                shiny::a(href="https://warwick.ac.uk", "University of Warwick"),
                " and ",
                shiny::a(href="https://www.nationalarchives.gov.uk"," The National Archives"), 
                "with support from the ",
                shiny::a(href="https://www.heritagefund.org.uk/", "National Lottery Heritage Fund"),
                " and the ",
                shiny::a(href="https://epsrc.ukri.org/", "Engineering and Physical Sciences Research Council."),
                "For more information about the project please see our ",
                shiny::a(href="https://www.nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/research-collaboration/safeguarding-the-nations-digital-memory/",
                  "project page.")),
              shiny::p(
                "Before using the tool for the first time, we would advise you to read the ",
                shiny::a(
                  href="https://www.dpconline.org/events/past-events/quantifying-digital-preservation-risk-online-workshop-2",
                  "presentations"
                ),
                " from our online workshop with the Digital Preservation Coalition, where there is also an ",
                shiny::a(href="https://www.dpconline.org/docs/miscellaneous/events/2020-events/2307-dpc-workshop-2-exercise/file", "exercise sheet"),
                " you can work though."
              ),
              shiny::br(),
              shiny::tags$style(HTML('#createModel{background-color:green}')),
              shiny::tags$style(HTML('#createModel{color:white}')),
              shiny::tags$style(HTML('#createModel{width:30%')),
              shiny::div(
                shiny::actionButton("createModel", "Create your model"),
                style="text-align:center"
              ),
              shiny::br(),
              shiny::h3("Introduction"),
              shiny::p(
                "This decision support tool enables users to score their archive's
                digital preservation risk and then explore how this would change under
                different policies and risk scenarios. The risk score is based on the proportion of 
                files in the archive that are renderable and where the archivist has full
                intellectual control."
              ),
              shiny::p(
                "The underlying methodology used to create this model is based on a Bayesian network
                - a probabilistic graphical model that captures the conditional dependencies of risk 
                events. When historical data were unavailable, data from an expert elicitation 
                session conducted in April 2020 were used to inform the probabilities needed for
                this model."
              ),
              shiny::p("This interface enables users to:"),
              shiny::tags$ul(
                shiny::tags$li(
                  "Understand the risk definitions used in the model and 
                  how the risk events are linked together"
                ),
                shiny::tags$li(
                  "Create a model that reflects the policies and practices for their 
                  Digital Archive"
                ),
                shiny::tags$li("Test alternative policies to see how this impacts the risk score"),
                shiny::tags$li("Download the model and a summary of the results"),
                shiny::tags$li("Upload a pre-built model and continue exploring scenarios from there"),
                shiny::tags$li(
                  "Update the probability tables for the model based on the user's own data or 
                  experience"
                ),
                shiny::tags$li(
                  "Create bespoke scenarios by directly manipulating the probabilities
                  used in the model"
                )
              ),
              shiny::br(),
              # Adding Logos
              shiny::img(
                src="https://www.nationalarchives.gov.uk/wp-content/uploads/2019/06/TNA-SQUARE-LOGO-POSITIVE-01-720x720.jpg",
                height=100,
                width=100
              ),
              shiny::img(
                src='https://www.underconsideration.com/brandnew/archives/university_of_warwick_logo_detail.png',
                height=80,
                width=120
              ),
              shiny::img(
                src="https://www.heritagefund.org.uk/sites/default/files/media/attachments/English%20logo%20-%20Colour%20%28JPEG%29.jpg",
                height=80,
                width=216
              ),
              shiny::img(src="UKRI_EPSR_Council-Logo_Horiz-RGB.png", height=75)),
            shinydashboard::box(
              width = 12,
              shiny::h3("Guidance"),
              shiny::br(),
              shiny::p(
                shiny::tags$b("Definitions"),
                ": This page has a visualisation of the underlying network of digital preservation risks and
                allows you to see the full definitions, states and data sources used for each 'node'."
              ),
              shiny::p(
                shiny::tags$b("1. Create your model"),
                ": This goes through 9 questions to create a risk model and a score which is
                based on the user's archive and policies."
              ),
              shiny::p(
                shiny::tags$b("Recommendations"),
                ": This page looks at the impact changing each of the answers to the input questions would
                have to the risk score."
              ),
              shiny::p(
                shiny::tags$b("2. Compare policies"),
                ": Create and save different policies and see how the risk score changes."
              ),
              shiny::p(
                shiny::tags$b("3. Advanced customisation"),
                ": This tab allows users to edit the marginal and conditional probabilities
                in the model directly. This allows for users to input their own data for any nodes within the model 
                or create scenarios by altering conditional probabilities."
              ),
              shiny::p(
                shiny::tags$b("4. Report"),
                ": This contains a summary and comparison of the policies for each model, and allows 
                the model and plots to be downloaded."
              )
              #br(),
              #p("If you have further questions, please contact the workshop facilitator.")
            )
          )
        ),
        
        # Network Tab
        shinydashboard::tabItem(
          tabName="Node_definitions",
          shinyalert::useShinyalert(),
          shinydashboard::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
              project progresses. Any feedback to inform the future development would be welcome 
              - please send your comments to a member of the project team."
            )
          ),
          shiny::h1("Definitions"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width=4,
              shinydashboard::box(
                title="Node selection",
                width=NULL,
                collapsible=TRUE,
                shiny::h4("Please select a node from the menu to view its definition."),
                shiny::br(),
                shiny::br(),
                shiny::tags$style(
                  type='text/css',
                  ".selectize-input { font-size: 15px; line-height: 15px;} 
                  .selectize-dropdown { font-size: 15px; line-height: 15px; }"
                ),
                shiny::selectInput(
                  inputId="NodeSelection",
                  label=NULL,
                  choices="No Nodes Available"
                )
              )
            ),
            shiny::column(
              width=8,
              shinydashboard::box(
                title="Node description",
                width=NULL,
                collapsible=TRUE,
                shiny::column(
                  width=6,
                  shiny::uiOutput("NodeDefinition"),
                  shiny::br(),
                  shiny::uiOutput("DataLink"),
                  shiny::br(),
                  shiny::uiOutput("DataYear")
                ),
                shiny::column(
                  width=6,
                  shiny::tableOutput("StateDefinition")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width=12,
              shinydashboard::box(
                title="DiAGRAM structure",
                collapsible=TRUE,
                width=NULL,
                shiny::plotOutput("NetworkStructure")
              )
            )
          )
        ),
        # Policy Tab
        shinydashboard::tabItem(
          tabName="CustomiseModel",
          shinyalert::useShinyalert(),
          shinydashboard::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
                project progresses. Any feedback to inform the future development would be welcome 
                - please send your comments to a member of the project team."
            )
          ),
          shiny::h1("Create your model"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width=12,
              shinydashboard::box(
                title=NULL,
                width=NULL,
                shinyWidgets::progressBar("Question_Progress", value=1, total=nquestions),
                #h4("Please answer the following question: "),
                shiny::uiOutput("Question"),
                shinyjs::useShinyjs(),
                shiny::br(),
                shiny::uiOutput("CustomisationInput")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width=8,
              shinydashboard::box(
                title="Model risk score",
                width=NULL,
                collapsible=TRUE,
                shiny::plotOutput("BasicUtilityComparison")
              )
            ),
            shiny::column(
              width=4,
              shinydashboard::box(
                title="Upload a previous model",
                collapsible=TRUE,
                width=NULL,
                shiny::strong("Please ensure any models uploaded have been generated from DiAGRAM"),
                shiny::br(),
                shiny::br(),
                shiny::fileInput(
                  "customModel",
                  "Choose custom model",
                  accept=c(".bif")
                ),
                shiny::textInput("uploadName", label="Custom Model Name"),
                shiny::tags$style(shiny::HTML('#uploadCustomModel{background-color:green}')),
                shiny::tags$style(shiny::HTML('#uploadCustomModel{color:white}')),
                shiny::actionButton("uploadCustomModel", "Add model")
              )
            )
          )
        ),
        
        # TODO:sid - change policyTab identifier to the most appropriate (once decided)
        shiny::dashboard::tabItem(
          tabName="CustomiseNode",
          shinyalert::useShinyalert(),
          shinydashboard::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
              project progresses. Any feedback to inform the future development would be welcome 
              - please send your comments to a member of the project team."
            )
          ),
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
                shiny::tags$style(shiny::HTML('#SimpleViewPolicyPrevious{background-color:grey}')),
                shiny::tags$style(shiny::HTML('#SimpleViewPolicyPrevious{color:white}')),
                shiny::actionButton("SimpleViewPolicyPrevious", label = "Previous"),
                shiny::tags$style(shiny::HTML('#SimpleViewPolicyNext{background-color:green}')),
                shiny::tags$style(shiny::HTML('#SimpleViewPolicyNext{color:white}')),
                shiny::actionButton("SimpleViewPolicyNext", label = "Next"),
                shiny::br()
              ),
              shinydashboard::box(
                id="SimpleViewPolicyAddBox",
                title=NULL,
                width=NULL,
                shiny::textInput("SimpleViewPolicyName", label = shiny::h4("Enter policy name"), value = ""),
                shiny::tags$style(shiny::HTML('#SimpleViewAddPolicy{background-color:green}')),
                shiny::tags$style(shiny::HTML('#SimpleViewAddPolicy{color:white}')),
                shiny::actionButton("SimpleViewAddPolicy", "Add policy")
              ),
              shiny::tags$style(shiny::HTML('#SimplePolicyReset{background-color:gray}')),
              shiny::tags$style(shiny::HTML('#SimplePolicyReset{color:white}')),
              shiny::div(shiny::actionButton('SimplePolicyReset', 'Reset model'), style="float:right")
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
                shiny::tags$style(shiny::HTML('#RemovePolicy{background-color:red}')),
                shiny::tags$style(shiny::HTML('#RemovePolicy{color:white}')),
                shiny::div(shiny::actionButton('RemovePolicy', 'Remove'), style="float:right")
              )       
            )
          )),
        
        # Policy Tab
        shinydashboard::tabItem(
          tabName="AdvancedCustomiseNode",
          shinyalert::useShinyalert(),
          shinydashbaord::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
              project progresses. Any feedback to inform the future development would be welcome 
              - please send your comments to a member of the project team."
            )
          ),
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
                  shiny::tags$style(shiny::HTML('#networkReset{background-color:gray}')),
                  shiny::tags$style(shiny::HTML('#networkReset{color:white}')),
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
                    shiny::tags$style(shiny::HTML('#updateProb{background-color:green}')),
                    shiny::tags$style(shiny::HTML('#updateProb{color:white}')),
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
                  shiny::tags$style(shiny::HTML('#addPolicy{background-color:green}')),
                  shiny::tags$style(shiny::HTML('#addPolicy{color:white}')),
                  shiny::actionButton("addPolicy", "Add as policy"),
                  shiny::tags$style(shiny::HTML('#addModelAdvanced{background-color:green}')),
                  shiny::tags$style(shiny::HTML('#addModelAdvanced{color:white}')),
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
          shinydashboard::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
              project progresses. Any feedback to inform the future development would be welcome 
              - please send your comments to a member of the project team."
            )
          ),
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
                shiny::tags$style(shiny::HTML('#Download{background-color:green}')),
                shiny::tags$style(shiny::HTML('#Download{color:white}')),
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
          shinydashboard::box(
            title = NULL,
            width = 12,
            background="orange",
            shiny::h3(shiny::strong("Important note: This model is still in development")),
            shiny::p(
              "There will be further user interface changes and additional functionality added as the 
              project progresses. Any feedback to inform the future development would be welcome 
              - please send your comments to a member of the project team."
            )
          ),
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
                shiny::plotlyOutput("SensitivityPlot")
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
}


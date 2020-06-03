
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

options(repos = BiocManager::repositories())
nquestions <- read_csv("setup_questions.csv") %>% nrow()

# create main dashboard page
dashboardPage(
  skin="purple",
  
  # Add header and title to Dashboard
  dashboardHeader(
    title="DiAGRAM"
  ),
  
  # Add dashboard sidebar
  dashboardSidebar(
    
    # Create sidebar menu
    sidebarMenu(
      id="sidebarMenu",
      
      # Create home page
      menuItem(
        "Home",
        tabName="Home",
        icon=icon("home")
      ),
      
      # Create Network Info Page
      menuItem("Definitions",
               tabName="Node_definitions",
               icon=icon("info")),
      
      # Customise model tab
      menuItem("1. Create your model",
               tabName="CustomiseModel",
               icon=icon("user-edit")),
      
      # Simple View Page
      menuItem("2. Compare policies",
               tabName = "CustomiseNode",
               icon=icon("chart-bar")),
      
      # Advanced Page
      menuItem("3. Advanced customisation", 
                 tabName = "AdvancedCustomiseNode",
                 icon=icon("project-diagram")),
      
      # Create Report Tab
      menuItem("4. Report",
               tabName="Report",
               icon=icon("book"))
    )
  ),
  
  # Create Dashboard body
  dashboardBody(
    id="dashboardBody",
    
    # include introjs UI
    introjsUI(),
    
    # create content for tabs
    tabItems(
      
      # Home Tab
      tabItem(
        tabName="Home",
        
        shiny::fluidRow(
          
          # Welcome box
          shinydashboard::box(
            title = "Welcome",
            width = 12,
            shiny::h2("Version 0.9.1"), #update in May
            br(),
            p("This is the prototype version of the Digital Archiving Graphical 
              Risk Assessment Model built by the ",
              a(href="https://warwick.ac.uk", "University of Warwick"),
              " and ",
              a(href="https://www.nationalarchives.gov.uk"," The National Archives"), 
              "with suport from the ",
              a(href="https://www.heritagefund.org.uk/", "National Lottery Heritage Fund.")),
            p("This decision support tool enables users to score their Archive's
            digital presservation risk and then explore how this would change under
            different policies and risk scenarios. The risk score is based on the proportion of 
              files in the archive that are renderable and where the archivist can have full
              intellectual control."),
            p("The underlying methodology used to create this model is based on a Bayesian network
            - a probabilistic graphical model that captures the conditional dependencies of risk 
            events. When historical data was unavailable, data from an expert elicitation 
            session conducted in April 2020 has been used to inform the probabilities needed for
            this model. For more information about the statistical techniques used, please see 
              the supporting documentation."),
            p("This interface supports the users to complete the following tasks:"),
            tags$ul(
              tags$li("Understand the risk definitions used in the model and 
                      how the risk events are linked together"),
              tags$li("Create a model that relects the policies and practises for your 
                      Digital Archive"),
              tags$li("Test alternative policies to see how this impacts your risk score"),
              tags$li("Download your model and a summary of the results"),
              tags$li("Upload a pre-built model and continue exploring scenarios from there"),
              tags$li("Update the probability tables for your model based on your own data or 
                      experience"),
              tags$li("Create bespoke scenarios by directly manipulating the probabilities
              used in the model")
            ),
            br(),
            # Adding National Archives and University of Warwick Logos
            img(src="http://www.nationalarchives.gov.uk/wp-content/uploads/2019/06/TNA-SQUARE-LOGO-POSITIVE-01-720x720.jpg",
                height=100,
                width=100),
            img(src='https://www.underconsideration.com/brandnew/archives/university_of_warwick_logo_detail.png',
                height=80,
                width=120),
            img(src="https://www.heritagefund.org.uk/sites/default/files/media/attachments/English%20logo%20-%20Colour%20%28JPEG%29.jpg",
                height=80,
                width=216)
            
          )
        )
      ),
      
      # Network Tab
      tabItem(
        tabName="Node_definitions",
        h1("Definitions"),
        br(),
        fluidRow(
          column(
            width=4,
            box(
              title="Node selection",
              width=NULL,
              collapsible=TRUE,
              h4("Please select a node from the menu to view its definition."),
              br(),
              br(),
              tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} 
                     .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
              selectInput(
                inputId="NodeSelection",
                label=NULL,
                choices="No Nodes Available"
              )
            )
          ),
          column(
            width=8,
            box(
              title="Node description",
              width=NULL,
              collapsible=TRUE,
              column(
                width=6,
                uiOutput("NodeDefinition"),
                br(),
                uiOutput("DataLink"),
                br(),
                uiOutput("DataYear")
              ),
              column(
                width=6,
                tableOutput("StateDefinition")
              )
            )
          )
        ),
        fluidRow(
          column(width=12,
                 box(
                   title="DiAGRAM structure",
                   collapsible=TRUE,
                   width=NULL,
                   plotOutput("NetworkStructure")
                 )
          )
        )
      ),
      
      # Policy Tab
      tabItem(
        tabName="CustomiseModel",
        h1("Create your model"),
        br(),
        fluidRow(
          column(
            width=12,
            box(
              title=NULL,
              width=NULL,
              progressBar("Question_Progress", value=0, total=nquestions),
              h3("Please answer the following question: "),
              uiOutput("Question"),
              useShinyjs(),
              br(),
              uiOutput("CustomisationInput")
            )
          )
        ),
        fluidRow(
          column(
            width=8,
            box(
              title="Model risk score",
              width=NULL,
              collapsible=TRUE,
              plotOutput("BasicUtilityComparison")
            )
          ),
          column(
            width=4,
            box(
              title="Upload a previous model",
              collapsible=TRUE,
              width=NULL,
              strong("Please ensure any models uploaded have been generated from DiAGRAM"),
              br(),
              br(),
              fileInput("customModel",
                        "Choose custom model",
                        accept=c(".bif")),
              textInput("uploadName",
                        label="Custom Model Name"),
              tags$style(HTML('#uploadCustomModel{background-color:green}')),
              tags$style(HTML('#uploadCustomModel{color:white}')),
              actionButton("uploadCustomModel",
                           "Add model")
            )
          )
        )
      ),
      
      # TODO:sid - change policyTab identifier to the most appropriate (once decided)
      tabItem(
        useShinyalert(),
        tabName="CustomiseNode",
        h1("Create and compare different policies"),
        br(),
        div(
          selectInput("customModelSelection",
                      h3("Select model"),
                      choices = "Default")
        ),
        fluidRow(
          column(
            width = 3,
            selectInput("customOaisEntitySelection",
                        h5("Select OAIS Function Entity"),
                        choices="None", 
                        multiple = TRUE),
            box(
              title="Nodes checklist",
              width=NULL,
              checkboxGroupInput("policyTabNodesChecklist", 
                                 label=NULL,
                                 choices=character(0))
            )
          ),
          column(
            width=4,
            useShinyjs(),
            box(
              id="StateNodeSliderBox",
              title=NULL,
              width=NULL,
              div(id="nodeSliderPlaceholder", h4('No nodes selected')),
              uiOutput("policyTabNodesSlider"),
              tags$style(HTML('#SimpleViewPolicyPrevious{background-color:grey}')),
              tags$style(HTML('#SimpleViewPolicyPrevious{color:white}')),
              actionButton("SimpleViewPolicyPrevious", label = "Previous"),
              tags$style(HTML('#SimpleViewPolicyNext{background-color:green}')),
              tags$style(HTML('#SimpleViewPolicyNext{color:white}')),
              actionButton("SimpleViewPolicyNext", label = "Next"),
              br()
            ),
            box(
              id="SimpleViewPolicyAddBox",
              title=NULL,
              width=NULL,
              textInput("SimpleViewPolicyName", label = h4("Enter policy name"), value = ""),
              tags$style(HTML('#SimpleViewAddPolicy{background-color:green}')),
              tags$style(HTML('#SimpleViewAddPolicy{color:white}')),
              actionButton("SimpleViewAddPolicy", "Add policy")
            ),
              tags$style(HTML('#SimplePolicyReset{background-color:gray}')),
               tags$style(HTML('#SimplePolicyReset{color:white}')),
               div(actionButton('SimplePolicyReset', 'Reset model'), style="float:right")
          ),
          column(
            width=5,
            plotOutput("policyTabUtilityScorePlot")
          )
        )
      ),
      
      # Policy Tab
      tabItem(
        tabName="AdvancedCustomiseNode",
        h1("Advanced model customisation"),
        br(),
        fluidRow(
          width=8,
          fluidRow(
            column(width=12,
                   # Plot the bayesian network
                   box(
                     title="DiAGRAM structure",
                     collapsible=TRUE,
                     width=NULL,
                     plotOutput("netPlot")
                   )
            )
          ),
          fluidRow(
            column(
              width=4,
              box(
                width=NULL,
                selectInput("model_version",
                            h3("Select Model"),
                            choices="Default"),
                selectInput("nodeProbTable",
                            h4("Select Node"),
                            choices=c("nodes loading")),
                tags$style(HTML('#networkReset{background-color:gray}')),
                tags$style(HTML('#networkReset{color:white}')),
                actionButton('networkReset','Reset model')
              )
            ),
            column(
              width=8,
              box(
                title="Probability table",
                width=NULL,
                column(
                  width=4,
                  radioButtons("probtabltype",
                               "Table Type",
                               choices=c("Independent Probability Table",
                                         "Conditional Probability Table")),
                  tags$style(HTML('#updateProb{background-color:green}')),
                  tags$style(HTML('#updateProb{color:white}')),
                  actionButton("updateProb", "Add changes")
                  
                ),
                column(
                  width=8,
                  hotable("probabilityTable")
                )

              )
            )
          ),
          fluidRow(
            column(
              width=4,
              box(
                title="Changed nodes",
                width=NULL,
                collapsible=TRUE,
                tags$ul(
                  uiOutput("ChangeNodes")
                )
              ),
              box(
                title="Save model",
                width=NULL,
                collapsible=TRUE,
                textInput("policyName",
                          label="Modified model name:",
                          value=""),
                tags$style(HTML('#addPolicy{background-color:green}')),
                tags$style(HTML('#addPolicy{color:white}')),
                actionButton("addPolicy",
                             "Add as policy"),
                tags$style(HTML('#addModelAdvanced{background-color:green}')),
                tags$style(HTML('#addModelAdvanced{color:white}')),
                actionButton("addModelAdvanced",
                             "Add as custom model")
              )
            ),
            column(
              width=8,
              box(
                title="Node probability",
                width=NULL,
                collapsible=TRUE,
                plotOutput("nodeProbability")
              )
            )
          ),
          fluidRow(
            column(
              width=6,
              box(
                title="Policy comparison",
                width=NULL,
                plotOutput("PolicyComparison")
              )
            ),
            column(
              width=6,
              box(
                title="Model comparison",
                width=NULL,
                plotOutput("BaseUtilityComparison")
              )
            )
          )
        )
      ),
      tabItem(
        tabName="Report",
        h1("Report"),
        br(),
        div(
          selectInput(
            "reportTabModelSelection",
            h3("Select model"),
            choices ="Default")
        ),
        fluidRow(
          column(
            width=6,
            box(
              title="Summary",
              width=NULL,
              htmlOutput("ReportTabSummaryText")
            ),
            box(
              title="Utility weightings",
              width=NULL,
              collapsible = TRUE,
              collapsed = TRUE,
              sliderInput(inputId="RenderabilityWeighting",
                          label="Renderability weighting",
                          min=0,
                          max=1,
                          value=1,
                          step=0.1),
              sliderInput(inputId="IntellectualWeighting",
                          label="Intellectual control weighting",
                          min=0,
                          max=1,
                          value=1,
                          step=0.1)
            ),
            box(
              title="Download model",
              width=NULL,
              selectInput("ReportTabPolicySelection",
                          h5("Select policy"),
                          choices="No policies added"),
              br(),
              "Select what you would like to download:",
              br(),
              br(),
              checkboxGroupInput("downloadOptions",
                                 NULL,
                                 choices=c("Policy comparison plot",
                                           "This policy model"
                                           #,"Documented Report"
                                           )),
              br(),
              tags$style(HTML('#Download{background-color:green}')),
              tags$style(HTML('#Download{color:white}')),
              downloadButton("reportTabDownloadBtn",
                             "Download")
              
            ),
          ),
          column(
            width=6,
            box(
              title="Comparing Policies",
              width=NULL,
              plotOutput("ReportTabUtilityComparisonPlot")
            )
          )
        )
      )
    )
  )
)

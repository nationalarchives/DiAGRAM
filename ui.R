
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

options(repos = BiocManager::repositories())

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
      menuItem("Node Definitions",
               tabName="Node_Definitions",
               icon=icon("globe")),
      
      # Customise model tab
      menuItem("1. Customise Model",
               tabName="CustomiseModel"),
      
      # Simple View Page
      menuItem("2. Policy",
               tabName = "PolicyView",
               menuSubItem("Simple Customisation", 
                           tabName = "CustomiseNode"),
               menuSubItem("Advanced Customisation", 
                           tabName = "AdvancedCustomiseNode")
      ),
      
      # Create Report Tab
      menuItem("Report",
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
            shiny::h2("Version 0.8.0"),
            br(),
            p("This an initial version of the decision support system
              built by the ",
              a(href="https://warwick.ac.uk", "University of Warwick"),
              " and ",
              a(href="https://www.nationalarchives.gov.uk"," The National Archives.")),
            p("This decision support tools enables users to compare different policies
              used in the preservation of digital assets."),
            p("This interface enbables users to complete the following tasks:"),
            tags$ul(
              tags$li("View Bayesian Network"),
              tags$li("Adjust Probabilities in Bayesian Network"),
              tags$li("Compare Different Policies"),
              tags$li("Save Policies and Bayesian Networks"),
              tags$li("Upload pre-built Bayesian Networks and Policies")
            ),
            br(),
            # Adding National Archives and University of Warwick Logos
            img(src="http://www.nationalarchives.gov.uk/wp-content/uploads/2019/06/TNA-SQUARE-LOGO-POSITIVE-01-720x720.jpg",
                height=100,
                width=100),
            img(src='https://www.underconsideration.com/brandnew/archives/university_of_warwick_logo_detail.png',
                height=80,
                width=120)
            
          )
        )
      ),
      
      # Network Tab
      tabItem(
        tabName="Node_Definitions",
        h1("Node Definitions"),
        br(),
        fluidRow(
          column(
            width=4,
            box(
              title="Node Selection",
              width=NULL,
              collapsible=TRUE,
              strong("Please select a node from the menu to view its definition."),
              br(),
              br(),
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
              title="Node Description",
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
                   title="Network Structure",
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
        h1("Customise Model"),
        br(),
        fluidRow(
          column(
            width=12,
            box(
              title=NULL,
              width=NULL,
              progressBar("Question_Progress", value=0, total=5),
              h3("Please answer the following questions: "),
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
              title="Utility Plot",
              width=NULL,
              collapsible=TRUE,
              plotOutput("BasicUtilityComparison")
            )
          ),
          column(
            width=4,
            box(
              title="Upload Custom Model",
              collapsible=TRUE,
              width=NULL,
              strong("Please ensure any models uploaded have been generated from DiAGRAM"),
              br(),
              br(),
              fileInput("customModel",
                        "Choose Custom Model",
                        accept=c(".bif")),
              textInput("uploadName",
                        label="Custom Model Name"),
              tags$style(HTML('#uploadCustomModel{background-color:green}')),
              tags$style(HTML('#uploadCustomModel{color:white}')),
              actionButton("uploadCustomModel",
                           "Add Model")
            )
          )
        )
      ),
      
      # TODO:sid - change policyTab identifier to the most appropriate (once decided)
      tabItem(
        useShinyalert(),
        tabName="CustomiseNode",
        h1("Policy Selection Support"),
        br(),
        fluidRow(
          column(
            width = 3,
            selectInput("customModelSelection",
                        "Select Model",
                        choices="TNA")
          ),
          column(
            width = 3,
            selectInput("customOaisEntitySelection",
                        "Select OAIS Function Entity",
                        choices="None", 
                        multiple = TRUE)
          )
        ),
        fluidRow(
          column(
            width=3,
            box(
              title="Nodes Checklist",
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
              tags$style(HTML('#SimpleViewPolicyNext{background-color:green}')),
              tags$style(HTML('#SimpleViewPolicyNext{color:white}')),
              actionButton("SimpleViewPolicyNext", label = "Next"),
              tags$style(HTML('#SimpleViewPolicyPrevious{background-color:grey}')),
              tags$style(HTML('#SimpleViewPolicyPrevious{color:white}')),
              actionButton("SimpleViewPolicyPrevious", label = "Previous"),
              br()
            ),
            box(
              id="SimpleViewPolicyAddBox",
              title=NULL,
              width=NULL,
              textInput("SimpleViewPolicyName", label = h4("Enter policy name"), value = ""),
              tags$style(HTML('#SimpleViewAddPolicy{background-color:green}')),
              tags$style(HTML('#SimpleViewAddPolicy{color:white}')),
              actionButton("SimpleViewAddPolicy", "Add Policy")
            )
          ),
          column(
            width=5,
            plotOutput("policyTabUtilityScorePlot")
          )
        )
        # sidebarLayout(
        #   sidebarPanel(h3("Nodes"), 
        #                uiOutput("policyTabNodes")),
        #   mainPanel(h3("Network"),
        #             plotOutput("policyTabNetwork"))
        # )
        
      ),
      
      # Policy Tab
      tabItem(
        tabName="AdvancedCustomiseNode",
        h1("Policy Selection"),
        br(),
        fluidRow(
          width=8,
          fluidRow(
            column(width=12,
                   # Plot the bayesian network
                   box(
                     title="Network",
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
                            "Select Model",
                            choices=c("TNA")),
                selectInput("nodeProbTable",
                            "Select Node",
                            choices=c("nodes loading")),
                tags$style(HTML('#networkReset{background-color:gray}')),
                tags$style(HTML('#networkReset{color:white}')),
                actionButton('networkReset',
                             'Reset Model')
              )
            ),
            column(
              width=8,
              box(
                title="Probability Table",
                width=NULL,
                column(
                  width=4,
                  radioButtons("probtabltype",
                               "Table Type",
                               choices=c("Independent Probability Table",
                                         "Conditional Probability Table")),
                  tags$style(HTML('#updateProb{background-color:green}')),
                  tags$style(HTML('#updateProb{color:white}')),
                  actionButton("updateProb",
                               "Add Changes")
                  
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
                title="Changed Nodes",
                width=NULL,
                collapsible=TRUE,
                tags$ul(
                  uiOutput("ChangeNodes")
                )
              ),
              box(
                title="Save Network",
                width=NULL,
                collapsible=TRUE,
                textInput("policyName",
                          label="Modified network name:",
                          value=""),
                tags$style(HTML('#networkUpdate{background-color:green}')),
                tags$style(HTML('#networkUpdate{color:white}')),
                actionButton("networkUpdate",
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
                title="Node Probability",
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
                title="Policy Comparison",
                width=NULL,
                plotOutput("PolicyComparison")
              )
            ),
            column(
              width=6,
              box(
                title="Model Base Utility Comparison",
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
            "Select Model",
            choices = "TNA"
          )
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
              title=NULL,
              width=NULL,
              selectInput("ReportTabPolicySelection",
                          "Select Policy",
                          choices="No policies added"),
              br(),
              "Select what you would like to download:",
              br(),
              br(),
              checkboxGroupInput("downloadOptions",
                                 NULL,
                                 choices=c("Archive Model Utility Comparison Plot",
                                           "Policy Model"
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
              title="Utility Comparison",
              width=NULL,
              plotOutput("ReportTabUtilityComparisonPlot")
            )
          )
          # column(
          #   width=4,
          #   box(
          #     title="Summary",
          #     width=NULL,
          #     htmlOutput("ReportTabSummaryText")
          #   )
          # ),
          # column(
          #   width=8,
          #   box(
          #     title="Utility Comparison",
          #     width=NULL,
          #       plotOutput("ReportTabUtiltiyComparisonPlot")
          #   )
          # ),
          
        ),
        # fluidRow(
        #   column(
        #     width=4,
        #     box(
        #       title=NULL,
        #       width=NULL,
        #       selectInput("ReportTabPolicySelection",
        #                   "Select Policy",
        #                   choices="No policies added")
        #     ),
        #     box(
        #       title="Download",
        #       width=NULL,
        #       "Select what you would like to download:",
        #       br(),
        #       br(),
        #       checkboxGroupInput("downloadOptions",
        #                          NULL,
        #                          choices=c("Model",
        #                                    "Model Plot",
        #                                    "Utility Plot",
        #                                    "Policy Summary")),
        #       br(),
        #       tags$style(HTML('#Download{background-color:green}')),
        #       tags$style(HTML('#Download{color:white}')),
        #       downloadButton("Download",
        #                      "Download")
        #       
        #     )
        #   ),
        #   column(
        #     width=8,
        #     box(
        #       title="Model",
        #       width=NULL,
        #       plotOutput("ReportModel")
        #     )
        #   )
        # )
      )
    )
  )
)

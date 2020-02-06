
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
      
      # Simple View Page
      menuItem("Simple View",
               tabName = "SimpleView",
               menuSubItem("Customize Nodes", 
                        tabName = "CustomizeNode")
               ),
      
      
      # Create Network ajdustment page
      menuItem("Policies",
               tabName="Policies",
               icon=icon("calculator")),
      
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
        tabName="CustomizeNode",
        h1("Policy Selection Support"),
        br(),
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
            width=6,
            uiOutput("policyTabNodesSlider")
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
        tabName="Policies",
        h1("Policy Selection"),
        br(),
        fluidRow(
          column(
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
              column(width=6,
                     box(
                       title="Pollution",
                       hotable("pollutionHotable"),
                       width=NULL,
                       collapsible=TRUE,
                       collapsed=FALSE
                     )
              ),
              column(width=6,
                     box(
                       title="Smoker",
                       hotable("smokerHotable"),
                       width=NULL,
                       collapsible=TRUE,
                       collapsed=FALSE
                     )
              )
            ),
            fluidRow(
              column(width=12,
                     box(
                       title="Cancer",
                       radioButtons("CancerProbTable",
                                    "Cancer Probability Table",
                                    c("Independent Probability Table",
                                      "Conditional Probability Table"),
                                    selected="Independent Probability Table"),
                       hotable("cancerHotable"),
                       width=NULL,
                       collapsible=TRUE,
                       collapsed=FALSE
                     )
              )
            ),
            fluidRow(
              column(
                width=4,
                textInput("policyName",
                          label=NULL,
                          value="Enter Policy Name...")
              ),
              column(width=2,
                     style='padding:0px;',
                     tags$style(HTML('#networkUpdate{background-color:green}')),
                     tags$style(HTML('#networkUpdate{color:white}')),
                     actionButton("networkUpdate",
                                  "Add Policy", 
                                  width='100%')
              ),
              column(
                width=2,
                offset=4,
                tags$style(HTML('#networkReset{width: 100%')),
                actionButton('networkReset',
                             'Reset')
              )
            )
          ),
          column(
            width=4,
            box(
              title="Policy Summary",
              width=NULL,
              collapsible=TRUE,
              plotOutput("utilityComparison")
            )
          )
        )
      ),
      tabItem(
        tabName="Report",
        h1("Report"),
        br(),
        fluidRow(
          column(
            width=4,
            box(
              title="Summary",
              width=NULL,
              textOutput("TextReport")
            )
          ),
          column(
            width=8,
            box(
              title="Utility Comparison",
              width=NULL,
              plotOutput("utilityComparisonFinal")
            )
          )
        ),
        fluidRow(
          column(
            width=4,
            box(
              title=NULL,
              width=NULL,
              selectInput("policySelection",
                          "Select Policy",
                          choices="No policies added")
            ),
            box(
              title="Download",
              width=NULL,
              "Select what you would like to download:",
              br(),
              br(),
              checkboxGroupInput("downloadOptions",
                                 NULL,
                                 choices=c("Model",
                                           "Model Plot",
                                           "Utility Plot",
                                           "Policy Summary")),
              br(),
              tags$style(HTML('#Download{background-color:green}')),
              tags$style(HTML('#Download{color:white}')),
              downloadButton("Download",
                             "Download")
              
            )
          ),
          column(
            width=8,
            box(
              title="Model",
              width=NULL,
              plotOutput("ReportModel")
            )
          )
        )
      )
    )
  )
)

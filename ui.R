
# Following script defines components displayed on the webpage.
# 
# This script belongs to the decision support system for preserving
# digital files built by the University of Warwick and The National
# Archive.
# 
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
      menuItem("Network",
               tabName="Network",
               icon=icon("globe")),
      
      
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
            shiny::h2("Version 0.6.1"),
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
        tabName="Network",
        h1("Network Structure"),
        br(),
        fluidRow(
          column(width=4,
                 box(
                   title="Pollution",
                   width=NULL,
                   collapsible =TRUE,
                   "This node relates to the air quality of
                    the subjects environment. Pollution is considered",
                   strong("high"),
                   " if the air quality index is above ",
                   em("151"),
                   " over ",
                   em("50%"),
                   " of the time.",
                   br(),
                   br(),
                   "The data used in estimating these probabilities
                    was collected by the World Health Organisation
                    and is stored in the ",
                  a(href="https://www.who.int/airpollution/data/cities/en/",
                    "Global Ambient Air Quality Database."),
                  " This database was last updated in 2018."
                 )
          ),
          column(width=4,
                 box(
                   title="Smoker",
                   width=NULL,
                   collapsible=TRUE,
                   "This node relates to the smoking status of a subject. A
                    subject is considered a ",
                    strong("smoker"),
                    " if they have smoked at least ",
                   em("100"),
                   " cigarettes in their lifetime and currently smoke.",
                   br(),
                   br(),
                   "The data used in estimating these probabilities was provided
                    by the UK governemnt and can be found ",
                   a(href="https://data.gov.uk/dataset/641597ce-17a1-4056-8c55-1a619cdc57c2/statistics-on-smoking-england",
                     "here"),
                   ". It was last updated in 2017."
                 )
          ),
          column(width=4,
                 box(
                   title="Cancer",
                   width=NULL,
                   collapsible=TRUE,
                   "This node relates to whether a subject as cancer or not. A subject is
                    is considered to ",
                    strong("have"),
                    " cancer if a qualified medical professional has diagnosed them with cancer.",
                   br(),
                   br(),
                   "The data used for this node was collected by ",
                   a(href="https://www.cancerdata.nhs.uk",
                     "The National Cancer Registration and Analysis Service."),
                   "It was last updated in 2018."
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
        ),
        fluidRow(
          column(width=6,
                 box(
                   title="Xray",
                   width=NULL,
                   collapsible=TRUE,
                   "This node represents the probability that an X-ray will come back
                    as positive for cancer given the probability of cancer. An X-ray test
                    is considered ",
                   strong("positive"),
                   "if a medical professional identifies that a subject has cancer given
                    the images.",
                   br(),
                   br(),
                   "The data used for this node was collected by ",
                   a(href="https://www.cancerdata.nhs.uk",
                     "The National Cancer Registration and Analysis Service."),
                   "It was last updated in 2018."
                 )
          ),
          column(width=6,
                 box(
                   title="Dyspnoea",
                   width=NULL,
                   collapsible=TRUE,
                   "This node represents the probability that a subject will have Dysnoea
                    given the probability that the subject has cancer. A subject is considered
                    to ",
                   strong("have"),
                   " Dyspnoea if a medical professional diagnoses the subject as having the condition.",
                   br(),
                   br(),
                   "The data used for this node was provided by the UK government and can be found ",
                   a(href="https://data.gov.uk/dataset/a3790bd8-8813-4ab0-a1b1-eff0af3ae9a1/people-with-chronic-obstructive-pulmonary-disease-and-medical-research-council-dyspnoea-scale-3-referred-to-a-pulmonary-rehabilitation-programme-ccgois-2-3",
                     "here."),
                   "This data was last updated in 2017."
                 )
          )
        )
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

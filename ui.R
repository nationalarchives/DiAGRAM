
# Following script defines components displayed on the webpage.
# 
# This script belongs to the decision support system for preserving
# digital files built by the University of Warwick and The National
# Archive.
# 
# @author: Stephen James Krol, University of Monash, Melbourne
# @email: stephen.james.krol@gmail.com

library(shiny)
library(rintrojs)
library(networkD3)
library(shinydashboard)

# create main dashboard page
dashboardPage(
  skin="purple",
  
  # Add header and title to Dashboard
  dashboardHeader(
    title="TNA Decision Support Tool"
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
      
      # Create Network visualisation page
      menuItem("Structure",
               tabName="Structure",
               icon=icon("globe"))
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
            shiny::h2("Decision Support System"),
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
      
      tabItem(
        tabName="Structure",
        column(width=8,
               
               box(
                 title="Network",
                 collapsible=TRUE,
                 width=NULL,
                 
                 simpleNetworkOutput("netPlot")
               ))
      )
    )
  )
)

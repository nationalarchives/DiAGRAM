
# Following script defines interactions between components displayed 
# on the webpage.
# 
# This script belongs to the decision support system for preserving
# digital files built by the University of Warwick and The National
# Archive.
# 
# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com

library(shiny)
library(graph)
library(bnlearn)
library(networkD3)
library(BiocManager)
library(Rgraphviz)
library(tidyverse)

options(repos = BiocManager::repositories())

shinyServer(function(input, output, session) {
  
  # initialise stable plot (unchanging) and reactive plot
  network <- reactiveValues(cancer.fit = read.bif("cancer.bif"))
  stable.fit <- read.bif("cancer.bif")
  
  # initialise policy list used for storing adjusted networks
  # policy_networks <- list()
  
  # itialise utility dataframe
  Utility <- reactiveValues(utility.df=tibble(name=character(),
                                              utility=numeric()),
                            policy_networks=list())
  
  # plot network used on the structure tab
  # TODO: Work out how to prevent first plot not drawing properly
  output$NetworkStructure <- renderPlot({
    
    first <- graphviz.chart(stable.fit, type = "barprob", grid=TRUE, main="Cancer Network")
    graphviz.chart(stable.fit, type = "barprob", grid=TRUE, main="Cancer Network")
    
  })
  
  # Plot network which changes for policy inputs
  output$netPlot <- renderPlot({
    
    first <- graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    
  })
  
  # Plot network for report page
  output$ReportModel <- renderPlot({
    
    model.fit <- Utility$policy_networks[[input$policySelection]]
    first <- graphviz.chart(model.fit, type = "barprob", grid=TRUE, main="Test Network")
    graphviz.chart(model.fit, type = "barprob", grid=TRUE, main="Test Network")
    
  })
  
  # Construct smoker probability table
  smoker <- reactive({
      # Convert to dataframe and multiply probability by 100
      # so it is more intuitive to non-statistical users
      smoker.df <- data.frame(network$cancer.fit$Smoker$prob) %>%
                   rename(Probability=Freq)
      
      if ("Var1" %in% colnames(smoker.df)) {
        
        # When loaded var1 is originally given to smoker variables
        # Once changed it does not revert back which can cause renaming errors
        # Program cannot find Var1 because it has already been changed to smoker
        smoker.df <- smoker.df %>% rename(Smoker=Var1)
    
      }
      
      smoker.df
  })
  
  # Construct Pollution Probability Table
  pollution <- reactive({
    
    pollution.df <- data.frame(network$cancer.fit$Pollution$prob) %>%
                    rename(Probability=Freq)
    
    # When loaded var1 is originally given to pollution variables
    # Once changed it does not revert back which can cause renaming errors
    # Program cannot find Var1 because it has already been changed to pollution
    if ("Var1" %in% colnames(pollution.df)){
      
      pollution.df <- pollution.df %>% rename(Pollution=Var1)
      
    }
    
    pollution.df
    
  })
  
  # Construct Cancer Probability Table
  cancer <- reactive({
    
    # Create initial cancer dataframe
    cancer.df <- data.frame(network$cancer.fit$Cancer$prob) %>% 
                 rename(Probability=Freq)
    
    if (input$CancerProbTable =="Conditional Probability Table") {
      # Spread the conditional table to make it easier for the user to understand
      cancer.df %>% 
      mutate(Probability=Probability*100) %>%
      pivot_wider(names_from=Cancer, values_from=Probability) %>%
      rename(`Cancer=True`=True,
             `Cancer=False`=False)
      
    } else {
      
      # Get probability that cancer = False
      cancer.false <- calculateUtility(cancer.df, smoker(), pollution())$TotalProb
      
      # Use probability to create independent table
      tibble("Cancer"=c("True", "False"), "Probability"=c(1-cancer.false, cancer.false)) %>%
      mutate(Probability=Probability*100)
      
    }

    
  })
  
  # Output Smoker Probability table
  output$smokerHotable <- renderHotable({
    
  smoker() %>% mutate(Probability=Probability*100)
        
  }, readOnly=FALSE)
  
  # Output Pollution Probability table
  output$pollutionHotable <- renderHotable({
    
  pollution() %>% mutate(Probability=Probability*100)
    
  }, readOnly=FALSE)
  
  # Output cancer probability table
  output$cancerHotable <- renderHotable({
  
  cancer()
    
  }, readOnly=FALSE)
  
  # create utility barchart
  utility.plot <- reactive({
    
    Utility$utility.df %>% ggplot(aes(x=name, y=utility)) + 
                           geom_col() + 
                           labs(title="Utility Comparison")
    
  })
  
  # plot utility barchart for policy page
  output$utilityComparison <- renderPlot({
    
    utility.plot()
    
  })
  
  # plot utility barchart for report page
  output$utilityComparisonFinal <- renderPlot({
    
    utility.plot()
    
  })
  
  # update network based off input changes
  observeEvent(input$networkUpdate, {
    
    # retrieve updated table data and convert to dataframe
    # normalise probabilities between 0 and 1
    smoker.df <- as.data.frame(hot.to.df(input$smokerHotable)) %>%
                 mutate(Probability=Probability/100)
    pollution.df <- as.data.frame(hot.to.df(input$pollutionHotable)) %>%
                    mutate(Probability=Probability/100)
    cancer.df <- as.data.frame(hot.to.df(input$cancerHotable))
    
    # Convert independent table to conditional table to update model
    if (input$CancerProbTable == "Independent Probability Table") {
      cancer.true <- cancer.df %>% filter(Cancer=="True") %>% select(Probability)
      cancer.true <- cancer.true$Probability/100
      cancer.df <- convertToConditional(cancer.true, 1 - cancer.true)
      
    } else{
      # Convert conditional table from wide form to long form
      cancer.df <- cancer.df %>%
                   rename(True=`Cancer=True`, False=`Cancer=False`) %>%
                   pivot_longer(c(True, False),
                                names_to="Cancer",
                                values_to="Probability") %>%
                   mutate(Probability=Probability/100)
      
    }

    # convert dataframes to table
    updatedSmokerTable <- xtabs(Probability~Smoker, smoker.df)
    updatedPollutionTable <- xtabs(Probability~Pollution, pollution.df)
    updatedCancerTable <- xtabs(Probability~Cancer+Smoker+Pollution, cancer.df)

    
    # retrieve model
    model.fit <- network$cancer.fit
    
    # updated model probabilities
    model.fit$Smoker <- updatedSmokerTable
    model.fit$Pollution <- updatedPollutionTable
    model.fit$Cancer <- updatedCancerTable
    
    # update reactive model
    network$cancer.fit <- model.fit
    
    # Save policy
    if (input$policyName != "Enter Policy Name...") {
       
       # Save updated model
       networks <- Utility$policy_networks
       networks[[input$policyName]] = model.fit
       Utility$policy_networks <- networks
       
       # calculate utility score
       utility <- calculateUtility(cancer.df, smoker.df, pollution.df)
       
       # update reactive utility dataframe
       Utility$utility.df <- Utility$utility.df %>% 
                             add_row(name=input$policyName, utility=utility$TotalProb)
       
       # Create new policy list
       policy.names <- Utility$utility.df %>% select(name) %>% unique()
       
       # update possible policy selections
       updateSelectInput(session,
                         "policySelection",
                         label="Select Policy",
                         choices=policy.names$name)
     }
    
  })
  
  # Function calculates utility
  calculateUtility <- function(cancer.df, smoker.df, pollution.df){
    
    cancer.true <- cancer.df %>% filter(Cancer == "True")
    cancer.prob <- cancer.true %>% 
                   left_join(smoker.df, by="Smoker") %>% 
                   left_join(pollution.df, by="Pollution") %>%
                   rename(CancerProb=Probability.x,
                          SmokerProb=Probability.y,
                          PollutionProb=Probability) %>%
                   mutate(SummedProb = CancerProb*SmokerProb*PollutionProb) %>%
                   summarise(TotalProb = 1- sum(SummedProb))
    
    return(cancer.prob)
  }
  
  # Function converts independent cancer table to conditional cancer table
  # TODO: make more elegant solution for more complicated network
  convertToConditional <- function(cancer.true, cancer.false){
    
    cancer <- c("True", "False", "True", "False", "True", "False", "True", "False")
    pollution <- c("low", "low", "high", "high", "low", "low", "high", "high")
    smoker <- c("True","True","True","True", "False", "False", "False", "False")
    Probability <- c(cancer.true, cancer.false, cancer.true, cancer.false,
                     cancer.true, cancer.false, cancer.true, cancer.false)
    
    cancer.df <- tibble("Cancer"=cancer, "Pollution"=pollution,
                        "Smoker"=smoker, "Probability"=Probability)
    
    return(cancer.df)
  }
  

})

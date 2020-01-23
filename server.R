
# Following script defines interactions between components displayed 
# on the webpage.
# 
# This script belongs to the decision support system for preserving
# digital files built by the University of Warwick and The National
# Archive.
# 
# @author: Stephen James Krol, University of Monash, Melbourne
# @email: stephen.james.krol@gmail.com

library(shiny)
library(graph)
library(bnlearn)
library(networkD3)
library(BiocManager)
library(Rgraphviz)
library(tidyverse)

options(repos = BiocManager::repositories())

shinyServer(function(input, output) {
  
  # initialise stable plot (unchanging) and reactive plot
  network <- reactiveValues(cancer.fit = read.bif("cancer.bif"))
  stable.fit <- read.bif("cancer.bif")
  
  # initialise policy list used for storing adjusted networks
  policy_networks <- list()
  
  # itialise utility dataframe
  Utility <- reactiveValues(utility.df=tibble(name=character(), utility=numeric()))
  
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
  
  # Output Smoker Probability table
  output$smokerHotable <- renderHotable({
    
    # Convert to dataframe and multiply probability by 100
    # so it is more intuitive to non-statistical users
    smoker.df <- data.frame(network$cancer.fit$Smoker$prob) %>%
                 rename(Probability=Freq) %>%
                 mutate(Probability=Probability*100)
    
    if ("Var1" %in% colnames(smoker.df)) {
      
      # When loaded var1 is originally given to smoker variables
      # Once changed it does not revert back which can cause renaming errors
      # Program cannot find Var1 because it has already been changed to smoker
      smoker.df <- smoker.df %>% rename(Smoker=Var1)
      
    }
    
    smoker.df
        
  }, readOnly=FALSE)
  
  # Output Pollution Probability table
  output$pollutionHotable <- renderHotable({
    
    pollution.df <- data.frame(network$cancer.fit$Pollution$prob) %>%
                    rename(Probability=Freq) %>%
                    mutate(Probability=Probability*100)
    
    # When loaded var1 is originally given to pollution variables
    # Once changed it does not revert back which can cause renaming errors
    # Program cannot find Var1 because it has already been changed to pollution
    if ("Var1" %in% colnames(pollution.df)){
      
      pollution.df <- pollution.df %>% rename(Pollution=Var1)
      
    }
    
  }, readOnly=FALSE)
  
  # Output cancer probability table
  output$cancerHotable <- renderHotable({
    
    data.frame(network$cancer.fit$Cancer$prob) %>% 
    rename(Probability=Freq) %>%
    mutate(Probability=Probability*100)
    
  }, readOnly=FALSE)
  
  # plot utility barchart
  output$utilityComparison <- renderPlot({
    
    Utility$utility.df %>% ggplot(aes(x=name, y=utility)) + 
                   geom_col() + 
                   labs(title="Utility Comparison")
    
  })
  
  # update network based off input changes
  observeEvent(input$networkUpdate, {
    
    # retrieve updated table data and convert to dataframe
    # normalise probabilities between 0 and 1
    smoker.df <- as.data.frame(hot.to.df(input$smokerHotable)) %>%
                 mutate(Probability=Probability/100)
    pollution.df <- as.data.frame(hot.to.df(input$pollutionHotable)) %>%
                    mutate(Probability=Probability/100)
    cancer.df <- as.data.frame(hot.to.df(input$cancerHotable)) %>%
                 mutate(Probability=Probability/100)

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
       policy_networks[input$policyName] = model.fit
       
       # calculate utility score
       utility <- calculateUtility(cancer.df, smoker.df, pollution.df)
       
       # update reactive utility dataframe
       Utility$utility.df <- Utility$utility.df %>% 
                             add_row(name=input$policyName, utility=utility$TotalProb)
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
  

})


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
  
  # initalise original probability tables
  
  
  # plot network used on the structure tab
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
    
    data.frame(network$cancer.fit$Smoker$prob) %>% rename(Smoker=Var1, Prob=Freq)
    
  }, readOnly=FALSE)
  
  # Output Pollution Probability table
  output$pollutionHotable <- renderHotable({
    
    data.frame(network$cancer.fit$Pollution$prob) %>% rename(Pollution=Var1, Prob=Freq)
    
  }, readOnly=FALSE)
  
  # Output cancer probability table
  output$cancerHotable <- renderHotable({
    
    data.frame(network$cancer.fit$Cancer$prob) %>% rename(Prob=Freq)
    
  }, readOnly=FALSE)
  
  # update network based off input changes
  observeEvent(input$networkUpdate, {
    
    # retrieve updated table data and convert to table
    updatedSmoker <- hot.to.df(input$smokerHotable) 
    updatedPollution <- hot.to.df(input$pollutionHotable)
    updatedCancer <- hot.to.df(input$cancerHotable)
    
    # variable naming is a hack due to naming updates
    names(updatedSmoker)[1] <- "Var1"
    names(updatedPollution)[1] <- "Var1" 
    
    # convert dataframes to table
    updatedSmoker <- xtabs(Prob~Var1, updatedSmoker)
    updatedPollution <- xtabs(Prob~Var1, updatedPollution)
    updatedCancer <- xtabs(Prob~Cancer+Smoker+Pollution, updatedCancer)
    
    # retrieve model
    model.fit <- network$cancer.fit
    
    model.fit$Smoker <- updatedSmoker
    model.fit$Pollution <- updatedPollution
    model.fit$Cancer <- updatedCancer
    
    # update reactive model
    network$cancer.fit <- model.fit
    
  })
  

})

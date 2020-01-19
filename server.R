
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

options(repos = BiocManager::repositories())

shinyServer(function(input, output) {
  
  network <- reactiveValues(cancer.fit = read.bif("cancer.bif"))
  stable.fit <- read.bif("cancer.bif")
  
  # Plot network which changes for policy inputs
  output$netPlot <- renderPlot({
    
    first <- graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    
  })
  
  # plot network used on the structure tab
  output$NetworkStructure <- renderPlot({
    
    first <- graphviz.chart(stable.fit, type = "barprob", grid=TRUE, main="Test Network")
    graphviz.chart(stable.fit, type = "barprob", grid=TRUE, main="Test Network")
    
  })
  
  observeEvent(input$networkUpdate, {
    
    cancer.fit <- network$cancer.fit
    
    # update Pollution
    pollution_prob = cancer.fit$Pollution$prob
    pollution_prob[1] = as.numeric(input$pollutionLow)
    pollution_prob[2] = as.numeric(input$pollutionHigh)
    
    cancer.fit$Pollution = pollution_prob
    
    # update Smoker
    smoker_prob = cancer.fit$Smoker$prob
    smoker_prob[1] = as.numeric(input$smokerTrue)
    smoker_prob[2] = as.numeric(input$smokerFalse)
    
    cancer.fit$Smoker = smoker_prob
    
    network$cancer.fit = cancer.fit
    
  })
  

})

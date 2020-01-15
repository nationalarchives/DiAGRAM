
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

options(repos = BiocManager::repositories())

shinyServer(function(input, output) {
  
  cancer.fit <- read.bif("cancer.bif")
  dag <- as.bn(as.graphAM(cancer.fit))
  
  # Plot the d3 force directed network
  output$netPlot <- renderSimpleNetwork({

    # Get the arc directions
    networkData <- data.frame(arcs(dag))
    
    simpleNetwork(
      networkData,
      Source = "from",
      Target = "to",
      opacity = 0.75,
      zoom = TRUE
    )
    
  })

})

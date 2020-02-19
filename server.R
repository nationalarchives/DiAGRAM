
# Following script defines interactions between components displayed 
# on the webpage.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com

library(shiny)
library(graph)
library(bnlearn)
library(networkD3)
library(BiocManager)
library(Rgraphviz)
library(tidyverse)
library(shinyjs)
library(shinyalert)

options(repos = BiocManager::repositories())

shinyServer(function(input, output, session) {
  
  # FUNCTIONS
  # function which caluclates utility
  calculate_utility <- function(model) {
    
    # convert model to grain object
    model.grain <- as.grain(model)
    
    # find probability of findability and renderability
    query.results <- querygrain(model.grain, nodes=c("Findability", "Renderability"))
    
    # Extract probabilities
    prob.findability <- as.numeric(query.results$Findability["True"])
    prob.renderability <- as.numeric(query.results$Renderability["True"])
    
    utility <- list("Findability"=prob.findability,
                    "Renderability"=prob.renderability)
    
    return(utility)    
  }
  
  # Function creates multiple sliders for the different states in a node
  create_sliders <- function(node, states) {
    # creates a list of sliders inputs for each state of the respective node
    j <- 1
    nodeStateSlider <- c()
    for(state in states){
      inputId <- paste(node, state, sep = "-")
      label <- paste(state, "(%)")
      nodeStateSlider[[j]] <- sliderInput(inputId, label, min = 0, max = 100, step = 10, value = 0, post = "%")
      
      j <- j+1
    }
    
    # list of nodes with corresponding state sliders
    return(nodeStateSlider)
  }
  
  
  # Function collects inputs from generated sliders and combines into dataframe
  collect_slider_inputs <- function(name){
    # create input ids to access slider information
    input.ids <- state.definitions %>%
      filter(node_name==name) %>%
      mutate(id=paste(node_name, node_state, sep = "-")) %>%
      select(id, node_state)
    
    # create vectors for states and probabilities
    states <- c()
    probabilities <- c()
    
    # iterate through all id's and collect values
    i <- 1
    for (id in input.ids$id) {
      # collect states and their probabilities
      state <- input.ids$node_state[i]
      probability <- input[[id]]
      
      # add to vectors
      states <- c(states, state)
      probabilities <- c(probabilities, probability)
      
      # iterate to next state
      i <- i + 1
    }
    
    # combine states into one dataframe
    return(tibble(state=states, probability=probabilities))
  }
  
  # Function collects inputs from Boolean sliders and combines into dataframe
  collect_boolean_slider_inputs <- function(name){
    
    # collect states of the node
    next_states <- state.definitions %>%
      filter(node_name==name) %>%
      select(node_state)
    
    # collect primary and secondary state
    primary_state <- next_states$node_state[1]
    secondary_state <- next_states$node_state[2]
    
    # create input id
    inputId <- paste(name, primary_state, sep="-")
    
    # collect probabilities
    primary_prob <- input[[inputId]]
    secondary_prob <- 100 - primary_prob
    
    # return tibble of probabilities
    return(tibble(state=c(primary_state, secondary_state),
                  probability=c(primary_prob, secondary_prob)))
  }
  
  # Function updates probability tables of model with user inputs
  update_probability <- function(node, model.probability.df, input.probability.df){
    
    # collect node states and corresponding probabilities
    node_states <- input.probability.df$state
    state_probabilities <- input.probability.df$probability
    # iterate through states and update model probability table
    i <- 1
    for (state in node_states){
      current.probability <- state_probabilities[i]
      print(state)
      model.probability.df <- model.probability.df %>%
        mutate(Freq=ifelse(model.probability.df[[node]]==state, current.probability, Freq))
      
      i <- i + 1
    }
    
    # normalise probability range between 0 and 1
    model.probability.df <- model.probability.df %>% mutate(Freq=Freq/100)
    
    # convert from data.frame to table
    model.probability.table <- xtabs(Freq~., model.probability.df)
    return(model.probability.table)
  }
  # STATIC VALUES
  stable.fit <- read.bif("Model.bif")
  
  # node definitions and state definitions
  node.definitions <- read_csv("node_information.csv") %>% arrange(node_name)
  state.definitions <- read_csv("node_states.csv")
  
  # csv containing nodes and questions used during setup
  setup_questions <- read_csv("setup_questions.csv")
  
  # TNA default risk
  tna_utility <- calculate_utility(stable.fit)
  
  # REACTIVE VALUES
  # initialise stable plot (unchanging) and reactive plot
  network <- reactiveValues(cancer.fit = read.bif("cancer.bif"))
  
  # Initialise Question Counter for model setup
  questionValues <- reactiveValues(question_number=1)
  
  # itialise utility dataframe
  Utility <- reactiveValues(utility.df=tibble(name=character(),
                                              utility=numeric()),
                            policy_networks=list())
  
  # Create vector to store answers to questions
  answers <- reactiveValues(radio_answers=list(),
                            slider_answers=list(),
                            boolean_slider_answers=list())
  
  # Customised models
  CustomModels <- reactiveValues(base_utility.df=tibble(name="TNA",
                                                 findability=tna_utility$Findability,
                                                 renderability=tna_utility$Renderability),
                                 custom_networks=list("TNA"=stable.fit))
  
  customModelChoices <- c('TNA')
  
  # Customised Policies
  CustomPolicies <- reactiveValues(archiveList=list("TNA"= tibble(name="TNA",
                                                    findability=tna_utility$Findability,
                                                    renderability=tna_utility$Renderability)),
                                   models=list("TNA"=list("Base"=stable.fit)))
  
  
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
  
  # create utility barchart
  utility.plot <- reactive({
    
    Utility$utility.df %>% ggplot(aes(x=name, y=utility)) + 
      geom_col() + 
      labs(title="Utility Comparison")
    
  })
  
  # NODE DEFINITION TAB
  
  # Update node drop down list with node names
  updateSelectInput(session,
                    "NodeSelection",
                    label=NULL,
                    choices=node.definitions$node_name)
  
  # plot network used on the network tab
  output$NetworkStructure <- renderPlot({
    graphviz.plot(stable.fit, layout = "dot",
                  highlight = list(nodes=c(input$NodeSelection), fill="lightgrey"),
                  shape = "ellipse",
                  render = TRUE,
                  main="Proposed network")
  })
  
  # Output node definiton text
  output$NodeDefinition <- renderUI({
    
    definition <- node.definitions %>% 
                  filter(node_name==input$NodeSelection) %>%
                  select(node_definition) %>%
                  as.character()
    
    tagList(strong("Definition: "), definition)
    
  })
  
  
  # Output hyperlink to data source
  output$DataLink <- renderUI({
    
    url <- node.definitions %>% 
           filter(node_name==input$NodeSelection) %>%
           select(data_source) %>%
           as.character()
    
    url <- a(input$NodeSelection, href=url)
    
    tagList(strong("Data Source: "), url)
    
  })
  
  # Output Year of node
  output$DataYear <- renderUI({
    
    year <- node.definitions %>% 
            filter(node_name==input$NodeSelection) %>%
            select(node_year) %>%
            as.character()
    
    tagList(strong("Data last updated: "), year)
    
  })
  
  # Output node state definition table
  output$StateDefinition <- renderTable({
    
    state.definitions %>%
      filter(node_name==input$NodeSelection) %>%
      select(-node_name) %>% 
      rename(`Node State`=node_state,
             `State Definition`=state_definition)
    
  })
  
  # SIMPLE TAB
  # MODEL CUSTOMISATION
  
  # Update state selection radio buttons
  # Collect the first node
  first_node <- setup_questions[1,]$node_name
  
  # retrieve states associated with the first node
  first_states <- state.definitions %>%
                  filter(node_name==first_node) %>%
                  select(node_state)
  
  # Create user input UI which is at the bottom of the box
  output$CustomisationInput <- renderUI({
    
    # collect next node questions
    next_node_name <- setup_questions[questionValues$question_number,]$node_name
    
    # collect type of input, radiobutton or slider
    next_node <- node.definitions %>% 
                 filter(node_name==next_node_name)
    number_of_questions <- nrow(setup_questions)
    
    # If all questions have not been answered yet render next button
    # If input type is slider, render multiple sliders for the different input states
    if (questionValues$question_number < number_of_questions+1 && next_node$type=="slider") {
      
      # collect next node name
      # next_node <- setup_questions[questionValues$question_number,]$node_name
      
      # collect states of the next node
      next_states <- state.definitions %>%
        filter(node_name==next_node$node_name) %>%
        select(node_state)
      
      rendered_element <- div(
                            fluidRow(
                              column(
                                width=5,
                                create_sliders(next_node$node_name, next_states$node_state)
                              )
                            ),
                            fluidRow(
                              column(
                                width=2,
                                tags$style(HTML('#NextQuestion{background-color:green}')),
                                tags$style(HTML('#NextQuestion{color:white}')),
                                tags$style(HTML('#NextQuestion{width:100%}')),
                                actionButton("NextQuestion", "Next")
                              ),
                              column(
                                width=2,
                                tags$style(HTML('#BackButton{background-color:grey}')),
                                tags$style(HTML('#BackButton{color:white}')),
                                tags$style(HTML('#BackButton{width:100%')),
                                actionButton("BackButton", "Back")
                              )
                            )
                          )
    } else if(questionValues$question_number < number_of_questions+1 && next_node$type=="radiobuttons") {
      
      # collect next node name
      # next_node <- setup_questions[questionValues$question_number,]$node_name
      
      # if node is equal to Copy_protocol, add description
      if (next_node$node_name == "Copy_protocol"){
        text_description <- column(
                              width=5,
                              tags$ol(
                                tags$li("The archive has multiple independent copies of the
                                         digital materials."),
                                tags$li("Copies are geographically separated
                                         into different locations."),
                                tags$li("Copies use different storage technologies."),
                                tags$li("Copies use a combination of online and
                                         offline storage techniques."),
                                tags$li("Storage is actively monitored to ensure any
                                         problems are detected and corrected quickly.")
                              )
                            )
      } else{
        text_description <- column(width=5)
      }
      
      # collect states of the next node
      next_states <- state.definitions %>%
        filter(node_name==next_node$node_name) %>%
        select(node_state)
      
      rendered_element <- div(
        fluidRow(
          column(
            width=5,
            radioButtons("StateSelection", label=NULL, choices=next_states$node_state)
          ),
          text_description
        ),
        fluidRow(
          column(
            width=2,
            tags$style(HTML('#NextQuestion{background-color:green}')),
            tags$style(HTML('#NextQuestion{color:white}')),
            tags$style(HTML('#NextQuestion{width:100%}')),
            actionButton("NextQuestion", "Next")
          ),
          column(
            width=2,
            tags$style(HTML('#BackButton{background-color:grey}')),
            tags$style(HTML('#BackButton{color:white}')),
            tags$style(HTML('#BackButton{width:100%')),
            actionButton("BackButton", "Back")
          )
        )
      )
    } else if (questionValues$question_number < number_of_questions+1 && next_node$type=="BooleanSlider"){

      # collect states of the next node
      next_states <- state.definitions %>%
        filter(node_name==next_node$node_name) %>%
        select(node_state)
      
      # collect primary state
      primary_state <- next_states$node_state[1]
      inputId <- paste(next_node$node_name, primary_state, sep="-")
      label <- paste(primary_state, "%")
      
      rendered_element <- div(
        fluidRow(
          column(
            width=5,
            sliderInput(inputId, label, min = 0, max = 100, step = 10, value = 0, post = "%")
          )
        ),
        fluidRow(
          column(
            width=2,
            tags$style(HTML('#NextQuestion{background-color:green}')),
            tags$style(HTML('#NextQuestion{color:white}')),
            tags$style(HTML('#NextQuestion{width:100%}')),
            actionButton("NextQuestion", "Next")
          ),
          column(
            width=2,
            tags$style(HTML('#BackButton{background-color:grey}')),
            tags$style(HTML('#BackButton{color:white}')),
            tags$style(HTML('#BackButton{width:100%')),
            actionButton("BackButton", "Back")
          )
        )
      )
      
    } else {
      rendered_element <- fluidRow(
                            column(
                              width=3,
                              textInput(
                                inputId="CustomisedModelName",
                                label=NULL
                              ),
                            ),  
                            column(
                              width=2,
                              tags$style(HTML('#SaveModel{background-color:green}')),
                              tags$style(HTML('#SaveModel{color:white}')),
                              actionButton("SaveModel",
                                           "Save Model")
                              ),
                            column(
                              width=1,
                              offset=5,
                              tags$style(HTML('#AddNew{background-color:grey}')),
                              tags$style(HTML('#AddNew{color:white}')),
                              actionButton("AddNew",
                                           "Create New Model")
                            )
                            )

                          
    }
    rendered_element
  })
  
  
  # Add question to setup page.
  # TODO: Make dynamic check rather than hardcoded 6
  output$Question <- renderUI({
    if (questionValues$question_number < 6 && questionValues$question_number>=1){
      h4(strong(setup_questions[questionValues$question_number,]$node_question))
    } else {
      h4(strong("All Questions Answered."))
    }
  })
  
  # Update question when next question button is pressed
  observeEvent(input$NextQuestion, {
    
    # collect next node question
    next_node_name <- setup_questions[questionValues$question_number,]$node_name
    
    # collect information on node
    next_node <- node.definitions %>%
                 filter(node_name==next_node_name)
    
    # add state to answer vector
    # if radio button, add to radio button answers
    if (next_node$type == "radiobuttons") {
      answers$radio_answers[[next_node$node_name]] = input$StateSelection
      
    } else if (next_node$type == "slider") {
      # collcet input probabilities and check the sum
      input.probabilities <- collect_slider_inputs(next_node$node_name)
      prob.summary <- input.probabilities %>% summarise(prob_sum=sum(probability))

      # if sum of probability is not 100 alert user and break out of function
      if (prob.summary$prob_sum != 100){
        errorMsg <- paste("Probabilities for '", next_node$node_name, "' do not add up to to 100%")
        shinyalert("Oops!", errorMsg, type = "error")
        
        return()
        
      } else {
        answers$slider_answers[[next_node$node_name]] = input.probabilities
      }
    } else if (next_node$type == "BooleanSlider"){
      input.probabilities<- collect_boolean_slider_inputs(next_node$node_name)
      answers$boolean_slider_answers[[next_node$node_name]] = input.probabilities
    }
    
    # update progress bar
    updateProgressBar(
      session=session,
      id="Question_Progress",
      value=questionValues$question_number,
      total=5
    )
    
    # update question number
    questionValues$question_number <- questionValues$question_number + 1
    
  })
  
  # Update questions when back button is pressed
  observeEvent(input$BackButton, {
    
    if (questionValues$question_number > 1) {
      # collect input type, whether radio button or slider
      input_type <- setup_questions[questionValues$question_number,]$type
      
      # collect node name
      node_name <- setup_questions[questionValues$question_number,]$node_name
      
      # remove last answer
      # if radio button input, remove from radio button list
      if (input_type == "radiobuttons"){
        answers$radio_answers[[node_name]] = NULL
      } else if (input_type == "slider"){
        answers$slider_answers[[node_name]] = NULL
      }

      # update question number
      questionValues$question_number <- questionValues$question_number - 1
    }

    if (questionValues$question_number >= 1) {
      
      # update progress bar
      updateProgressBar(
        session=session,
        id="Question_Progress",
        value=questionValues$question_number - 1,
        total=5
      )
    }
  })
  
  # Save model to memory
  observeEvent(input$SaveModel, {
    # Check if model has been named correctly
    if (input$CustomisedModelName == "") {
      errorMsg <-"Please give your custom model a name!"
      shinyalert("Oops!", errorMsg, type = "error")
      
      return()
    }
    
    if (input$CustomisedModelName %in% names(CustomModels$custom_network)){
      errorMsg <-"You have already used this name for another custom model!"
      shinyalert("Oops!", errorMsg, type = "error")
      
      return()
    }
    # create custom model and save to memory
    # first update inputs from radio buttons
    custom_model <- mutilated(stable.fit, evidence=answers$radio_answers)
    
    # second update states with inputs as sliders
    for (node in names(answers$slider_answers)) {
      input.probability.df <- answers$slider_answers[[node]]
      model.probability.df <- as.data.frame(custom_model[[node]]$prob)
      model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
      
      # update probability table for node
      custom_model[[node]] = model.probability.table
    }
    
    # third update states with boolean sliders as inputs
    for (node in names(answers$boolean_slider_answers)) {
      input.probability.df <- answers$boolean_slider_answers[[node]]
      model.probability.df <- as.data.frame(custom_model[[node]]$prob)
      model.probability.df <- rename(model.probability.df, !!node:=Var1)
      
      model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
  
      # update probability table for node
      custom_model[[node]] = model.probability.table
    }
    print(custom_model)
    # Add custom network to memory 
    CustomModels$custom_network[[input$CustomisedModelName]] = custom_model
    CustomPolicies$models[[input$CustomisedModelName]] = list('Base'=custom_model)
    
    # calculate utility and store
    utility <- calculate_utility(custom_model)
    CustomModels$base_utility.df <- CustomModels$base_utility.df %>% add_row(name=input$CustomisedModelName,
                                                                              findability=utility$Findability,
                                                                              renderability=utility$Renderability)
    # TODO: Why do we have two structures saving the same information?
    CustomPolicies$archiveList[[input$CustomisedModelName]] <- tibble(name=input$CustomisedModelName,
                                         findability=utility$Findability,
                                         renderability=utility$Renderability)
    
    # setting choices for the drop down list in the Simple view Node customisation tab
    customModelChoices <- CustomModels$base_utility.df %>% select(name)
    updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
  })
  
  # plot utility
  output$BasicUtilityComparison <- renderPlot({
    
    CustomModels$base_utility.df %>%
    mutate(utility=findability+renderability) %>% 
    pivot_longer(c(findability, renderability), names_to="node") %>%
    ggplot(aes(x=name, fill=node, y=value)) +
    geom_bar(position="stack", stat="identity")
  })
  
  # Reset so new custom model can be created
  observeEvent(input$AddNew, {
    
    # reset question number to 1
    questionValues$question_number = 1
    
    # reset answers to an empty list
    # answers$states <- list()
    
    # update radio buttons
    updateRadioButtons(session, "StateSelection", choices=first_states$node_state)
    
    # update progress bar
    updateProgressBar(
      session=session,
      id="Question_Progress",
      value=questionValues$question_number - 1,
      total=5
    )
    
  })
  
  # SIMPLE POLICY
  
  # Plot the policy comparison stacked bar chart
  output$policyTabUtilityScorePlot <- renderPlot(
    {
      CustomPolicies$archiveList[[input$customModelSelection]] %>%
        mutate(utility=findability+renderability) %>%
        pivot_longer(c(findability, renderability), names_to="policy") %>%
        ggplot(aes(x=name, fill=policy, y=value)) +
        geom_bar(position="stack", stat="identity")
    }
  )
  
  # OAIS Entities list
  OAISentities <- node.definitions$OAIS_Entity
  OAISentities <- c('None', unique(OAISentities)) # adding None to provide option of listing all nodes
  
  updateSelectInput(session, 
                    "customOaisEntitySelection",
                    choices = OAISentities, 
                    selected = 'None')
  
  uiNode <- reactiveValues(checklist=c())
  
  observeEvent(input$customOaisEntitySelection, {
    if(input$customOaisEntitySelection == 'None'){
      uiNode$checklist <- nodes(stable.fit)
    }
    else{
      tmp <- node.definitions %>%
        filter(OAIS_Entity==input$customOaisEntitySelection) %>%
        select(node_name)
      
      uiNode$checklist <- tmp$node_name
    }
    
    # update the checklist options with nodes list
    updateCheckboxGroupInput(session,
                             "policyTabNodesChecklist",
                             label=NULL,
                             choices = uiNode$checklist)
  })
  
  currModel <- reactiveValues(model=stable.fit)
  uiNodeSlider <- reactiveValues(node=c())
  totalNumberOfNode <- reactiveValues(i=0)
  nodeStateProgress <- reactiveValues(progress=0)
  
  # make the necessary changes when the model is changed from dropdown menu
  observeEvent(input$customModelSelection,{
    # list the nodes checklist dynamically based on model instead of hardcoding
    
    if(input$customModelSelection == 'TNA'){
      currModel$model <- stable.fit
    }
    else{
      currModel$model <- CustomPolicies$models[[input$customModelSelection]]$Base
    }
    
    # reset the progress for selected model
    nodeStateProgress$progress <- 0
    uiNodeSlider$node <- c()
 })
  
  # observe the input for checklist to update uiNodeSlider$node with respective states
  observeEvent(input$policyTabNodesChecklist, {
    totalNumberOfNode$i <- 1
    uiNodeSlider$node <- c()
    nodeStateProgress$progress <- 1
    
    for(node in input$policyTabNodesChecklist){
      
      ## TODO: change this to list of list (of nodes with node state) to avoid having to create a new list for every node
      nodeStates <- state.definitions %>%
        filter(node_name==node) %>%
        select(-node_name) 

      # Create sliders
      nodeStateSlider <- create_sliders(node, nodeStates$node_state)

      
      # remove the _ from the node to ease readability
      nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
      nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
      
      # list of nodes with corresponding state sliders
      uiNodeSlider$node[[totalNumberOfNode$i]] <- div(h4(nodeLabel), nodeStateSlider )
      totalNumberOfNode$i <- totalNumberOfNode$i+1
    }
  })
  
  
  #NOTE: When length(input$policyTabNodesChecklist) == 0, the observeEvent(input$policyTabNodesChecklist, {}) method is not called. 
  # As a result, uiNodeSlider$node remains unchanged (contains last selected node as the only element)
  # For this reason, checks (for length !=0) have been added to update content when no node is selected.
  
  # display the slider inputs for each selected node (as a progress bar walkthrough)
  output$policyTabNodesSlider <- renderUI({
    # Enabling/disabling buttons based on progress
    
    # if nothing is selected everything is hidden and disabled
    if (length(input$policyTabNodesChecklist)!=0){
      shinyjs::show(id="SimpleViewPolicyNext")
      shinyjs::show(id="SimpleViewPolicyPrevious")
      shinyjs::enable(id="SimpleViewPolicyNext")
      shinyjs::enable(id="SimpleViewPolicyPrevious")
      shinyjs::hide(id="nodeSliderPlaceholder")
    }
    else{
      shinyjs::hide(id="SimpleViewPolicyNext")
      shinyjs::hide(id="SimpleViewPolicyPrevious")
      shinyjs::hide(id="SimpleViewPolicyAddBox")
      shinyjs::show(id="nodeSliderPlaceholder")
      # shinyjs::hide(id="SimpleViewPolicyName")
      # shinyjs::hide(id="SimpleViewAddPolicy")
    }
    
    # disable previous button to avoid negative index (<1)
    if(nodeStateProgress$progress == 1){
      shinyjs::disable(id="SimpleViewPolicyPrevious")
    }
    
    # Policy can only be added when all the selected nodes have been updated
    if(length(input$policyTabNodesChecklist) != 0 & nodeStateProgress$progress == length(uiNodeSlider$node)){
      shinyjs::disable(id="SimpleViewPolicyNext") # disable next button to avoid exceeding array size
      shinyjs::show(id="SimpleViewPolicyAddBox")
      #shinyjs::show(id="SimpleViewAddPolicy")
    }
    else{
      shinyjs::hide(id="SimpleViewPolicyAddBox")
      #shinyjs::hide(id="SimpleViewAddPolicy")
    }
    
    
    if (length(input$policyTabNodesChecklist)!=0){
      uiNodeSlider$node[[nodeStateProgress$progress]]
    }
  })
  
  observeEvent(input$SimpleViewPolicyNext, {
    nodeStateProgress$progress <- nodeStateProgress$progress + 1
  })
  
  observeEvent(input$SimpleViewPolicyPrevious, {
    nodeStateProgress$progress <- nodeStateProgress$progress - 1
  })
  
  
  # Add policy action
  observeEvent(input$SimpleViewAddPolicy, {
    isProbabilityMismatchError <- TRUE
    
    for(node in input$policyTabNodesChecklist){
      # conditional probability table (cpt) of each node
      cpt <- as.data.frame(stable.fit[[node]]$prob)
      
      nodeStates <- state.definitions %>%
        filter(node_name==node) %>%
        select(-node_name)
      
      currSumOfProbabilities <- 0
      # updating the cpt for each state
      for(state in nodeStates$node_state){
        currId = paste(node, state, sep ="-")
        index <- cpt[[node]] == state
        cpt$Freq[index] <- input[[currId]]/100
        currSumOfProbabilities <- currSumOfProbabilities +  input[[currId]]/100
      }
      
      # Display a pop-up when the probabilities don't add up to 1.0 (divided by 100)
      
      if(currSumOfProbabilities != 1.0){
        isProbabilityMismatchError = TRUE
        
        #TODO: make it a function since it is also used in output$policyTabNodesSlider
        
        # remove the _ from the node to ease readability
        nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
        nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
        
        errorMsg <- paste("Probabilities for '", nodeLabel, "' does not add upto to 1.0")
        shinyalert("Oops!", errorMsg, type = "error")
        
        break
      }
      else if(input$SimpleViewPolicyName == ""){
        isProbabilityMismatchError = TRUE
        
        shinyalert("Oops!", "Please provide a policy name", type = "error")
      }
      else{
        isProbabilityMismatchError = FALSE
        
        # Updating the model
        # The data frame should be converted to a contigency and then the model is updated.
        # The table should be Freq~'all other columns'
        
        # get the column names excluding frequency
        cptFactors <- colnames(cpt)[1:length(colnames(cpt))-1]
        
        # formula for xtabs
        formula <- paste('Freq~', paste(cptFactors, collapse = "+"), sep="")
        
        # update the model
        currModel$model[[node]] <- xtabs(formula, cpt)
      }
    }
    
    if(isProbabilityMismatchError == FALSE)
    {
      # Calculate the utility of the new model
      currPolicyUtility <- calculate_utility(currModel$model)
      
      # update reactive policy list
      CustomPolicies$archiveList[[input$customModelSelection]] <- CustomPolicies$archiveList[[input$customModelSelection]] %>%
        add_row(name=input$SimpleViewPolicyName,
                findability=currPolicyUtility$Findability,
                renderability=currPolicyUtility$Renderability)
      
      CustomPolicies$models[[input$customModelSelection]][[input$SimpleViewPolicyName]] <- currModel$model
    }
  })
  
  
  
  
  
  # POLICIES TAB -- OLD
  
  # Plot network which changes for policy inputs
  output$netPlot <- renderPlot({
    
    first <- graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    graphviz.chart(network$cancer.fit, type = "barprob", grid=TRUE, main="Test Network")
    
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
  
  # plot utility barchart for policy page
  output$utilityComparison <- renderPlot({
    
    utility.plot()
    
  })
  
  # Reset network to original probabilities
  observeEvent(input$networkReset, {
    
    network$cancer.fit <- stable.fit
    
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
    print(updatedCancerTable)
    
    
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
  
  # REPORT TAB
  # Plot network for report page
  output$ReportModel <- renderPlot({
    
    if (input$policySelection != "No policies added") {
      model.fit <- Utility$policy_networks[[input$policySelection]]
      first <- graphviz.chart(model.fit, type = "barprob", grid=TRUE, main="Test Network")
      graphviz.chart(model.fit, type = "barprob", grid=TRUE, main="Test Network")
    }
    
  })
  
  # Render text for summary on report page
  output$TextReport <- renderText({
    
    if (dim(Utility$utility.df)[1] == 0) {
      
      "No Policies have been added yet."
      
    }
    
  })
  
  # plot utility barchart for report page
  output$utilityComparisonFinal <- renderPlot({
    
    utility.plot()
    
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
  
  # Download selected files
  output$Download <- downloadHandler(

    filename = function() {
      paste0(input$policySelection, ".zip")
    },
    
    content = function(file){
      
      # write model
      if ("Model" %in% input$downloadOptions) {
        write.bif(paste0(input$policySelection, ".bif"),
                  Utility$policy_networks[[input$policySelection]])
      }
      
      # write utility plot
      if ("Utility Plot" %in% input$downloadOptions) {
        png(filename=paste0(input$policySelection, ".png"))
        print(utility.plot())
        dev.off()
      }
    
      
      # create zip file to return
      filenames <- c(paste0(input$policySelection, ".bif"),
                     paste0(input$policySelection, ".png"))
      
      zip(file, filenames)
      
      # delete all files on server
      for (filename in filenames){
        file.remove(filename)
      }
    }
  )

})

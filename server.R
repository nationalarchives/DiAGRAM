
# Following script defines interactions between components displayed 
# on the webpage.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com
# @author: Sidhant Bhatia, Monash University, Malaysia
# @email: sidhant3b@gmail.com

library(shiny)
library(graph)
library(bnlearn)
library(networkD3)
library(BiocManager)
library(Rgraphviz)
library(tidyverse)
library(shinyjs)
library(shinyalert)
library(gridExtra)

options(repos = BiocManager::repositories())
options(shiny.fullstacktrace = FALSE)

# TODO: policy plotting should be one reactive variable

shinyServer(function(input, output, session) {
  
  # -------------------- FUNCTIONS --------------------
  # function which caluclates utility
  calculate_utility <- function(model) {
    
    # convert model to grain object
    model.grain <- as.grain(model)
    
    # find probability of Intellectual_Control and Renderability
    query.results <- querygrain(model.grain, nodes=c("Intellectual_Control", "Renderability"))
    
    # Extract probabilities
    prob.Intellectual_Control <- as.numeric(query.results$Intellectual_Control["Yes"])
    prob.Renderability <- as.numeric(query.results$Renderability["Yes"])
    
    utility <- list("Intellectual_Control"=prob.Intellectual_Control,
                    "Renderability"=prob.Renderability)
    
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
      nodeStateSlider[[j]] <- sliderInput(inputId, label, min = 0, max = 100, step = 1, value = 0, post = "%")
      
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
    if(name=="Physical_Disaster"){
      # High is above 3.3%. For the purposes of the model, treat as 5%
      if(input[[inputId]]=="High") {primary_prob <- 5}
      # Medium is between 1% and 3.3% .For the purposes of the model, treat as 2%
      if(input[[inputId]]=="Medium") {primary_prob <- 2}
      # Low is between 0.1% and 1%. For the purposes of the model, treat as 0.5%
      if(input[[inputId]]=="Low") {primary_prob <- 0.5}
      # Very low is less than 0.1%. For the purposes of the model, treat as 0.05%
      if(input[[inputId]]=="Very Low") {primary_prob <- 0.05}
    }
    else {
      primary_prob <- input[[inputId]]
    }
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
  
  
  # --------------------   FUNCTIONS   --------------------
  
  
  
  
  
  
  
  
  
  
  # -------------------- STATIC VALUES --------------------
  stable.fit <- read.bif("Model.bif")
  
  # node definitions and state definitions
  node.definitions <- read_csv("node_information.csv") %>% arrange(node_name)
  state.definitions <- read_csv("node_states.csv")
  
  # csv containing nodes and questions used during setup
  setup_questions <- read_csv("setup_questions.csv")
  
  # Default risk
  default_utility <- calculate_utility(stable.fit)
  
  # --------------------   STATIC VALUES    --------------------
  
  
  
  
  
  
  
  
  
  
  # --------------------   REACTIVE VALUES  ---------------------
  
  # initialise stable plot (unchanging) and reactive plot
  network <- reactiveValues(model.fit = read.bif("Model.bif"),
                            advanced.fit = stable.fit)
  
  # Initialise Question Counter for model setup
  questionValues <- reactiveValues(question_number=1)
  
  # Create vector to store answers to questions
  answers <- reactiveValues(radio_answers=list(),
                            slider_answers=list(),
                            boolean_slider_answers=list())
  
  # Customised models
  CustomModels <- reactiveValues(base_utility.df=tibble(name="Default",
                                                        Intellectual_Control=default_utility$Intellectual_Control,
                                                        Renderability=default_utility$Renderability),
                                 custom_networks=list("Default"=stable.fit))
  
  ## TODO:sid - combine both into a single data structure
  # Customised Policies
  
  # CustomPolicies <- reactiveValues(archiveList=list("Default"= tibble(name="Default",
  #                                                                 findability=default_utility$Findability,
  #                                                                 renderability=default_utility$Renderability)),
  #                                  models=list("Default"=list("Base"=stable.fit)))
  CustomPolicies <- reactiveValues(archiveList=list(),
                                   models=list())
  
  utility_weighting <- reactiveValues(Renderability=1,
                                      Intellectual=1)

  # --------------------   REACTIVE VALUES  ---------------------
  
  
  
  
  
  
  

  
  # -------------------- NODE DEFINITION TAB --------------------
  
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
                  render = TRUE)
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
     
  #   url <- a(input$NodeSelection, href=url) remove hyperlink
     
     tagList(strong("Data source: "), url)
     
   })

  
  # Output Year of node
  output$DataYear <- renderUI({
    
    year <- node.definitions %>% 
      filter(node_name==input$NodeSelection) %>%
      select(node_year) %>%
      as.character()
    
    tagList(strong("Data collected: "), year)
    
  })
  
  # Output node state definition table
  output$StateDefinition <- renderTable({
    
    state.definitions %>%
      filter(node_name==input$NodeSelection) %>%
      select(-node_name) %>% 
      rename(`Node State`=node_state,
             `State Definition`=state_definition)
    
  })
  
  # -------------------- NODE DEFINITION TAB --------------------
  
  
  
  
  
  
  
  
  
  # -------------------- MODEL CUSTOMISATION --------------------
  
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
                                tags$style(HTML('#BackButton{background-color:grey}')),
                                tags$style(HTML('#BackButton{color:white}')),
                                tags$style(HTML('#BackButton{width:100%}')),
                                actionButton("BackButton", "Back") #changed to uk order
                              ),
                              column(
                                width=2,
                                tags$style(HTML('#NextQuestion{background-color:green}')),
                                tags$style(HTML('#NextQuestion{color:white}')),
                                tags$style(HTML('#NextQuestion{width:100%')),
                                actionButton("NextQuestion", "Next") #changed to uk order
                              )
                            )
                          )
    } else if(questionValues$question_number < number_of_questions+1 && next_node$type=="radiobuttons") {
      
      # collect next node name
      # next_node <- setup_questions[questionValues$question_number,]$node_name
        text_description <- column(width=5)
        
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
            tags$style(HTML('#BackButton{background-color:grey}')),
            tags$style(HTML('#BackButton{color:white}')),
            tags$style(HTML('#BackButton{width:100%}')),
            actionButton("BackButton", "Back") #changed to uk order
          ),
          column(
            width=2,
            tags$style(HTML('#NextQuestion{background-color:green}')),
            tags$style(HTML('#NextQuestion{color:white}')),
            tags$style(HTML('#NextQuestion{width:100%')),
            actionButton("NextQuestion", "Next") #changed to uk order
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
      
      if (next_node$node_name == "Physical_Disaster"){
        node_text <- a(href="https://flood-warning-information.service.gov.uk/long-term-flood-risk/postcode",
                       'Click here to check your flood risk here.')
        rendered_element <- div(
          fluidRow(
            column(
              width=5,
              node_text,
              br(),
              br(),
              sliderTextInput(inputId, "Risk rating from gov.uk", grid = TRUE, force_edges = TRUE,
                              choices = c("Very Low", "Low", "Medium", "High"))
            )
          ),
          fluidRow(
            column(
              width=2,
              tags$style(HTML('#BackButton{background-color:grey}')),
              tags$style(HTML('#BackButton{color:white}')),
              tags$style(HTML('#BackButton{width:100%}')),
              actionButton("BackButton", "Back") #changed to uk order
            ),
            column(
              width=2,
              tags$style(HTML('#NextQuestion{background-color:green}')),
              tags$style(HTML('#NextQuestion{color:white}')),
              tags$style(HTML('#NextQuestion{width:100%')),
              actionButton("NextQuestion", "Next") #changed to uk order
            )
          )
        )
      }
      else{
        node_text <- ""
      rendered_element <- div(
        fluidRow(
          column(
            width=5,
            node_text,
            br(),
            br(),
            sliderInput(inputId, label, min = 0, max = 100, step = 1, value = 0, post = "%")
          )
        ),
        fluidRow(
          column(
            width=2,
            tags$style(HTML('#BackButton{background-color:grey}')),
            tags$style(HTML('#BackButton{color:white}')),
            tags$style(HTML('#BackButton{width:100%}')),
            actionButton("BackButton", "Back") #changed to uk order
          ),
          column(
            width=2,
            tags$style(HTML('#NextQuestion{background-color:green}')),
            tags$style(HTML('#NextQuestion{color:white}')),
            tags$style(HTML('#NextQuestion{width:100%')),
            actionButton("NextQuestion", "Next") #changed to uk order
          )
        )
      )
      
    }} else {
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
  output$Question <- renderUI({
    if (questionValues$question_number < nrow(setup_questions)+1 && questionValues$question_number>=1){
      h4(strong(setup_questions[questionValues$question_number,]$node_question))
    } else {
      h4(strong("All questions answered. Please give model a name:"))
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
      total=nrow(setup_questions)
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
        total=nrow(setup_questions)
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
    
    if (input$CustomisedModelName %in% names(CustomModels$custom_networks)){
      errorMsg <-"You have already used this name for another custom model!"
      shinyalert("Oops!", errorMsg, type = "error")
      
      return()
    }
    # create custom model and save to memory
    # first update inputs from radio buttons
    if(is.null(dim(answers$radio_answers))) {
      custom_model <- stable.fit
    }
    else {custom_model <- mutilated(stable.fit, evidence=answers$radio_answers)}
    
    # second, update states with inputs as sliders
    for (node in names(answers$slider_answers)) {
      input.probability.df <- answers$slider_answers[[node]]
      model.probability.df <- as.data.frame(custom_model[[node]]$prob)
      if ("Var1" %in% colnames(model.probability.df)){
        model.probability.df <- rename(model.probability.df, !!node:=Var1)
      }
      model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
      # update probability table for node
      custom_model[[node]] = model.probability.table
      }
    # third update states with boolean sliders as inputs
    for (node in names(answers$boolean_slider_answers)) {
      input.probability.df <- answers$boolean_slider_answers[[node]]
      model.probability.df <- as.data.frame(custom_model[[node]]$prob) %>% rename(!!node:=Var1)

      model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
      
      # update probability table for node
      custom_model[[node]] = model.probability.table
    }
    
    

    # Add custom network to memory 
    CustomModels$custom_networks[[input$CustomisedModelName]] = custom_model
    CustomPolicies$models[[input$CustomisedModelName]] = list('Base'=custom_model)

    # calculate utility and store
    utility <- calculate_utility(custom_model)
    CustomModels$base_utility.df <- CustomModels$base_utility.df %>% add_row(name=input$CustomisedModelName,
                                                                             Intellectual_Control=utility$Intellectual_Control,
                                                                             Renderability=utility$Renderability)
    # TODO: Why do we have two structures saving the same information?
    CustomPolicies$archiveList[[input$CustomisedModelName]] <- tibble(name=input$CustomisedModelName,
                                                                      Intellectual_Control=utility$Intellectual_Control,
                                                                      Renderability=utility$Renderability)
    
    # setting choices for the drop down list in the Simple view Node customisation tab
    customModelChoices <- CustomModels$base_utility.df %>% select(name)
    updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
    updateSelectInput(session, "model_version", label="Select Model", choices=customModelChoices)

    # set choices for the drop down list in the Report tab
    updateSelectInput(session, 'reportTabModelSelection', choices=CustomModels$base_utility.df$name)
  })
  
  # plot utility
  output$BasicUtilityComparison <- renderPlot({
    
    CustomModels$base_utility.df %>%
      mutate(utility=Intellectual_Control+Renderability) %>% 
      pivot_longer(c(Intellectual_Control, Renderability), names_to="node") %>%
      ggplot(aes(x=reorder(name, -value), fill=node, y=value)) +
      geom_bar(position="stack", stat="identity") + xlab("Model Name") + ylab("Score") +
      scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
  })
  
  # Reset so new custom model can be created
  observeEvent(input$AddNew, {
    
    # reset question number to 1
    questionValues$question_number = 1
    
    # update radio buttons
    updateRadioButtons(session, "StateSelection", choices=first_states$node_state)
    
    # update progress bar
    updateProgressBar(
      session=session,
      id="Question_Progress",
      value=questionValues$question_number - 1,
      total=nrow(setup_questions)
    )
    
  })
  
  # upload custom model
  
  observeEvent(input$uploadCustomModel, {
    
    req(input$customModel)
    
    if (is.null(input$customModel)){
      errorMsg <-"You have not uploaded a model."
      shinyalert("Oops!", errorMsg, type = "error")
      return()
    } else{
      # check if name is already being used
      if(input$uploadName %in% names(CustomModels$custom_networks)){
        errorMsg <-"You have already used this name for another custom model!"
        shinyalert("Oops!", errorMsg, type = "error")
        return()
      }
      
      # check if name is empty
      if (input$uploadName == ""){
        errorMsg <-"Please provide uploaded model a name!"
        shinyalert("Oops!", errorMsg, type = "error")
        return()
      }
      
      # load model into memory and calculate base utility
      custom_model <- read.bif(input$customModel$datapath)
      utility <- calculate_utility(custom_model)
      
      
      CustomModels$base_utility.df <- CustomModels$base_utility.df %>% add_row(name=input$uploadName,
                                                                         Intellectual_Control=utility$Intellectual_Control,
                                                                         Renderability=utility$Renderability)
      
      CustomModels$custom_networks[[input$uploadName]] <- custom_model
      
    }
  })
  
  # SIMPLE POLICY
  
  # Plot the policy comparison stacked bar chart
  output$policyTabUtilityScorePlot <- renderPlot(
    {
      CustomPolicies$archiveList[[input$customModelSelection]] %>%
        mutate(utility=Intellectual_Control+Renderability) %>%
        pivot_longer(c(Intellectual_Control, Renderability), names_to="policy") %>%
        ggplot(aes(x=reorder(name, -value), fill=policy, y=value)) +
        geom_bar(position="stack", stat="identity") + xlab("Policy") + ylab("Score") +
        scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
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
    oaisSelected <- TRUE
    
    if(length(input$customOaisEntitySelection) == 1 & input$customOaisEntitySelection[1] == 'None'){
      uiNode$checklist <- node.definitions$node_name
      oaisSelected <- FALSE
    }
    else{
      uiNode$checklist <- c()
      for(oaisEntity in input$customOaisEntitySelection){
        
        # display error if 'None' is still selected alongwith other OAIS entities. 
        if(oaisEntity == 'None'){
          shinyalert("Oops!", "You can't select 'None' and other OAIS Entities together. If you wish to view features within an OAIS
                    entity, please click on 'None' and delete/backspace, followed by selection of the desired OAIS entities", type = "error")
          return()
        }
        
        tmp <- node.definitions %>%
          filter(OAIS_Entity==oaisEntity) %>%
          select(node_name)
        
        uiNode$checklist <- c(tmp$node_name, uiNode$checklist)
      }
    }
    
    # auto-select all the nodes in checklist if any 'non-None' OAIS entity is selected
    if(oaisSelected == TRUE){
      # update the checklist options with nodes list
      updateCheckboxGroupInput(session,
                               "policyTabNodesChecklist",
                               label=NULL,
                               choices = uiNode$checklist, 
                               selected = uiNode$checklist)
    }
    else{
      updateCheckboxGroupInput(session,
                               "policyTabNodesChecklist",
                               label=NULL,
                               choices = uiNode$checklist)
    }
    
  })
  
  currModel <- reactiveValues(model=stable.fit)
  uiNodeSlider <- reactiveValues(node=c())
  nodeStateProgress <- reactiveValues(progress=0)
  
  # make the necessary changes when the model is changed from dropdown menu
  observeEvent(input$customModelSelection,{
    # list the nodes checklist dynamically based on model instead of hardcoding
    
    if(input$customModelSelection == 'Default'){
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
    i <- 1
    uiNodeSlider$node <- c()
    nodeStateProgress$progress <- 1
    
    for(node in input$policyTabNodesChecklist){
      
      ## TODO: change this to list of list (of nodes with node state) to avoid having to create a new list for every node
      nodeStates <- state.definitions %>%
        filter(node_name==node) %>%
        select(-node_name) 
      
      nodeStateType <- node.definitions %>% filter(node_name==node) %>% select(type)
      
      if(nodeStateType == 'BooleanSlider'){
        nodeStateSlider <- sliderInput(node, "True (%)", min = 0, max = 100, step = 1, value = 0, post = "%")
      }
      else if(nodeStateType == "slider"){
        nodeStateSlider <- create_sliders(node, nodeStates$node_state)
      }
      else{ ## radio buttons
        nodeStateSlider <- radioButtons(node, label="", choices=nodeStates$node_state)
      }
      
      # remove the _ from the node to ease readability
      nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
      nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
      
      # list of nodes with corresponding state sliders
      uiNodeSlider$node[[i]] <- div(h4(nodeLabel), nodeStateSlider )
      i <- i+1
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
      shinyjs::hide(id="SimpleViewPolicyPrevious")
    }
    
    # Policy can only be added when all the selected nodes have been updated
    if(length(input$policyTabNodesChecklist) != 0 & nodeStateProgress$progress == length(uiNodeSlider$node)){
      shinyjs::hide(id="SimpleViewPolicyNext") # hide next button to avoid exceeding array size
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
  
  # method to check for the correctness of the input -- specifically for slider input of all states == 100
  checkForInputCorrectness <- function(node) {
    nodeStates <- state.definitions %>%
      filter(node_name==node) %>%
      select(-node_name)
    
    nodeStateType <- node.definitions %>% filter(node_name==node) %>% select(type)
    
    # sum to 100 is important when the input type is slider. For boolean slider and radio buttons, it is always equal to 100
    if(nodeStateType == 'slider'){
      
      # getting the total probability for slider input of each state
      currSumOfProbabilities <- 0
      for(state in nodeStates$node_state){
        currId = paste(node, state, sep ="-")
        currSumOfProbabilities <- currSumOfProbabilities +  input[[currId]]
      }
      
      if(currSumOfProbabilities != 100){
        # remove the _ from the node to ease readability
        nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
        nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
        
        errorMsg <- paste("Probabilities for '", nodeLabel, "' does not add up to to 100")
        shinyalert("Oops!", errorMsg, type = "error")
        
        return(FALSE)
      }
      else{
        return(TRUE)
      }
    }
    else {
      return(TRUE)
    }
    
  }
  
  
  observeEvent(input$SimpleViewPolicyNext, {
    node <- input$policyTabNodesChecklist[[nodeStateProgress$progress]]
    
    if(checkForInputCorrectness(node) == TRUE){
      nodeStateProgress$progress <- nodeStateProgress$progress + 1
    }
    
  })
  
  observeEvent(input$SimpleViewPolicyPrevious, {
    nodeStateProgress$progress <- nodeStateProgress$progress - 1
  })
  
  # Add policy action
  observeEvent(input$SimpleViewAddPolicy, {
    
    # check for input correctness of the last selected node
    lastNode <- input$policyTabNodesChecklist[[nodeStateProgress$progress]]
    if(checkForInputCorrectness(lastNode) == FALSE){
      return()
    }
    
    if(input$SimpleViewPolicyName == ""){
      shinyalert("Oops!", "Please provide a policy name", type = "error")
      return()
    }
    
    for(existingPolicyModel in names(CustomPolicies$models[[input$customModelSelection]])){
      if(input$SimpleViewPolicyName == existingPolicyModel){
        shinyalert("Oops!", "Policy name already exists", type = "error")
        return()
      }
    }
    
    
    for(node in input$policyTabNodesChecklist){
      # conditional probability table (cpt) of each node
      cpt <- as.data.frame(currModel$model[[node]]$prob)
      if ("Var1" %in% colnames(cpt)){
        cpt<- rename(cpt, !!node:=Var1)
      }
      
      ## For debugging
      # print(node)
      # print(cpt)
      # print("----------------------\n")
      
      nodeStates <- state.definitions %>%
        filter(node_name==node) %>%
        select(-node_name)
      
      nodeStateType <- node.definitions %>% filter(node_name==node) %>% select(type)
      
      # extract and set the values in CPT based on the input type -- BooleanSlider, slider, radiobutton
      if(nodeStateType == 'BooleanSlider'){
        # update cpt for the True state as the single slider signifies input for True %
        index <- cpt[[node]] == 'Yes'
        cpt$Freq[index] <- input[[node]]/100
        
        index <- cpt[[node]] == 'No'
        cpt$Freq[index] <- 1 - input[[node]]/100
      }
      else if(nodeStateType == "slider"){
        # updating the cpt for each state
        for(state in nodeStates$node_state){
          currId = paste(node, state, sep ="-")
          index <- cpt[[node]] == state
          
          
          cpt$Freq[index] <- input[[currId]]/100
        }
      }
      else{ ## radio buttons
        index <- cpt[[node]] == input[[node]]
        cpt$Freq[index] <- 1
        
        for(state in nodeStates$node_state){
          if(state != input[[node]]){
            index <- cpt[[node]] == state
            cpt$Freq[index] <- 0
          }
        }
      }
      
      ## For debugging
      # print("Updated CPT")
      # print(cpt)
      # print("---------------------------")
      
      # Updating the model
      # The data frame should be converted to a contigency and then the model is updated.
      # The table should be Freq~'all other columns'
      
      # get the column names excluding frequency
      cptFactors <- colnames(cpt)[1:length(colnames(cpt))-1]
      
      # formula for xtabs
      formula <- paste('Freq~', paste(cptFactors, collapse = "+"), sep="")
      
      # update the model
      currModel$model[[node]] <- xtabs(formula, cpt)
      
      print(as.data.frame(currModel$model[[node]]$prob))
    }
    
    # Calculate the utility of the new model
    currPolicyUtility <- calculate_utility(currModel$model)
    
    # update reactive policy list
    CustomPolicies$archiveList[[input$customModelSelection]] <- CustomPolicies$archiveList[[input$customModelSelection]] %>%
      add_row(name=input$SimpleViewPolicyName,
              Intellectual_Control=currPolicyUtility$Intellectual_Control,
              Renderability=currPolicyUtility$Renderability)
    
    CustomPolicies$models[[input$customModelSelection]][[input$SimpleViewPolicyName]] <- currModel$model
  })
  #Add reset button
  observeEvent(input$SimplePolicyReset, {
    if(input$customModelSelection == 'Default'){
      currModel$model <- stable.fit
    }
    else{
      currModel$model <- CustomPolicies$models[[input$customModelSelection]]$Base
    }
    
    # reset the progress for selected model
    nodeStateProgress$progress <- 0
    uiNodeSlider$node <- c()
    
    #reset check boxes
    updateCheckboxGroupInput(session,
                             "policyTabNodesChecklist",
                             label=NULL,
                             choices = uiNode$checklist, 
                             selected = c())
    
    #reset OAIS
    updateSelectInput(session, 
                      "customOaisEntitySelection",
                      choices = OAISentities, 
                      selected = 'None')
  })

  
  # ADVANCED POLICIES
  # save changes user has made
  advanced <- reactiveValues(updated_nodes = list(),
                             node_counter = 1)
  
  # add nodes to drop down list
  updateSelectInput(session, inputId="nodeProbTable", label="Select Node", choices=node.definitions$node_name)
  
  # Plot network which changes for policy inputs
  output$netPlot <- renderPlot({
    
    #model.label <- paste(input$model_version, "model", sep=" ")
    
    graphviz.plot(network$advanced.fit, layout = "dot",
                  highlight = list(nodes=c(input$nodeProbTable), fill="lightgrey"),
                  shape = "ellipse",
                  render = TRUE ) #,
                  #main=model.label)
    
  })
  
  # update model if different model version is selected
  observe({
    network$advanced.fit <- CustomModels$custom_networks[[input$model_version]]
  })
  
  # output probability table
  output$probabilityTable <- renderHotable({
    
    if (input$probtabltype == "Conditional Probability Table") {
      conditional.table <- as.data.frame(network$advanced.fit[[input$nodeProbTable]]$prob)
      
      # If a column is named Var1, rename to be variable name
      if ("Var1" %in% colnames(conditional.table)){
        conditional.table <- rename(conditional.table, !!input$nodeProbTable:=Var1)
      }
      
      # Spread dataframe so that it is easier to see which probabilities should add to 1.0
      conditional.table <- conditional.table %>%
                           mutate(Freq=Freq*100) %>%
                           pivot_wider(names_from=!!input$nodeProbTable, values_from=Freq)
      
      # Change column names to make them easier to understand
      # collect states
      node.states <- state.definitions %>% filter(node_name==input$nodeProbTable)
      # create read only vector
      read_table_temp <- rep(TRUE, ncol(conditional.table))
      
      # iterate and change names 
      i <- 0
      for (state in node.states$node_state){
        label <- paste(input$nodeProbTable, state, sep="=")
        conditional.table <- rename(conditional.table, !!label:=!!state)
        read_table_temp[length(read_table_temp) - i] <- FALSE
        i <- i + 1
      }

      
      data <- conditional.table
        
    } else {
      # convert model to grain object
      model.grain <- as.grain(network$advanced.fit)
      
      # find probability of findability and renderability
      query.results <- querygrain(model.grain, nodes=c(input$nodeProbTable))
      # control which columns can be read
      read_table_temp <- c(TRUE, FALSE)
      # return independent probability table
      data <- data.frame(query.results) %>%
      rownames_to_column() %>%
      rename(probability:=!!input$nodeProbTable,
             !!input$nodeProbTable:=rowname) %>%
      mutate(probability=100*probability)
    }

    data
    
  }, readOnly=FALSE)

  # plot node conditional probabilities
  output$nodeProbability <- renderPlot({
    bn.fit.barchart(network$advanced.fit[[input$nodeProbTable]])
  })
  
  # update node probability when changed
  observeEvent(input$updateProb, {
  
    data.df <- as.data.frame(hot.to.df(input$probabilityTable))
    custom_model <- network$advanced.fit
    
    # update model if table is conditional
    if (input$probtabltype == "Conditional Probability Table") {
      # collect column names related to probability
      probability_columns <- data.df %>% select(starts_with(input$nodeProbTable)) %>% colnames()
      remove_pattern <- paste(input$nodeProbTable, "=", sep="")
      new_columns <- c()
      
      # Check if all probabilities sum to 100% if not give error message
      prob_sum <- rowSums(data.df[, probability_columns])
      for (sum in prob_sum){
        if (sum != 100){
          errorMsg <-"One or more conditional probabilities do not sum to 100%."
          shinyalert("Oops!", errorMsg, type = "error")
          return()
        }
      }

      # iterate through columns and change to state name
      for (probability_column in probability_columns){
        
        # create new column name and rename old one, add to new columns vector
        new_column_name <- str_remove(probability_column, remove_pattern)
        data.df <- rename(data.df, !!new_column_name:=!!probability_column)
        new_columns <- c(new_column_name, new_columns)
      }
      
      # convert df to long form and divide probabilities by 100
      data.df <- data.df %>%
                 pivot_longer(new_columns,
                              names_to=input$nodeProbTable,
                              values_to="Freq") %>%
                 mutate(Freq=Freq/100)
      
      # columns need to be reordered so it can be correctly converted to a table
      # get column names except for node column to rearrange column order
      data.columns <- data.df %>% select(-!!input$nodeProbTable) %>% colnames
      data.columns <- c(input$nodeProbTable, data.columns)
      data.df <- data.df[data.columns]
      
      # convert df to table
      model.probability.table <- xtabs(Freq~., data.df)

    # update model if table is marginal
    } else{
      # check if the marginal probabilities sum to 100%
      sum <- data.df %>% summarise(total_prob=sum(probability))
      if (sum$total_prob != 100){
        errorMsg <-"The marginal probabilities do not sum to 100%."
        shinyalert("Oops!", errorMsg, type = "error")
        return()
      }
      
      # prepare table for update probability function
      data.df <- rename(data.df, state:=!!input$nodeProbTable)
      
      # collect model probability
      model.probability <- as.data.frame(custom_model[[input$nodeProbTable]]$prob)
      
      # if var1 is a column name, rename to be variable name
      if ("Var1" %in% colnames(model.probability)){
        model.probability <- rename(model.probability, !!input$nodeProbTable:=Var1)
      }
      
      model.probability.table <- update_probability(input$nodeProbTable,
                                                    model.probability, 
                                                    data.df)
    }
    # update model with new table
    custom_model[[input$nodeProbTable]] <- model.probability.table
    
    # update reactive value
    network$advanced.fit <- custom_model
    
    # check if node has already been added to checked list
    for (updated_node in advanced$updated_nodes){
      if (input$nodeProbTable == updated_node$children){
        return()
      }
    }
    
    advanced$updated_nodes[[advanced$node_counter]] <- tags$li(input$nodeProbTable)
    advanced$node_counter <- advanced$node_counter + 1
    
  })
  
  # update list of changed nodes
  output$ChangeNodes <- renderUI({
    advanced$updated_nodes
  })
  
  # add policy
  observeEvent(input$addPolicy, {
    # check if a name has been provided
    if (input$policyName == ""){
      errorMsg <-"No name was provided for the model!"
      shinyalert("Oops!", errorMsg, type = "error")
      return()
    }
    
    # check if name has already been used for another policy
    current_policies <- CustomPolicies$archiveList[[input$model_version]]
    if (input$policyName %in% current_policies$name){
      errorMsg <-"You have already used this policy name!"
      shinyalert("Oops!", errorMsg, type = "error")
      return()
    }
    
    utility <- calculate_utility(network$advanced.fit)
    
    CustomPolicies$archiveList[[input$model_version]] <- current_policies %>% 
                                                         add_row(name=input$policyName,
                                                                 Intellectual_Control=utility$Intellectual_Control,
                                                                 Renderability=utility$Renderability)
    
    CustomPolicies$models[[input$model_version]][[input$policyName]] = network$advanced.fit
  
  })
  
  # add custom model
  observeEvent(input$addModelAdvanced, {
    # check if a name has been provided
    if (input$policyName == ""){
      errorMsg <-"No name was provided for the model!"
      shinyalert("Oops!", errorMsg, type = "error")
      return()
    }
    
    # check if name is already being used for another custom model
    if (input$policyName %in% names(CustomModels$custom_networks)){
      errorMsg <-"You are already using this name for another custom model!"
      shinyalert("Oops!", errorMsg, type = "error")
      return()
    }
    
    # Add custom network to memory 
    CustomModels$custom_networks[[input$policyName]] = network$advanced.fit
    CustomPolicies$models[[input$policyName]] = list('Base'= network$advanced.fit)
    
    # calculate utility and store
    utility <- calculate_utility(network$advanced.fit)
    CustomModels$base_utility.df <- CustomModels$base_utility.df %>% add_row(name=input$policyName,
                                                                             Intellectual_Control=utility$Intellectual_Control,
                                                                             Renderability=utility$Renderability)
    # TODO: Why do we have two structures saving the same information?
    CustomPolicies$archiveList[[input$policyName]] <- tibble(name=input$policyName,
                                                             Intellectual_Control=utility$Intellectual_Control,
                                                             Renderability=utility$Renderability)
    
    # setting choices for the drop down list in the Simple view Node customisation tab
    customModelChoices <- CustomModels$base_utility.df %>% select(name)
    updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
    updateSelectInput(session, "model_version", label="Select Model", choices=customModelChoices)
    updateSelectInput(session, 'reportTabModelSelection', choices=CustomModels$base_utility.df$name)
  })
  
  # Reset network to original probabilities
  observeEvent(input$networkReset, {
    
    network$advanced.fit <- CustomModels$custom_networks[[input$model_version]]
    advanced$updated_nodes <- list()
    advanced$node_counter <- 1
  })

  # plot policy comparison
  output$PolicyComparison <- renderPlot({
    CustomPolicies$archiveList[[input$model_version]] %>%
      mutate(utility=Intellectual_Control+Renderability) %>% 
      pivot_longer(c(Intellectual_Control, Renderability), names_to="node") %>%
      ggplot(aes(x=reorder(name, -value), fill=node, y=value)) +
      geom_bar(position="stack", stat="identity") + xlab("Policy") + ylab("Score") +
      scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
  })
  
  # plot custom model comparison
  output$BaseUtilityComparison <- renderPlot({
    CustomModels$base_utility.df %>%
      mutate(utility=Intellectual_Control+Renderability) %>% 
      pivot_longer(c(Intellectual_Control, Renderability), names_to="node") %>%
      ggplot(aes(x=reorder(name, -value), fill=node, y=value)) +
      geom_bar(position="stack", stat="identity") + xlab("Model") + ylab("Score") +
      scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
  })
  
  # REPORT TAB
  
  setReportTabSummary <- function(currModelName, currModel){
    
    # utility weighting
    a <- input$RenderabilityWeighting
    b <- input$IntellectualWeighting
    
    # constructing text for the summary section
    summary <- paste("The", currModelName, "model has", length(currModel$name), "policy(ies) customised by the user (including the original):<br/><br/>")
    
    # to keep track of best policy
    maxUtility <- -99999
    maxUtilityPolicyName <- ""
    
    summary <- paste(summary, "<pre>", sep="")

    # getting list of policies
    for(policy in currModel$name){
      policyUtility <- currModel %>% filter(name==policy) %>% select(Renderability, Intellectual_Control)
      currUtility <- b*policyUtility$Intellectual_Control + a*policyUtility$Renderability 
      
      summary <- paste(summary, policy, "\t", currUtility, "<br/>", sep = "")
      
      if(currUtility > maxUtility){
        maxUtility <- currUtility
        maxUtilityPolicyName <- policy
      }
    }
    summary <- paste(summary, "</pre>", sep="")
    summary <- paste(summary, "<br/>", "The policy with best score for intellectual control and renderability is: <b>", maxUtilityPolicyName, "</b>")
    
    return(summary)
  }
  
  initialModelSetup <- reactiveValues(flag=TRUE)
  initialSimpleCustomisationPopup <- reactiveValues(flag=TRUE)
  
  observeEvent(input$sidebarMenu, {
    
    ## Initial Model and Pop setup flags
    if(initialModelSetup$flag){
      CustomPolicies$archiveList[['Default']] <- tibble(name="Base", 
                                                    Intellectual_Control=default_utility$Intellectual_Control,
                                                    Renderability=default_utility$Renderability)
      
      CustomPolicies$models[['Default']][['Base']] <- stable.fit
      
      initialModelSetup$flag = FALSE
    }
    
    if(input$sidebarMenu == 'CustomiseNode' & initialSimpleCustomisationPopup$flag){
      shinyalert("Please select the model for your archive. If you skipped step 1 - 'Customise Model', please create a model for your own archive by navigating to the tab 
                 '1. Customise Model'", type = "info")
      
      initialSimpleCustomisationPopup$flag = FALSE
    }
    
    # FOR REPORT TAB
    
    summary <- reactive({
      input$RenderabilityWeighting
      input$IntellectualWeighting
      
      setReportTabSummary(currModel, 
                          CustomPolicies$archiveList[[currModel]])
    })
    
    if(input$sidebarMenu == "Report"){
      currModel <- input$reportTabModelSelection
      # summary <- setReportTabSummary(currModel, 
      #                                CustomPolicies$archiveList[[currModel]])
      
      output$ReportTabSummaryText <- renderText(summary())
      
      # set the list of policies in drop down
      updateSelectInput(session, 
                        "ReportTabPolicySelection",
                        choices = CustomPolicies$archiveList[[currModel]]$name)
    }
  })
  
  observeEvent(input$reportTabModelSelection, {
    currModel <- input$reportTabModelSelection
    
    # set the summary
    summary <- setReportTabSummary(currModel, 
                                   CustomPolicies$archiveList[[currModel]])
    
    output$ReportTabSummaryText <- renderText(summary)
    
    # set the list of policies in drop down
    updateSelectInput(session, 
                      "ReportTabPolicySelection",
                      choices = CustomPolicies$archiveList[[currModel]]$name)
    
  })
  
  # Check if utiltiy weighting changes
  observe({
    utility_weighting$Renderability <- input$RenderabilityWeighting
    utility_weighting$Intellectual <- input$IntellectualWeighting
  })
  
  plotUtility <- reactive({
    a <- utility_weighting$Renderability
    b <- utility_weighting$Intellectual
    
    CustomPolicies$archiveList[[input$reportTabModelSelection]] %>%
      pivot_longer(c(Intellectual_Control, Renderability), names_to="policy") %>%
      mutate(value=ifelse(policy=="Renderability", value*a, value*b)) %>%
      ggplot(aes(x=reorder(name, -value), fill=policy, y=value)) +
      geom_bar(position="stack", stat="identity") + xlab("Policy") + ylab("Score") +
      scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
  })
  
  # Plot the policy comparison stacked bar chart
  output$ReportTabUtilityComparisonPlot <- renderPlot(
    {
      plotUtility()
    }
  )
  
  output$reportTabDownloadBtn <- downloadHandler(
    
    filename = function() {
      paste0(input$reportTabModelSelection, ".zip")
    },
    
    content = function(file){
      
      # write model
      if ("Policy Model" %in% input$downloadOptions) {
        write.bif(paste0(input$ReportTabPolicySelection, ".bif"),
                  CustomPolicies$models[[input$reportTabModelSelection]][[input$ReportTabPolicySelection]])
      }
      
      # write utility plot
      if ("Archive Model Score Comparison Plot" %in% input$downloadOptions) {
        png(filename=paste0(input$reportTabModelSelection, ".png"))
        print(plotUtility())
        dev.off()
      }
      
      # if ("Documented Report" %in% input$downloadOptions){
      #   pdf(file=paste0(input$reportTabModelSelection, ".pdf"), onefile = TRUE)
      # 
      #   currModel <- input$reportTabModelSelection
      #   summary <- setReportTabSummary(currModel,
      #                                  CustomPolicies$archiveList[[currModel]])
      # 
      #   grid.arrange(output$ReportTabSummaryText, plotUtility())
      #   dev.off()
      # }
      
      # create zip file to return
      filenames <- c(paste0(input$ReportTabPolicySelection, ".bif"),
                     paste0(input$reportTabModelSelection, ".png")
                     #,paste0(input$reportTabModelSelection, ".pdf")
                     )
      
      zip(file, filenames)
      
      # delete all files on server
      for (filename in filenames){
        file.remove(filename)
      }
    }
  )

})



# Following script defines interactions between components displayed
# on the webpage.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com
# @author: Sidhant Bhatia, Monash University, Malaysia
# @email: sidhant3b@gmail.com

# library(shiny)
# library(graph)
# library(bnlearn)
# library(networkD3)
# library(BiocManager)
# library(Rgraphviz)
# library(tidyverse)
# library(shinyjs)
# library(shinyalert)
# library(gridExtra)
# library(data.table)

# options(repos = BiocManager::repositories())
# options(shiny.fullstacktrace = FALSE)

# TODO: policy plotting should be one reactive variable

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param question_data The question data, named list, likely loaded from the YAML file internal to this package
#' @param default_response The default responses used to initialise the model
#' @param model The stored model, loaded from a .bif file
#' @importFrom plotly renderPlotly plot_ly
#' @importFrom utils zip
#' @importFrom grDevices png dev.off
#' @importFrom bnlearn read.bif graphviz.plot mutilated as.grain bn.fit.barchart write.bif
#' @importFrom gRain querygrain
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggplot geom_bar aes xlab geom_hline stat_summary geom_text theme_light theme element_blank element_text scale_fill_manual position_stack ylab
#' @importFrom dplyr arrange filter select rename add_row summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom shiny reactiveValues updateSelectInput plotOutput renderUI tagList strong renderTable
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyWidgets sliderTextInput updateProgressBar
#' @importFrom shinyalert shinyalert
#' @importFrom tibble tibble rownames_to_column
#' @importFrom rlang .data
#' @importFrom shinyjs show hide enable
#' @importFrom shinysky renderHotable hot.to.df
#' @importFrom stringr str_remove
#' @importFrom DT datatable renderDataTable
#' @importFrom stats reorder
#' @export

app_server = function(input, output, session, question_data, default_response, model, scoring_funcs) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  # output from the model builder tab
  q_output = callModule(questions_module_server, 'model-questions', question_data = question_data, default_response = default_response)
  p_output = callModule(policy_creation_module_server, 'policy-questions', reactive(model_obj$data), question_data = question_data, model = model, scoring_funcs = scoring_funcs)

  mod_only_table = callModule(model_table_module_server, "model_table", data = reactive(model_obj$data), model = model, selection = "none", show_policy = FALSE, scoring_funcs = scoring_funcs)
  save_table = callModule(model_table_module_server, "save_table", data = reactive(model_obj$data), model = model, selection = "multiple", show_policy = TRUE, scoring_funcs = scoring_funcs)

  policy_vis = callModule(policy_visualisation_module_server, 'bar', model_data = reactive(model_obj$data), model = model, scoring_funcs = scoring_funcs)


  observeEvent(input$createModel, {
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'model')
  })
  output$download = shiny::downloadHandler(
    filename = function() {
      paste0("Diagram-data-", Sys.Date(), ".json")
    },
    content = function(file) {
      save_responses(model_obj$data[save_table(),], file)
    }
  )

  observeEvent(input$upload, {
    file = input$upload
    print(file)
    req(file)
    shiny::validate(need(tools::file_ext(file$datapath) == "json", "Please upload a json file."))
    content = load_responses(file$datapath)
    print("content")
    model_obj$data = dplyr::bind_rows(
      model_obj$data, content
    )
    print("upload complete")
  })

  observeEvent(q_output$scenario(),{
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'scenario')
  })

  observeEvent(q_output$visualise(),{
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
  })

  model_obj = reactiveValues(
    data = tibble(model = character(), policy = character(), notes = character(), response = list())
  )

  # observeEvent(model_obj$data, {
  #   shinyjs::toggleElement("no-model-container", condition = nrow(model_obj$data) == 0)
  #   # nrow(model_obj$data) != 0
  #   # shinyjs::toggleElement("model-table-container", condition = nrow(model_obj$data) != 0)
  # })

  observeEvent(model_obj$data, {
    # shinyjs::toggleElement("no-model-container", condition = nrow(model_obj$data) == 0)
    if(nrow(model_obj$data) != 0) {
      print("show element")
      shinyjs::show("model-table-container")
    }
  })

  observeEvent(q_output$finish(), {
    print("finished")
    new_row = model_policy_row(q_output$state(), model_name = q_output$name(), notes = q_output$comments())
    model_obj$data = dplyr::bind_rows(model_obj$data, new_row)
  })

  all_policy = reactiveValues(data = NULL)

  observeEvent(p_output$state(), {
    req(!is.null(p_output$state()))
    # vals = dplyr::distinct(dplyr::bind_rows(p_output(), model_obj$data))
    model_obj$data = dplyr::bind_rows(model_obj$data, p_output$state())
  })

  observeEvent(p_output$visualise(), {
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
  })
}


#' app_server = function(input, output, session) {
#'
#'   # -------------------- FUNCTIONS --------------------
#'
#'   # Function creates multiple sliders for the different states in a node
#'   #' @importFrom shiny sliderInput
#'   create_sliders <- function(node, states) {
#'     # creates a list of sliders inputs for each state of the respective node
#'     j <- 1
#'     nodeStateSlider <- c()
#'     for(state in states){
#'       inputId <- paste(node, state, sep = "-")
#'       label <- paste(state, "(%)")
#'       nodeStateSlider[[j]] <- shiny::sliderInput(inputId, label, min = 0, max = 100, step = 1, value = 0, post = "%")
#'       j <- j+1
#'     }
#'     # list of nodes with corresponding state sliders
#'     return(nodeStateSlider)
#'   }
#'
#'   # Function collects inputs from generated sliders and combines into dataframe
#'   #' @importFrom dplyr filter mutate select
#'   #' @importFrom tibble tibble
#'   #' @importFrom rlang .data
#'   collect_slider_inputs <- function(name){
#'     # create input ids to access slider information
#'     input.ids <- state.definitions %>%
#'       dplyr::filter(.data$node_name==name) %>%
#'       dplyr::mutate(id=paste(.data$node_name, .data$node_state, sep = "-")) %>%
#'       dplyr::select(.data$id, .data$node_state)
#'     # create vectors for states and probabilities
#'     states <- c()
#'     probabilities <- c()
#'     # iterate through all id's and collect values
#'     i <- 1
#'     for (id in input.ids$id) {
#'       # collect states and their probabilities
#'       state <- input.ids$node_state[i]
#'       probability <- input[[id]]
#'       # add to vectors
#'       states <- c(states, state)
#'       probabilities <- c(probabilities, probability)
#'       # iterate to next state
#'       i <- i + 1
#'     }
#'     # combine states into one dataframe
#'     return(tibble::tibble(state=states, probability=probabilities))
#'   }
#'
#'   # Function collects inputs from Boolean sliders and combines into dataframe
#'   #' @importFrom dplyr filter select
#'   #' @importFrom tibble tibble
#'   #' @importFrom rlang .data
#'   collect_boolean_slider_inputs <- function(name){
#'     # collect states of the node
#'     next_states <- state.definitions %>%
#'       dplyr::filter(.data$node_name==name) %>%
#'       dplyr::select(.data$node_state)
#'
#'     # collect primary and secondary state
#'     primary_state <- next_states$node_state[1]
#'     secondary_state <- next_states$node_state[2]
#'
#'     # create input id
#'     inputId <- paste(name, primary_state, sep="-")
#'
#'     # collect probabilities
#'     if(name=="Physical_Disaster"){
#'       # High is above 3.3%. For the purposes of the model, treat as 5%
#'       if(input[[inputId]]=="High") {primary_prob <- 5}
#'       # Medium is between 1% and 3.3% .For the purposes of the model, treat as 2%
#'       if(input[[inputId]]=="Medium") {primary_prob <- 2}
#'       # Low is between 0.1% and 1%. For the purposes of the model, treat as 0.5%
#'       if(input[[inputId]]=="Low") {primary_prob <- 0.5}
#'       # Very low is less than 0.1%. For the purposes of the model, treat as 0.05%
#'       if(input[[inputId]]=="Very Low") {primary_prob <- 0.05}
#'     }
#'     else {
#'       primary_prob <- input[[inputId]]
#'     }
#'     secondary_prob <- 100 - primary_prob
#'     # return tibble of probabilities
#'     return(
#'       tibble::tibble(
#'         state=c(primary_state, secondary_state),
#'         probability=c(primary_prob, secondary_prob)
#'       )
#'     )
#'   }
#'
#'   # Function updates probability tables of model with user inputs
#'   #' @importFrom dplyr mutate
#'   #' @importFrom stats xtabs
#'   update_probability <- function(node, model.probability.df, input.probability.df){
#'     # collect node states and corresponding probabilities
#'     node_states <- input.probability.df$state
#'     state_probabilities <- input.probability.df$probability
#'     # iterate through states and update model probability table
#'     i <- 1
#'     for (state in node_states){
#'       current.probability <- state_probabilities[i]
#'       model.probability.df <- model.probability.df %>%
#'         dplyr::mutate(Freq=ifelse(model.probability.df[[node]]==state, current.probability, .data$Freq))
#'       i <- i + 1
#'     }
#'     # normalise probability range between 0 and 1
#'     model.probability.df <- model.probability.df %>% dplyr::mutate(Freq=.data$Freq/100)
#'     # convert from data.frame to table
#'     model.probability.table <- stats::xtabs(Freq~., model.probability.df)
#'     return(model.probability.table)
#'   }
#'
#'
#'   #Function to sensitivity test based on hard evidence
#'   #' @import data.table
#'   #' @importFrom gRain setEvidence
#'   hard.test <- function(model) { #work with custom_model which is a bif
#'     hard.evidence <- as.data.table(state.definitions[,1:2])
#'     input.nodes <- as.list(setup_questions)$node_name
#'     #remove renderability and intellectual control
#'
#'     #####
#'     # suggested work around by data.table authors for no visible binding package check warning
#'     Type = Score = R_score = IC_score = Difference = R_diff = IC_diff = node_name = NULL
#'     #####
#'
#'     hard.evidence <- hard.evidence[
#'       node_name != "Renderability" &
#'         node_name != "Intellectual_Control",
#'     ]
#'     hard.evidence[, Type := ifelse((node_name %in% input.nodes), "Input", "Cond")] #Inputs and conditionals
#'     #Set up columns
#'     hard.evidence[, Score := as.numeric()]
#'     hard.evidence[, R_score := as.numeric()]
#'     hard.evidence[, IC_score := as.numeric()]
#'     hard.evidence[, Difference := as.numeric()]
#'     hard.evidence[, R_diff := as.numeric()]
#'     hard.evidence[, IC_diff := as.numeric()]
#'     # JR note: calculate utility is diagram defined
#'     R_orig <- as.numeric(calculate_utility(model)$Renderability)*50
#'     IC_orig <- as.numeric(calculate_utility(model)$Intellectual_Control)*50
#'     for (i in 1:(dim(hard.evidence)[1])) {
#'       #reset node probability
#'       temp.model <- model
#'       #if (hard.evidence$Type[i]=="Input") { #might be an unneccessary condition, but will speed things up
#'       node <- hard.evidence$node_name[i]
#'       #   default.probability.df <- as.data.frame(stable.fit[[node]]$prob) #relies on default all being 'soft'
#'       #   model.probability.df <- as.data.frame(model[[node]]$prob)
#'       #
#'       # temp.probability.table <- update_probability(node, model.probability.df, default.probability.df)
#'       # # update probability table for node
#'       temp.model[[node]] <- as.array(stable.fit[[node]]$prob)
#'       #}
#'       grain.model <- bnlearn::as.grain(temp.model)
#'
#'       test <- gRain::setEvidence(
#'         grain.model,
#'         as.character(hard.evidence$node_name[i]), #node name
#'         as.character(hard.evidence$node_state[i]) #node state
#'       )
#'
#'       # JR note: calculate utility is diagram defined
#'       R_new <- as.numeric(calculate_utility(test)$Renderability)*50
#'       IC_new <- as.numeric(calculate_utility(test)$Intellectual_Control)*50
#'
#'       hard.evidence$Score[i] <- as.numeric(format(round(R_new+IC_new,2),nsmall=2))
#'       hard.evidence$R_score[i] <- as.numeric(format(round(R_new,2),nsmall=2))
#'       hard.evidence$IC_score[i] <- as.numeric(format(round(IC_new,2),nsmall=2))
#'
#'
#'       hard.evidence$Difference[i] <- as.numeric(format(round(R_new+IC_new - R_orig-IC_orig,2),nsmall=2))
#'       hard.evidence$R_diff[i] <- as.numeric(format(round(R_new - R_orig,2),nsmall=2))
#'       hard.evidence$IC_diff[i] <- as.numeric(format(round(IC_new - IC_orig,2),nsmall=2))
#'
#'       rm(test,R_new,IC_new)
#'
#'       i <- i + 1
#'     }
#'     hard.evidence[order(-Score)]
#'   }
#'   # --------------------   FUNCTIONS   --------------------
#'
#'   # -------------------- STATIC VALUES --------------------
#'   stable.fit <- bnlearn::read.bif(system.file("default_model","Model.bif", package = "diagramNAT"))
#'
#'   # node definitions and state definitions
#'   node.definitions <- readr::read_csv(system.file("text_content", "node_information.csv", package = "diagramNAT")) %>%
#'     dplyr::arrange(.data$node_name)
#'   state.definitions <- readr::read_csv(system.file("text_content", "node_states.csv", package = "diagramNAT"))
#'
#'   # csv containing nodes and questions used during setup
#'   setup_questions <- readr::read_csv(system.file("text_content", "setup_questions.csv", package = "diagramNAT"))
#'
#'   # Default risk
#'   # JR note: calculate utility is diagram defined
#'   default_utility <- calculate_utility(stable.fit)
#'
#'   # --------------------   STATIC VALUES    --------------------
#'
#'   # --------------------   REACTIVE VALUES  ---------------------
#'
#'   # initialise stable plot (unchanging) and reactive plot
#'   network <- shiny::reactiveValues(
#'     model.fit = bnlearn::read.bif(system.file("default_model", "Model.bif", package= "diagramNAT")),
#'     advanced.fit = stable.fit
#'   )
#'
#'   # Initialise Question Counter for model setup
#'   questionValues <- reactiveValues(question_number=1)
#'
#'   # Create vector to store answers to questions
#'   answers <- reactiveValues(
#'     radio_answers=list(),
#'     slider_answers=list(),
#'     boolean_slider_answers=list()
#'   )
#'
#'   # Customised models
#'   CustomModels <- reactiveValues(
#'     base_utility.df=tibble::tibble(
#'       name="Default",
#'       Intellectual_Control=default_utility$Intellectual_Control,
#'       Renderability=default_utility$Renderability
#'     ),
#'     custom_networks=list("Default"=stable.fit)
#'   )
#'
#'   ## TODO:sid - combine both into a single data structure
#'   # Customised Policies
#'
#'   # CustomPolicies <- reactiveValues(archiveList=list("Default"= tibble(name="Default",
#'   #                                                                 findability=default_utility$Findability,
#'   #                                                                 renderability=default_utility$Renderability)),
#'   #                                  models=list("Default"=list("Base"=stable.fit)))
#'   CustomPolicies <- reactiveValues(
#'     archiveList=list(),
#'     models=list()
#'   )
#'
#'   utility_weighting <- reactiveValues(
#'     Renderability=1,
#'     Intellectual=1
#'   )
#'
#'   # --------------------   REACTIVE VALUES  ---------------------
#'
#'   # -------------------- NODE DEFINITION TAB --------------------
#'
#'   # Update node drop down list with node names
#'   shiny::updateSelectInput(
#'     session, "NodeSelection",
#'     label=NULL,
#'     choices=node.definitions$node_name
#'   )
#'
#'   # plot network used on the network tab
#'   output$NetworkStructure <- shiny::renderPlot({
#'     bnlearn::graphviz.plot(
#'       stable.fit, layout = "dot",
#'       highlight = list(nodes=c(input$NodeSelection), fill="lightgrey"),
#'       shape = "ellipse",
#'       render = TRUE
#'     )
#'   })
#'
#'   # Output node definiton text
#'   output$NodeDefinition <- shiny::renderUI({
#'     definition <- node.definitions %>%
#'       dplyr::filter(.data$node_name==input$NodeSelection) %>%
#'       dplyr::select(.data$node_definition) %>%
#'       as.character()
#'     shiny::tagList(shiny::strong("Definition: "), definition)
#'   })
#'
#'
#'   # Output hyperlink to data source
#'    output$DataLink <- shiny::renderUI({
#'      url <- node.definitions %>%
#'        dplyr::filter(.data$node_name==input$NodeSelection) %>%
#'        dplyr::select(.data$data_source) %>%
#'        as.character()
#'   #   url <- a(input$NodeSelection, href=url) remove hyperlink
#'      shiny::tagList(shiny::strong("Data source: "), url)
#'    })
#'
#'   # Output Year of node
#'   output$DataYear <- shiny::renderUI({
#'     year <- node.definitions %>%
#'       dplyr::filter(.data$node_name==input$NodeSelection) %>%
#'       dplyr::select(.data$node_year) %>%
#'       as.character()
#'     shiny::tagList(shiny::strong("Data collected: "), year)
#'   })
#'
#'   # Output node state definition table
#'   output$StateDefinition <- shiny::renderTable({
#'     state.definitions %>%
#'       dplyr::filter(.data$node_name==input$NodeSelection) %>%
#'       dplyr::select(-.data$node_name) %>%
#'       dplyr::rename(
#'         `Node State`=.data$node_state,
#'         `State Definition`=.data$state_definition
#'       )
#'   })
#'
#'   # -------------------- NODE DEFINITION TAB --------------------
#'
#'   # -------------------- MODEL CUSTOMISATION --------------------
#'   shiny::observeEvent(input$createModel, {
#'     shinydashboard::updateTabItems(session, "sidebarMenu", "CustomiseModel")
#'   })
#'   # Update state selection radio buttons
#'   # Collect the first node
#'   first_node <- setup_questions[1,]$node_name
#'
#'   # retrieve states associated with the first node
#'   first_states <- state.definitions %>%
#'     dplyr::filter(.data$node_name==first_node) %>%
#'     dplyr::select(.data$node_state)
#'
#'   # Create user input UI which is at the bottom of the box
#'   output$CustomisationInput <- shiny::renderUI({
#'     # collect next node questions
#'     next_node_name <- setup_questions[questionValues$question_number,]$node_name
#'     # collect type of input, radiobutton or slider
#'     next_node <- node.definitions %>%
#'       dplyr::filter(node_name==next_node_name)
#'     number_of_questions <- nrow(setup_questions)
#'     # If all questions have not been answered yet render next button
#'     # If input type is slider, render multiple sliders for the different input states
#'     if (questionValues$question_number < number_of_questions+1 && next_node$type=="slider") {
#'       # collect next node name
#'       # next_node <- setup_questions[questionValues$question_number,]$node_name
#'       # collect states of the next node
#'       next_states <- state.definitions %>%
#'         dplyr::filter(.data$node_name==next_node$node_name) %>%
#'         dplyr::select(.data$node_state)
#'       node_text <- ""
#'       if (next_node$node_name == "Storage_Medium"){
#'         node_text <- shiny::HTML(paste(
#'           shiny::h5(
#'             shiny::tags$b("A"),
#'             "- Expected lifespan below 10 years or unknown, highly susceptible to physical
#'             damage, requires specific environmental conditions and very sensitive to changes,
#'             does not support error-detection methods, supporting technology is novel, proprietary
#'             and limited. Examples include USB flash drive, floppy disk, SD drive and CD-R discs."
#'           ),
#'           shiny::h5(
#'             shiny::tags$b("B"),
#'             "- A proven lifespan of at least 10 years, low susceptibility to physical damage,
#'             tolerant of a wide range of environmental conditions without data loss, supports
#'             robust error-detection methods, supporting technology is well established and widely
#'             available. Examples include LTO tapes, blu ray discs and CD-ROM discs."
#'           ),
#'           shiny::h5(
#'             shiny::tags$b("C"),
#'             "- An external company is taking responsibility for our data storage. Examples
#'             include Amazon Simple Storage Service, Microsoft Azure Archive Storage and Google
#'             Cloud Storage."
#'           )
#'         ))
#'       }
#'       rendered_element <- shiny::div(
#'         shiny::fluidRow(
#'           shiny::column(
#'             width=5,
#'             # JR note create_slider is Diagram defined function
#'             create_sliders(next_node$node_name, next_states$node_state)
#'           ),
#'           shiny::column(
#'             width=5, offset=1,
#'             node_text
#'           )
#'         ),
#'         shiny::fluidRow(
#'           shiny::column(
#'             width=2,
#'             shiny::tags$style(shiny::HTML('#BackButton{background-color:grey}')),
#'             shiny::tags$style(shiny::HTML('#BackButton{color:white}')),
#'             shiny::tags$style(shiny::HTML('#BackButton{width:100%}')),
#'             shiny::actionButton("BackButton", "Back") #changed to uk order
#'           ),
#'           shiny::column(
#'             width=2,
#'             shiny::tags$style(shiny::HTML('#NextQuestion{background-color:green}')),
#'             shiny::tags$style(shiny::HTML('#NextQuestion{color:white}')),
#'             shiny::tags$style(shiny::HTML('#NextQuestion{width:100%')),
#'             shiny::actionButton("NextQuestion", "Next") #changed to uk order
#'           )
#'         )
#'       )
#'     } else if(questionValues$question_number < number_of_questions+1 && next_node$type=="radiobuttons") {
#'
#'       # collect next node name
#'       # next_node <- setup_questions[questionValues$question_number,]$node_name
#'       text_description <- shiny::column(width=5)
#'
#'       # collect states of the next node
#'       next_states <- state.definitions %>%
#'         dplyr::filter(.data$node_name==next_node$node_name) %>%
#'         dplyr::select(.data$node_state)
#'
#'       rendered_element <- shiny::div(
#'         shiny::fluidRow(
#'           shiny::column(
#'             width=5,
#'             shiny::radioButtons("StateSelection", label=NULL, choices=next_states$node_state)
#'           ),
#'           text_description
#'         ),
#'         shiny::fluidRow(
#'           shiny::column(
#'             width=2,
#'             shiny::tags$style(shiny::HTML('#BackButton{background-color:grey}')),
#'             shiny::tags$style(shiny::HTML('#BackButton{color:white}')),
#'             shiny::tags$style(shiny::HTML('#BackButton{width:100%}')),
#'             shiny::actionButton("BackButton", "Back") #changed to uk order
#'           ),
#'           shiny::column(
#'             width=2,
#'             shiny::tags$style(shiny::HTML('#NextQuestion{background-color:green}')),
#'             shiny::tags$style(shiny::HTML('#NextQuestion{color:white}')),
#'             shiny::tags$style(shiny::HTML('#NextQuestion{width:100%')),
#'             shiny::actionButton("NextQuestion", "Next") #changed to uk order
#'           )
#'         )
#'       )
#'     } else if (questionValues$question_number < number_of_questions+1 && next_node$type=="BooleanSlider"){
#'
#'       # collect states of the next node
#'       next_states <- state.definitions %>%
#'         dplyr::filter(.data$node_name==next_node$node_name) %>%
#'         dplyr::select(.data$node_state)
#'
#'       # collect primary state
#'       primary_state <- next_states$node_state[1]
#'       inputId <- paste(next_node$node_name, primary_state, sep="-")
#'       label <- paste(primary_state, "%")
#'       node_text <- ""
#'       if (next_node$node_name == "Technical_Skills"){
#'         node_text <- shiny::h5(
#'           "The default is based on responses to the JISC digital skills
#'           survey and how many said that there was full capability within their organisation
#'           to do file format migration, software emulation or data recovery. (15%)"
#'         )
#'       }
#'       if (next_node$node_name == "System_Security"){
#'         node_text <- shiny::h5(
#'           "The default is based on responses to the JISC digital skills
#'           survey and how many agreed that their IT provider supports the requirements of
#'           the archival activities of your organisation toa large or very great extent and
#'           that their digital collections are protected with access restrictions/
#'           permissions. (17%)"
#'         )
#'       }
#'       if (next_node$node_name == "Info_Management"){
#'         node_text <- shiny::h5(
#'           "The default is based on responses to the JISC digital skills
#'           survey. 70% of respondents agreed that their catalogue management
#'           system meets the needs of the organisation and 40% that their
#'           digital asset management system meets the needs of the organisation.
#'           We have estimated that 55% would therefore have sufficient
#'           information management systems, as you don't need a bespoke
#'           digital asset management system to have support for coherent
#'           information management and documentation of preservation actions,
#'           but you may need more than just a catalogue system."
#'         )
#'       }
#'       if (next_node$node_name == "Physical_Disaster"){
#'         node_text <- shiny::a(
#'           href="https://flood-warning-information.service.gov.uk/long-term-flood-risk/postcode",
#'           'Click here to check your flood risk.',target="_blank"
#'         )
#'         rendered_element <- shiny::div(
#'           shiny::fluidRow(
#'             shiny::column(
#'               width=5,
#'               node_text,
#'               shiny::br(),
#'               shiny::br(),
#'               shinyWidgets::sliderTextInput(
#'                 inputId, "Risk rating from gov.uk", grid = TRUE, force_edges = TRUE,
#'                 choices = c("Very Low", "Low", "Medium", "High")
#'               )
#'             )
#'           ),
#'           shiny::fluidRow(
#'             shiny::column(
#'               width=2,
#'               shiny::tags$style(shiny::HTML('#BackButton{background-color:grey}')),
#'               shiny::tags$style(shiny::HTML('#BackButton{color:white}')),
#'               shiny::tags$style(shiny::HTML('#BackButton{width:100%}')),
#'               shiny::actionButton("BackButton", "Back") #changed to uk order
#'             ),
#'             shiny::column(
#'               width=2,
#'               shiny::tags$style(shiny::HTML('#NextQuestion{background-color:green}')),
#'               shiny::tags$style(shiny::HTML('#NextQuestion{color:white}')),
#'               shiny::tags$style(shiny::HTML('#NextQuestion{width:100%')),
#'               shiny::actionButton("NextQuestion", "Next") #changed to uk order
#'             )
#'           )
#'         )
#'       }
#'       else {
#'         rendered_element <- shiny::div(
#'           shiny::fluidRow(
#'             shiny::column(
#'               width=5,
#'               shiny::sliderInput(inputId, label, min = 0, max = 100, step = 1, value = 0, post = "%")
#'             ),
#'             shiny::column(
#'               width=5, offset=1,
#'               node_text
#'             )
#'           ),
#'           shiny::fluidRow(
#'             shiny::column(
#'               width=2,
#'               shiny::tags$style(shiny::HTML('#BackButton{background-color:grey}')),
#'               shiny::tags$style(shiny::HTML('#BackButton{color:white}')),
#'               shiny::tags$style(shiny::HTML('#BackButton{width:100%}')),
#'               shiny::actionButton("BackButton", "Back") #changed to uk order
#'             ),
#'             shiny::column(
#'               width=2,
#'               shiny::tags$style(shiny::HTML('#NextQuestion{background-color:green}')),
#'               shiny::tags$style(shiny::HTML('#NextQuestion{color:white}')),
#'               shiny::tags$style(shiny::HTML('#NextQuestion{width:100%')),
#'               shiny::actionButton("NextQuestion", "Next") #changed to uk order
#'             )
#'           )
#'         )
#'
#'       }
#'     } else {
#'       rendered_element <- shiny::fluidRow(
#'         shiny::column(
#'           width=3,
#'           shiny::textInput(
#'             inputId="CustomisedModelName",
#'             label=NULL
#'           ),
#'         ),
#'         shiny::column(
#'           width=2,
#'           shiny::tags$style(shiny::HTML('#SaveModel{background-color:green}')),
#'           shiny::tags$style(shiny::HTML('#SaveModel{color:white}')),
#'           shiny::actionButton("SaveModel", "Name Model")
#'         ),
#'         shiny::column(
#'           width=1,
#'           offset=5,
#'           shiny::tags$style(shiny::HTML('#AddNew{background-color:grey}')),
#'           shiny::tags$style(shiny::HTML('#AddNew{color:white}')),
#'           shiny::actionButton("AddNew", "Create New Model")
#'         )
#'       )
#'     }
#'     rendered_element
#'   })
#'
#'   # Add question to setup page.
#'   output$Question <- shiny::renderUI({
#'     if (questionValues$question_number < nrow(setup_questions)+1 && questionValues$question_number>=1){
#'       shiny::HTML(
#'         paste(
#'           shiny::h3(
#'             shiny::strong(
#'               paste0(questionValues$question_number, ". ", setup_questions[questionValues$question_number,]$node_name)
#'             )
#'           ),
#'           shiny::h4("Please answer the following question:"),
#'           shiny::h4(setup_questions[questionValues$question_number,]$node_question)
#'         )
#'       )
#'     } else {
#'       shiny::h4(shiny::strong("All questions answered. Please give model a name:"))
#'     }
#'   })
#'
#'   # Update question when next question button is pressed
#'   shiny::observeEvent(input$NextQuestion, {
#'     # collect next node question
#'     next_node_name <- setup_questions[questionValues$question_number,]$node_name
#'     # collect information on node
#'     next_node <- node.definitions %>%
#'       dplyr::filter(.data$node_name==next_node_name)
#'
#'     # add state to answer vector
#'     # if radio button, add to radio button answers
#'     if (next_node$type == "radiobuttons") {
#'       answers$radio_answers[[next_node$node_name]] = input$StateSelection
#'     } else if (next_node$type == "slider") {
#'       # collcet input probabilities and check the sum
#'       # JR note : collect_slider_inputs is DIAGRAM defined function
#'       input.probabilities <- collect_slider_inputs(next_node$node_name)
#'       prob.summary <- input.probabilities %>%
#'         dplyr::summarise(prob_sum=sum(.data$probability))
#'
#'       # if sum of probability is not 100 alert user and break out of function
#'       if (prob.summary$prob_sum != 100){
#'         errorMsg <- paste("Probabilities for '", next_node$node_name, "' do not add up to to 100%")
#'         shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'         return()
#'       } else {
#'         answers$slider_answers[[next_node$node_name]] = input.probabilities
#'       }
#'     } else if (next_node$type == "BooleanSlider"){
#'       # JR note : collect_boolean_slider_inputs is DIAGRAM defined function
#'       input.probabilities<- collect_boolean_slider_inputs(next_node$node_name)
#'       answers$boolean_slider_answers[[next_node$node_name]] = input.probabilities
#'     }
#'     # update question number
#'     questionValues$question_number <- questionValues$question_number + 1
#'     # update progress bar
#'     shinyWidgets::updateProgressBar(
#'       session=session,
#'       id="Question_Progress",
#'       value=min(questionValues$question_number,nrow(setup_questions)),
#'       total=nrow(setup_questions)
#'     )
#'   })
#'
#'   # Update questions when back button is pressed
#'   shiny::observeEvent(input$BackButton, {
#'     if (questionValues$question_number > 1) {
#'       # collect input type, whether radio button or slider
#'       input_type <- setup_questions[questionValues$question_number,]$type
#'       # collect node name
#'       node_name <- setup_questions[questionValues$question_number,]$node_name
#'       # remove last answer
#'       # if radio button input, remove from radio button list
#'       if (input_type == "radiobuttons"){
#'         answers$radio_answers[[node_name]] = NULL
#'       } else if (input_type == "slider"){
#'         answers$slider_answers[[node_name]] = NULL
#'       }
#'       # update question number
#'       questionValues$question_number <- questionValues$question_number - 1
#'     }
#'     if (questionValues$question_number >= 1) {
#'       # update progress bar
#'       shinyWidgets::updateProgressBar(
#'         session=session,
#'         id="Question_Progress",
#'         value=questionValues$question_number,
#'         total=nrow(setup_questions)
#'       )
#'     }
#'   })
#'
#'   # Save model to memory
#'   shiny::observeEvent(input$SaveModel, {
#'     # Check if model has been named correctly
#'     if (input$CustomisedModelName == "") {
#'       errorMsg <-"Please give your custom model a name."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'     if (input$CustomisedModelName %in% names(CustomModels$custom_networks)){
#'       errorMsg <-"You have already used this name for another custom model."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'     # create custom model and save to memory
#'     # first update inputs from radio buttons
#'     if(is.null(dim(answers$radio_answers))) {
#'       custom_model <- stable.fit
#'     }
#'     else {custom_model <- bnlearn::mutilated(stable.fit, evidence=answers$radio_answers)}
#'
#'     # second, update states with inputs as sliders
#'     for (node in names(answers$slider_answers)) {
#'       input.probability.df <- answers$slider_answers[[node]]
#'       model.probability.df <- as.data.frame(custom_model[[node]]$prob)
#'       if ("Var1" %in% colnames(model.probability.df)){
#'         model.probability.df <- dplyr::rename(model.probability.df, !!node:=Var1)
#'       }
#'       # JR note : update_probability is a DIAGRAM defined function
#'       model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
#'       # update probability table for node
#'       custom_model[[node]] = model.probability.table
#'       }
#'     # third update states with boolean sliders as inputs
#'     for (node in names(answers$boolean_slider_answers)) {
#'       input.probability.df <- answers$boolean_slider_answers[[node]]
#'       model.probability.df <- as.data.frame(custom_model[[node]]$prob) %>% dplyr::rename(!!node:=Var1)
#'       model.probability.table <- update_probability(node, model.probability.df, input.probability.df)
#'       # update probability table for node
#'       custom_model[[node]] = model.probability.table
#'     }
#'
#'     # Add custom network to memory
#'     CustomModels$custom_networks[[input$CustomisedModelName]] = custom_model
#'     CustomPolicies$models[[input$CustomisedModelName]] = list('Base'=custom_model)
#'
#'     # calculate utility and store
#'     # JR note calculate_utility is a DIAGRAM defined function
#'     utility <- calculate_utility(custom_model)
#'     CustomModels$base_utility.df <- CustomModels$base_utility.df %>%
#'       dplyr::add_row(
#'         name=input$CustomisedModelName,
#'         Intellectual_Control=utility$Intellectual_Control,
#'         Renderability=utility$Renderability
#'       )
#'     # TODO: Why do we have two structures saving the same information?
#'     CustomPolicies$archiveList[[input$CustomisedModelName]] <-
#'       tibble::tibble(
#'         name=input$CustomisedModelName,
#'         Intellectual_Control=utility$Intellectual_Control,
#'         Renderability=utility$Renderability
#'       )
#'
#'     # setting choices for the drop down list in the Simple view Node customisation tab
#'     customModelChoices <- CustomModels$base_utility.df %>%
#'       dplyr::select(.data$name)
#'     shiny::updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
#'     shiny::updateSelectInput(session, "model_version", label="Select Model", choices=customModelChoices)
#'
#'     # set choices for the drop down list in the Report ans sens tab
#'     shiny::updateSelectInput(session, 'reportTabModelSelection', choices=CustomModels$base_utility.df$name)
#'     shiny::updateSelectInput(session, 'sensTabModelSelection', choices=CustomModels$base_utility.df$name)
#'   })
#'
#'   # plot utility
#'   output$BasicUtilityComparison <- shiny::renderPlot({
#'     CustomModels$base_utility.df %>%
#'       column_chart("Policy")
#'   })
#'
#'   # Reset so new custom model can be created
#'   shiny::observeEvent(input$AddNew, {
#'     # reset question number to 1
#'     questionValues$question_number = 1
#'     # update radio buttons
#'     shiny::updateRadioButtons(session, "StateSelection", choices=first_states$node_state)
#'     # update progress bar
#'     shinyWidgets::updateProgressBar(
#'       session=session,
#'       id="Question_Progress",
#'       value=questionValues$question_number,
#'       total=nrow(setup_questions)
#'     )
#'   })
#'
#'   # upload custom model
#'
#'   shiny::observeEvent(input$uploadCustomModel, {
#'
#'     shiny::req(input$customModel)
#'
#'     if (is.null(input$customModel)){
#'       errorMsg <-"You have not uploaded a model."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     } else{
#'       # check if name is already being used
#'       if(input$uploadName %in% names(CustomModels$custom_networks)){
#'         errorMsg <-"You have already used this name for another custom model."
#'         shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'         return()
#'       }
#'
#'       # check if name is empty
#'       if (input$uploadName == ""){
#'         errorMsg <-"Please provide uploaded model a name."
#'         shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'         return()
#'       }
#'
#'       # load model into memory and calculate base utility
#'       custom_model <- bnlearn::read.bif(input$customModel$datapath)
#'       utility <- calculate_utility(custom_model)
#'
#'
#'       CustomModels$base_utility.df <- CustomModels$base_utility.df %>%
#'         dplyr::add_row(
#'           name=input$uploadName,
#'           Intellectual_Control=utility$Intellectual_Control,
#'           Renderability=utility$Renderability
#'         )
#'
#'       CustomPolicies$archiveList[[input$uploadName]] <- tibble::tibble(
#'         name=input$uploadName,
#'         Intellectual_Control=utility$Intellectual_Control,
#'         Renderability=utility$Renderability
#'       )
#'
#'       CustomModels$custom_networks[[input$uploadName]] <- custom_model
#'       CustomPolicies$models[[input$uploadName]] = list('Base'=custom_model)
#'
#'       # setting choices for the drop down list in the Simple view Node customisation tab
#'       customModelChoices <- CustomModels$base_utility.df %>% select(name)
#'       shiny::updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
#'       shiny::updateSelectInput(session, "model_version", label="Select Model", choices=customModelChoices)
#'
#'       # set choices for the drop down list in the Report and sens tab
#'       shiny::updateSelectInput(session, 'reportTabModelSelection', choices=CustomModels$base_utility.df$name)
#'       shiny::updateSelectInput(session, 'sensTabModelSelection', choices=CustomModels$base_utility.df$name)
#'     }
#'   })
#'
#'   # SIMPLE POLICY
#'
#'   # Plot the policy comparison stacked bar chart
#'   output$policyTabUtilityScorePlot <- shiny::renderPlot({
#'     CustomPolicies$archiveList[[input$customModelSelection]] %>%
#'       column_chart("Policy")
#'   })
#'
#'   # OAIS Entities list
#'   OAISentities <- node.definitions$OAIS_Entity
#'   OAISentities <- c('None', unique(OAISentities)) # adding None to provide option of listing all nodes
#'   #remove NA (Renderability nad Intellectual Control are blanks)
#'   OAISentities <- OAISentities[-which(is.na(OAISentities))]
#'
#'   updateSelectInput(
#'     session,
#'     "customOaisEntitySelection",
#'     choices = OAISentities,
#'     selected = 'None'
#'   )
#'
#'   uiNode <- reactiveValues(checklist=c())
#'
#'   shiny::observeEvent(input$customOaisEntitySelection, {
#'     oaisSelected <- TRUE
#'     if(length(input$customOaisEntitySelection) == 1 & input$customOaisEntitySelection[1] == 'None'){
#'       uiNode$checklist <- node.definitions$node_name
#'       #remove.outcomes <- node.definitions$node_name[-which(node.definitions$node_name %in% c("Intellectual_Control","Renderability"))]
#'       #uiNode$checklist <- remove.outcomes
#'       #uiNode$checklist <- list("Technical_Skills","Checksum","Digital_Object","System_Security","Info_Management","Storage_Medium","Op_Environment","Rep_and_Refresh")
#'       oaisSelected <- FALSE
#'     }
#'     else{
#'       uiNode$checklist <- c()
#'       for(oaisEntity in input$customOaisEntitySelection){
#'         # display error if 'None' is still selected alongwith other OAIS entities.
#'         if(oaisEntity == 'None'){
#'           shinyalert::shinyalert(
#'             "Error:",
#'             "You can't select 'None' and other OAIS Entities together. If you wish to view features within an OAIS
#'             entity, please click on 'None' and delete/backspace, followed by selection of the desired OAIS entities",
#'             type = "error"
#'           )
#'           return()
#'         }
#'
#'         tmp <- node.definitions %>%
#'           dplyr::filter(.data$OAIS_Entity==oaisEntity) %>%
#'           dplyr::select(.data$node_name)
#'
#'         uiNode$checklist <- c(tmp$node_name, uiNode$checklist)
#'       }
#'     }
#'
#'     # auto-select all the nodes in checklist if any 'non-None' OAIS entity is selected
#'     if(oaisSelected == TRUE){
#'       # update the checklist options with nodes list
#'       shiny::updateCheckboxGroupInput(
#'         session,
#'         "policyTabNodesChecklist",
#'         label=NULL,
#'         choices = uiNode$checklist,
#'         selected = uiNode$checklist
#'       )
#'     }
#'     else{
#'       shiny::updateCheckboxGroupInput(
#'         session,
#'         "policyTabNodesChecklist",
#'         label=NULL,
#'         choices = uiNode$checklist
#'       )
#'     }
#'   })
#'
#'   currModel <- shiny::reactiveValues(model=stable.fit)
#'   uiNodeSlider <- shiny::reactiveValues(node=c())
#'   nodeStateProgress <- shiny::reactiveValues(progress=0)
#'
#'   # make the necessary changes when the model is changed from dropdown menu
#'   shiny::observeEvent(input$customModelSelection,{
#'     # list the nodes checklist dynamically based on model instead of hardcoding
#'
#'     if(input$customModelSelection == 'Default'){
#'       currModel$model <- stable.fit
#'     }
#'     else{
#'       currModel$model <- CustomPolicies$models[[input$customModelSelection]]$Base
#'     }
#'
#'     # reset the progress for selected model
#'     nodeStateProgress$progress <- 0
#'     uiNodeSlider$node <- c()
#'
#'     #reset the checklist
#'     shiny::updateCheckboxGroupInput(
#'       session,
#'       "policyTabNodesChecklist",
#'       label=NULL,
#'       choices = uiNode$checklist,
#'       selected = c()
#'     )
#'
#'     #reset OAIS
#'     shiny::updateSelectInput(
#'       session,
#'       "customOaisEntitySelection",
#'       choices = OAISentities,
#'       selected = 'None'
#'     )
#'
#'     #set the remove policy options
#'     shiny::updateSelectInput(
#'       session,
#'       "policyTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'
#'     shiny::updateSelectInput(
#'       session,
#'       "reportTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'   })
#'
#'   # observe the input for checklist to update uiNodeSlider$node with respective states
#'   shiny::observeEvent(input$policyTabNodesChecklist, {
#'     i <- 1
#'     uiNodeSlider$node <- c()
#'     nodeStateProgress$progress <- 1
#'
#'     for(node in input$policyTabNodesChecklist){
#'
#'       ## TODO: change this to list of list (of nodes with node state) to avoid having to create a new list for every node
#'       nodeStates <- state.definitions %>%
#'         dplyr::filter(.data$node_name==node) %>%
#'         dplyr::select(-.data$node_name)
#'
#'       nodeStateType <- node.definitions %>%
#'         dplyr::filter(.data$node_name==node) %>%
#'         dplyr::select(.data$type)
#'       primary_state <- nodeStates$node_state[1]
#'       label <- paste(primary_state, "%")
#'       if(nodeStateType == 'BooleanSlider'){
#'         nodeStateSlider <- shiny::sliderInput(node, label, min = 0, max = 100, step = 1, value = 0, post = "%")
#'       }
#'       else if(nodeStateType == "slider"){
#'         nodeStateSlider <- create_sliders(node, nodeStates$node_state)
#'       }
#'       else{ ## radio buttons
#'         nodeStateSlider <- shiny::radioButtons(node, label="", choices=nodeStates$node_state)
#'       }
#'
#'       # remove the _ from the node to ease readability
#'       nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
#'       nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
#'
#'       # list of nodes with corresponding state sliders
#'       uiNodeSlider$node[[i]] <- shiny::div(shiny::h4(nodeLabel), nodeStateSlider )
#'       i <- i+1
#'     }
#'   })
#'
#'
#'   #NOTE: When length(input$policyTabNodesChecklist) == 0, the observeEvent(input$policyTabNodesChecklist, {}) method is not called.
#'   # As a result, uiNodeSlider$node remains unchanged (contains last selected node as the only element)
#'   # For this reason, checks (for length !=0) have been added to update content when no node is selected.
#'
#'   # display the slider inputs for each selected node (as a progress bar walkthrough)
#'   output$policyTabNodesSlider <- shiny::renderUI({
#'     # Enabling/disabling buttons based on progress
#'
#'     # if nothing is selected everything is hidden and disabled
#'     if (length(input$policyTabNodesChecklist)!=0){
#'       shinyjs::show(id="SimpleViewPolicyNext")
#'       shinyjs::show(id="SimpleViewPolicyPrevious")
#'       shinyjs::enable(id="SimpleViewPolicyNext")
#'       shinyjs::enable(id="SimpleViewPolicyPrevious")
#'       shinyjs::hide(id="nodeSliderPlaceholder")
#'     }
#'     else{
#'       shinyjs::hide(id="SimpleViewPolicyNext")
#'       shinyjs::hide(id="SimpleViewPolicyPrevious")
#'       shinyjs::hide(id="SimpleViewPolicyAddBox")
#'       shinyjs::show(id="nodeSliderPlaceholder")
#'       # shinyjs::hide(id="SimpleViewPolicyName")
#'       # shinyjs::hide(id="SimpleViewAddPolicy")
#'     }
#'
#'     # disable previous button to avoid negative index (<1)
#'     if(nodeStateProgress$progress == 1){
#'       shinyjs::hide(id="SimpleViewPolicyPrevious")
#'     }
#'
#'     # Policy can only be added when all the selected nodes have been updated
#'     if(length(input$policyTabNodesChecklist) != 0 & nodeStateProgress$progress == length(uiNodeSlider$node)){
#'       shinyjs::hide(id="SimpleViewPolicyNext") # hide next button to avoid exceeding array size
#'       shinyjs::show(id="SimpleViewPolicyAddBox")
#'       #shinyjs::show(id="SimpleViewAddPolicy")
#'     }
#'     else{
#'       shinyjs::hide(id="SimpleViewPolicyAddBox")
#'       #shinyjs::hide(id="SimpleViewAddPolicy")
#'     }
#'
#'
#'     if (length(input$policyTabNodesChecklist)!=0){
#'       uiNodeSlider$node[[nodeStateProgress$progress]]
#'     }
#'   })
#'
#'   # method to check for the correctness of the input -- specifically for slider input of all states == 100
#'   #' @importFrom dplyr filter select
#'   #' @importFrom rlang .data
#'   checkForInputCorrectness <- function(node) {
#'     nodeStates <- state.definitions %>%
#'       dplyr::filter(.data$node_name==node) %>%
#'       dplyr::select(-.data$node_name)
#'
#'     nodeStateType <- node.definitions %>%
#'       dplyr::filter(.data$node_name==node) %>%
#'       dplyr::select(.data$type)
#'
#'     # sum to 100 is important when the input type is slider. For boolean slider and radio buttons, it is always equal to 100
#'     if(nodeStateType == 'slider'){
#'
#'       # getting the total probability for slider input of each state
#'       currSumOfProbabilities <- 0
#'       for(state in nodeStates$node_state){
#'         currId = paste(node, state, sep ="-")
#'         currSumOfProbabilities <- currSumOfProbabilities +  input[[currId]]
#'       }
#'
#'       if(currSumOfProbabilities != 100){
#'         # remove the _ from the node to ease readability
#'         nodeLabel <- strsplit(node, split = "_", fixed = TRUE)
#'         nodeLabel <- paste(nodeLabel[[1]], collapse = ' ')
#'
#'         errorMsg <- paste("Probabilities for '", nodeLabel, "' does not add up to to 100")
#'         shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'
#'         return(FALSE)
#'       } else{
#'         return(TRUE)
#'       }
#'     } else {
#'       return(TRUE)
#'     }
#'   }
#'
#'
#'   shiny::observeEvent(input$SimpleViewPolicyNext, {
#'     node <- input$policyTabNodesChecklist[[nodeStateProgress$progress]]
#'
#'     if(checkForInputCorrectness(node) == TRUE){
#'       nodeStateProgress$progress <- nodeStateProgress$progress + 1
#'     }
#'
#'   })
#'
#'   shiny::observeEvent(input$SimpleViewPolicyPrevious, {
#'     nodeStateProgress$progress <- nodeStateProgress$progress - 1
#'   })
#'
#'   # Add policy action
#'   shiny::observeEvent(input$SimpleViewAddPolicy, {
#'
#'     # check for input correctness of the last selected node
#'     lastNode <- input$policyTabNodesChecklist[[nodeStateProgress$progress]]
#'     # JR note checkForInputCorrectness is a DIAGRAM defined function
#'     if(checkForInputCorrectness(lastNode) == FALSE){
#'       return()
#'     }
#'
#'     if(input$SimpleViewPolicyName == ""){
#'       shinyalert::shinyalert("Error:", "Please provide a policy name", type = "error")
#'       return()
#'     }
#'
#'     for(existingPolicyModel in names(CustomPolicies$models[[input$customModelSelection]])){
#'       if(input$SimpleViewPolicyName == existingPolicyModel){
#'         shinyalert::shinyalert("Error:", "Policy name already exists", type = "error")
#'         return()
#'       }
#'     }
#'
#'     for(node in input$policyTabNodesChecklist){
#'       # conditional probability table (cpt) of each node
#'       cpt <- as.data.frame(currModel$model[[node]]$prob)
#'       if ("Var1" %in% colnames(cpt)){
#'         cpt<- dplyr::rename(cpt, !!node:=Var1)
#'       }
#'
#'       ## For debugging
#'       # print(node)
#'       # print(cpt)
#'       # print("----------------------\n")
#'
#'       nodeStates <- state.definitions %>%
#'         dplyr::filter(.data$node_name==node) %>%
#'         dplyr::select(-.data$node_name)
#'
#'       nodeStateType <- node.definitions %>%
#'         dplyr::filter(node_name==node) %>%
#'         dplyr::select(type)
#'       primary_state <- nodeStates$node_state[1]
#'       # extract and set the values in CPT based on the input type -- BooleanSlider, slider, radiobutton
#'       if(nodeStateType == 'BooleanSlider'){
#'         # update cpt for the True state as the single slider signifies input for True %
#'         index <- cpt[[node]] == primary_state
#'         cpt$Freq[index] <- input[[node]]/100
#'
#'         index <- cpt[[node]] != primary_state
#'         cpt$Freq[index] <- 1 - input[[node]]/100
#'       }
#'       else if(nodeStateType == "slider"){
#'         # updating the cpt for each state
#'         for(state in nodeStates$node_state){
#'           currId = paste(node, state, sep ="-")
#'           index <- cpt[[node]] == state
#'
#'
#'           cpt$Freq[index] <- input[[currId]]/100
#'         }
#'       }
#'       else{ ## radio buttons
#'         index <- cpt[[node]] == input[[node]]
#'         cpt$Freq[index] <- 1
#'
#'         for(state in nodeStates$node_state){
#'           if(state != input[[node]]){
#'             index <- cpt[[node]] == state
#'             cpt$Freq[index] <- 0
#'           }
#'         }
#'       }
#'
#'       ## For debugging
#'       # print("Updated CPT")
#'       # print(cpt)
#'       # print("---------------------------")
#'
#'       # Updating the model
#'       # The data frame should be converted to a contigency and then the model is updated.
#'       # The table should be Freq~'all other columns'
#'
#'       # get the column names excluding frequency
#'       cptFactors <- colnames(cpt)[1:length(colnames(cpt))-1]
#'
#'       # formula for xtabs
#'       formula <- paste('Freq~', paste(cptFactors, collapse = "+"), sep="")
#'
#'       # update the model
#'       currModel$model[[node]] <- xtabs(formula, cpt)
#'
#'       print(as.data.frame(currModel$model[[node]]$prob))
#'     }
#'
#'     # Calculate the utility of the new model
#'     currPolicyUtility <- calculate_utility(currModel$model)
#'
#'     # update reactive policy list
#'     CustomPolicies$archiveList[[input$customModelSelection]] <- CustomPolicies$archiveList[[input$customModelSelection]] %>%
#'       dplyr::add_row(
#'         name=input$SimpleViewPolicyName,
#'         Intellectual_Control=currPolicyUtility$Intellectual_Control,
#'         Renderability=currPolicyUtility$Renderability
#'       )
#'
#'     CustomPolicies$models[[input$customModelSelection]][[input$SimpleViewPolicyName]] <- currModel$model
#'
#'     # set the list of policies in drop down
#'     shiny::updateSelectInput(
#'       session,
#'       "policyTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'
#'     shiny::updateSelectInput(
#'       session,
#'       "reportTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'
#'   })
#'   #Add reset button
#'   shiny::observeEvent(input$SimplePolicyReset, {
#'     if(input$customModelSelection == 'Default'){
#'       currModel$model <- stable.fit
#'     }
#'     else{
#'       currModel$model <- CustomPolicies$models[[input$customModelSelection]]$Base
#'     }
#'
#'     # reset the progress for selected model
#'     nodeStateProgress$progress <- 0
#'     uiNodeSlider$node <- c()
#'
#'     #reset check boxes
#'     shiny::updateCheckboxGroupInput(
#'       session,
#'       "policyTabNodesChecklist",
#'       label=NULL,
#'       choices = uiNode$checklist,
#'       selected = c()
#'     )
#'
#'     #reset OAIS
#'     shiny::updateSelectInput(
#'       session,
#'       "customOaisEntitySelection",
#'       choices = OAISentities,
#'       selected = 'None'
#'     )
#'   })
#'
#'   #remove policy
#'   shiny::observeEvent(input$RemovePolicy, {
#'
#'     CustomPolicies$archiveList[[input$customModelSelection]] <- CustomPolicies$archiveList[[input$customModelSelection]] %>% filter(name != input$policyTabPolicyRemove)
#'
#'     shiny::updateSelectInput(
#'       session,
#'       "policyTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'
#'     shiny::updateSelectInput(
#'       session,
#'       "reportTabPolicyRemove",
#'       choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1]
#'     )
#'   })
#'
#'
#'   # ADVANCED POLICIES
#'   # save changes user has made
#'   advanced <- shiny::reactiveValues(
#'     updated_nodes = list(),
#'     node_counter = 1
#'   )
#'
#'   # add nodes to drop down list
#'   shiny::updateSelectInput(session, inputId="nodeProbTable", label="Select Node", choices=node.definitions$node_name)
#'
#'   # Plot network which changes for policy inputs
#'   output$netPlot <- shiny::renderPlot({
#'     #model.label <- paste(input$model_version, "model", sep=" ")
#'     bnlearn::graphviz.plot(
#'       network$advanced.fit, layout = "dot",
#'       highlight = list(nodes=c(input$nodeProbTable), fill="lightgrey"),
#'       shape = "ellipse",
#'       render = TRUE
#'     ) #,
#'     #main=model.label)
#'   })
#'
#'   # update model if different model version is selected
#'   shiny::observe({
#'     network$advanced.fit <- CustomModels$custom_networks[[input$model_version]]
#'   })
#'
#'   # output probability table
#'   output$probabilityTable <- shinysky::renderHotable({
#'
#'     if (input$probtabltype == "Conditional Probability Table") {
#'       conditional.table <- as.data.frame(network$advanced.fit[[input$nodeProbTable]]$prob)
#'
#'       # If a column is named Var1, rename to be variable name
#'       if ("Var1" %in% colnames(conditional.table)){
#'         conditional.table <- dplyr::rename(conditional.table, !!input$nodeProbTable:=Var1)
#'       }
#'
#'       # Spread dataframe so that it is easier to see which probabilities should add to 1.0
#'       conditional.table <- conditional.table %>%
#'         dplyr::mutate(Freq=Freq*100) %>%
#'         tidyr::pivot_wider(names_from=!!input$nodeProbTable, values_from=Freq)
#'
#'       # Change column names to make them easier to understand
#'       # collect states
#'       node.states <- state.definitions %>%
#'         dplyr::filter(node_name==input$nodeProbTable)
#'       # create read only vector
#'       read_table_temp <- rep(TRUE, ncol(conditional.table))
#'
#'       # iterate and change names
#'       i <- 0
#'       for (state in node.states$node_state){
#'         label <- paste(input$nodeProbTable, state, sep="=")
#'         conditional.table <- dplyr::rename(conditional.table, !!label:=!!state)
#'         read_table_temp[length(read_table_temp) - i] <- FALSE
#'         i <- i + 1
#'       }
#'
#'       data <- conditional.table
#'
#'     } else {
#'       # convert model to grain object
#'       model.grain <- bnlearn::as.grain(network$advanced.fit)
#'
#'       # find probability of findability and renderability
#'       query.results <- gRain::querygrain(model.grain, nodes=c(input$nodeProbTable))
#'       # control which columns can be read
#'       read_table_temp <- c(TRUE, FALSE)
#'       # return independent probability table
#'       data <- data.frame(query.results) %>%
#'       tibble::rownames_to_column() %>%
#'       dplyr::rename(probability:=!!input$nodeProbTable,
#'              !!input$nodeProbTable:=rowname) %>%
#'       dplyr::mutate(probability=100*.data$probability)
#'     }
#'     data
#'   }, readOnly=FALSE)
#'
#'   # plot node conditional probabilities
#'   output$nodeProbability <- shiny::renderPlot({
#'     bnlearn::bn.fit.barchart(network$advanced.fit[[input$nodeProbTable]])
#'   })
#'
#'   # update node probability when changed
#'   shiny::observeEvent(input$updateProb, {
#'
#'     data.df <- as.data.frame(shinysky::hot.to.df(input$probabilityTable))
#'     custom_model <- network$advanced.fit
#'
#'     # update model if table is conditional
#'     if (input$probtabltype == "Conditional Probability Table") {
#'       # collect column names related to probability
#'       probability_columns <- data.df %>%
#'         dplyr::select(dplyr::starts_with(input$nodeProbTable)) %>%
#'         colnames()
#'       remove_pattern <- paste(input$nodeProbTable, "=", sep="")
#'       new_columns <- c()
#'
#'       # Check if all probabilities sum to 100% if not give error message
#'       prob_sum <- rowSums(data.df[, probability_columns])
#'       for (sum in prob_sum){
#'         if (sum != 100){
#'           errorMsg <-"One or more conditional probabilities do not sum to 100%."
#'           shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'           return()
#'         }
#'       }
#'
#'       # iterate through columns and change to state name
#'       for (probability_column in probability_columns){
#'
#'         # create new column name and rename old one, add to new columns vector
#'         new_column_name <- stringr::str_remove(probability_column, remove_pattern)
#'         data.df <- dplyr::rename(data.df, !!new_column_name:=!!probability_column)
#'         new_columns <- c(new_column_name, new_columns)
#'       }
#'
#'       # convert df to long form and divide probabilities by 100
#'       data.df <- data.df %>%
#'         tidyr::pivot_longer(
#'           new_columns,
#'           names_to=input$nodeProbTable,
#'           values_to="Freq"
#'         ) %>%
#'         dplyr::mutate(Freq=.data$Freq/100)
#'
#'       # columns need to be reordered so it can be correctly converted to a table
#'       # get column names except for node column to rearrange column order
#'       data.columns <- data.df %>%
#'         dplyr::select(-!!input$nodeProbTable) %>%
#'         colnames
#'       data.columns <- c(input$nodeProbTable, data.columns)
#'       data.df <- data.df[data.columns]
#'
#'       # convert df to table
#'       model.probability.table <- stats::xtabs(Freq~., data.df)
#'
#'     # update model if table is marginal
#'     } else{
#'       # check if the marginal probabilities sum to 100%
#'       sum <- data.df %>%
#'         dplyr::summarise(total_prob=sum(.data$probability))
#'       if (sum$total_prob != 100){
#'         errorMsg <-"The marginal probabilities do not sum to 100%."
#'         shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'         return()
#'       }
#'
#'       # prepare table for update probability function
#'       data.df <- dplyr::rename(data.df, state:=!!input$nodeProbTable)
#'
#'       # collect model probability
#'       model.probability <- as.data.frame(custom_model[[input$nodeProbTable]]$prob)
#'
#'       # if var1 is a column name, rename to be variable name
#'       if ("Var1" %in% colnames(model.probability)){
#'         model.probability <- dplyr::rename(model.probability, !!input$nodeProbTable:=Var1)
#'       }
#'
#'       model.probability.table <- update_probability(
#'         input$nodeProbTable,
#'         model.probability,
#'         data.df
#'       )
#'     }
#'     # update model with new table
#'     custom_model[[input$nodeProbTable]] <- model.probability.table
#'
#'     # update reactive value
#'     network$advanced.fit <- custom_model
#'
#'     # check if node has already been added to checked list
#'     for (updated_node in advanced$updated_nodes){
#'       if (input$nodeProbTable == updated_node$children){
#'         return()
#'       }
#'     }
#'
#'     advanced$updated_nodes[[advanced$node_counter]] <- shiny::tags$li(input$nodeProbTable)
#'     advanced$node_counter <- advanced$node_counter + 1
#'
#'   })
#'
#'   # update list of changed nodes
#'   output$ChangeNodes <- shiny::renderUI({
#'     advanced$updated_nodes
#'   })
#'
#'   # add policy
#'   shiny::observeEvent(input$addPolicy, {
#'     # check if a name has been provided
#'     if (input$policyName == ""){
#'       errorMsg <-"No name was provided for the model."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'
#'     # check if name has already been used for another policy
#'     current_policies <- CustomPolicies$archiveList[[input$model_version]]
#'     if (input$policyName %in% current_policies$name){
#'       errorMsg <-"You have already used this policy name."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'
#'     utility <- calculate_utility(network$advanced.fit)
#'
#'     CustomPolicies$archiveList[[input$model_version]] <- current_policies %>%
#'       dplyr::add_row(
#'         name=input$policyName,
#'         Intellectual_Control=utility$Intellectual_Control,
#'         Renderability=utility$Renderability
#'       )
#'
#'     CustomPolicies$models[[input$model_version]][[input$policyName]] = network$advanced.fit
#'   })
#'
#'   # add custom model
#'   shiny::observeEvent(input$addModelAdvanced, {
#'     # check if a name has been provided
#'     if (input$policyName == ""){
#'       errorMsg <-"No name was provided for the model."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'
#'     # check if name is already being used for another custom model
#'     if (input$policyName %in% names(CustomModels$custom_networks)){
#'       errorMsg <-"You are already using this name for another custom model."
#'       shinyalert::shinyalert("Error:", errorMsg, type = "error")
#'       return()
#'     }
#'
#'     # Add custom network to memory
#'     CustomModels$custom_networks[[input$policyName]] = network$advanced.fit
#'     CustomPolicies$models[[input$policyName]] = list('Base'= network$advanced.fit)
#'
#'     # calculate utility and store
#'     utility <- calculate_utility(network$advanced.fit)
#'     CustomModels$base_utility.df <- CustomModels$base_utility.df %>%
#'       dplyr::add_row(
#'         name=input$policyName,
#'         Intellectual_Control=utility$Intellectual_Control,
#'         Renderability=utility$Renderability
#'       )
#'     # TODO: Why do we have two structures saving the same information?
#'     CustomPolicies$archiveList[[input$policyName]] <- tibble::tibble(
#'       name=input$policyName,
#'       Intellectual_Control=utility$Intellectual_Control,
#'       Renderability=utility$Renderability
#'     )
#'
#'     # setting choices for the drop down list in the Simple view Node customisation tab
#'     customModelChoices <- CustomModels$base_utility.df %>% dplyr::select(name)
#'     shiny::updateSelectInput(session, 'customModelSelection', choices=customModelChoices)
#'     shiny::updateSelectInput(session, "model_version", label="Select Model", choices=customModelChoices)
#'     shiny::updateSelectInput(session, 'reportTabModelSelection', choices=CustomModels$base_utility.df$name)
#'     shiny::updateSelectInput(session, 'sensTabModelSelection', choices=CustomModels$base_utility.df$name)
#'   })
#'
#'   # Reset network to original probabilities
#'   shiny::observeEvent(input$networkReset, {
#'     network$advanced.fit <- CustomModels$custom_networks[[input$model_version]]
#'     advanced$updated_nodes <- list()
#'     advanced$node_counter <- 1
#'   })
#'
#'   # plot policy comparison
#'   output$PolicyComparison <- shiny::renderPlot({
#'     CustomPolicies$archiveList[[input$model_version]] %>%
#'       column_chart("Policy")
#'   })
#'
#'   # plot custom model comparison
#'   output$BaseUtilityComparison <- shiny::renderPlot({
#'     CustomModels$base_utility.df %>%
#'       column_chart("Model")
#'   })
#'
#'   # REPORT TAB
#'
#'   setReportTabSummary <- function(currModelName, currModel){
#'
#'     # utility weighting
#'     a <- input$RenderabilityWeighting
#'     b <- input$IntellectualWeighting
#'
#'     # constructing text for the summary section
#'     summary <- paste(
#'       "The", currModelName, "model has", length(currModel$name),
#'       "policy(ies), including the original. The overall risk scores for each are:<br/><br/>"
#'     )
#'
#'     # to keep track of best policy
#'     maxUtility <- -99999
#'     maxUtilityPolicyName <- ""
#'
#'     summary <- paste(summary, "<pre>", sep="")
#'
#'     # getting list of policies
#'     for(policy in currModel$name){
#'       policyUtility <- currModel %>%
#'         dplyr::filter(.data$name==policy) %>%
#'         dplyr::select(.data$Renderability, .data$Intellectual_Control)
#'       currUtility <- (b*policyUtility$Intellectual_Control + a*policyUtility$Renderability) /(a+b)*100
#'
#'       summary <- paste(summary, policy, "\t", format(round(currUtility,4),nsmall=4), "<br/>", sep = "")
#'
#'       if(currUtility > maxUtility){
#'         maxUtility <- currUtility
#'         maxUtilityPolicyName <- policy
#'       }
#'     }
#'
#'     summary <- paste(summary, "</pre>", sep="")
#'     summary <- paste(summary, "<br/>", "The policy with best score is <b>", maxUtilityPolicyName, "</b>")
#'
#'     return(summary)
#'   }
#'
#'   initialModelSetup <- shiny::reactiveValues(flag=TRUE)
#'   initialSimpleCustomisationPopup <- shiny::reactiveValues(flag=TRUE)
#'
#'   shiny::observeEvent(input$sidebarMenu, {
#'
#'     ## Initial Model and Pop setup flags
#'     if(initialModelSetup$flag){
#'       CustomPolicies$archiveList[['Default']] <- tibble::tibble(
#'         name="Base",
#'         Intellectual_Control=default_utility$Intellectual_Control,
#'         Renderability=default_utility$Renderability
#'       )
#'
#'       CustomPolicies$models[['Default']][['Base']] <- stable.fit
#'
#'       initialModelSetup$flag = FALSE
#'     }
#'
#'     if(input$sidebarMenu == 'CustomiseNode' & initialSimpleCustomisationPopup$flag){
#'       shinyalert::shinyalert("Please select the model for your archive","
#'       If you skipped '1. Create your model', please return to this page and create or upload a customised model.", type = "info")
#'
#'       initialSimpleCustomisationPopup$flag = FALSE
#'     }
#'
#'     # FOR REPORT TAB
#'
#'     summary <- shiny::reactive({
#'       input$RenderabilityWeighting
#'       input$IntellectualWeighting
#'
#'       setReportTabSummary(
#'         currModel, CustomPolicies$archiveList[[currModel]]
#'       )
#'     })
#'
#'     if(input$sidebarMenu == "Report"){
#'       currModel <- input$reportTabModelSelection
#'       # summary <- setReportTabSummary(currModel,
#'       #                                CustomPolicies$archiveList[[currModel]])
#'
#'       output$ReportTabSummaryText <- shiny::renderText(summary())
#'
#'       # set the list of policies in drop down
#'       shiny::updateSelectInput(
#'         session,
#'         "ReportTabPolicySelection",
#'         choices = CustomPolicies$archiveList[[currModel]]$name[-1]
#'       )
#'     }
#'   })
#'
#'   shiny::observeEvent(input$reportTabModelSelection, {
#'     currModel <- input$reportTabModelSelection
#'
#'     # set the summary
#'     summary <- setReportTabSummary(
#'       currModel,
#'       CustomPolicies$archiveList[[currModel]]
#'     )
#'
#'     output$ReportTabSummaryText <- shiny::renderText(summary)
#'
#'     # set the list of policies in drop down
#'     shiny::updateSelectInput(
#'       session,
#'       "ReportTabPolicySelection",
#'       choices = CustomPolicies$archiveList[[currModel]]$name[-1]
#'     )
#'
#'   })
#'
#'   # Check if utiltiy weighting changes
#'   shiny::observe({
#'     utility_weighting$Renderability <- input$RenderabilityWeighting
#'     utility_weighting$Intellectual <- input$IntellectualWeighting
#'   })
#'
#'   plotUtility <- shiny::reactive({
#'     a <- utility_weighting$Renderability
#'     b <- utility_weighting$Intellectual
#'     # hack for build check warning on non exported global variables
#'     ..y.. = NULL
#'     CustomPolicies$archiveList[[input$reportTabModelSelection]] %>%
#'       tidyr::pivot_longer(c(.data$Intellectual_Control, .data$Renderability), names_to="policy") %>%
#'       dplyr::mutate(value=ifelse(policy=="Renderability", .data$value*a/(a+b)*100, .data$value*b/(a+b)*100)) %>%
#'       ggplot2::ggplot(ggplot2::aes(x=stats::reorder(name, -value), fill=.data$policy, y=.data$value)) +
#'       ggplot2::geom_bar(position="stack", stat="identity") +
#'       ggplot2::xlab("Policy") + ggplot2::ylab("Score") +
#'       ggplot2::geom_hline(yintercept=0.1013*50, linetype="dashed", color = "black") +
#'       ggplot2::geom_hline(yintercept=1.4255*50, linetype="dashed", color = "black") +
#'       #geom_text(aes(1,0.3,label = "Min", vjust = -1)) + geom_text(aes(1,1.3,label = "Max", vjust = -1)) +
#'       ggplot2::stat_summary(
#'         fun.y = sum, ggplot2::aes(label = format(round(..y..,0),nsmall=0), group = .data$name),
#'         geom = "text", size=7, fontface="bold", vjust=-0.25
#'       ) +
#'       ggplot2::geom_text(
#'         ggplot2::aes(label=format(round(.data$value,0),nsmall=0)), size=5, colour="white",
#'         fontface = "bold", position = ggplot2::position_stack(vjust = 0.5)) +
#'       ggplot2::theme_light() +
#'       ggplot2::theme(panel.border = ggplot2::element_blank(), text = ggplot2::element_text(size =20), legend.position="top", legend.title = ggplot2::element_blank())  +
#'       ggplot2::scale_fill_manual(values=c("#FF6E3A","#8400CD")) #colour blind scheme
#'   })
#'
#'
#'   # Plot the policy comparison stacked bar chart
#'   output$ReportTabUtilityComparisonPlot <- shiny::renderPlot(
#'     {
#'       plotUtility()
#'     }
#'   )
#'
#'   #remove policy
#'   # observeEvent(input$RemovePolicyReport, {
#'   #
#'   #   CustomPolicies$archiveList[[input$customModelSelection]] <- CustomPolicies$archiveList[[input$customModelSelection]] %>% filter(name != input$reportTabPolicyRemove)
#'   #
#'   #   updateSelectInput(session,
#'   #                     "policyTabPolicyRemove",
#'   #                     choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1])
#'   #
#'   # updateSelectInput(session,
#'   #                   "reportTabPolicyRemove",
#'   #                   choices = CustomPolicies$archiveList[[input$customModelSelection]]$name[-1])
#'   #
#'   #})
#'
#'   #Download
#'   output$reportTabDownloadBtn <- shiny::downloadHandler(
#'     filename = function() {
#'       paste0(input$reportTabModelSelection, ".zip")
#'     },
#'
#'     content = function(file){
#'       #edit to make three options 09/06
#'       # write policy
#'       if ("A policy" %in% input$downloadOptions) {
#'         bnlearn::write.bif(
#'           paste0(input$ReportTabPolicySelection, ".bif"),
#'           CustomPolicies$models[[input$reportTabModelSelection]][[input$ReportTabPolicySelection]]
#'         )
#'       }
#'       # write model
#'       if ("The model" %in% input$downloadOptions) {
#'         bnlearn::write.bif(
#'           paste0(input$reportTabModelSelection, ".bif"),
#'           CustomPolicies$models[[input$reportTabModelSelection]][[1]]
#'         )
#'       }
#'       # write utility plot
#'       if ("The plot" %in% input$downloadOptions) {
#'         grDevices::png(filename=paste0(input$reportTabModelSelection, "_plot.png"))
#'         print(plotUtility())
#'         grDevices::dev.off()
#'       }
#'
#'       # if ("Documented Report" %in% input$downloadOptions){
#'       #   pdf(file=paste0(input$reportTabModelSelection, ".pdf"), onefile = TRUE)
#'       #
#'       #   currModel <- input$reportTabModelSelection
#'       #   summary <- setReportTabSummary(currModel,
#'       #                                  CustomPolicies$archiveList[[currModel]])
#'       #
#'       #   grid.arrange(output$ReportTabSummaryText, plotUtility())
#'       #   dev.off()
#'       # }
#'
#'       # create zip file to return
#'       filenames <- c(
#'         paste0(input$ReportTabPolicySelection, ".bif"),
#'         paste0(input$reportTabModelSelection, ".bif"),
#'         paste0(input$reportTabModelSelection, "_plot.png")
#'         #,paste0(input$reportTabModelSelection, ".pdf")
#'       )
#'
#'       utils::zip(file, filenames)
#'
#'       # delete all files on server
#'       for (filename in filenames){
#'         file.remove(filename)
#'       }
#'     }
#'   )
#'   output$SensitivityTable <- DT::renderDataTable({
#'     # JR notes: hard.test is a DIAGRAM defined function
#'     DT::datatable(hard.test(CustomModels$custom_networks[[input$sensTabModelSelection]])[Type=="Input",])
#'   })
#'    # output$clickevent <- renderPrint({
#'    #   clickData <- event_data("plotly_click")
#'    #   if (is.null(clickData)) return(NULL)
#'    #
#'    #   # Obtain the clicked x/y variables and fit linear model
#'    #   #vars <- c(clickData[["x"]], clickData[["y"]])
#'    #   tb <- hard.test(CustomModels$custom_networks[[input$sensTabModelSelection]])[!is.na(R_score), ]
#'    #   tb2 <- tb[(R_diff==clickData[["x"]]) & (IC_diff==clickData[["y"]]),]
#'    #   #d <- setNames(mtcars[vars], c("x", "y"))
#'    #   #yhat <- fitted(lm(y ~ x, data = d))
#'    #   summary0 <- paste0("The node selected is ", tb2$node_name,". ")
#'    #   if(tb2$Type=="Input") {
#'    #     summary1 <- paste0(summary0, "In most cases, you should be able to make changes to this node directly. Go to '2. Compare Policies' and give it a try.")
#'    #   }
#'    #   if(tb2$Type=="Cond") {
#'    #     summary1 <- paste0(summary0, "In most cases, you would not be able to make changes to this node directly. ",
#'    #     "Instead, you should consider changing the nodes that are parents or ancestors to ",tb2$node_name,
#'    #     #"The parents are ", CustomModels$custom_networks[[input$sensTabModelSelection]][[tb2$node_name]]$parents,
#'    #     ". To find out what these are, go to the 'Definitions' page.")
#'    #   }
#'    #   return(summary1)
#'    #   })
#'   output$SensitivityPlot <- plotly::renderPlotly({
#'     plotly::plot_ly(
#'       hard.test(CustomModels$custom_networks[[input$sensTabModelSelection]])[!is.na(R_score)&Type=="Input", ],
#'       x =  ~ R_diff,
#'       y =  ~ IC_diff,
#'       type = 'scatter',
#'       mode = 'markers',
#'       text =  ~ paste0(node_name, ":", node_state),
#'       color = ~ Type,
#'       marker = list(size = 10, symbol = 'circle')
#'     )
#'     #ggplot(hard.test(CustomModels$custom_networks[[input$sensTabModelSelection]]), aes(R_diff,IC_diff)) + geom_jitter() + theme_bw() + scale_shape(solid = FALSE)
#'   })
#' }
#'

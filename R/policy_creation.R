
#' policy creation module ui
#'
#' Lays out the user interface for the scenario building, choosing the model
#' to build a policy from, the questions and then cycling through the questions to
#' build a set of responses
#'
#' @param id namespace id requried by shiny modules
policy_creation_module_ui = function(id) {
  ns = shiny::NS(id)
  shiny::column(
    width = 10, offset = 1,
    shiny::div(
      shiny::div(
        id = ns('policy-start-container'),
        shiny::h3("Create a scenario"),
        shiny::p(
          "By creating a scenario you will be able to change the answers you used to create your model and see how it impacts your score."
        ),
        shiny::uiOutput(ns('scenario_text')),

        model_table_module_ui(ns('policy-starter'))
      ),
      shinyjs::hidden(shiny::div(
        id = ns('policy-response-picker-container'),
        shiny::p(
          "Select the responses you would like to change in order to see the impact on your digital preservation risk."
        ),
        reactable::reactableOutput(ns('policy_response_picker'))
      )),
      shinyjs::hidden(
        shiny::div(
          id = ns('policy-questions-container'),
          shiny::fluidRow(
            shiny::column(
              width = 10,
              uiOutput(ns('policy-questions-ui'))
            )
          )
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns("back-container"),
          shiny::actionButton(ns('back'), "Back", class = "btn-orange")
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns("next-container"),
          shiny::actionButton(ns("go"), "Next", class = "btn-green")
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns('create-container'),
          shiny::actionButton(ns('create'), "Create Scenario")
        )
      )
    )
  )
}

#' policy creation module server
#'
#' Server side logic to handle policy creation
#'
#' @param input necessary for shiny module
#' @param output necessary for shiny module
#' @param session necessary for shiny module
#' @param input_data The data which lays out the list of existing models/scenarios
#' @param question_data The named list of questions
#' @param model The underlying bayesian network model
#' @param scoring_funcs A named list of functions which translate user inputs into probabilities for the mode
policy_creation_module_server = function(input, output, session, input_data, question_data, model, scoring_funcs) {
  ns = session$ns
  # output from table in policy creation
  model_obj = reactiveValues(data = NULL)
  # update from global list of models/scenarios
  observeEvent(input_data(), {
    model_obj$data= input_data()
  })

  # values to track the specifics of a policy
  subset_picked = reactiveValues(
    data = NULL,
    response = NULL,
    question_ui = NULL,
    server_response = NULL,
    observers = list(),
    vis_clicked = 0
  )

  # sequence of UI states to cycle through on clicking next/back
  state_ids = paste0(c(
    "policy-start",
    "policy-response-picker",
    "policy-questions"
    # "policy-questions"
  ), "-container")


  output$scenario_text = renderUI({
    if(nrow(model_obj$data) > 0){
      shiny::p(
        "Choose a model first before you create a new scenario."
      )
    }else{
      shiny::p(
        "Create a model first before creating a scenario."
      )
    }
  })

  # table module for choosing existing model to start the policy
  policy_picker = callModule(model_table_module_server, 'policy-starter', data = reactive(model_obj$data), model = model, selection = "single", scoring_funcs = scoring_funcs,  question_data = question_data)

  # state tracking for UI
  current_state = reactiveVal(1)
  hide_back = reactiveVal(FALSE)

  # update visible UI based on state
  observeEvent({
    current_state()
    original_response()
    selected_questions()
    hide_back()
    }, {
    ix = current_state()
    purrr::iwalk(state_ids, function(x, i) {
      shinyjs::toggleElement(id = x, condition = i == ix, anim = TRUE)
    })
    shinyjs::toggleElement('back-container', condition = ix > 1 & !hide_back())
    shinyjs::toggleElement('next-container', condition = ix < 2 & !is.null(original_response()))
    shinyjs::toggleElement('create-container', condition = ix == 2 & length(selected_questions()) > 0)
  })

  # data from picked policy builder
  original_response = reactive({
    req(!is.null(policy_picker$selected()))
    model_obj$data$response[[policy_picker$selected()]]
  })

  observeEvent(original_response(), {
    shinyjs::show("next-container")
  })

  ## cycle the states of UI by the next and back buttons
  observeEvent(input$go, {
    current_state(current_state() + 1 )
  })


  observeEvent(input$back, {
    current_state(current_state() - 1)
  })

  # create a question builder from the chosen model and questions
  observeEvent(input$create, {
    # browser()
    question_clone = question_data
    to_remove = c()
    for(i in seq_along(question_clone)) {
      if(! question_clone[[i]]$node %in% selected_questions()) {
        to_remove = c(to_remove, i)
      }
    }
    question_clone[to_remove] = NULL

    unique_id = uuid::UUIDgenerate()

    questions_data_subset = question_clone
    response_subset = original_response()[selected_questions()]
    # browser()
    q_ui = questions_module_ui(ns(unique_id), questions_data_subset, response_subset, is_policy = TRUE)
    q_response = callModule(questions_module_server, unique_id, question_data = questions_data_subset, default_response = response_subset, is_policy = TRUE)
    # browser()
    subset_picked$data = questions_data_subset
    subset_picked$response = response_subset
    subset_picked$question_ui = q_ui
    subset_picked$server_response = q_response
    current_state(current_state() + 1)

    # because we are dynamically starting a module dependent on selected questions
    # we register the handlers for the different events that we need and destroy
    # any existing ones that are registered
    if(!is.null(subset_picked$observers$go)) {
      subset_picked$observers$go$destroy
    }
    subset_picked$observers$go = observeEvent(subset_picked$server_response$go(),{
      # remove the policy back button, show the questions one
      hide_back(TRUE)
      # current_state(current_state() + 1)
    })
    if(!is.null(subset_picked$observers$restart)) {
      subset_picked$observers$restart$destroy
    }
    subset_picked$observers$restart = observeEvent(subset_picked$server_response$restart(), {
      current_state(1)
      hide_back(FALSE)
    })
    if(!is.null(subset_picked$observers$finish)) {
      subset_picked$observers$restart$destroy
    }
    subset_picked$observers$finish = observeEvent(subset_picked$server_response$finish(), {
      full_state = original_response()
      new_state = subset_picked$server_response$state()
      # browser()
      names = intersect(names(full_state), names(new_state))
      full_state[names] = new_state[names]
      new_row = model_policy_row(full_state, model_name = model_obj$data$model[[policy_picker$selected()]], policy_name = subset_picked$server_response$name(), notes = subset_picked$server_response$comments())
      return_val(new_row)
    })
    if(!is.null(subset_picked$observer$visualise)) {
      subset_picked$observer$visualise$destroy
    }
    subset_picked$observer$visualise = observeEvent(subset_picked$server_response$visualise(), {
      subset_picked$observer$vis_clicked = runif(1)#subset_picked$server_response$visualise()
    })
  })

  output$`policy-questions-ui` = renderUI({
    subset_picked$question_ui
  })

  # table of questions to choose for building the scenario
  output$policy_response_picker = reactable::renderReactable({
    responses = format_responses(original_response())
    responses$response = purrr::map(responses$response, unlist)
    q_text = tibble::tibble(node = unique(purrr::map_chr(question_data,'node')))
    responses %>%
      dplyr::left_join(q_text) %>%
      dplyr::rename(
        "Question" = .data$node,
        "Response" = .data$response
      ) %>%
      dplyr::mutate(Question = .node_map[.data$Question]) %>%
      reactable::reactable(onClick = "select", selection = "multiple")
  })

  # tracks the selected quetsions
  selected_questions = reactive({
    names(original_response())[reactable::getReactableState('policy_response_picker', 'selected')]
  })

  # a reactive to track the state of the policy responses ready to return
  return_val = reactiveVal(NULL)

  return(list(
    state = return_val,
    visualise = reactive(subset_picked$observer$vis_clicked),
    data = policy_picker$data
  ))
}


## example
# temp = readRDS("temp.rds")
# questions = question_data = read_config("temp.yaml")
# default_response = load_responses("inst/default_model/default_response.json")
# model = bnlearn::read.bif(system.file("default_model/Model.bif", package = "diagramNAT"))
# # q_id = purrr::map_chr(questions, 'node')
# question_data = question_data[-1]
#
# ui = bootstrapPage(
#   shinyjs::useShinyjs(),
#   policy_creation_module_ui('test')
# )
#
# server = function(input, output, session, data, question_data) {
#   callModule(policy_creation_module_server, 'test', input_data = reactive(data), question_data = question_data, model = model)
# }
#
# shiny::shinyApp(ui, purrr::partial(server, data = temp, question_data = question_data))

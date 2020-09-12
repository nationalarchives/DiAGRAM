


policy_creation_module_ui = function(id) {
  ns = shiny::NS(id)
  shiny::column(
    width = 10, offset = 1,
    shiny::div(
      shiny::div(
        id = ns('policy-start-container'),
        shiny::div(
          "Please choose a model or scenario to build a new scenario from."
        ),
        model_table_module_ui(ns('policy-starter'))
      ),
      shinyjs::hidden(shiny::div(
        id = ns('policy-response-picker-container'),
        shiny::div(
          "Which responses would you like to change?"
        ),
        reactable::reactableOutput(ns('policy_response_picker'))
      )),
      shinyjs::hidden(
        shiny::div(
          id = ns('policy-questions-container'),
          uiOutput(ns('policy-questions-ui'))
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns("back-container"),
          shiny::actionButton(ns('back'), "Back")
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns("next-container"),
          shiny::actionButton(ns("go"), "Next")
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

policy_creation_module_server = function(input, output, session, input_data, question_data) {
  ns = session$ns
  # output from table in policy creation
  model_obj = reactiveValues(data = NULL)
  observeEvent(input_data(), {
    model_obj$data= input_data()
  })

  subset_picked = reactiveValues(
    data = NULL,
    response = NULL,
    question_ui = NULL,
    server_response = NULL,
    observers = list()
  )

  state_ids = paste0(c(
    "policy-start",
    "policy-response-picker",
    "policy-questions"
    # "policy-questions"
  ), "-container")

  policy_picker = callModule(model_table_module_server, 'policy-starter', data = reactive(model_obj$data), select_multiple = FALSE)

  current_state = reactiveVal(1)
  hide_back = reactiveVal(FALSE)

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
    req(!is.null(policy_picker()))
    model_obj$data$response[[policy_picker()]]
  })
  observeEvent(original_response(), {
    # req(!is.null(policy_picker()))
    # current_state(1)
    # req(!is.null(original_response()))
    # print("ok")
    shinyjs::show("next-container")
    # shinyjs::hide('policy-start-container')
    # shinyjs::show('policy-response-picker-container')
  })

  observeEvent(input$go, {
    current_state(current_state() + 1 )
  })


  observeEvent(input$back, {
    current_state(current_state() - 1)
  })

  observeEvent(input$create, {
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
    q_ui = questions_module_ui(ns(unique_id), questions_data_subset, response_subset, is_policy = TRUE)
    q_response = callModule(questions_module_server, unique_id, question_data = questions_data_subset, default_response = response_subset, is_policy = TRUE)
    subset_picked$data = questions_data_subset
    subset_picked$response = response_subset
    subset_picked$question_ui = q_ui
    subset_picked$server_response = q_response
    current_state(current_state() + 1)
    # shinyjs::hide('back-container')
    if(!is.null(subset_picked$observers$namer)) {
      # print('destroy')
      subset_picked$observers$namer$destroy()
    }
    subset_picked$observers$namer = observeEvent(subset_picked$server_response$name(), {
      print("naming time")
    })

    if(!is.null(subset_picked$observers$go)) {
      subset_picked$observers$go$destroy
    }
    subset_picked$observers$go = observeEvent(subset_picked$server_response$go(),{
      # remove the policy back button, show the questions one
      hide_back(TRUE)
      # current_state(current_state() + 1)
    })

    # observeEvent(subset_picked$server_response$go()), {}
  })



  output$`policy-questions-ui` = renderUI({
    subset_picked$question_ui
  })

  observeEvent(subset_picked$server_response, {
    req(subset_picked)
    print('server response')
    print(subset_picked$server_response)
  })

  output$policy_response_picker = reactable::renderReactable({
    responses = format_responses(original_response())
    q_text = purrr::map_dfr(question_data, function(x){
      tibble(Text = x$text, node = x$node)
    })
    responses %>%
      dplyr::left_join(q_text) %>%
      dplyr::rename(
        "Question" = .data$node,
        "Response" = .data$response
      ) %>%
      dplyr::mutate(Question = .node_map[.data$Question]) %>%
      reactable::reactable(onClick = "select", selection = "multiple")
  })

  selected_questions = reactive({
    names(original_response())[reactable::getReactableState('policy_response_picker', 'selected')]
  })

  observe({
    print(selected_questions())
  })
}

temp = readRDS("temp.rds")
questions = question_data = read_config("temp.yaml")
default_response = load_responses("inst/default_model/default_response.json")
model = bnlearn::read.bif(system.file("default_model/Model.bif", package = "diagramNAT"))
# q_id = purrr::map_chr(questions, 'node')
question_data = question_data[-1]

ui = bootstrapPage(
  shinyjs::useShinyjs(),
  policy_creation_module_ui('test')
)

server = function(input, output, session, data, question_data) {
  callModule(policy_creation_module_server, 'test', input_data = reactive(data), question_data = question_data)
}

shiny::shinyApp(ui, purrr::partial(server, data = temp, question_data = question_data))

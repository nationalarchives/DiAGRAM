formulate_question = function(question, default_response, ns) {
  # find the appropriate module funcs for this question type
  func_pair = switch(
    question$type,
    "multiple choice" = list(ui = radio_group_module_ui, server = radio_group_module_server),
    "grouped slider" = list(ui = sliders_group_module_ui, server = sliders_group_module_server),
    "slider" = list(ui = text_slider_pair_module_ui, server = text_slider_pair_module_server),
    stop(glue::glue("No UI layout functions found for type {question$type}."))
  )
  # formulate inputs to ui func
  # uniqueid = uuid::UUIDgenerate()
  uniqueid = question$node
  f_input = list(
    id = ns(uniqueid),
    # grab from the model loaded?
    state = default_response[[question$node]],
    content = question$detail,
    label = question$options#,
    # options =
  )

  server_args = switch(
    question$type,
    "multiple choice" = NULL,
    "grouped slider" = list(state = default_response[[question$node]]),
    "slider" = list(state = shiny::reactive(0)),
    stop(glue::glue("No module server functions found for type {question$type}."))
  )


  ui_el = #div(id = paste0(uniqueid,"-container"),
    do.call(func_pair$ui, f_input)
  # )
  list(id = uniqueid, ui_el = ui_el, server_func = func_pair$server, server_args = server_args)
}

create_question_block = function(questions, default_response = NA, ns) {
  purrr::map(questions, formulate_question, default_response = default_response, ns = ns)
}

questions_module_ui = function(id, question_data, default_response) {
  ns = shiny::NS(id)
  question_block = create_question_block(question_data, default_response, ns)
  questions_el = purrr::map(seq_along(question_block), function(i) {
    # if(i == 1) {
      return(
        shinyjs::hidden(div(
          id = ns(paste0(question_block[[i]]$id, "-container")),
          # title element
          shiny::div(.node_map[question_data[[i]]$node], class = "question-title"),
          div(class = "title-hint",
              shinyBS::bsButton(
                inputId = ns(paste0('title-hint-',question_data[[i]]$node)),label = "", icon = icon("question"),
                style = "info", size = "extra-small"
              ))
          ,
          shinyBS::bsPopover(
            id = ns(paste0('title-hint-',question_data[[i]]$node)), title = .node_map[question_data[[i]]$node],
            content = question_data[[i]]$definition,
            placement = "right",
            trigger = "click",
            options = list(container = "body")
          ),
          div(class = "question-prefix", "Please answer the following question:"),
          div(class = "question-content", question_data[[i]]$text),
          question_block[[i]]$ui_el
        )
      ))
    # }else {
    #   return(
    #     shinyjs::hidden(div(
    #       id = ns(paste0(question_block[[i]]$id, "-container")),
    #       shiny::tags$h3(.node_map[question_data[[i]]$node], style = "display: inline-block;"),
    #       div(style = "display: inline-block;",
    #           shinyBS::bsButton(
    #             inputId = ns(paste0('title-hint-',question_data[[i]]$node)),label = "", icon = icon("question"),
    #             style = "info", size = "extra-small"
    #           )),
    #       shinyBS::bsPopover(
    #         id = ns(paste0('title-hint-',question_data[[i]]$node)), title = .node_map[question_data[[i]]$node],
    #         content = question_data[[i]]$definition,
    #         placement = "right",
    #         trigger = "click",
    #         options = list(container = "body")
    #       ),
    #       p(glue::glue("
    #       Please answer the following question:
    #
    #       {question_data[[i]]$text}
    #                    ")),
    #       question_block[[i]]$ui_el
    #     ))
    #   )
    # }
  })

  # model initialisation
  launch_el = div(
    id= ns('question-launch-container'),
    class = "question-launch",
    shiny::actionButton(ns("start"), "Start")
  )
  # model naming starts hidden
  naming_el = shinyjs::hidden(div(
    id = ns('question-naming-container'),
    class = "question-name",
    shiny::textInput(ns("name"), label = "Give it a name", placeholder = "Baseline")
  ))

  # model finishing starts hidden
  finish_el = shinyjs::hidden(div(
    id = ns('question-finish-container'),
    class = "question-finish",
    shiny::actionButton(ns("end"), "Finish")
  ))

  # a header banner to follow across the top as you go through questions
  # starts hidden
  header_el = shinyjs::hidden(div(
    id = ns('question-header-container'),
    class = "question-header",
    div(
      class = "question-header-name",
      uiOutput(ns("header_name"))
    ),
    div(
      class = "question-comments",
      textAreaInput(ns("comment"), label = "Comments", placeholder = "These comments will appear in the summary table and report.")
    )

  ))

  back_el = shinyjs::hidden(div(
    id = ns('question-back-container'),
    class = "question-back",
    shiny::actionButton(ns('back'), "Back")
  ))

  forward_el = shinyjs::hidden(div(
    id = ns('question-next-container'),
    class = "question-next",
    shiny::actionButton(ns("go"), "Next")
  ))


  shiny::tagList(
    div(
      class = "question-container",
      shinyjs::useShinyjs(),
      header_el,
      launch_el,
      naming_el,
      questions_el,
      back_el,
      forward_el,
      finish_el
    )
  )
}

questions_module_server = function(input, output, session, question_data, default_response) {
  ## set up the question block ready to cycle through questions
  ns = session$ns
  question_block = create_question_block(question_data, default_response, ns)

  state_ids = paste0(c(
    "question-launch", "question-naming",
    purrr::map_chr(question_block,'id')
  ), "-container")

  n_q = length(question_block)
  current_state = shiny::reactiveVal(1)
  # previous_state = shiny_reactiveVal(0)
  observeEvent({
    current_state()
    input$name
    }, {
    ix = current_state()
    # only show next when current state is bigger than 1
    shinyjs::toggleElement('question-next-container', condition = ix > 1  & ix < length(state_ids) & input$name != '')
    shinyjs::toggleElement('question-back-container', condition = ix > 2)
    shinyjs::toggleElement('question-header-container', condition = ix > 2, anim = TRUE, animType = "fade")
    shinyjs::toggleElement('question-finish-container', condition = ix == length(state_ids))

    purrr::iwalk(state_ids, function(x, i) {
      shinyjs::toggleElement(id = x, condition = i == ix, anim = TRUE)
    })

    # shinyjs::toggleElement()
  })

  output$header_name = renderUI({
    div(glue::glue("Currently defining: {input$name}"))
  })

  observeEvent(input$start, {
    current_state(current_state() + 1)
  })

  observeEvent(input$go, {
    current_state(current_state() + 1)
  })

  observeEvent(input$back, {
    current_state(current_state() - 1)
  })


  ns = session$ns
  outputs = purrr::map(question_block, function(question) {
    module_args = c(list(module = question$server_func, id = question$id), question$server_args)
    do.call(callModule, module_args)
  }) %>% setNames(purrr::map(question_data, 'node'))
  rv = reactiveValues()

  observe({
    for(nam in names(outputs)) {
      rv[[nam]] = outputs[[nam]]()
    }
  })

  return_val = reactive({
    reactiveValuesToList(rv)
  })

  return(return_val)
}

# q_block = create_question_block(question_data[-1], default_response)

# q_block = q_block[2:3]

## example

questions = question_data = load_config("temp.yaml")
default_response = load_responses("inst/default_model/default_response.json")
# q_id = purrr::map_chr(questions, 'node')
question_data = question_data[-1]
# q = question = question_data[[1]]

ui = bootstrapPage(
  questions_module_ui('test', question_data, default_response)
)

server = function(input, output, session) {
  output = callModule(questions_module_server, 'test', question_data = question_data, default_response = default_response)
  observeEvent(output(), {
    print(output())
  })
}

shiny::shinyApp(ui, server)

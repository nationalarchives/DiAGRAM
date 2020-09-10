#question block

questions = question_data = load_config("temp.yaml")
default_response = load_responses("inst/default_model/default_response.json")
# q_id = purrr::map_chr(questions, 'node')
question_data = question_data[-1]
# q = question = question_data[[1]]

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
    if(i == 1) {
      return(
        div(
          id = ns(paste0(question_block[[i]]$id, "-container")),
          # title element
          shiny::tags$h3(.node_map[question_data[[i]]$node], style = "display: inline-block;"),
          div(style = "display: inline-block;",
            shinyBS::bsButton(
            inputId = ns(paste0('title-hint-',question_data[[i]]$node)),label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          )),
          shinyBS::bsPopover(
            id = ns(paste0('title-hint-',question_data[[i]]$node)), title = .node_map[question_data[[i]]$node],
            content = question_data[[i]]$definition,
            placement = "right",
            trigger = "click",
            options = list(container = "body")
          ),
          p(glue::glue("
          Please answer the following question:

          {question_data[[i]]$text}
                       ")),
          question_block[[i]]$ui_el
        )
      )
    }else {
      return(
        shinyjs::hidden(div(
          id = ns(paste0(question_block[[i]]$id, "-container")),
          shiny::tags$h3(.node_map[question_data[[i]]$node], style = "display: inline-block;"),
          div(style = "display: inline-block;",
              shinyBS::bsButton(
                inputId = ns(paste0('title-hint-',question_data[[i]]$node)),label = "", icon = icon("question"),
                style = "info", size = "extra-small"
              )),
          shinyBS::bsPopover(
            id = ns(paste0('title-hint-',question_data[[i]]$node)), title = .node_map[question_data[[i]]$node],
            content = question_data[[i]]$definition,
            placement = "right",
            trigger = "click",
            options = list(container = "body")
          ),
          p(glue::glue("
          Please answer the following question:

          {question_data[[i]]$text}
                       ")),
          question_block[[i]]$ui_el
          ))
      )
    }
  })
  shiny::tagList(
    div(
      style = "padding-top: 50px; padding-left: 50px;",
      shinyjs::useShinyjs(),
      questions_el,
      actionButton(ns("go"), "Next"),
      actionButton(ns('back'), "Back")
    )

  )
}

  questions_module_server = function(input, output, session, question_data, default_response) {
  ns = session$ns
  question_block = create_question_block(question_data, default_response, ns)
  current_question = shiny::reactiveVal(1)
  ns = session$ns
  outputs = purrr::map(question_block, function(question) {
    module_args = c(list(module = question$server_func, id = question$id), question$server_args)
    do.call(callModule, module_args)
  }) %>% setNames(purrr::map(question_data, 'node'))

  observeEvent(input$go, {
    # print(question_block[[current_question()]]$id)
    shinyjs::hide(
      paste0(question_block[[current_question()]]$id, "-container"),
      anim = TRUE, animType = "slide"
    )
    current_question(current_question() + 1)
    shinyjs::show(
      paste0(question_block[[current_question()]]$id, "-container"),
      anim = TRUE, animType = "slide"
    )
  })

  observeEvent(input$back, {
    shinyjs::hide(
      paste0(question_block[[current_question()]]$id, "-container"),
      anim = TRUE, animType = "slide"
    )
    current_question(current_question() - 1)
    shinyjs::show(
      paste0(question_block[[current_question()]]$id, "-container"),
      anim = TRUE, animType = "slide"
    )
  })

  rv = reactiveValues()

  observe({
    for(nam in names(outputs)) {
      rv[[nam]] = outputs[[nam]]()
    }
  })

  observe({
    print(reactiveValuesToList(rv))
  })
}

# q_block = create_question_block(question_data[-1], default_response)

# q_block = q_block[2:3]

ui = bootstrapPage(
  questions_module_ui('test', question_data, default_response)
  # shinyjs::useShinyjs(),
  # div(id = 'jamie-container', sliders_group_module_ui('jamie', state = default_response$Checksum, label = unlist(question_data[[1]]$options), content = question_data[[1]]$detail)),
  # shinyjs::hidden(div(id = 'bob-container', text_slider_pair_module_ui('bob',state = default_response$System_Security, label = '', content = question_data[[2]]$detail))),
  # actionButton('go',"Next")
)

server = function(input, output, session) {
  callModule(questions_module_server, 'test', question_data = question_data, default_response = default_response)
  # callModule(sliders_group_module_server, 'jamie', state = default_response$Checksum)
  # callModule(text_slider_pair_module_server, 'bob', state = default_response$System_Security, reactive_input = FALSE)
  #
  # observeEvent(input$go ,{
  #   shinyjs::hide('jamie-container', anim = TRUE, animType = 'slide')
  #   shinyjs::show('bob-container', anim = TRUE, animType = 'slide')
  # })
}

shiny::shinyApp(ui, server)

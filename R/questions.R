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
    "grouped slider" = list(state = default_response[[question$node]]) ,
    "slider" = list(state = default_response[[question$node]]),
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
      return(
        div(
          shinyjs::hidden(div(
            id = ns(paste0(question_block[[i]]$id, "-container")),
            # title element
            shiny::div(.node_map[question_data[[i]]$node], class = "question-title"),
            div(
              class = "title-hint",
              shinyBS::bsButton(
                inputId = ns(paste0('title-hint-',question_data[[i]]$node)),label = "", icon = icon("question"),
                style = "info", size = "extra-small"
              )
            ),
            div(class = "question-prefix", "Please answer the following question:"),
            div(class = "question-content", question_data[[i]]$text),
            question_block[[i]]$ui_el
          )),
          shinyBS::bsPopover(
            id = ns(paste0('title-hint-',question_data[[i]]$node)), title = .node_map[question_data[[i]]$node],
            content = question_data[[i]]$definition,
            placement = "right",
            trigger = "click"#,
            # options = list(container = "body")
          ),
        )
      )
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
    shiny::actionButton(ns("finish"), "Finish")
  ))

  what_next_el = shinyjs::hidden(div(
    id = ns('question-what-next-container'),
    class = "question-what-next",
    # shiny::div(
      shiny::column(
        width = 6, offset = 3,
        div(
          "Your responses have been stored!",
          shiny::actionButton(ns("restart"), "Create Another"),
          shiny::actionButton(ns("visualise"), "Visualise Results")
        )
      )
    # )
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
      finish_el,
      what_next_el
    )
  )
}

questions_module_server = function(input, output, session, question_data, default_response) {
  ## set up the question block ready to cycle through questions
  ns = session$ns
  question_block = create_question_block(question_data, default_response, ns)
  orig_state = purrr::map(question_block, ~.x$server_args$state) %>%
      setNames(purrr::map(question_data, 'node'))
  orig_state_rv = do.call(reactiveValues, orig_state)

  question_block = purrr::map(question_block, function(x) {
    x[['server_args']][['state']] = reactive(orig_state_rv[[x$id]])
    x
  })

  print(question_block[[1]]$server_args)

  observe({
    print("orig state rv")
    print(reactiveValuesToList(orig_state_rv))
  })
  #
  # for(i in seq_along(question_block)){
  #   question_block[[i]]$server_args$state = reactive(orig_state_rv[[question_block[[i]]$id]])
  # }

  state_ids = paste0(c(
    "question-launch", "question-naming",
    purrr::map_chr(question_block,'id'),
    "question-what-next"
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
    shinyjs::toggleElement('question-back-container', condition = ix > 2 & ix < length(state_ids))
    shinyjs::toggleElement('question-header-container', condition = ix > 2 & ix < length(state_ids), anim = TRUE, animType = "fade")
    shinyjs::toggleElement('question-finish-container', condition = ix == (length(state_ids) - 1))

    purrr::iwalk(state_ids, function(x, i) {
      shinyjs::toggleElement(id = x, condition = i == ix, anim = TRUE)
    })

    # shinyjs::toggleElement()
  })

  observeEvent(input$finish, {
    current_state(current_state() + 1)
  })

  observeEvent(input$restart, {
    updateTextInput(session, 'name', value = '')
    updateTextAreaInput(session, 'comment', value = '')
    for(nam in names(orig_state_rv)) {
      orig_state_rv[[nam]] = orig_state_rv[[nam]] + 1
      orig_state_rv[[nam]] = orig_state_rv[[nam]] - 1
    }
    # outputs <<- purrr::map(question_block, function(question) {
    #   module_args = c(list(module = question$server_func, id = question$id), question$server_args)
    #   do.call(callModule, module_args)
    # }) %>% setNames(purrr::map(question_data, 'node'))
    current_state(1)
  }, ignoreInit = TRUE)

  output$header_name = renderUI({
    div(glue::glue("Currently defining: {input$name}"))
  })

  observeEvent(input$start, {
    print("start")
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
  observe({
    print(return_val())
  })

  return(list(state = return_val, name = reactive(input$name), comments = reactive(input$comment), finish = reactive(input$finish)))
}

# q_block = create_question_block(question_data[-1], default_response)

# q_block = q_block[2:3]

## example

questions = question_data = read_config("temp.yaml")
default_response = load_responses("inst/default_model/default_response.json")
model = bnlearn::read.bif(system.file("default_model/Model.bif", package = "diagramNAT"))
# q_id = purrr::map_chr(questions, 'node')
question_data = question_data[-1]
# q = question = question_data[[1]]
library(reactable)
my_ui = function(question_data, default_response) {

  # create main dashboard page
  shiny::addResourcePath(
    "www", system.file("assets/www", package = "diagramNAT")
  )
  shiny::tagList(
    shiny::tags$head(shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "www/ui.css"
    )),
    shinydashboard::dashboardPage(
      skin="purple",
      # Add header and title to Dashboard
      shinydashboard::dashboardHeader(
        title="DiAGRAM"
      ),
      # Add dashboard sidebar
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebarMenu",
          shinydashboard::menuItem(
            "Home", tabName = "Home", icon = shiny::icon("home")
          ),
          shinydashboard::menuItem(
            "Model", tabName = "model", icon = shiny::icon("user-edit")
          )
        )
      ),
      shinydashboard::dashboardBody(
        id = "dashboardBody",
        dev_banner_module_ui('dev-banner'),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "Home",
            home_tab()
          ),
          shinydashboard::tabItem(
            tabName = "model",
            shiny::column(
              width = 12,
              shinydashboard::box(
                title = NULL, width = 12,
                questions_module_ui('model-questions', question_data, default_response)
              )
            )
          )
        )
      )

    )
  )
}

  #,
  # reactable::reactableOutput('table')
  # tableOutput("rand")
# )

model_policy_row = function(responses, model_name, policy_name = NA, notes = NA) {
  tibble::tibble(model = model_name, policy = policy_name, notes = notes ,response = list(responses))
}

server = function(input, output, session) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  q_output = callModule(questions_module_server, 'model-questions', question_data = question_data, default_response = default_response)

  model_obj = reactiveValues(
    data = tibble(model = character(), policy = character(), notes = character(), response = list())
  )

  table_data = reactive({
    df = model_obj$data
    print(nrow(df) > 0)
    # req(nrow(df) > 0)
    if(nrow(df) > 0) {
      mods = dplyr::bind_cols(df, purrr::map_dfr(df$response, ~{
        score_model(model, format_responses(.x)) %>% unlist
      }))
      print("table")
      mods %>%
        dplyr::select(
          model, policy, "Intellectual Control" = Intellectual_Control,
          Renderability, notes, response
        ) %>%
        dplyr::mutate_if(is.numeric, ~round(.x,2))
    }
    else {
      NA
    }

  })

  observe({
    print(model_obj$data)
  })

  observe(print(str(table_data())))


  # output$rand = renderTable({
  #   table_data()
  # })

  output$table = reactable::renderReactable({
    print("draw table")
    if(!is.na(table_data())){
      mods = table_data()
      reactable::reactable(
        mods %>% select(-response),
        groupBy = "model",
        details = function(index) {
          print(index)
          res = mods[index,]$response[[1]]
          tbl = reactable::reactable(format_responses(res))
          htmltools::div(style = list(margin = "12px 45px"), tbl)
        },
        onClick = "expand",
        rowStyle = list(cursor = "pointer")
      )
    }

  })

  observeEvent(q_output$finish(), {
    print("finished")
    new_row = model_policy_row(q_output$state(), model_name = q_output$name(), notes = q_output$comments())
    model_obj$data = dplyr::bind_rows(model_obj$data, new_row)
  })
}

shiny::shinyApp(ui = my_ui(question_data, default_response), server)

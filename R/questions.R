formulate_question = function(question, default_response, ns) {
  # find the appropriate module funcs for this question type

  func_pair = switch(
    question$type,
    "multiple choice" = list(ui = radio_group_module_ui, server = radio_group_module_server),
    "grouped slider" = list(ui = sliders_group_module_ui, server = sliders_group_module_server),
    "slider" = list(ui = text_slider_pair_module_ui, server = text_slider_pair_module_server),
    "non-numeric slider" = list(ui = text_slider_module_ui, server = text_slider_module_server),
    stop(glue::glue("No UI layout functions found for type {question$type}."))
  )
  # formulate inputs to ui func
  # uniqueid = uuid::UUIDgenerate()
  content = switch(
    question$type,
    "slider" = question$extra,
    "grouped slider" = question$detail,
    "multiple choice" = question$detail,
    "non-numeric slider" = NULL,
  )

  label = switch (
    question$type,
    "slider" = NULL,
    "grouped slider" = question$options,
    "multiple choice" = question$options,
    "non-numeric slider" = question$options
  )

  uniqueid = if(is.null(question$part)) question$node else paste(question$node, question$part, sep = "-")
  f_input = list(
    id = ns(uniqueid),
    # grab from the model loaded?
    state = default_response[[question$node]],
    content = content,
    label = label
    # options =
  )

  server_args = switch(
    question$type,
    "multiple choice" = list(state = default_response[[question$node]]),
    "grouped slider" = list(state = default_response[[question$node]]) ,
    "slider" = list(state = default_response[[question$node]]),
    "non-numeric slider" = list(state = default_response[[question$node]]),
    stop(glue::glue("No module server functions found for type {question$type}."))
  )


  ui_el = #div(id = paste0(uniqueid,"-container"),
    do.call(func_pair$ui, f_input)
  # )
  list(id = uniqueid, ui_el = ui_el, server_func = func_pair$server, server_args = server_args)
}

create_question_block = function(questions, default_response = NA, ns) {
  # purrr::map(questions, formulate_question, default_response = default_response, ns = ns)
  block = list()
  counter = 1
  for(q in questions) {
    node = q$node
    part = q$part
    print(node)
    print(part)
    if(is.null(part)) {
      block[[counter]] = formulate_question(q, default_response[node], ns)
    }else { # multi part question
      intermediate_response = default_response[[node]][part] %>% setNames(node)
      block[[counter]] = formulate_question(q, intermediate_response, ns)
    }
    counter = counter + 1
  }
  return(block)
}

#' @importFrom markdown markdownToHTML
questions_module_ui = function(id, question_data, default_response, is_policy = FALSE) {
  ns = shiny::NS(id)

  question_block = create_question_block(question_data, default_response, ns)
  questions_el = purrr::map(seq_along(question_block), function(i) {
   #html_text = "hi"
   html_text = shiny::tags$div(
     shiny::tags$div(
       class = "question-definition",
       markdown::renderMarkdown(text = as.character(paste0("Definition: ",question_data[[i]]$definition))) %>%
         htmltools::HTML()
     ),
     shiny::tags$div(
       class = "question-explanation",
       markdown::renderMarkdown(text = as.character(question_data[[i]]$explanation)) %>%
         htmltools::HTML()
     ),
     shiny::tags$div(
       class = "question-text",
       markdown::renderMarkdown(text = as.character(
         paste0(
           ifelse(is.null(question_data[[i]]$part), "1. " ,paste0(question_data[[i]]$part,". ")),
           question_data[[i]]$text
         )
       )) %>%
         htmltools::HTML()
     )
   )

   popover_html = markdown::renderMarkdown(text = as.character(question_data[[i]]$definition)) %>%
     # shiny::div() #%>%
   htmltools::HTML()
    # html_text = markdown::markdownToHTML(text = question_data[[i]]$text,
    #                                      fragment.only = TRUE)
      return(
        div(
          tags$head(
            # shiny::tags$link(
            #   rel = "stylesheet", type = "text/css",
            #   href = "www/shinyBS.css"
            # ),
            shiny::includeScript(system.file("assets", "js", "shinyBS.js",
                                             package = "diagramNAT"))),
          shinyjs::useShinyjs(),
          shinyjs::hidden(div(
            id = ns(paste0(question_block[[i]]$id, "-container")),
            style = "width: 100%",
            # title element
            shiny::div(
              class = "question-title",
              .node_map[question_data[[i]]$node]
              ),
            # ,
            #            ": ", question_data[[i]]$part,),
            # div(
            #   class = "title-hint",
            #   bsButton(
            #     inputId = ns(paste0('title-hint-',question_data[[i]]$node,question_data[[i]]$part)),label = "", icon = icon("question"),
            #     style = "info", size = "extra-small"
            #   )
            # ),
            # div(class = "question-prefix", "Please answer the following question:"),
            div(class = "question-content", html_text), #question_data[[i]]$text),
            question_block[[i]]$ui_el
          ))#,
          # bsPopover(
          #   id = ns(paste0('title-hint-',question_data[[i]]$node,question_data[[i]]$part)), title = .node_map[question_data[[i]]$node],
          #   content = popover_html,
          #     # glue::glue("`{shiny::HTML(knitr::knit2html(text = question_data[[i]]$definition,fragment = TRUE))}`"),
          #   placement = "right",
          #   trigger = "click"#,
          #   # options = list(container = "body")
          # ),
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
    shiny::textInput(ns("name"), label = "Give it a name", placeholder = if(is_policy) "Scenario" else "Baseline")
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
    shiny::div(
      shiny::column(
        width = 6, offset = 3,
    #     div(
    #       p("Your responses have been stored!"),
          shiny::actionButton(ns("guide"), "What next?"),
          shiny::actionButton(ns("restart"), "Create Another"),
    #       if(is_policy) NULL else shiny::actionButton(ns("policy"), "Add a scenario"),
    #       shiny::actionButton(ns("visualise"), "View Results")
    #     )
      )
    )
  ))

  # a header banner to follow across the top as you go through questions
  # starts hidden
  header_el = shinyjs::hidden(div(
    id = ns('question-header-container'),
    class = "question-header",
    div(
      class = "question-progress",
      shinyWidgets::progressBar(ns('progress'), 0, display_pct = TRUE)
    ),
    div(
      class = "question-header-name",
      uiOutput(ns("header_name"))
    ),
    div(
      class = "question-comments",
      textAreaInput(ns("comment"), label = "Comments", placeholder = "These comments will appear in the summary table and report. They are for you to use to make any notes for your reference as you answer the questions.")
    )

  ))

  back_el = shinyjs::hidden(div(
    id = ns('question-back-container'),
    class = "question-back",
    shiny::actionButton(ns('back'), "Back", class = "btn-orange")
  ))

  forward_el = shinyjs::hidden(div(
    id = ns('question-next-container'),
    class = "question-next",
    shiny::actionButton(ns("go"), "Next", class = "btn-green")
  ))


  shiny::tagList(
    # rintrojs::introjsUI()
    cicerone::use_cicerone(),
    shiny::h2("Create your baseline model"),
    div(
      class = "question-container",
      shinyjs::useShinyjs(),
      header_el,
      if(is_policy) NULL else launch_el,
      naming_el,
      questions_el,
      div(
        class = "question-button-row",
        back_el,
        forward_el,
        finish_el
      ),
      what_next_el
    )
  )
}

questions_module_server = function(input, output, session, question_data, default_response, is_policy = FALSE) {
  ## set up the question block ready to cycle through questions
  ns = session$ns
  question_block = create_question_block(question_data, default_response, ns)
  orig_state = purrr::map(question_block, ~.x$server_args$state) %>%
    setNames(purrr::map(question_data, ~paste(.x$node,.x$part,sep = "_")))
  orig_state_rv = do.call(reactiveValues, orig_state)

  question_block = purrr::map(question_block, function(x) {
    x[['server_args']][['state']] = reactive(orig_state_rv[[x$id]])
    x
  })

  next_steps_guide = if(!is_policy){
    cicerone::Cicerone$
      new(opacity = 0)$
      step(
        ns("restart"),
        "Create another",
        "Click here to build another model."
      )$
      step(
        "li > a[data-value='scenario']",
        "Create a Scenario",
        "Navigate here if you want to create a scenario from your model.",
        is_id = FALSE
      )$
      step(
        "li > a[data-value='visualise']",
        "View Results",
        "Come here if you want to see a graph of your models and scenarios.",
        is_id = FALSE
      )$
      step(
        "li > a[data-value='report']",
        "Download a report",
        "This tab is to create a downloadable report of your data in different formats.",
        is_id = FALSE
      )
  }else{
    cicerone::Cicerone$
      new(opacity = 0)$
      step(
        ns("restart"),
        "Create another",
        "Click here to build another scenario."
      )$
      step(
        "li > a[data-value='model']",
        "Create a Model",
        "Navigate here if you want to create a new model.",
        is_id = FALSE
      )$
      step(
        "li > a[data-value='visualise']",
        "View Results",
        "Come here if you want to see a graph of your models and scenarios.",
        is_id = FALSE
      )$
      step(
        "li > a[data-value='report']",
        "Download a report",
        "This tab is to create a downloadable report of your data in different formats.",
        is_id = FALSE
      )
  }

  observeEvent(input$guide, {
    next_steps_guide$init()$start()
  })
  # print(question_block[[1]]$server_args)

  # observe({
  #   print("orig state rv")
  #   print(reactiveValuesToList(orig_state_rv))
  # })
  #
  # for(i in seq_along(question_block)){
  #   question_block[[i]]$server_args$state = reactive(orig_state_rv[[question_block[[i]]$id]])
  # }

  state_ids = paste0(c(
    "question-launch", "question-naming",
    purrr::map_chr(question_block,'id'),
    "question-what-next"
  ), "-container")
  percentage_sequence = seq(0, 100, length.out = length(question_block) + 1)

  n_q = length(question_block)
  current_state = shiny::reactiveVal(if(is_policy) 2 else 1)
  # previous_state = shiny_reactiveVal(0)
  observeEvent({
    current_state()
    input$name

    }, {
    ix = current_state()
    # only show next when current state is bigger than 1
    shinyjs::toggleElement('question-next-container', condition = ix > 1  & ix < length(state_ids) -1 & input$name != '')
    shinyjs::toggleElement('question-back-container', condition = ix > 2 & ix < length(state_ids))
    shinyjs::toggleElement('question-header-container', condition = ix > 2 & ix < length(state_ids), anim = TRUE, animType = "fade")
    shinyjs::toggleElement('question-finish-container', condition = ix == (length(state_ids) - 1))
    shinyWidgets::updateProgressBar(session, "progress", value = percentage_sequence[ix-2])

    purrr::iwalk(state_ids, function(x, i) {
      shinyjs::toggleElement(id = x, condition = i == ix, anim = TRUE)
    })

    # shinyjs::toggleElement()
  })

  observeEvent(input$finish, {
    current_state(current_state() + 1)
  })

  observeEvent(input$restart, {
    if(!is_policy) {
      updateTextInput(session, 'name', value = '')
      updateTextAreaInput(session, 'comment', value = '')
      for(nam in names(orig_state_rv)) {
        init = orig_state_rv[[nam]]
        repit = rep(NA, length(init))
        orig_state_rv[[nam]] = repit #orig_state_rv[[nam]] + 1
        orig_state_rv[[nam]] = init #orig_state_rv[[nam]] - 1
      }
      current_state(1)
    }

  }, ignoreInit = TRUE)

  output$header_name = renderUI({
    div(glue::glue("Currently defining: {input$name}"))
  })

  observeEvent(input$start, {
    # print("start")
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
  }) %>% setNames(purrr::map(question_data, ~paste(.x$node,.x$part,sep = "_")))
  rv = reactiveValues()

  observe({
    for(nam in names(outputs)) {
      rv[[nam]] = outputs[[nam]]()
    }
  })

  return_val = reactive({
    orig_state = reactiveValuesToList(rv)
    req(length(orig_state) > 0)

    print("original state questions")
    print(orig_state)

    node = stringr::str_replace(names(orig_state), "_[0-9]$", "") %>% stringr::str_replace("_$","")
    res = list()
    # seen = c()
    for(name in unique(node)){
      print(name)
      if(sum(name == node) > 1) {
        print("multi")
        # appears as multipart
        temp = orig_state[name == node]
        res[[name]] = temp[sort(names(temp))] %>% setNames(1:sum(name == node))
      }else{
        print("single")
        res[[name]] = orig_state[[paste0(name, "_")]]
      }
    }
    # if(is_policy) browser()
    print("returned result")
    print(res)
    res
  })
  # observe({
  #   print(return_val())
  # })

  return(list(state = return_val, name = reactive(input$name), comments = reactive(input$comment), finish = reactive(input$finish),
              scenario = reactive(input$policy), visualise = reactive(input$visualise),
              go = reactive(input$go), restart = reactive(input$restart)))
}

model_policy_row = function(responses, model_name, policy_name = NA, notes = NA) {
  tibble::tibble(model = model_name, policy = policy_name, notes = notes ,response = list(responses))
}

# q_block = create_question_block(question_data[-1], default_response)

# q_block = q_block[2:3]

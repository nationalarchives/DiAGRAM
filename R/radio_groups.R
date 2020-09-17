## question layout for multi radio
#test > div > div:nth-child(1) > button
radio_group_module_ui = function(id = 'test', state, label = LETTERS[1:4], content) {
  ns = shiny::NS(id)
  # ns = identity

  questions = content
  options = label
  # questions = replicate(10, shinipsum::random_text(nwords = rpois(1,15)))
  if(!is.list(options)) {
    options = rep(list(options), length(questions))
  }
  # if(is.na(state)){
  #   state = purrr::map(options, 1) %>% unlist
  # }

  n_q = length(questions)
  buttons = purrr::map2(seq_along(questions), options, function(i, opt) {
    div(
      style = "clear: right;",
      div(
        style = if(length(questions) > 1) "max-width: 40%; display: inline-block; vertical-align: middle;",
        shiny::p(questions[i])
      ),
      div(
        style = if(length(questions) > 1) "max-width: 55%; display: inline-block; vertical-align: middle; float: right;" else NULL,
        shinyWidgets::radioGroupButtons(
          # class = "jamie",
          inputId = ns(paste0('test-',i)),
          label = NULL,
          choices = opt,
          selected = state[i],
          justified = TRUE,
          # individual = TRUE,
          status = "primary",
          checkIcon = list(
            yes = shiny::icon("ok",
                              lib = "glyphicon"),
            no = shiny::icon("remove",
                             lib = "glyphicon"))
        )
      )

    )


  })
  shiny::tagList(
    shinyjs::hidden(shiny::numericInput(ns("x"), NULL, min = n_q, max = n_q, value = n_q)),
    shiny::div(
      id = 'radio-group-container',
      buttons
    )
  )
}

radio_group_module_server = function(input, output, session, state) {
  return_val = reactiveVal(NULL)

  observeEvent(state(), {
    print("Update Radio Group")
    for(i in seq_along(state())) {
      shinyWidgets::updateRadioGroupButtons(session, paste0("test-",i), selected = state()[i])
    }
  })

  observe({
    vals = purrr::map_chr(paste0("test-",1:input$x), ~input[[.x]])
    print(vals)
    return_val(vals)
  })
  return(return_val)
}


# ## example
# temp = rep(list(LETTERS[1:4], LETTERS[1:5], LETTERS[1:3]), each = 4)
# temp[[5]] = NULL
# temp[[11]] = NULL
# ui = shiny::bootstrapPage(
#   shinyjs::useShinyjs(),
#   radio_group_module_ui('test', label = temp)
# )
#
# server = function(input, output, session) {
#   ns = session$ns
#   radio_vals = shiny::callModule(radio_group_module_server, 'test')
#   observeEvent(radio_vals(),{
#     print(radio_vals())
#   })
# }
#
# shiny::shinyApp(ui, server)

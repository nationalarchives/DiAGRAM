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
  if(is.na(state)){
    state = purrr::map(options, 1) %>% unlist
  }

  n_q = length(questions)
  buttons = purrr::map2(seq_along(questions), options, function(i, opt) {
    div(
      div(
        style = "max-width: 25%; display: inline-block; vertical-align: middle;",
        shinyWidgets::radioGroupButtons(
          # class = "jamie",
          inputId = ns(paste0('test-',i)),
          label = NULL,
          choices = opt,
          justified = TRUE,
          # individual = TRUE,
          status = "primary",
          checkIcon = list(
            yes = shiny::icon("ok",
                              lib = "glyphicon"),
            no = shiny::icon("remove",
                             lib = "glyphicon"))
        )
      ),
      div(
        style = "max-width: 62%; display: inline-block; vertical-align: middle;",
        shiny::p(questions[i])
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
  observe({
    return_val(purrr::map_chr(paste0("test-",1:input$x), ~input[[.x]]))
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

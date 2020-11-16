#
# # example
# pkgload::load_all()
# questions = question_data = read_config("temp.yaml")
# default_response = load_single_response("inst/default_model/default_response.json")
#
# default_response$made_up = sample(LETTERS[1:4], 10, replace = TRUE)
#
# model = bnlearn::read.bif(system.file("default_model/Model.bif", package = "diagramNAT"))
# # q_id = purrr::map_chr(questions, 'node')
# question_data = question_data[-1]
# # q = question = question_data[[1]]
# # library(reactable)
# my_ui = app_ui
#
#   #,
#   # reactable::reactableOutput('table')
#   # tableOutput("rand")
# # )
#
#
# #
# server =
#
# shiny::shinyApp(ui = my_ui(question_data, default_response), purrr::partial(server, question_data = question_data, default_response = default_response, model = model))

#
# # example
# pkgload::load_all()
# questions = question_data = read_config("temp.yaml")
# default_response = load_responses("inst/default_model/default_response.json")
# model = bnlearn::read.bif(system.file("default_model/Model.bif", package = "diagramNAT"))
# # q_id = purrr::map_chr(questions, 'node')
# question_data = question_data[-1]
# # q = question = question_data[[1]]
# # library(reactable)
# my_ui = function(question_data, default_response) {
#
#   # create main dashboard page
#   shiny::addResourcePath(
#     "www", system.file("assets/www", package = "diagramNAT")
#   )
#   shiny::tagList(
#     shiny::tags$head(shiny::tags$link(
#       rel = "stylesheet", type = "text/css",
#       href = "www/ui.css"
#     )),
#     shinydashboard::dashboardPage(
#       skin="purple",
#       # Add header and title to Dashboard
#       shinydashboard::dashboardHeader(
#         title="DiAGRAM"
#       ),
#       # Add dashboard sidebar
#       shinydashboard::dashboardSidebar(
#         shinydashboard::sidebarMenu(
#           id = "sidebarMenu",
#           shinydashboard::menuItem(
#             "Home", tabName = "Home", icon = shiny::icon("home")
#           ),
#           shinydashboard::menuItem(
#             "Model", tabName = "model", icon = shiny::icon("user-edit")
#           ),
#           shinydashboard::menuItem(
#             "Scenario", tabName = "scenario"
#           ),
#           shinydashboard::menuItem(
#             "Visualise", tabName = "visualise"
#           )
#         )
#       ),
#       shinydashboard::dashboardBody(
#         id = "dashboardBody",
#         dev_banner_module_ui('dev-banner'),
#         shinydashboard::tabItems(
#           # id = "menu-select",
#           shinydashboard::tabItem(
#             tabName = "Home",
#             home_tab()
#           ),
#           shinydashboard::tabItem(
#             tabName = "model",
#             shiny::column(
#               width = 12,
#               shinydashboard::box(
#                 title = NULL, width = 12,
#                 questions_module_ui('model-questions', question_data, default_response)
#               )
#             )
#           ),
#           shinydashboard::tabItem(
#             tabName = "scenario",
#             shiny::column(
#               width = 12,
#               shinydashboard::box(
#                 title = NULL, width = 12,
#                 policy_creation_module_ui('policy-questions')
#               )
#             )
#
#           ),
#           shinydashboard::tabItem(
#             tabName = "visualise",
#             shiny::column(
#               width = 12,
#               shinydashboard::box(
#                 width = 12,
#                 title = "Check back soon",
#                 "Coming soon"
#               )
#             )
#           )
#         )
#       )
#
#     )
#   )
# }
#
#   #,
#   # reactable::reactableOutput('table')
#   # tableOutput("rand")
# # )
#
#
# #
# server = function(input, output, session, question_data, default_response, model) {
#   shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
#   # output from the model builder tab
#   q_output = callModule(questions_module_server, 'model-questions', question_data = question_data, default_response = default_response)
#   p_output = callModule(policy_creation_module_server, 'policy-questions', reactive(model_obj$data), question_data = question_data, model = model)
#
#   observeEvent(q_output$scenario(),{
#     shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'scenario')
#   })
#
#   observeEvent(q_output$visualise(),{
#     shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
#   })
#
#   model_obj = reactiveValues(
#     data = tibble(model = character(), policy = character(), notes = character(), response = list())
#   )
#
#   observeEvent(q_output$finish(), {
#     print("finished")
#     new_row = model_policy_row(q_output$state(), model_name = q_output$name(), notes = q_output$comments())
#     model_obj$data = dplyr::bind_rows(model_obj$data, new_row)
#   })
#
#   all_policy = reactiveValues(data = NULL)
#
#   observeEvent(p_output$state(), {
#     req(!is.null(p_output$state()))
#     # vals = dplyr::distinct(dplyr::bind_rows(p_output(), model_obj$data))
#     model_obj$data = dplyr::bind_rows(model_obj$data, p_output$state())
#   })
#
#   observeEvent(p_output$visualise(), {
#     shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
#   })
#
#
#
#
#
#
#
#
#
#   # table_data = reactive({
#   #   df = model_obj$data
#   #   print(nrow(df) > 0)
#   #   # req(nrow(df) > 0)
#   #   if(nrow(df) > 0) {
#   #     mods = dplyr::bind_cols(df, purrr::map_dfr(df$response, ~{
#   #       score_model(model, format_responses(.x)) %>% unlist
#   #     }))
#   #     print("table")
#   #     mods %>%
#   #       dplyr::select(
#   #         model, policy, "Intellectual Control" = Intellectual_Control,
#   #         Renderability, notes, response
#   #       ) %>%
#   #       dplyr::mutate_if(is.numeric, ~round(.x,2))
#   #   }
#   #   else {
#   #     NA
#   #   }
#   #
#   # })
#
#   # observe({
#   #   print(model_obj$data)
#   # })
#
#   # observe(print(str(table_data())))
#
#
#   # output$rand = renderTable({
#   #   table_data()
#   # })
#
#   # output$table = reactable::renderReactable({
#   #   print("draw table")
#   #   if(!is.na(table_data())){
#   #     mods = table_data()
#   #     reactable::reactable(
#   #       mods %>% select(-response),
#   #       groupBy = "model",
#   #       details = function(index) {
#   #         print(index)
#   #         res = mods[index,]$response[[1]]
#   #         tbl = reactable::reactable(format_responses(res))
#   #         htmltools::div(style = list(margin = "12px 45px"), tbl)
#   #       },
#   #       onClick = "expand",
#   #       rowStyle = list(cursor = "pointer")
#   #     )
#   #   }
#   #
#   # })
#
#
# }
#
# shiny::shinyApp(ui = my_ui(question_data, default_response), purrr::partial(server, question_data = question_data, default_response = default_response, model = model))

#' model tab
#'
#' lays out the UI structure for the model building tab
#'
#' @return a shiny tag list object
# # model_tab = function() {
#   shiny::fluidRow(
#     shinydashboard::box(
#       title = NULL,
#       width = 12,
#
#     shiny::h1("Create your model"),
#     shiny::br(),
#     shiny::fluidRow(
#       shiny::column(
#         width=12,
#         shinydashboard::box(
#           title=NULL,
#           width=NULL,
#           shinyWidgets::progressBar("Question_Progress", value=1, total=nquestions),
#           shiny::uiOutput("Question"),
#           shinyjs::useShinyjs(),
#           shiny::br(),
#           shiny::uiOutput("CustomisationInput")
#         )
#       )
#     ),
#     shiny::fluidRow(
#       shiny::column(
#         width=8,
#         shinydashboard::box(
#           title="Model risk score",
#           width=NULL,
#           collapsible=TRUE,
#           shiny::plotOutput("BasicUtilityComparison")
#         )
#       ),
#       shiny::column(
#         width=4,
#         shinydashboard::box(
#           title="Upload a previous model",
#           collapsible=TRUE,
#           width=NULL,
#           shiny::strong("Please ensure any models uploaded have been generated from DiAGRAM"),
#           shiny::br(),
#           shiny::br(),
#           shiny::fileInput(
#             "customModel",
#             "Choose custom model",
#             accept=c(".bif")
#           ),
#           shiny::textInput("uploadName", label="Custom Model Name"),
#           shiny::actionButton("uploadCustomModel", "Add model")
#         )
#       )
#     )
#   )
#   )
# }

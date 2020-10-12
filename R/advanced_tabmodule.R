#' advanced_tab module ui
#'
#' @param id shiny id necessary for module
#' @importFrom shiny NS tagList
#' @export
advanced_tab_module_ui = function(id){
  ns = NS(id) # no lint (excluded from lint for jrshinyapp template)
  tagList(
    shinyjs::useShinyjs(),
    shinydashboard::box(
      title = "Configure",
      width = 12,
      shiny::fluidRow(
        model_table_module_ui(ns("models"))
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinyjs::hidden(textInput(ns("name"), "Name", placeholder = "Name your custom model or scenario"))
        ),
        shiny::column(
          width = 6,
          shinyjs::hidden(shiny::selectInput(ns("node_select"), "Choose", choices = NULL))
        )

      )
    ),
    fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Edit",
        actionButton(ns("add_change"), "Add change"),
        rhandsontable::rHandsontableOutput(ns("table"))
      ),
      shinydashboard::box(
        width = 6,
        title = "Changes",
        reactable::reactableOutput(ns("changes"))
      )
    )

    # shinyjs::hidden(

      # ),
    # shinyjs::hidden(

      # )
  )
}

#' advanced_tab module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
advanced_tab_module_server = function(input, output, session, data, model, scoring_funcs){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  mod_names = names(model)
  nice_nodes = setNames(mod_names, .node_map[mod_names])
  shiny::updateSelectInput(session = shiny::getDefaultReactiveDomain(), inputId = "node_select", choices = nice_nodes, selected = nice_nodes[1])
  original_nodes = do.call(reactiveValues, purrr::imap(model, ~{
    df = as.data.frame(.x$prob)
    if("Var1" %in% colnames(df)){
      df = dplyr::rename(df, !!.y := .data$Var1)
    }
      df %>% tidyr::spread(!!.y, Freq) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate_if(is.character, ~{
          stringr::str_replace_all(.x, "_", " ") %>%
            stringr::str_to_title()
        })
  }))

  intermediate_nodes = isolate(original_nodes)
  my_nodes = isolate(original_nodes)
  node_changes = eventReactive(reactiveValuesToList(my_nodes), {
    print("fire")
    purrr::map_dfr(setNames(names(original_nodes), names(original_nodes)), ~{
      dplyr::anti_join(my_nodes[[.x]], original_nodes[[.x]]) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(Node = .x) %>%
        tidyr::nest(data = -.data$Node)
    })
  })


  # observe({print(data())})
  selected = shiny::callModule(model_table_module_server, 'models', data = data, model = model, scoring_funcs = scoring_funcs, selection = "single")

  output$changes = reactable::renderReactable({
    df = node_changes()
    reactable::reactable(
      df[, "Node"],
      details = function(index) {
        shiny::div(
          reactable::reactable(df$data[[df$Node == df$Node[index]]])
        )
      }
    )
  })

  # response = reactive({
  #   data = reactiveValuesToList(my_nodes)
  #   class(data) = c("custom_model", class(data))
  #   # standard inputs
  #   data[.user_nodes] = purrr::map(data[.user_nodes], unlist)
  #
  #
  #   # temp for testing
  #   m_data = model_policy_row(data, "custom")
  #   responses = format_responses(m_data$response[[1]])
  # })


  output$table = rhandsontable::renderRHandsontable({
    req(input$node_select != "")
    req(!is.null(selected()))
    req(input$name != "")
    df = intermediate_nodes[[input$node_select]]
    print("draw table")
    is_numeric = sapply(df, is.numeric)
    ix_not_numeric = which(!is_numeric)
    ix_numeric = which(is_numeric)

    headers = colnames(df) %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title()
    rht = rhandsontable::rhandsontable(
      df,
      colHeaders = headers
    )
    # make all cols which aren't numeric read only
    for(i in ix_not_numeric) {
      rht = rht %>% rhandsontable::hot_col(col = i, readOnly = TRUE)
    }
    if(length(ix_numeric) == 2){
      # rht
      rht = rht %>% htmlwidgets::onRender(
        .numeric_pair_renderer
      )
    }
    rht

  })

  observeEvent(selected(), {
    row = data()[selected(),]
    row_response = row$response[[1]]
    probs = calculate_probabilities(model, format_responses(row_response), scoring_funcs)
    for(node in names(probs)){
      node_probs = probs[[node]]
      df = tibble::as_tibble_row(setNames(as.vector(node_probs), names(node_probs)))
      original_nodes[[node]] = df
      intermediate_nodes[[node]] = df
    }

  })

  observeEvent(input$add_change, {
    my_nodes[[input$node_select]] = intermediate_nodes[[input$node_select]]
  })

  observeEvent(input$table, {
    intermediate_nodes[[input$node_select]] = rhandsontable::hot_to_r(input$table)
  })

  # observe({
  #   req(input$node_select != "")
  #   print(original_nodes[[input$node_select]])
  #   print(my_nodes[[input$node_select]])
  #   print(all.equal(original_nodes[[input$node_select]], my_nodes[[input$node_select]]))
  #   print(!identical(original_nodes[[input$node_select]], reactiveValuesToList(my_nodes)[[input$node_select]]))
  #   shinyjs::toggle(
  #     "add_change",
  #     condition = !identical(original_nodes[[input$node_select]], reactiveValuesToList(my_nodes)[[input$node_select]])
  #   )
  # })

  observe({
    shinyjs::toggle("name", condition = !is.null(selected()))
    shinyjs::toggle("node_select", condition = !is.null(selected()) & input$name != "")
  })

  # observeEvent(selected(),{
  #
  # }, ignoreNULL = FALSE)
}


.numeric_pair_renderer = "function(el, x) {
        var hot = this.hot;
        max_col = hot.countCols() - 1;
        hot.addHook(
          'afterChange',
          function(changes, source) {
            if(source !== 'calculate' & source !== 'loadData'){
              changes.forEach(function(change) {
                var col = change[1] + 1 > max_col ? change[1] - 1 : change[1] + 1;
                hot.setDataAtCell(change[0], col, 1.0 - change[3], 'calculate');
              });
            }

            console.log(source);
            console.log(changes);
          }
        );
      }"

# "
# /*
#   var data = hot.getDataAtRow(change[0])
# var sum = data.map(
#   function(d) {
#     return parseFloat(d) || 0
#   }
# ).reduce(
#   function(a, b){
#     return a + b
#   },
#   0
# );
# */"

## example
ui = shiny::bootstrapPage(
  advanced_tab_module_ui('adv')
)

my_server = function(input, output, session, data, model, scoring_funcs) {
  shiny::callModule(advanced_tab_module_server, 'adv', data = reactive(data), model = model, scoring_funcs = scoring_funcs)
}

questions = diagramNAT::read_config(system.file("text_content", "final_questions.yml", package = "diagramNAT"))
default_response = diagramNAT::load_single_response(system.file("default_model", "new.json", package = "diagramNAT"))
model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))
server_data = model_policy_row(default_response, "default")
grouped_questions = group_questions(questions)
scoring_funcs = make_scoring_functions(grouped_questions)
server = purrr::partial(my_server, data = server_data, model = model, scoring_funcs = scoring_funcs)
shiny::shinyApp(ui, server)

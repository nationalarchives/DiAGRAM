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
        shiny::column(
          width = 6,
          actionButton(ns("new"), "Create from scratch")
        ),
        shiny::column(
          width = 6,
          actionButton(ns("modify"), "Modify existing")
        ),
      ),
      shiny::fluidRow(
        shinyjs::hidden(
          div(
            id = ns("model-table"),
            model_table_module_ui(ns("models"))
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shinyjs::hidden(textInput(ns("name"), "Name", placeholder = "Name your custom model or scenario"))
        ),
        shiny::column(
          width = 4,
          shinyjs::hidden(radioButtons(ns("type"), "Type", c("Model", "Scenario"), selected = "Model"))
        ),
        shiny::column(
          width = 4,
          shinyjs::hidden(shiny::selectInput(ns("node_select"), "Choose", choices = NULL))
        )

      )
    ),
      shinydashboard::box(
        width = 6,
        title = "Edit",
        fluidRow(column(
          width = 6,
          actionButton(ns("add_change"), "Add change")
        ),
        column(
          width = 6,
          actionButton(ns("store"), "Store Model")
        )),
        rhandsontable::rHandsontableOutput(ns("table"))
      ),
      shinydashboard::box(
        width = 6,
        title = "Changes",
        reactable::reactableOutput(ns("changes"))
      )
  )
}

#' advanced_tab module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
advanced_tab_module_server = function(input, output, session, data, model, scoring_funcs, question_data){
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
        stringr::str_replace_all(.x, "_", " ")
      })
  }))

  intermediate_nodes = isolate(do.call(reactiveValues, reactiveValuesToList(original_nodes)))
  my_nodes = isolate(do.call(reactiveValues, reactiveValuesToList(original_nodes)))

  observe({
    shinyjs::toggle("node_select", condition = input$name != "")
  })

  observeEvent({
    input$new
  }, {
    shinyjs::show("name")
  })

  observeEvent(
    input$modify,{
      shinyjs::show("type")
      shinyjs::show("name")
      shinyjs::show("model-table")
    }
  )

  node_changes = eventReactive(reactiveValuesToList(my_nodes), {
    print("fire")
    purrr::map_dfr(setNames(names(original_nodes), names(original_nodes)), ~{
      dplyr::anti_join(my_nodes[[.x]], original_nodes[[.x]]) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(Node = .x) %>%
        tidyr::nest(data = -.data$Node)
    })
  })


  selected = shiny::callModule(model_table_module_server, 'models', data = data, model = model, scoring_funcs = scoring_funcs, selection = "single",  question_data = question_data)

  output$changes = reactable::renderReactable({
    df = node_changes()
    print("draw changes")
    print(df)
    reactable::reactable(
      df[, "Node"],
      details = function(index) {
        print("details")
        print(index)
        print(df$Node[index])
        print(df$data)
        shiny::div(
          reactable::reactable(df$data[[index]])
        )
      }
    )
  })

  new_row = reactiveVal(NULL)
  observeEvent(reactiveValuesToList(my_nodes), {
    my_data = reactiveValuesToList(my_nodes)
    # is this a model or policy?
    type = input$type
    if(type == "Model"){
      m_data = model_policy_row(my_data, input$name)
    }else{
      row = data()[selected$selected(),]
      prev_name = row$model
      m_data = model_policy_row(my_data, prev_name, input$name)
    }


    new_row(m_data)
  })

  output$table = rhandsontable::renderRHandsontable({
    req(input$node_select != "")
    # req(!is.null(selected()))
    req(input$name != "")
    df = intermediate_nodes[[input$node_select]]
    print("draw table")
    print(df)
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

  observeEvent(selected$selected(), {
    row = data()[selected$selected(),]
    row_response = row$response[[1]]
    probs = calculate_probabilities(model, format_responses(row_response), scoring_funcs)
    for(node in .user_nodes){
      node_probs = probs[[node]]
      df = tibble::as_tibble_row(setNames(as.vector(node_probs), names(node_probs)))
      original_nodes[[node]] = df
      intermediate_nodes[[node]] = df
      my_nodes[[node]] = df
    }

  })

  observeEvent(input$add_change, {
    print("add change")
    df = data_in_table()
    print(df)
    intermediate_nodes[[input$node_select]] = df
    my_nodes[[input$node_select]] = df
  })

  data_in_table = reactiveVal(NULL)
  observeEvent(input$table, {
    df = rhandsontable::hot_to_r(input$table)
    data_in_table(df)
  })

  return(list(new_data = new_row, store = reactive(input$store), data = selected$data))
}


.numeric_pair_renderer = "function(el, x) {
          var hot = this.hot;
          hot.addHook(
            'afterChange',
            function(changes, source) {
              console.log('source = ' + source);
              // console.log('trig');
              if(source !== 'loadData' & source !== 'mycalc'){
                max_col = hot.countCols() - 1;
                console.log('cols = ' + max_col);
                console.log('execute source = ' + source);
                changes.forEach(function(change) {
                  var col = change[1] + 1 > max_col ? change[1] - 1 : change[1] + 1;
                  hot.setDataAtCell(change[0], col, 1.0 - change[3], 'mycalc');
                });
              } else{
              return;
              }


              //console.log(changes);
            }
          );
        }"


# # example
# pkgload::load_all()
# ui = shiny::bootstrapPage(
#   advanced_tab_module_ui('adv')
# )
#
# my_server = function(input, output, session, data, model, scoring_funcs) {
#   shiny::callModule(advanced_tab_module_server, 'adv', data = reactive(data), model = model, scoring_funcs = scoring_funcs)
# }
#
# questions = diagramNAT::read_config(system.file("text_content", "final_questions.yml", package = "diagramNAT"))
# default_response = diagramNAT::load_single_response(system.file("default_model", "new.json", package = "diagramNAT"))
# model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))
# server_data = model_policy_row(default_response, "default")
# grouped_questions = group_questions(questions)
# scoring_funcs = make_scoring_functions(grouped_questions)
# server = purrr::partial(my_server, data = server_data, model = model, scoring_funcs = scoring_funcs)
# shiny::shinyApp(ui, server)

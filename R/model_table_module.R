#' @export
model_table_module_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tagList(
    # diagramNAT_dependencies(),
    tags$head(
      shiny::includeScript(system.file("assets", "js", "reactable_supplement.js", package = "diagramNAT")),
      tags$style(
        "
input.table-input {
    width: 60%;
    display: inline;
}

input.table-input:disabled {
    background-color: transparent !important;
    border: transparent;
    cursor: default !important;
}
        "
      )
    ),
    shinyjs::useShinyjs(),
    # shiny::actionButton(ns("edit"), "", icon = shiny::icon("cog")),
    reactable::reactableOutput(ns('table'))
  )
}

format_model_table = function(intermediate, model, scoring_funcs, show_policy) {
  intermediate = dplyr::bind_cols(intermediate, purrr::map_dfr(intermediate$response, ~{
    score_model(model, format_responses(.x), scoring_funcs) %>% unlist
  }))
  if(show_policy) {
    res = intermediate %>%
      dplyr::select(.data$model, .data$policy, "Intellectual Control" = .data$Intellectual_Control, .data$Renderability, .data$notes, .data$response)

  }else{
    res = intermediate %>%
      dplyr::filter(is.na(.data$policy)) %>%
      dplyr::select(.data$model, "Intellectual Control" = .data$Intellectual_Control, .data$Renderability, .data$notes, .data$response)
  }
  res %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::mutate_if(is.numeric, ~ round(.x, 2))
}


#' @importFrom rlang .data
#' @export
model_table_module_server = function(input, output, session, data, model, scoring_funcs, selection = "multiple", show_policy = TRUE) {

  ns = session$ns
  # observe({
  #   print(class(data()))
  #   print(class(data))
  #   print(str(data()))
  #   print(data())
  # })
  formatted_data = shiny::reactive({
    # req(nrow(data()) > 0)
    if(nrow(data()) == 0){
      # print("Early return")
      return(NULL)
    }
    intermediate = data()
    format_model_table(intermediate, model, scoring_funcs, show_policy)
  })

  # output$dt = DT::renderDataTable({
  #   df = formatted_data()
  #   if(is.null(df)) return(NULL)
  #   DT::datatable(df)
  # })

  output$table = reactable::renderReactable({
    df = formatted_data()
    if(is.null(df)) return(NULL)
    reactable::reactable(
      df %>% dplyr::select(-.data$Response),
      groupBy = if(show_policy) "Model" else NULL,
      # defaultExpanded =
      columns = list(
        Model = reactable::colDef(
          html = TRUE,
          cell = htmlwidgets::JS(
            glue::glue(
              "function(cellInfo) {{
                console.log(cellInfo);
                var ix = typeof cellInfo.index === 'undefined' ? cellInfo.subRows[0]._index : cellInfo.index;
                return table_input(`{ns('name')}_${{ix}`, cellInfo.value, 'Name');
              }"
            )
          )
        ),
        `Intellectual Control` = reactable::colDef(
          align = "left",
          cell = function(value) {
            width = paste0(value / 1 * 100, "%")
            table_bar_chart(value, width = width, fill = "#358694")
          }
        ),
        Renderability = reactable::colDef(
          align = "left",
          cell = function(value) {
            width = paste0(value / 1 * 100, "%")
            table_bar_chart(value, width = width, fill = "#354694")
          }
        )
      ),
      # details = function(index) {
      #   # print(index)
      #   res = df[index, ]$Response[[1]]
      #   tbl = reactable::reactable(format_responses(res))
      #   htmltools::div(style = list(margin = "12px 45px"), tbl)
      # },
      # onClick = "expand",
      #   rowStyle = list(cursor = "pointer"),
      borderless = TRUE,
      onClick = if(selection %in% c("single", "multiple")) "select" else NULL,
      selection = if(selection %in% c("single", "multiple")) selection else NULL,
      highlight = TRUE,
      rowStyle = htmlwidgets::JS("
        function(rowInfo) {
          if (rowInfo && rowInfo.selected) {
            return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #358694' }
          }
        }
      "),
      style = list(fontFamily = "lato"),
    )
  })

  # we need to kick off a listener for the table edits as we cant' attach the event until it is ready
  # start_input_listener(ns('table'), ns('reactableEdit'))
  #
  # observeEvent(input$reactableEdit, {
  #   print(input$reactableEdit)
  # })

  # observeEvent(input$edit, {
  #   purrr::walk(seq_len(nrow(formatted_data()))-1, ~ {
  #     print(glue::glue("Enable {ns('name')}_{.x}"))
  #     shinyjs::toggleState(glue::glue("{ns('name')}_{.x}"))
  #   })
  # })

  selected = shiny::reactive(reactable::getReactableState("table", "selected"))

  return(selected)
}


table_bar_chart = function(
  label, width = "100%", height = "16px",
  fill = "#00bfc4", background = NULL
) {
  bar = shiny::div(
    style = list(
      background = fill,
      width = width,
      height = height
    )
  )
  chart = shiny::div(
    style = list(
      flexGrow = 1,
      marginLeft = "8px",
      background = background
    ), bar
  )
  shiny::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

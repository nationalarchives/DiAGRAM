#' @export
model_table_module_ui = function(id) {
  ns = shiny::NS(id)
  reactable::reactableOutput(ns('table'))
}

#' @importFrom rlang .data
#' @export
model_table_module_server = function(input, output, session, data, select_multiple = TRUE) {
  observe({
    print(data())
  })
  formatted_data = shiny::reactive({
    intermediate = data()
    intermediate = dplyr::bind_cols(intermediate, purrr::map_dfr(intermediate$response, ~{
      score_model(model, format_responses(.x)) %>% unlist
    }))
    intermediate %>%
      dplyr::select(.data$model, .data$policy, "Intellectual Control" = .data$Intellectual_Control, .data$Renderability, .data$notes, .data$response) %>%
      dplyr::rename_with(stringr::str_to_title) %>%
      dplyr::mutate_if(is.numeric, ~ round(.x, 2))
  })

  output$table = reactable::renderReactable({
    df = formatted_data()
    reactable::reactable(
      df %>% dplyr::select(-.data$Response),
      groupBy = "Model",
      columns = list(
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
      details = function(index) {
        # print(index)
        res = df[index, ]$Response[[1]]
        tbl = reactable::reactable(format_responses(res))
        htmltools::div(style = list(margin = "12px 45px"), tbl)
      },
      # onClick = "expand",
      #   rowStyle = list(cursor = "pointer"),
      borderless = TRUE,
      onClick = "select",
      selection = if(select_multiple) "multiple" else "single",
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

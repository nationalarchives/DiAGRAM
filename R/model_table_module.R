#' @export
model_table_module_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tagList(
    # diagramNAT_dependencies(),
#     tags$head(
#       shiny::includeScript(system.file("assets", "js", "reactable_supplement.js", package = "diagramNAT")),
#       tags$style(
#         "
# input.table-input {
#     width: 60%;
#     display: inline;
# }
#
# input.table-input:disabled {
#     background-color: transparent !important;
#     border: transparent;
#     cursor: default !important;
# }
#         "
#       )
#     ),
#     shinyjs::useShinyjs(),
    # shiny::actionButton(ns("edit"), "", icon = shiny::icon("cog")),
    # reactable::reactableOutput(ns('table'))
    DT::dataTableOutput(ns("table"))
  )
}

format_model_table = function(intermediate, model, scoring_funcs, show_policy) {
  intermediate = tryCatch(
    {dplyr::bind_cols(intermediate, purrr::map_dfr(intermediate$response, ~{
    score_model(model, format_responses(.x), scoring_funcs) %>% unlist
  }))}, error = function(e) browser())
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
model_table_module_server = function(input, output, session, data, model, scoring_funcs, selection = "multiple", show_policy = TRUE, pre_selected = selection == "multiple", editable = TRUE, response_show = TRUE, question_data) {
  select_opts = list(
    mode = selection
  )
  ns = session$ns
  data_src= reactiveVal(NULL)
  observeEvent(data(), {
    data_src(data())
  })
  table_contents = reactiveVal(NULL)

  observeEvent(data_src(), {
    print("data change")
    df = data_src()
    if(nrow(df) == 0){
      table_contents(DT::datatable(NULL))
      return()
    }

    if(pre_selected & selection == "multiple"){
      select_opts$selected = seq_len(nrow(df))
    }
    df = format_model_table(df, model, scoring_funcs, TRUE)
    print(df)
    if(!show_policy) {
      df = dplyr::filter(df, is.na(.data$Policy))
    }
    df = df %>% dplyr::mutate_if(is.numeric, ~{.x*100})
    if(editable){
      df = add_delete_column(df, ns)
      df = add_edit_column(df, ns)
    }
    if(response_show){
      df = add_show_column(df, ns)
    }
    tab = DT::datatable(
      dplyr::select(df, #Select,
                    .data$Edit, .data$Delete, tidyr::everything(), -.data$Response, Response = .data$Show),
      extensions = 'RowGroup',
      selection = select_opts,
      escape = FALSE,
      editable = list(target = "cell", disable = list(columns = c(1,4,5,7))),
      options = list(
        rowGroup = list(dataSrc = 3),
        preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    ) %>%
      DT::formatStyle(
        'Renderability',
        background = DT::styleColorBar(c(0,100), '#8400CD'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white'
      ) %>%
      DT::formatStyle(
        'Intellectual Control',
        background = DT::styleColorBar(c(0,100), '#FF6E3A'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white'
      )
    # to_delete(numeric(0))
    table_contents(tab)
  })

  output$table = DT::renderDataTable({
    print("draw table")
    table_contents()
  })

  observeEvent(input$editPressed, {
    print("edit press")
    ix = parse_column_event(input$editPressed)
    df = data_src()
    row = df[ix, ]
    # print(row)
    model_name = row$model
    policy_name = row$policy
    notes = row$notes
    # print(policy_name)
    modal = createModal(ns, model_name, policy_name, notes)
    showModal(modal)
  })

  observeEvent(input$modal_cancel, {
    print("modal cancel")
    removeModal()
  })

  observeEvent(input$modal_submit, {
    print("modal submit")
    will_close = TRUE
    ix = parse_column_event(input$editPressed)
    df = data_src()
    model_name = input$modal_model
    is_baseline = is.na(df[ix,]$policy)
    policy = input$modal_policy # if non existent = NULL, if empty
    comment = input$modal_comment # if placeholder = ''
    if(is_baseline){ # update all those with the same name
      if(!is.null(model_name) && model_name != ''){
        df[df$model == df[ix,]$model, ]$model = model_name
      }else{
        will_close = FALSE
      }

    }else {
      # check that policy is not empty
      if(!is.null(policy) && policy == ''){
        will_close = FALSE
      }else{
        df[ix,]$policy = policy
      }
    }
    if(comment == '') {
      comment = NA
    }
    df[ix,]$notes = comment

    if(will_close) {
      removeModal()
      data_src(df)
    }
  })

  show_modal_data = reactiveVal(NULL)
  observeEvent(input$showPressed, {
    ix = parse_column_event(input$showPressed)
    df = data_src()
    row = df[ix,]
    print(row)
    to_show = row %>%
      format_model_table(model, scoring_funcs, TRUE) %>%
      format_data_for_download(question_data)
    print(to_show)
    # print()
    show_modal_data(to_show)
    modal = create_data_modal(ns, to_show)
    showModal(modal)
  })
  observeEvent(
    input$data_modal_cancel,{
      removeModal()
    }
  )
  output$modal_table = DT::renderDataTable({
    DT::datatable(dplyr::select(show_modal_data(), .data$Question:.data$Response))
  })

  observeEvent(input$deletePressed, {
    print("delete pressed")
    ix = parse_column_event(input$deletePressed)
    df = data_src()
    df = df[-ix,]
    data_src(df)
  })

  selected = shiny::reactive({
    print("change selection")
    input$table_rows_selected
  })

  observe({
    print(selected())
  })

  return(list(selected = selected, data = data_src))
#
#   ns = session$ns
#   # observe({
#   #   print(class(data()))
#   #   print(class(data))
#   #   print(str(data()))
#   #   print(data())
#   # })
#   formatted_data = shiny::reactive({
#     # req(nrow(data()) > 0)
#     print(data())
#     if(nrow(data()) == 0){
#       # print("Early return")
#       return(NULL)
#     }
#     intermediate = data()
#     print(intermediate)
#     format_model_table(intermediate, model, scoring_funcs, show_policy)
#   })
#
#   # output$dt = DT::renderDataTable({
#   #   df = formatted_data()
#   #   if(is.null(df)) return(NULL)
#   #   DT::datatable(df)
#   # })
#
#   output$table = reactable::renderReactable({
#     df = formatted_data()
#     if(is.null(df)) return(NULL)
#     reactable::reactable(
#       df %>% dplyr::select(-.data$Response),
#       groupBy = if(show_policy) "Model" else NULL,
#       # defaultExpanded =
#       columns = list(
#         Model = reactable::colDef(
#           html = TRUE,
#           cell = htmlwidgets::JS(
#             glue::glue(
#               "function(cellInfo) {{
#                 console.log(cellInfo);
#                 var ix = typeof cellInfo.index === 'undefined' ? cellInfo.subRows[0]._index : cellInfo.index;
#                 return table_input(`{ns('name')}_${{ix}`, cellInfo.value, 'Name');
#               }"
#             )
#           )
#         ),
#         `Intellectual Control` = reactable::colDef(
#           align = "left",
#           cell = function(value) {
#             width = paste0(value / 1 * 100, "%")
#             table_bar_chart(value, width = width, fill = "#358694")
#           }
#         ),
#         Renderability = reactable::colDef(
#           align = "left",
#           cell = function(value) {
#             width = paste0(value / 1 * 100, "%")
#             table_bar_chart(value, width = width, fill = "#354694")
#           }
#         )
#       ),
#       # details = function(index) {
#       #   # print(index)
#       #   res = df[index, ]$Response[[1]]
#       #   tbl = reactable::reactable(format_responses(res))
#       #   htmltools::div(style = list(margin = "12px 45px"), tbl)
#       # },
#       # onClick = "expand",
#       #   rowStyle = list(cursor = "pointer"),
#       borderless = TRUE,
#       onClick = if(selection %in% c("single", "multiple")) "select" else NULL,
#       selection = if(selection %in% c("single", "multiple")) selection else NULL,
#       highlight = TRUE,
#       rowStyle = htmlwidgets::JS("
#         function(rowInfo) {
#           if (rowInfo && rowInfo.selected) {
#             return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #358694' }
#           }
#         }
#       "),
#       style = list(fontFamily = "lato"),
#     )
#   })
#
#   # we need to kick off a listener for the table edits as we cant' attach the event until it is ready
#   # start_input_listener(ns('table'), ns('reactableEdit'))
#   #
#   # observeEvent(input$reactableEdit, {
#   #   print(input$reactableEdit)
#   # })
#
#   # observeEvent(input$edit, {
#   #   purrr::walk(seq_len(nrow(formatted_data()))-1, ~ {
#   #     print(glue::glue("Enable {ns('name')}_{.x}"))
#   #     shinyjs::toggleState(glue::glue("{ns('name')}_{.x}"))
#   #   })
#   # })
#
#   selected = shiny::reactive(reactable::getReactableState("table", "selected"))
#
#   return(selected)
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



## extra utility funcs for adding events to the tables


add_delete_column = function(df, ns, ...){
  f = function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(ns("delete"), i, sep="_"),
        label = "Delete",
        icon = icon('trash'),
        onclick = glue::glue('Shiny.setInputValue(\"{ns("deletePressed")}\", this.id, {{priority: "event"}})')
      )
    )
  }

  deleteCol = unlist(lapply(seq_len(nrow(df)), f))
  dplyr::bind_cols(df, Delete = deleteCol)
}

add_show_column = function(df, ns, ...) {
  f = function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(ns("show"), i, sep="_"),
        label = "Show",
        # icon = icon('trash'),
        onclick = glue::glue('Shiny.setInputValue(\"{ns("showPressed")}\", this.id, {{priority: "event"}})')
      )
    )
  }

  showCol = unlist(lapply(seq_len(nrow(df)), f))
  dplyr::bind_cols(df, Show = showCol)
}

add_edit_column = function(df, ns, ...) {
  f = function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(ns("edit"), i, sep="_"),
        label = "Edit",
        icon = icon('edit'),
        onclick = glue::glue('Shiny.setInputValue(\"{ns("editPressed")}\", this.id, {{priority: "event"}})')
      )
    )
  }

  editCol = unlist(lapply(seq_len(nrow(df)), f))
  dplyr::bind_cols(df, Edit = editCol)
}

parse_column_event = function(idstr) {
  res = as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

create_data_modal = function(ns, data) {
  is_custom = unique(data$`Custom Model`)
  if(!is_custom) {
    el = DT::dataTableOutput(ns('modal_table'))
  }else{
    el = div("Data from custom models can not be shown.")
  }
  modalDialog(
    title = "View",
    el,
    footer = tagList(
      actionButton(ns("data_modal_cancel"), "Close"),
      # actionButton(ns("data_modal_submit"), "Submit")
    )
  )
}

createModal = function(ns, model, policy, comment) {
  # print(paste0("policy = ", policy))
  model_element = if(!is.na(policy)) {
    NULL
  } else {
    textInput(ns("modal_model"), "Model Name", value = model, placeholder = "This model must have a name")
  }
  policy_element = if(!is.na(policy)) {
    textInput(ns("modal_policy"), "Scenario Name", value = policy, placeholder = "This scenario must have a name.")
  }else{
    NULL
  }
  comment_element = if(!is.na(comment)) {
    textAreaInput(ns("modal_comment"), "Comments", value = comment)
  }else {
    textAreaInput(ns("modal_comment"), "Comments", placeholder = "You could add some comments here.")
  }
  modalDialog(
    title = "Edit",
    tagList(
      model_element,
      policy_element,
      comment_element
    ),
    footer = tagList(
      actionButton(ns("modal_cancel"), "Cancel"),
      actionButton(ns("modal_submit"), "Submit")
    )
  )
}

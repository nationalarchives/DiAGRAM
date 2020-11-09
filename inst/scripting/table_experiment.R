# model
model = bnlearn::read.bif(system.file("default_model/Model.bif",
                                      package = "diagramNAT"))
questions = diagramNAT::read_config(system.file("text_content", "final_questions.yml", package = "diagramNAT"))
default_response = diagramNAT::load_single_response(system.file("default_model", "new.json", package = "diagramNAT"))
model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))
grouped_questions = group_questions(questions)
scoring_funcs = make_scoring_functions(grouped_questions)

# original response
default = diagramNAT:::load_single_response("inst/default_model/new.json")

# model_policy_row = function(responses, model_name, policy_name = NA, notes = NA) {
#   tibble::tibble(model = model_name, policy = policy_name, notes = notes ,response = list(responses))
# }

# model 1 policy 1back
p1 = default
p1[[3]] = c(20,20,60)



# policy 2
p2 = p1
p2[[7]][1:2] = 40

model1 = dplyr::bind_rows(
  default %>% model_policy_row("default"),
  p1 %>% model_policy_row("default", "policy 1", "If we could increase info management to 70%"),
  p2 %>% model_policy_row("default", "policy 2", "If we could increase info management to 70% and technical skills to 40%")
)

model2 = model1
model2$model = "other"

unformatted = bind_rows(model1, model2)
df = format_model_table(unformatted, model, scoring_funcs, TRUE)

# create a character vector of shiny inputs
shinyButtonInput = function(FUN, len, id, ..., icon = NULL) {
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, icon = icon, width = NULL, ...))
  }
  inputs
}
shinyCheckboxInput = function(FUN, len, id, ..., default = FALSE) {
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, value = default, width = NULL, ...))
  }
  inputs
}

shinyLinkInput = function(len, id) {
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(actionLink(paste0(id, i), label = NULL, icon = shiny::icon('trash')))
  }
  inputs
}
shinyGroupInput = function(len, id, editIcon = shiny::icon('edit'), deleteIcon = shiny::icon('trash'), selectable = TRUE, default = FALSE){
  inputs = character(len)
  for(i in seq_len(len)) {
    if(selectable){
      check = checkboxInput(paste0(id, "-check-", i), label = NULL, value = default)
    }else{
      check = NULL
    }
    edit = actionButton(paste0(id, "-edit-", i), label = NULL, icon = editIcon)
    delete = actionButton(paste0(id, "-delete-", i), label = NULL, icon = deleteIcon)
    inputs[i] = as.character(div(check, edit, delete))
  }
  inputs
}

# obtain the values of inputs

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

# df$Response_button = shinyButtonInput(actionButton, nrow(df), 'res_1', "response")
# df$Check = shinyCheckboxInput(checkboxInput, nrow(df), 'check_1')
# df$Control = shinyGroupInput(nrow(df), "dt_control")

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



dt_table_ui = function(id) {
  ns = shiny::NS(id)
  DT::dataTableOutput(ns("table"))
}

dt_table_server = function(input, output, session, data, model, scoring_funcs, selection = "none", show_policy = TRUE, pre_selected = selection == "multiple", editable = TRUE, response_show = TRUE) {
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
    print(df)
    if(pre_selected & selection == "multiple"){
      select_opts$selected = seq_len(nrow(df))
    }
    df = format_model_table(df, model, scoring_funcs, TRUE)
    if(!show_policy) {
      df = dplyr::filter(is.na(.data$Policy))
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
        background = DT::styleColorBar(c(0,100), 'orange'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Intellectual Control',
        background = DT::styleColorBar(c(0,100), 'purple'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
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

  return(selected)
}

server = function(input, output, session) {
  selection_mode = "multiple"
  pre_selected = TRUE

  select_opts = list(
    mode = selection_mode
  )


  ns = session$ns

  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  data_src= reactiveVal(unformatted)
  table_contents = reactiveVal(NULL)

  observeEvent(data_src(), {
    df = data_src()
    if(pre_selected & selection_mode == "multiple"){
      select_opts$selected = seq_len(nrow(df))
    }
    df = format_model_table(df, model, scoring_funcs, TRUE)
    df = df %>% mutate_if(is.numeric, ~{.x*100})
    # df$Response_button = shinyButtonInput(actionButton, nrow(df), 'res_1', "response")
    # df$Select = shinyCheckboxInput(checkboxInput, nrow(df), 'check_1')
    # df$Edit = shinyButtonInput(actionButton, nrow(df), "edit_1", icon = shiny::icon('edit'), 'Edit')
    # df$Delete = shinyButtonInput(actionButton, nrow(df), "del_1", icon = shiny::icon('trash'), 'Delete')
    df = add_delete_column(df, ns)
    df = add_edit_column(df, ns)
    df = add_show_column(df, ns)
    # df$Control = shinyGroupInput(nrow(df), "dt_control")
    tab = datatable(
      select(df, #Select,
             Edit, Delete, everything(), -Response, Response = Show),
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
        background = DT::styleColorBar(c(0,100), 'orange'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Intellectual Control',
        background = DT::styleColorBar(c(0,100), 'purple'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    # to_delete(numeric(0))
    table_contents(tab)
  })

  output$table = DT::renderDataTable({
    table_contents()
  })

  # to_show = eventReactive(shinyValue('res_1', nrow(data_src())), {
  #   btn_press = shinyValue('res_1', nrow(data_src()))
  #   req(!all(is.na(btn_press)))
  #   ix = which(btn_press != old_response_state())
  #   old_response_state(btn_press)
  #   ix
  # })

  observeEvent(input$editPressed, {
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
    removeModal()
  })

  observeEvent(input$modal_submit, {
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
      # df[ix,]$model = model_name
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

  observeEvent(input$deletePressed, {
    ix = parse_column_event(input$deletePressed)
    df = data_src()
    df = df[-ix,]
    data_src(df)
  })

  selected = shiny::reactive({
    input$table_rows_selected
  })

  observe({
    print(selected())
  })

  # observeEvent(shinyValue('del_1', nrow(data_src())), {
  #   btn_press = shinyValue('del_1', nrow(data_src()))
  #   print(btn_press)
  #   print(old_delete_state)
  #   req(!all(is.na(btn_press)))
  #   ix = which(btn_press != old_delete_state())
  #   old_delete_state(btn_press)
  #   print(ix)
  #   to_delete(ix)
  # })

  # to_edit = eventReactive(shinyValue('edit_1', nrow(data_src())), {
  #   btn_press = shinyValue('edit_1', nrow(data_src()))
  #   req(!all(is.na(btn_press)))
  #   ix = which(btn_press != old_edit_state())
  #   old_edit_state(btn_press)
  #   ix
  # })

  # observeEvent(to_delete(), {
  #   req(to_delete())
  #   print("delete")
  #   print(to_delete())
  #   df = data_src()
  #   df = df[-to_delete(), ]
  #   print(df)
  #   old_delete_state(numeric(nrow(df)))
  #   data_src(df)
  # })
}

ui = fluidPage(
  dt_table_ui('test')
)

server = function(input, output, session) {
  selected = callModule(dt_table_server, "test", data = reactive(unformatted), model = model, scoring_funcs = scoring_funcs, selection = "single", session= session)
}

shiny::shinyApp(ui, server)



df %>% group_by(model)


# new model
library("diagramNAT")
m2 =diagramNAT:::generate_example_reponse()

# model 2 policy 1
m2p1 = m2
m2p1[[6]] = 1:3*100/6

model2 = dplyr::bind_rows(
  m2 %>% model_policy_row("random model", notes = "Just set a model with random values"),
  m2p1 %>% model_policy_row("random model", "random policy", "same random model, with some values randomly tweaked")
)

temp = dplyr::bind_rows(
  model1,
  model2
)

temp = dplyr::bind_cols(temp, purrr::map_dfr(temp$response, ~{
  diagramNAT:::score_model(model, diagramNAT:::format_responses(.x)) %>% unlist
}))


# mutate(response = purrr::map_chr(response, ~{.x %>% jsonlite::toJSON(auto_unbox = TRUE) %>% as.character()})) %>%
library("htmltools")

bar_chart = function(label,
                     width = "100%",
                     height = "16px",
                     fill = "#00bfc4",
                     background = NULL) {
  bar = div(style = list(
    background = fill,
    width = width,
    height = height
  ))
  chart = div(style = list(
    flexGrow = 1,
    marginLeft = "8px",
    background = background
  ),
  bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

temp %>%
  reactable::reactable(
    groupBy = "Model",
    columns = list(
      `Intellectual Control` = reactable::colDef(
        align = "left",
        cell = function(value) {
          width = paste0(value / 1 * 100, "%")
          bar_chart(value, width = width, fill = "#358694")
        }
      ),
      Renderability = reactable::colDef(
        align = "left",
        cell = function(value) {
          width = paste0(value / 1 * 100, "%")
          bar_chart(value, width = width, fill = "#354694")
        }
      )
    ),
    details = function(index) {
      print(index)
      res = temp[index, ]$response[[1]]
      tbl = reactable::reactable(format_responses(res))
      htmltools::div(style = list(margin = "12px 45px"), tbl)
    },
    # onClick = "expand",
    #   rowStyle = list(cursor = "pointer"),
    borderless = TRUE,
    onClick = "select",
    selection = "single",
    highlight = TRUE,
    rowStyle = htmlwidgets::JS(
      "function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #358694' }
    }
  }"
    ),
    style = list(fontFamily = "lato"),
  )

plot = sparkline::sparkline(values = c(1, 0.8), type = "bullet", chartRangeMin = 0, chartRangeMax = 1)
plot$jsHooks

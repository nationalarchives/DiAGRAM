
# Following script defines interactions between components displayed
# on the webpage.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com
# @author: Sidhant Bhatia, Monash University, Malaysia
# @email: sidhant3b@gmail.com

# library(shiny)
# library(graph)
# library(bnlearn)
# library(networkD3)
# library(BiocManager)
# library(Rgraphviz)
# library(tidyverse)
# library(shinyjs)
# library(shinyalert)
# library(gridExtra)
# library(data.table)

# options(repos = BiocManager::repositories())
# options(shiny.fullstacktrace = FALSE)

# TODO: policy plotting should be one reactive variable

#' The application server-side
#'
#' This function largely calls the sub modules for the application and deals with the top level data manipulation.
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param question_data The question data, named list, likely loaded from the YAML file internal to this package
#' @param default_response The default responses used to initialise the model
#' @param model The stored model, loaded from a .bif file
#' @importFrom plotly renderPlotly plot_ly
#' @importFrom utils zip
#' @importFrom grDevices png dev.off
#' @importFrom bnlearn read.bif graphviz.plot mutilated as.grain bn.fit.barchart write.bif
#' @importFrom gRain querygrain
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggplot geom_bar aes xlab geom_hline stat_summary geom_text theme_light theme element_blank element_text scale_fill_manual position_stack ylab
#' @importFrom dplyr arrange filter select rename add_row summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom shiny reactiveValues updateSelectInput plotOutput renderUI tagList strong renderTable
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyWidgets sliderTextInput updateProgressBar
#' @importFrom shinyalert shinyalert
#' @importFrom tibble tibble rownames_to_column
#' @importFrom rlang .data
#' @importFrom shinyjs show hide enable
#' @importFrom shinysky renderHotable hot.to.df
#' @importFrom stringr str_remove
#' @importFrom DT datatable renderDataTable
#' @importFrom stats reorder
#' @export
app_server = function(input, output, session, question_data, default_response, model, scoring_funcs) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))

  # reactive value to store the state of models/scenarios
  model_obj = shiny::reactiveValues(
    data = tibble::tibble(model = character(), policy = character(), notes = character(), response = list())
  )
  # output from the model builder tab
  q_output = shiny::callModule(questions_module_server, 'model-questions', question_data = question_data, default_response = default_response)
  # output from the policy builder tab
  p_output = shiny::callModule(policy_creation_module_server, 'policy-questions', reactive(model_obj$data), question_data = question_data, model = model, scoring_funcs = scoring_funcs)


  # listen for changes to data in the table in the model building tab
  mod_only_table = shiny::callModule(model_table_module_server, "model_table", data = reactive(model_obj$data), model = model, selection = "none", show_policy = FALSE, scoring_funcs = scoring_funcs, question_data = question_data)
  shiny::observeEvent(mod_only_table$data(), {
    model_obj$data = mod_only_table$data()
  },ignoreInit = TRUE)

  # listen for changes to data in the visualisation tab
  policy_vis = shiny::callModule(policy_visualisation_module_server, 'bar', model_data = reactive(model_obj$data), model = model, scoring_funcs = scoring_funcs,  question_data = question_data)
  observeEvent(policy_vis(), {
    model_obj$data = policy_vis()
  }, ignoreInit = TRUE)

  # listen for changes to data in the reporting tab
  report_update = shiny::callModule(report_tab_module_server, 'report', data = reactive(model_obj$data), question_data = question_data, model = model, scoring_funcs = scoring_funcs)
  shiny::observeEvent(report_update(), {
    model_obj$data = report_update()
  }, ignoreInit = TRUE)

  # control when to show the warning about data loss to users
  seen_warning = shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$sidebarMenu,{
    if(input$sidebarMenu == "model" & !seen_warning()) {
      shinyalert::shinyalert(
        title = "Warning", type = "warning",
        text = "Your data will be lost after 15 minutes of inactivity.",
        closeOnEsc = FALSE, closeOnClickOutside = FALSE
      )
      seen_warning(TRUE)
    }
  })

  shiny::observeEvent(input$createModel, {
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'model')
  })

  # load data into the table from upload
  shiny::observeEvent(input$upload, {
    file = input$upload
    req(file)
    shiny::validate(need(tools::file_ext(file$datapath) == "rds", "Please upload a .rds file."))
    # content = load_responses(file$datapath)
    content = readRDS(file$datapath)
    print("content")
    model_obj$data = dplyr::bind_rows(
      model_obj$data, content
    )
    print("upload complete")
  })

  # listen to events from the model creation tab
  shiny::observeEvent(q_output$scenario(),{
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'scenario')
  })

  shiny::observeEvent(q_output$visualise(),{
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
  })

  shiny::observeEvent(q_output$finish(), {
    print("finished")
    print(q_output$state())
    new_row = model_policy_row(q_output$state(), model_name = q_output$name(), notes = q_output$comments())
    model_obj$data = dplyr::bind_rows(model_obj$data, new_row)
    print("row added")
  })

  # decide when to show the box for the table in model creation tab
  shiny::observeEvent(model_obj$data, {
    # shinyjs::toggleElement("no-model-container", condition = nrow(model_obj$data) == 0)
    if(nrow(model_obj$data) != 0) {
      print("show element")
      shinyjs::show("model-table-container")
    }
  })

  ## listen to events from policy creation
  all_policy = reactiveValues(data = NULL)

  shiny::observeEvent(p_output$data(), {
    model_obj$data = p_output$data()
  }, ignoreInit = TRUE)

  shiny::observeEvent(p_output$state(), {
    shiny::req(!is.null(p_output$state()))
    model_obj$data = dplyr::bind_rows(model_obj$data, p_output$state())
  })

  shiny::observeEvent(p_output$visualise(), {
    shinydashboard::updateTabItems(session = shiny::getDefaultReactiveDomain(), inputId = "sidebarMenu", selected = 'visualise')
  })

  ## listen to the timeout warning
  observeEvent(input$timeout,{
    shinyjs::alert(
      # title = "Warning", type = "warning",
      text = "Your session is due to time out in two minutes. Please take an action or your session will be lost."
    )
  })
  ### listen to events from advanced tab
  advanced_return = shiny::callModule(advanced_tab_module_server, 'adv', data = reactive(model_obj$data), model = model, scoring_funcs = scoring_funcs,  question_data = question_data)

  shiny::observeEvent(advanced_return$data(), {
    # if table is edited in advanced, update it here
    print("advanced update")
    print(advanced_return$data)
    print(advanced_return$data())
    model_obj$data = advanced_return$data()
  })

  shiny::observeEvent(advanced_return$store(), {
    print(advanced_return$new_data())
    model_obj$data = dplyr::bind_rows(model_obj$data, advanced_return$new_data())
  })

}

#' report_tab module ui
#'
#' @param id shiny id necessary for module
#' @importFrom shiny NS tagList
#' @export
report_tab_module_ui = function(id){
  ns = NS(id) # no lint (excluded from lint for jrshinyapp template)
  tagList(
    shinyjs::useShinyjs(),
    shinydashboard::box(
      width = 12,
      shiny::h1("Download a Report")
    ),
    shinydashboard::box(
      title = "Summary", width = 12,
      # shinipsum::random_text(nwords = 50)
      includeMarkdown(system.file("text_content", "report_summary.md", package = "diagramNAT"))
    ),
    shinydashboard::box(
      width = 12,
      shiny::div(
        "1. Select what you would like to download"
      ),
      model_table_module_ui(ns('table')),
      shiny::div(
        "2. Select the format of your report",
        # shiny::checkboxGroupInput(ns('download_select'), "Formats", choices = c("pdf", "csv", "json")),
        # shiny::downloadButton(ns("download"))
        shiny::div(
          shinyjs::disabled(shiny::downloadButton(ns("pdf"), "PDF")),
          "download a PDF to see a presentation version of your results."
        ),
        shiny::div(
          shinyjs::disabled(shiny::downloadButton(ns("csv"), "CSV")),
          "download a CSV file to create your own graphs from the data."
        ),
        shiny::div(
          shinyjs::disabled(shiny::downloadButton(ns("json"), "App data")),
          "download a binary data file to upload your model to DiAGRAM in the future."
        )


      )
    )
  )
}

#' report_tab module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @param data reactive data source for table
#' @param model The bnlearn model to calculate scores
#' @param scoring_funcs a named list of custom scoring functions to apply to raw responses
#' @import shiny
report_tab_module_server = function(input, output, session, data, model, question_data, scoring_funcs){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  selected = shiny::callModule(model_table_module_server, 'table', data = data, model = model, scoring_funcs = scoring_funcs)

  output$pdf = shiny::downloadHandler(
    filename = function() {
      paste0("DiAGRAM-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      td = tempdir()
      temp = file.path(td, "report.Rmd")
      temp_sub = file.path(td, "pdf_section.Rmd")
      file.copy(system.file("assets", "templates", "pdf_template.Rmd", package = "diagramNAT"), temp, overwrite = TRUE)
      file.copy(system.file("assets", "templates", "pdf_section.Rmd", package = "diagramNAT"), temp_sub, overwrite = TRUE)
      pdf_file = prepare_pdf(data()[selected$selected(),], question_data, model, scoring_funcs, template = temp, file = file)
      # on.exit(file.remove(pdf_file))
      # file.copy(pdf_file, file)
    }
  )

  observe({
    shinyjs::toggleState(id = "pdf", condition = length(selected$selected()) > 0)
    shinyjs::toggleState(id = "csv", condition = length(selected$selected()) > 0)
    shinyjs::toggleState(id = "json", condition = length(selected$selected()) > 0)
  })

  output$csv = shiny::downloadHandler(
    filename = function() {
      paste0("DiAGRAM-data-",Sys.Date(), ".csv")
    },
    content = function(file) {
      csv_file = data()[selected$selected(),] %>%
        format_model_table(model, scoring_funcs, TRUE) %>%
        format_data_for_download(question_data) %>%
        prepare_csv()
      on.exit(file.remove(csv_file))
      file.copy(csv_file, file)
    }
  )

  output$json = shiny::downloadHandler(
    filename = function() {
      paste0("DiAGRAM-data-", Sys.Date(), ".rds")
    },
    content = function(file) {
      json_file = tempfile(fileext = ".rds")
      saveRDS(data()[selected$selected(),], json_file)#prepare_json(data()[selected$selected(),])
      on.exit(file.remove(json_file))
      file.copy(json_file, file)
    }
  )

  # output$download = shiny::downloadHandler(
  #   filename = function() {
  #     paste0("DiAGRAM-data-", Sys.Date(), ".zip")
  #   },
  #   content = function(file) {
  #     json_file = if("json" %in% input$download_select) prepare_json(data()[selected(),]) else NULL
  #     # prepare data for download
  #     csv_file = if("csv" %in% input$download_select) {
  #       data()[selected(),] %>%
  #         format_model_table(model, scoring_funcs, TRUE) %>%
  #         format_data_for_download(question_data) %>%
  #         prepare_csv()
  #     } else {
  #       NULL
  #     }
  #     pdf_file = if("pdf" %in% input$download_select) {
  #       prepare_pdf(data()[selected(),], question_data, model, scoring_funcs, template = system.file("assets", "templates", "pdf_template.Rmd", package = "diagramNAT"))
  #     } else {
  #       NULL
  #     }
  #     all_files = c(csv_file, json_file, pdf_file)
  #     print(all_files)
  #     zip(file, all_files[!is.null(all_files)], flags = "-j")
  #   },
  #   contentType = "application/zip"
  # )
  return(selected$data)
}

## Functions for data IO

#' Prepare json
#'
#' Will prepare the data in a JSON format which is suitable for upload at a later date
#' @param data Tibble to be stored
#' @param file A file location to write to
#' @return The file location
prepare_json = function(data, file = tempfile(fileext = ".json")) {
  save_responses(data, file)
  return(file)
}

#' Prepare CSV
#'
#' Will prepare the data for CSV download
#' @param data Tibble to be stored
#' @param file A file location to write to
#' @return The file location
prepare_csv = function(data, file = tempfile(fileext = ".csv")) {
  readr::write_csv(data, file)
  return(file)
}

format_data_for_download = function(data, question_data) {
  data %>% dplyr::mutate(
    Policy = tidyr::replace_na(.data$Policy, "baseline"),
    csv_data = purrr::map(.data$Response, ~bind_questions(question_data, .x))
  ) %>%
    dplyr::select(-.data$Response) %>%
    tidyr::unnest(
      .data$csv_data
    )
}

prepare_pdf = function(data, question_data, model, scoring_funcs, template = system.file("assets", "templates", "pdf_template.Rmd", package = "diagramNAT"), file = tempfile(fileext = ".pdf")) {
    download_data = format_data_for_download(format_model_table(data, model, scoring_funcs, TRUE), question_data)
    vis_data = format_vis_data(data, model, scoring_funcs)
    rmarkdown::render(
      template,
      output_file = file,
      params = list(
        question_data = question_data,
        vis_data = vis_data,
        download_data = download_data
      )
    )
    return(file)
}

#' Find diff
#'
#' Find the change in responses for all policies for a given model
#' @param download_data Data formatted ready for download see \code{format_data_for_download}
#' @param model_name Character, model name for which to compare responses on policies
#' @return a named list of the policy response differences
#' @export
find_diff = function(download_data, model_name) {
  base = download_data %>% dplyr::filter(.data$Model == model_name & .data$Policy == "baseline")
  policies = setdiff(download_data$Policy, "baseline")
  purrr::map(policies, function(p) {
    df = download_data %>% dplyr::filter(.data$Model == model_name & .data$Policy == p)
    df[which(df$Response != base$Response),]
    # comp = compare::compare(base, df, allowAll = TRUE)
  }) %>% setNames(policies)
}


bind_questions = function(questions, responses) {
  q_names = unique(purrr::map_chr(questions, 'node'))
  purrr::map_dfr(seq_along(q_names), function(i){
    node = q_names[i]
    question = Filter(function(x) x$node == node, questions)
    response = responses[[node]]
    if(length(question) > 1){
      bind_multiple_question(question, response)
    } else {
      bind_single_question(question, response)
    }
  })
}

bind_multiple_question = function(question, response) {
  purrr::map_dfr(seq_along(question), ~bind_single_question(question[.x], response[[.x]]))
}

bind_single_question = function(question, response) {
  q_text = switch (question[[1]]$type,
                   "grouped slider" = question[[1]]$text,
                   "non-numeric slider" = question[[1]]$text,
                   "multiple choice" = question[[1]]$text,
                   "slider" = question[[1]]$text
  )
  q_detail = switch (question[[1]]$type,
                     "grouped slider" = question[[1]]$detail,
                     "non-numeric slider" = NA,
                     "slider" = NA,
                     "multiple choice" = question[[1]]$detail
  )
  dplyr::bind_cols("Question" = q_text, "Detail" = q_detail, "Response" = as.character(response))
}

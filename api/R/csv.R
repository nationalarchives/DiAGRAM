# @param parsed raw data parsed from json (list)
build_csv = function(parsed) {
  res = extract_responses(parsed)
  adv_flag = advanced_flags(parsed)
  q = get_questions_tibble()
  scores = extract_scores(parsed[!adv_flag])
  purrr::map_dfr(res[!adv_flag], ~{
    responses = join_questions_responses(.x, questions = q)
    table = build_csv_data(responses)
    dplyr::bind_cols(
      name = .x$model_name, scenario = .x$scenario,
      notes = .x$notes,
      table
    ) %>%
      dplyr::left_join(scores)
  })
}

# @param responses a set of responses (tibble) of class
# simple_responses
build_csv_data = function(responses) {
  purrr::pmap_dfr(responses, csv_part)
}

csv_part = function(question, response, node) {
  q_length = length(question)
  r_length = length(response)
  is_complex = is.list(question) && length(question) > 1
  if (is_complex) {
    if (q_length != r_length) {
      stop("List question length not equal to list response length")
    }
    qs = purrr::map2_dfr(question, response, ~{
      tibble::tibble(
        topic = node, question = .x,
        part = NA_character_, response = as.character(.y)
      )
    })
  } else {
    if (r_length > q_length) {
      qs = tibble::tibble(question = question) %>%
        tidyr::separate(
          .data$question,
          into = c("question", paste0("part_", seq_len(r_length))),
          sep = "\n\n- "
        ) %>%
        tidyr::pivot_longer(
          -.data$question,
          names_to = "part_index",
          values_to = "part"
        )
        qs = dplyr::bind_cols(
          topic = node, qs, response = as.character(response)
        ) %>%
        dplyr::select(-.data$part_index)
    } else {
      qs = tibble::tibble(
        topic = node,
        question = question,
        part = NA_character_,
        response = as.character(response)
      )
    }
  }
  qs
}

# @param parsed_json raw data as parsed from json (list)
extract_scores = function(parsed_json) {
  purrr::map_dfr(parsed_json, ~{
    tibble::tibble(
      name = .x$model_name,
      scenario = .x$scenario,
      intellectual_control = .x$intellectual_control,
      renderability = .x$renderability
    )
  })
}

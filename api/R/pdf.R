# Write a pdf report to a temporary file
# @return path to temporary pdf file
write_temp_pdf = function(data) {
  td = tempdir()
  temp_base = file.path(td, "report.Rmd")
  temp_section = file.path(td, "pdf_section.Rmd")
  file.copy(
    system.file(
      "assets", "templates", "pdf_template.Rmd", package = "diagramAPI",
      mustWork = TRUE
    ),
    temp_base, overwrite = TRUE
  )
  file.copy(
    system.file(
      "assets", "templates", "pdf_section.Rmd", package = "diagramAPI",
      mustWork = TRUE
    ),
    temp_section, overwrite = TRUE
  )
  pdf_file = prepare_pdf(data, td, temp_base)
  pdf_file
}

# Build a report pdf from package templates
prepare_pdf = function(data, dir, template) {
  target_file = tempfile(tmpdir = dir, fileext = ".pdf")
  rmarkdown::render(
    template,
    output_file = target_file,
    params = list(req = data)
  )
  target_file
}

# Get a pkg resource for building pdfs
pkg_resource = function(file) {
  system.file(
    "rmarkdown", "templates",
    "nata-report", "resources",
    file, package = "diagramAPI",
    mustWork = TRUE
  )
}

#' Custom format for NATA reports, based on \code{bookdown::pdf_book}
#'
#' Used in the template under the inst/rmarkdown/skeleton.
#' @export
pdf_report = function() {
  # constants for our template
  keep_tex = TRUE
  latex_engine = "xelatex"
  toc = FALSE
  toc_depth = 2
  number_sections = FALSE
  extra_pandoc_args = NULL

  # Locate template
  template = pkg_resource("template.tex")

  # Locate path to fonts
  font_path = system.file(
    "rmarkdown", "templates",
    "nata-report", "resources",
    package = "diagramAPI",
    mustWork = TRUE
  )
  tna_logo = pkg_resource("tna-logo.png")

  pandoc_args = c(
    rmarkdown::pandoc_variable_arg("font_path", font_path),
    rmarkdown::pandoc_variable_arg("tna_logo", tna_logo),
    extra_pandoc_args
  )

  bookdown::pdf_book(
    template = template,
    keep_tex = keep_tex,
    toc = toc,
    toc_depth = toc_depth,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    number_sections = number_sections
  )
}

#' Build a single row of the table for pdf
#'
#' Collapses questions and responses with multiple parts
#' and structure to a single consistent tibble chunk
#'
#' @param question either a list or a character vector
#' @param response either a list of equivalent shape to question, or a vector
#' @param node character label of node corresponding to question
pdf_table_part = function(question, response, node) {
  q_length = length(question)
  r_length = length(response)
  is_complex = is.list(question) && length(question) > 1
  if (is_complex) {
    if (q_length != r_length) {
      stop("List question length not equal to list response length")
    }
    res = purrr::map2_dfr(question, response, ~{
      tibble::tibble(Topic = node, Question = .x, Response = as.character(.y))
    })
  } else {
    if (r_length > q_length) {
      response = paste0("- ", response, collapse = "\n\n")
    }
    res = tibble::tibble(
      Topic = node,
      Question = question,
      Response = response
    )
  }
  res
}

#' Build a nice tibble for pdf
#'
#' Builds a nicely formatted tibble of questions and responses
#' for a model (not scenario). Requires full set of simple_response
#' nodes.
#'
#' @param obj is a tibble full of questions and responses, typically
#' from \code{diagramAPI::join_questions_responses}
#' @export
pdf_table = function(obj) {
  purrr::pmap_dfr(obj, pdf_table_part)
}

# convenience wrappers to load question text from package
get_questions = function() {
  f = system.file(
    "extdata", "config", "pdf_questions.yml",
    package = "diagramAPI", mustWork = TRUE
  )
  yaml::read_yaml(f)[.user_nodes]
}

#' Get a formatted set of questions
#'
#' Convenience function exported for building pdfs that
#' grabs the question and answer text used in the front end
#' and formats it as tibble.
#'
#' @export
get_questions_tibble = function() {
  q = get_questions()
  tibble::tibble(
    node = names(q),
    question = q
  )
}

#' Bind questions to repsonses
#'
#' Generic (for simple_responses and scenario_diff) for
#' building a single tibble that contains nicely formatted
#' questions and responses for the pdfs.
#'
#' @param questions a tibble of questions, see \code{get_questions_tibble()}
#' @param responses a tibble of responses for a model/scenario
#' @param ... arguments passed to method, currently unused
#' @export
join_questions_responses = function(questions, responses, ...) {
  UseMethod("join_questions_responses", responses)
}

#' @importFrom rlang .data
#' @method join_questions_responses simple_responses
#' @export
join_questions_responses.simple_responses = function(questions, responses, ...) { # nolint
  questions %>%
    dplyr::left_join(responses$data, by = "node") %>%
    dplyr::select(.data$node, .data$question, .data$response) %>%
    dplyr::mutate(
      node = .user_node_map[.data$node]
    )
}

#' @method join_questions_responses scenario_diff
#' @export
join_questions_responses.scenario_diff = function(questions, responses, ...) { # nolint
  dplyr::right_join(questions, responses$data, by = "node")
}

#' Get the base model
#'
#' From a list of responses, return the one which represents a base model
#' @param responses, a whole group of responses
#' @export
get_base_model = function(responses) {
  purrr::keep(responses, ~.x$scenario == "Base Model")[[1]]
}

# from a list of responses, return those which represents a scenario
# @param responses, a whole group of responses
get_policies = function(responses) {
  purrr::discard(responses, ~.x$scenario == "Base Model")
}

#' Calculate model/scenario diff
#'
#' Calculate the difference between each scenario and the base-model
#' in a grouped set of responses with the same base-model $name.
#' Exported for pdf.
#'
#' @param responses, a whole group of responses that come from the same model name
#' @export
find_scenario_diffs = function(responses) {
  # get the base model
  base = get_base_model(responses)
  policies = get_policies(responses)
  scenario_names = purrr::map(policies, "scenario")
  res = purrr::map(policies, scenario_diff_one, base_model = base)
  names(res) = scenario_names
  res
}

# find the differences of a scenario from it's base-model
scenario_diff_one = function(base_model, scenario) {
  UseMethod("scenario_diff_one", base_model)
}

#' @method scenario_diff_one simple_responses
#' @export
scenario_diff_one.simple_responses = function(base_model, scenario) { # nolint
  UseMethod("scenario_diff_one.simple_responses", scenario)
}

#' @method scenario_diff_one.simple_responses simple_responses
#' @export
scenario_diff_one.simple_responses.simple_responses = function(base_model, scenario) { # nolint
  res = scenario$data %>%
    dplyr::anti_join(base_model$data, by = c("node", "response")) %>%
    dplyr::rename(scenario = .data$response) %>%
    dplyr::left_join(base_model$data, by = "node") %>%
    dplyr::rename(base_model = .data$response)

  out = list(
    data = res,
    name = scenario$scenario,
    notes = scenario$notes
  )
  class(out) = c("scenario_diff", class(out))
  out
}

diff_table_part = function(question, scenario, base_model, node) {
  q_length = length(question)
  r_length = length(scenario)
  is_complex = is.list(question) & length(question) > 1
  if (is_complex) {
    if (! q_length == r_length) {
      stop("List question length not equal to list response length")
    }
    res = purrr::pmap_dfr(c(question, scenario, base_model), ~{
      tibble::tibble(
        Topic = node, Question = .x,
        "Scenario Response" = as.character(.y),
        "Base Model Response" = as.character(.z)
      )
    })
  } else {
    if (r_length > q_length) {
      scenario = paste0("- ", scenario, collapse = "\n")
      base_model = paste0("- ", base_model, collapse = "\n")
    }
    res = tibble::tibble(
      Topic = node,
      Question = question,
      "Scenario Response" = scenario,
      "Base Model Response" = base_model
    )
  }
  res
}

#' Build a table of diffs
#'
#' A table which builds a table showing only
#' the responses to questions that are different
#' to the base model.
#'
#' @param obj a collection of models/policies (full responses),
#' each with the same $name
#' @export
diff_table = function(obj) {
  purrr::pmap_dfr(obj, diff_table_part)
}

#' Group similar models
#'
#' Group sets of responses by their base-model $name
#'
#' @param obj a full collection of models/policies (responses)
#' @export
group_objects_by_model = function(obj) {
  names = get_model_names(obj)
  models = list()
  for (i in unique(names)) {
    models[[i]] = purrr::keep(obj, ~.x$model_name == i)
  }
  models
}

get_model_names = function(obj) {
  purrr::map_chr(obj, "model_name")
}

#' Get scenario names
#'
#' Convenience function exported for pdf template
#' to grab the scenario names from a set of models/policies
#' (responses)
#'
#' @inheritParams group_objects_by_model
#' @export
get_scenario_names = function(obj) {
  purrr::map_chr(obj, "scenario")
}

#' Get comment data
#'
#' Convenience function exported for pdf template
#' to grab the scenario names from a set of models/policies
#' (responses)
#'
#' @inheritParams group_objects_by_model
#' @export
get_comments = function(obj) {
  purrr::map_chr(obj, "notes")
}

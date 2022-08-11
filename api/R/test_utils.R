#' Test Req
#'
#' Utility for creating fake requests to the model_score endpoint.
#' Used for testing functions related to processing model responses
#' and generating scores.
#'
#' @param type character reflecting which type of responses to generate.
#' @return an environment mimicking the necessary parts of Rook request
#' object that are required for testing purposes.
# curl -X POST -H "Content-Type: application/json" -d
# @./inst/extdata/test_data/simple_model.json http://localhost:4341/
test_req_score = function(
  type = c(
    "simple", "advanced", "simple_scenario",
    "mixed", "mixed_simple", "simple_single"
  )
) {
  type = match.arg(type)
  file = system.file("extdata", "test_data", "score_requests", paste0(type, "_model.json"),
    package = "diagramAPI", mustWork = TRUE)
  obj = readLines(file)
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json" # nolint
  req$postBody = paste(obj, collapse = "") # nolint
  req
}

test_req_csv = function(type = c("mixed", "mixed_simple")) {
  type = match.arg(type)
  file = system.file(
    "extdata", "test_data", "csv_requests",
    paste0(type, "_model.json"),
    package = "diagramAPI", mustWork = TRUE
  )
  obj = readLines(file)
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json" # nolint
  req$postBody = paste(obj, collapse = "") # nolint
  req
}

test_req_plot = function(type = c("simple", "simple_scenario", "advanced", "simple_scenario2")) {
  type = match.arg(type)
  file = system.file(
    "extdata", "test_data", "plot_requests",
    paste0(type, "_model.json"), package = "diagramAPI", mustWork = TRUE
  )
  obj = readLines(file)
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json" # nolint
  req$postBody = paste(obj, collapse = "") # nolint
  req
}

test_req_pdf = function(type = "advanced") {
  type = match.arg(type)
  file = system.file(
    "extdata", "test_data", "pdf_requests",
    paste0(type, "_model.json"), package = "diagramAPI", mustWork = TRUE
  )
  obj = readLines(file)
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json" # nolint
  req$postBody = paste(obj, collapse = "") # nolint
  req
}

# set up a fake request for previous models that generated
# some sort of error or unexpected behaviour
# argument is filename stub in inst/test_data/error_generators
test_previous_error = function(type = "score_error") {
  file = system.file(
    "extdata", "test_data", "error_generators",
    paste0(type, ".json"), package = "diagramAPI", mustWork = TRUE
  )
  obj = readLines(file)
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json" # nolint
  req$postBody = paste(obj, collapse = "") # nolint
  req
}

#' Score model
#'
#' Currently method stub, used to obtain the renderability and intellectual
#' control scores for a given set of model inputs.
#'
#' @eval param_req()
#' @param model A network model, likely of class bn.fit
#' @return List with the two scores and the nodes information
#' @include model.R
#' @export
score_model = function(req, model = load_default_model()) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  if (is_array(parsed_json)) {
    if (length(parsed_json) > 1) {
      stop("Expected single object, got array.")
    } else {
      parsed_json = parsed_json[[1]]
    }
  }
  responses = extract_responses(parsed_json)
  score = with_log_handle(score_model_(responses, model))
  serialise_r(score)
}

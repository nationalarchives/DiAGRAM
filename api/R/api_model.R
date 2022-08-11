#' Score model
#'
#' Currently method stub, used to obtain the renderability and
#' intellectual control scores for a given set of model inputs.
#'
#' @param req a request object satisfying the Rook interface.
#' @param model a network model, likely of class bn.fit
#' @return list with the two scores and the nodes information
#' @export
#' @include model.R
score_model = function(req, model = load_default_model()) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  if (is_array(parsed_json)) {
    stop("Expected single object, got array.")
  }
  responses = extract_responses(parsed_json)
  score = with_log_handle(score_model_(responses, model))
  # unbox(score)
  score
}

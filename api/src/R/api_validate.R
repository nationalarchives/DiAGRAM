#' Validate JSON
#'
#' Validate whether the JSON data posted fits with the restrictions of expected
#' data structure for DiAGRAM
#'
#' @eval param_req()
#' @export
validate_json = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  valid = validate(parsed_json)
  serialise_r(list("status" = valid))
}

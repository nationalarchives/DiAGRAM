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
  if (!is_array(parsed_json)) {
    # if we only have a single entity box it into an array
    parsed_json = list(parsed_json)
  }
  valid = validate(parsed_json)
  list(
    "status" = valid
  )
}

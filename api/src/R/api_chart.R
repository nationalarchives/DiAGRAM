#' Generate plot from request object
#'
#' @eval param_req()
#' @export
plot = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  tmp_file = with_log_handle(write_temp_png(parsed_json))
  body = readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
  serialise_png(body)
}

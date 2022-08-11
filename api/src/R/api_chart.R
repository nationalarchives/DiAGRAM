#' Generate plots
#'
#' API endpoints
#' @param req rook request object
#' @export
plot_output = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  tmp_file = with_log_handle(write_temp_png(parsed_json))
  readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
}

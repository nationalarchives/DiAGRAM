#' @rdname csv_report
#' @export
# see https://www.rplumber.io/articles/rendering-output.html#customizing-image-serializers
pdf_report_gen = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  adv_flag = advanced_flags(parsed_json)
  tmp_file = with_log_handle(
    write_temp_pdf(extract_responses(parsed_json[!adv_flag]))
  )
  readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
}


#' Generate reports
#'
#' API endpoints
#' @param req rook request object
#' @export
csv_report = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  csv_data = build_csv(parsed_json)
  csv_data
}

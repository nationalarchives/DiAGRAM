#' Generate PDF report from request object
#'
#' @rdname csv_report
#' @eval param_req()
#' @export
make_pdf_report = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  adv_flag = advanced_flags(parsed_json)
  tmp_file = with_log_handle(
    write_temp_pdf(extract_responses(parsed_json[!adv_flag]))
  )
  on.exit(unlink(tmp_file))
  body = readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
  serialise_pdf(body)
}


#' Generate CSV report from request object
#'
#' @eval param_req()
#' @export
make_csv_report = function(req) {
  ensure_json_input(req)
  parsed_json = jsonlite::parse_json(req$postBody)
  csv_data = build_csv(parsed_json)
  serialise_csv(csv_data)
}

#' Convert a test input from extdata/test_data into the format which they would
#' be received by a Lambda function sitting behind API Gateway
#'
#' Requests coming via AWS' API Gateway adhere to the payload format outlined
#' \href{here}{https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-lambda.html}
#'
#' @param test_dir The directory in extdata/test_data which holds the targetted
#' input file
#' @param test_file The name of the file to read from
test_data_to_gateway_payload = function(
  test_dir = c("csv_requests", "error_generators", "pdf_requests", "plot_requests", "score_requests"),
  test_file = list.files(system.file("extdata", "test_data", test_dir, package = "diagramLambda"))
) {

  test_file = match.arg(test_file)
  test_file_path = system.file("extdata", "test_data", test_dir, test_file,
    package = "diagramLambda", mustWork = TRUE)

  body = paste(readLines(test_file_path), collapse = "")
  gateway_payload = body_to_gateway_payload(body)
}

#' Insert a request body into AWS' API Gateway's payload format
#'
#' @param body String representing request body
body_to_gateway_payload = function(body) {
  api_gateway_format_path = system.file("extdata", "test_data", "api_gateway_format.json",
    package = "diagramLambda", mustWork = TRUE)
  api_gateway_payload = jsonlite::read_json(api_gateway_format_path)
  api_gateway_payload$body = body
  api_gateway_payload
}

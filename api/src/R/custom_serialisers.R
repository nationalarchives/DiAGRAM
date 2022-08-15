#' Serialise R object to pass as response to API Gateway
#'
#' Responses posted from AWS Lambda to AWS API Gateway must adhere to the
#' format outlined
#' \href{here}{https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-lambda.html#http-api-develop-integrations-lambda.response}
#'
#' @param body The content to be serialised
#' @param status_code The HTTP response status code to return
#' @param headers Additional content headers to append
serialise_r = function(body,
                       status_code = 200L,
                       content_type = "application/json",
                       headers = list()) {
  response_list = list(
    "body" = lambdr::as_stringified_json(body),
    "isBase64Encoded" = FALSE,
    "statusCode" = 200L
  )
  headers[["Content-Type"]] = content_type
  response_list[["headers"]] = headers

  response = lambdr::as_stringified_json(response_list)

  return(response)
}

#' Serialise tibble to be returned as raw CSV via API Gateway
#' 
#' @inheritParams serialise_r
serialise_csv = function(body,
                         status_code = 200L,
                         headers = list()) {

  response_list = list(
    "body" = readr::format_csv(body),
    "isBase64Encoded" = FALSE,
    "statusCode" = 200L
  )
  headers[["Content-Type"]] = "text/csv; charset=UTF-8"
  response_list[["headers"]] = headers

  response = lambdr::as_stringified_json(response_list)

  return(response)
}

#' Serialise binary data to pass as response to API Gateway
#'
#' @inheritParams serialise_r
#' @param content_type The type of binary content to be returned
serialise_binary_data = function(body,
                                 content_type = c("application/pdf", "image/png"),
                                 status_code = 200L,
                                 headers = list()) {
  content_type = match.arg(content_type)

  response_list = list(
    "body" = jsonlite::base64_enc(body),
    "isBase64Encoded" = TRUE,
    "statusCode" = status_code
  )
  headers[["Content-Type"]] = content_type
  response_list[["headers"]] = headers

  response = lambdr:::mark_as_already_serialised(
    lambdr::as_stringified_json(
      response_list
    )
  )

  return(response)
}

#' Serialise PDF document to pass as response to API Gateway
#'
#' @inheritParams serialise_r
serialise_pdf = function(body,
                         status_code = 200L,
                         headers = list()) {
  response = serialise_binary_data(body, content_type = "application/pdf", status_code = status_code, headers = headers)

  return(response)
}

#' Serialise PNG image to pass as response to API Gateway
#'
#' @inheritParams serialise_r
serialise_png = function(body,
                         status_code = 200L,
                         headers = list()) {
  response = serialise_binary_data(body, content_type = "image/png", status_code = status_code, headers = headers)

  return(response)
}

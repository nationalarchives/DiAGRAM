#' Transform a request as received by API Gateway into the Rook format used by
#' the earlier {diagramAPI} package.
#'
#' Adhering to the format used by {diagramAPI} allows reuse of all of
#' {diagramAPI}'s deserialisation routines and JSON wrangling.
#'
#' @eval param_event_content()
gateway_payload_to_rook = function(event_content) {
  parsed_event = jsonlite::fromJSON(event_content)
  base64_encoded = parsed_event$isBase64Encoded

  body = parsed_event$body
  if (base64_encoded) {
    body = rawToChar(jsonlite::base64_dec(body))
  }

  # Create a request object as used in {diagramAPI}
  req = new.env()
  req$HTTP_CONTENT_TYPE = "application/json"
  req$postBody = body
  # Extract route, removing a trailing slash is there is one
  req$route = sub("/$", "", parsed_event$pathParameters$proxy)
  req
}

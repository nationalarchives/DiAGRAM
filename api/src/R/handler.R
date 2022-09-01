#' Route requests from API Gateway
#'
#' There are a few different approaches to emulating multiple endpoints with
#' AWS Lambda and AWS API Gateway.
#' \href{This StackOverflow question}{https://stackoverflow.com/questions/41425511/aws-api-gateway-lambda-multiple-endpoint-functions-vs-single-endpoint}
#' provides a good overview of some of the pros and cons of the different
#' possible approaches.
#'
#' @eval param_event_content()
#' @param context Metadata about the received event
#'
#' @export
handler = function(event_content, context) {

  request = gateway_payload_to_rook(event_content)

  out = switch(request$route,
    "chart/plot" = diagramLambda::plot(request),
    "model/score" = diagramLambda::score_model(request),
    "report/pdf" = diagramLambda::make_pdf_report(request),
    "report/csv" = diagramLambda::make_csv_report(request),
    "test/is_alive" = diagramLambda::is_alive(),
    "env/dev" = diagramLambda::is_dev()
  )

  return(out)
}

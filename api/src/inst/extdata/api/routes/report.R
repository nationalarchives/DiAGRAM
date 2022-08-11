#' @post /pdf
#' @serializer contentType list(type="application/pdf")
# see https://www.rplumber.io/articles/rendering-output.html#customizing-image-serializers
diagramAPI::pdf_report_gen

#' @post /csv
#' @serializer csv
diagramAPI::csv_report

#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom shiny restoreInput
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
customSliderTripleInput <- function(inputId, state = c(20,20,60), label, content) {
  default = list(
    state = state, label = label, content = content
  )
  main = reactR::createReactShinyInput(
    inputId,
    "customSliderTriple",
    htmltools::htmlDependency(
      name = "customSliderTriple-input",
      version = "1.0.0",
      src = "www/customSlider/customSliders",
      package = "diagramNAT",
      script = "multi.js"
    ),
    default,
    list(),
    htmltools::tags$span
  )
  main
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @export
updateCustomSliderTripleInput <- function(session, inputId, value, configuration = NULL) {
  message <- list(value = value)
  if (!is.null(configuration)) message$configuration <- configuration
  session$sendInputMessage(inputId, message);
}

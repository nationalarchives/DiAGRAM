#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom shiny restoreInput
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
customSliderInput <- function(inputId, state = 10, label, content) {
  default = list(
    state = state, label = label, content = content
  )
  reactR::createReactShinyInput(
    inputId,
    "customSlider",
    htmltools::htmlDependency(
      name = "customSlider-input",
      version = "1.0.0",
      src = "www/customSlider/customSliders",
      package = "diagramNAT",
      script = "single.js"
    ),
    default,
    list(),
    htmltools::tags$span
  )
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @export
updateCustomSliderInput <- function(session, inputId, value, configuration = NULL) {
  message <- list(value = value)
  if (!is.null(configuration)) message$configuration <- configuration
  session$sendInputMessage(inputId, message);
}

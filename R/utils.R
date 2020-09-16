#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Graphical Independence Network
#' 
#' Function to check if model is of class "grain",
#' to replace the now removed gRain::is.grain
#' 
#' @param obj Object to test for class "grain"
#' @export
is_grain = function(obj) {
  inherits(obj, "grain")
}

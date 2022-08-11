#' Is alive
#'
#' Simple endpoint that can be used to check whether a router
#' is alive.
#'
#' @export
is_alive = function() {
  unbox(list("alive" = TRUE))
}

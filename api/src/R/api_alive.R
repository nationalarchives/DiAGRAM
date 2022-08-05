#' A simple endpoint that can be used to check whether an API is alive.
#'
#' @export
is_alive = function() {
  serialise_r(list("alive" = TRUE))
}

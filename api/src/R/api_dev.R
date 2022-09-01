#' A simple endpoint to confirm that this is deployed to the dev environment
#'
#' @export
is_dev = function() {
  serialise_r("You are working on the dev environment")
}

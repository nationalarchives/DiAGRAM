#' Get API Routes
#'
#' Convenience function.
#' @param path Additional path added to extdata/api/routes/path
#' @export
get_internal_routes = function(path = ".") {
  system.file("extdata", "api", "routes", path,
              package = utils::packageName(),
              mustWork = TRUE)
}

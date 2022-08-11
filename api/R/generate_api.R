ensure_slash = function(string) {
  assertthat::assert_that(length(string) == 1)
  has_slash = grepl("^/", string)
  if (has_slash) string else paste0("/", string)
}

#' Mount plumber definition
#'
#' Mount a new set of routes to a plumber object from a file.
#'
#' @param pr The host Plumber router.
#' @param endpoint route endpoint
#' @param file Path to file to plumb
#' @param ... arguments passed to plumber::pr
#' @return a plumber object with additional mounted routes
add_plumber_definition = function(pr, endpoint, file, ...) {
  router = plumber::pr(file = file, ...)
  plumber::pr_mount(pr = pr,
                    path = endpoint,
                    router = router
  )
}

#' create_routes: For a given directory, create a named vector of files and endpoints
#' @param dir directory containing end points
#' @rdname generate_api
#' @export
create_routes = function(dir) {
  routes = list.files(dir, recursive = TRUE, full.names = TRUE, pattern = "*\\.R$")
  add_default_route_names(routes, dir)
}

add_default_route_names = function(routes, dir) {
  names = stringr::str_remove(routes, pattern = dir)
  names = stringr::str_remove(names, pattern = "\\.R$")
  names(routes) = names
  routes
}

#' Generate APIs
#'
#' routes is typically created by create_routes
#' @param routes A list of file. Element names are the plumber endpoints
#' @param ... filters and/or envir args to pass to plumber::pr
#' @return A Plumber object
#' @export
generate_api = function(routes, ...) {
  endpoints = purrr::map_chr(names(routes), ensure_slash)
  purrr::reduce2(
    .x = endpoints, .y = routes,
    .f = add_plumber_definition, ...,
    .init =  plumber::pr(NULL)
  )
}

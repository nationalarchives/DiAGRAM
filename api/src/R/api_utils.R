ensure_json_input = function(req) {
  assertthat::assert_that(req$HTTP_CONTENT_TYPE == "application/json")
}

# box / unbox JSON
unbox = function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
box = function(x) jsonlite::toJSON(x)

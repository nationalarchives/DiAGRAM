validation_generator = function(class_name = NULL) {
  if (is.null(class_name)) stop("Missing class")
  function(res) {
    class(res) = class_name
    validate_node(res)
  }
}

.validate_node_funcs = sapply(
  .node_map,
  function(x) validation_generator(tolower(x))
)

#' Validate data on a single node
#'
#' @param res result object for a single node in the bayesian network
#' @export
validate_node = function(res) {
  check(res, options = get_option_val(res))
  UseMethod("validate_node")
}

#' @export
validate_node.default = function(res) { # nolint
  TRUE
}

validate_keys = function(obj) {
  condition = all(c(
      "model_name", "scenario", "notes",
      "intellectual_control", "renderability",
      "response", "advanced", "is_advanced"
    ) %in% names(obj))
  condition
}

validate_simple_keys = function(data) {
  all(names(data) %in% .reverse_user_node_map)
}

validate_advanced_keys = function(data) {
  all(names(data) %in% .reverse_node_map)
}

validate_simple_nodes = function(obj) {
  data = obj$response
  valid_keys = validate_simple_keys(data)
  valid_nodes = purrr::imap_lgl(data, ~{
    .validate_node_funcs[[.y]](.x)
  })
  all(c(
    valid_keys,
    valid_nodes
  ))
}

validate_advanced_nodes = function(obj) {
  data = obj$response
  valid_keys = validate_advanced_keys(data)
  valid_nodes = purrr::imap_lgl(data, ~{
    .validate_node_funcs[[.y]](.x)
  })
  all(c(
    valid_keys,
    valid_nodes
  ))
}

validate_object = function(obj) {
  obj = unpack_json(obj)
  valid_keys = validate_keys(obj)
  valid_simple_nodes = validate_simple_nodes(obj)
  valid_advanced_nodes = validate_advanced_nodes(obj)
  all(c(
    valid_keys,
    valid_simple_nodes,
    valid_advanced_nodes
  ))
}

validate = function(parsed_json) {
  valid = purrr::map_lgl(parsed_json, validate_object)
  all(valid)
}

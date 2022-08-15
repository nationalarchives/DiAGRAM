#' To numeric generator
#'
#' Utility function for creating to_numeric functions for each node in a
#' response.
#'
#' @param class_name class attribute to give a single response node that will
#' be passed to the generic to_numeric
#' @return a function that takes a single response node, attaches the
#' class_name as a class and calls to_numeric
to_numeric_generator = function(class_name = NULL) {
  if (is.null(class_name)) stop("Missing class")
  function(res) {
    class(res) = class_name
    to_numeric(res)
  }
}

#' To numeric funcs
#'
#' List of functions that will convert each response from a simple model to a
#' numeric one according to the fixed weightings attributed to each response.
#' List is named according to the node in the model that it represents.
.to_numeric_funcs = sapply(
  c("Technical_Skills", "Physical_Disaster", "System_Security", "Checksum",
    "Digital_Object", "Storage_Medium", "Info_Management", "Op_Environment",
    "Rep_and_Refresh"
    ),
  function(x) to_numeric_generator(tolower(x)),
  USE.NAMES = TRUE
)

#' To numeric
#'
#' Generic function for calling to_numeric on response nodes. First calls the
#' generic check() for assertion checking the received response value.
#'
#' @param res response (typically numeric or character vector) for a single node
#' @return a numeric vector
#' @export
to_numeric = function(res) {
  check(res, options = get_option_val(res))
  UseMethod("to_numeric")
}

#' @export
to_numeric.default = function(res) { # nolint
  as.numeric(res)
}

#' @param res character vector of length 10
#' @export
to_numeric.technical_skills = function(res) { # nolint
  option_val = get_option_val(res)
  res = sum(option_val[res])
  NextMethod()
}

#' @param res length 1 character vector, see get_option_val.physical_disaster
#' for allowed values
#' @export
to_numeric.physical_disaster = function(res) { # nolint
  option_val = get_option_val(res)
  res = option_val[res]
  NextMethod()
}

#' @param res a named list of 4 responses, each response is length 1.
#' @export
to_numeric.system_security = function(res) { # nolint
  option_val = get_option_val(res)
  value = purrr::map_dbl(1:4, ~option_val[[.x]][res[[.x]]])
  res = sum(value)
  NextMethod()
}

#' @param res numeric vector of length 3
#' @export
to_numeric.checksum = function(res) { # nolint
  NextMethod()
}

#' @export
to_numeric.digital_object = to_numeric.checksum # nolint

#' @export
to_numeric.storage_medium = to_numeric.checksum # nolint

#' @param res a named list of 3 responses with the following length character
#' vectors c(1, 1, 2)
#' @export
to_numeric.info_management = function(res) { # nolint
  option_val = get_option_val(res)
  res = option_val[[1]][res[[1]]] +
    option_val[[2]][res[[2]]] +
    sum(option_val[[3]][res[[3]]])
  NextMethod()
}

#' Max score to either question (100, Yes) gives a final score of 100, if (x,
#' No) return x
#'
#' @param res a named list of 2 responses: structure list(numeric(1),
#' character(1)).
#' @export
to_numeric.op_environment = function(res) { # nolint
  if (res[[1]] == 100 || res[[2]] == "Yes") {
    value = 100
  } else {
    value = res[[1]]
  }
  res = value
  NextMethod()
}

#' @param res a named list of 2 responses with length 1 numeric vectors.
#' @export
to_numeric.rep_and_refresh = function(res) { # nolint
  res = res[[1]] * res[[2]] / 100
  NextMethod()
}

#' Apply the to numeric conversion to all responses
#' in a set of simple responses. Does this by mapping over
#' .to_numeric_funcs list
#'
#' @param res a tibble with class simple_responses
#' @return a numeric vector of length 9
#' @export
to_numeric.simple_responses = function(res) { # nolint
  out = purrr::imap(res$data$response, ~{
    .to_numeric_funcs[[.y]](.x)
  })
  class(out) = unique(c("simple", class(out)))
  out
}

#' @param res a list object with a simple part and the advanced part
#' @export
to_numeric.advanced_responses = function(res) { # nolint
  simple = to_numeric(res$data$simple)
  # want to leave advanced nodes untouched as already in probability format,
  # just pass them on
  out = list(
    simple = simple, advanced = res$data$advanced
  )
  class(out) = unique(c("advanced", class(out)))
  return(out)
}

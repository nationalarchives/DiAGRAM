#' Model node to probability table names map
#'
#' each node in the network has fixed outcomes which are attributed
#' probabilities. This object allows a map between the node names
#' and the names of the probability tables in the network object.
.model_prob_names = list(
  Technical_Skills = c("Good", "Poor"),
  System_Security = c("Good", "Poor"),
  Checksum = c("Yes", "Self_Generated", "No"),
  Info_Management = c("Sufficient", "Insufficient"),
  Digital_Object = c("Born_digital", "Digitised", "Surrogate"),
  Storage_Medium = c("A", "B", "C"),
  Rep_and_Refresh = c("Good", "Poor"),
  Op_Environment = c("Yes", "No"),
  Physical_Disaster = c("Yes", "No")
)

#' Unpack json
#'
#' Convert unnamed lists in a list of lists to a vector.
#' This ensures that JSON arrays are converted to vectors rather than
#' lists. Operates recursively across the whole list of lists.
#'
#' @param res a list (of lists)
#' @return a list (of lists) where unnamed lists have become vectors.
unpack_json = function(res) {
  purrr::map(res, function(x) {
    if (is.null(names(x))) {
      unlist(x)
    } else {
      unpack_json(x)
    }
  })
}

# @param parsed_json an array of json objects
extract_simple_responses = function(parsed_json) {
  if (is_array(parsed_json)) {
    purrr::map(parsed_json, ~create_simple_responses(.x))
  } else {
    create_simple_responses(parsed_json)
  }
}

is_array = function(parsed_json) {
  is.null(names(parsed_json))
}

advanced_flags = function(parsed_json) {
  if (is_array(parsed_json)) {
    # implies array of json objects
    purrr::map_lgl(parsed_json, ~isTRUE(.x$is_advanced))
  } else {
    isTRUE(parsed_json$is_advanced)
  }
}

format_simple_responses = function(response) {
  res = tibble::tibble(node = names(response), response = response)
  res
}

# @param parsed_json a single json object
create_simple_responses = function(parsed_json) {
  res = list(
    data = format_simple_responses(unpack_json(parsed_json$response)),
    model_name = parsed_json$model_name,
    scenario = parsed_json$scenario,
    notes = parsed_json$notes,
    intellectual_control = parsed_json$intellectual_control,
    renderability = parsed_json$renderability
  )
  class(res) = c("simple_responses", class(res))
  res
}

extract_responses = function(parsed_json) {
  flag = advanced_flags(parsed_json)
  if (is_array(parsed_json)) {
    c(
      extract_simple_responses(parsed_json[!flag]),
      extract_advanced_responses(parsed_json[flag])
    )
  } else {
    if (flag) {
      extract_advanced_responses(parsed_json)
    } else {
      extract_simple_responses(parsed_json)
    }
  }
}

extract_advanced_responses = function(parsed_json) {
  if (is_array(parsed_json)) {
    purrr::map(parsed_json, format_advanced_responses)
  } else {
    format_advanced_responses(parsed_json)
  }
}

# a full advanced model is composed of both the simple
# and advanced parts
format_advanced_responses = function(parsed_json) {
  simple = unpack_json(parsed_json$response)
  advanced = parsed_json$advanced
  overlapping_keys = intersect(names(simple), names(advanced))

  simple_part = simple[setdiff(names(simple), overlapping_keys)]
  simple_part = format_simple_responses(simple_part)
  class(simple_part) = unique(c("simple_responses", class(simple_part)))

  advanced_tables = purrr::map(unpack_json(advanced), ~{
    tibble::as_tibble(.x)
  })

  res = list(
    data = list(
      "simple" = simple_part,
      "advanced" = advanced_tables
    ),
    model_name = parsed_json$model_name,
    scenario = parsed_json$scenario,
    notes = parsed_json$notes,
    intellectual_control = parsed_json$intellectual_control,
    renderability = parsed_json$renderability
  )
  class(res) = c("advanced_responses", class(res))
  res
}


response_type = function(obj) {
  if (inherits(obj, "advanced_responses")) return("advanced")
  else if (inherits(obj, "simple_responses")) return("simple")
  else stop("object does not look like a response.")
}

#' Numeric to Probability
#'
#' Convert a list of numeric responses to model form
#' questions into approriate probability tables for
#' executing the model.
#'
#' @param numeric_response a named list of numeric values
numeric_to_probability = function(numeric_response) {
  UseMethod("numeric_to_probability")
}

#' @method numeric_to_probability simple
#' @export
numeric_to_probability.simple = function(numeric_response) { # nolint
  out = purrr::imap(numeric_response, ~{
    numeric_to_probability_single(.x, .y)
  })
  class(out) = c("probability", "simple", class(out))
  out
}

#' @export
numeric_to_probability.advanced = function(numeric_response) { # nolint
  simple = numeric_to_probability(numeric_response$simple)
  advanced = purrr::imap(numeric_response$advanced, ~{
    numeric_to_probability_single_advanced(.x, .y)
  })
  # combine the two lists of table objects
  # intentionally stripping out the additional classes
  # from the component parts ( a side effect of c())
  out = c(
    simple, advanced
  )
  class(out) = c("probability", "advanced", class(out))
  out
}

numeric_to_probability_single_advanced = function(res, name) { # nolint
  assertthat::assert_that(name %in% .reverse_node_map)
  if (name %in% .user_nodes) {
    tab = as.table(unlist(res))
  } else {
    intermediate = tidyr::pivot_longer(
      res,
      -tidyselect::any_of(.reverse_node_map),
      names_to = name
    ) %>%
      dplyr::select(!!name, tidyselect::everything()) %>%
      dplyr::mutate_if(
        is.character,
        ~stringr::str_replace_all(.x, " ", "_")
      )
    tab = stats::xtabs(value ~., data = intermediate)
  }
  tab
}

#' Numeric to Probability
#'
#' Convert a single numeric response from the model form
#' answers to a probability.
#'
#' @param res numeric vector
#' @param name character, name of corresponding node in network
numeric_to_probability_single = function(res, name) {
  assertthat::assert_that(name %in% names(.model_prob_names))
  if (length(res) == 1) intermediate = c(res, 100 - res)
  else intermediate = res # implies length 3
  tab = intermediate / 100
  names(tab) = .model_prob_names[[name]]
  as.table(tab)
}

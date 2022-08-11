#' Generic functions for checking assertions on response nodes.
#'
#' @param x Typically single response node, may be a numeric or character
#' vector, or a list of such objects.
#' @param options named vector (or list) of allowed options for a response
check = function(x, ...) UseMethod("check")

check.default = function(x, ...) return(invisible(NULL)) # nolint

check.technical_skills = function(x, options, ...) { # nolint
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(length(x) == 10)
  assertthat::assert_that(all(x %in% names(options)))
}

check.physical_disaster = function(x, options, ...) { # nolint
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(length(x) == 1)
  assertthat::assert_that(x %in% names(options))
}

check.system_security = function(x, options, ...) { # nolint
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(length(x) == 4)
  assertthat::assert_that(all(purrr::map_int(x, length) == 1))
  assertthat::assert_that(all(purrr::map_lgl(x, is.character)))
  assertthat::assert_that(all(purrr::map2_lgl(
    x, options, ~{
      .x %in% names(.y)
    }
  )))
}

check.checksum = function(x, ...) { # nolint
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(length(x) == 3)
  assertthat::assert_that(sum(x) == 100)
}

check.digital_object = check.checksum # nolint
check.storage_medium = check.checksum # nolint

check.info_management = function(x, options, ...) { # nolint
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(length(x) == 3)
  assertthat::assert_that(all(purrr::map_int(x[1:2], length) == 1))
  assertthat::assert_that(length(x[[3]]) == 2)
  assertthat::assert_that(all(purrr::map_lgl(x, is.character)))
  assertthat::assert_that(all(purrr::map2_lgl(
    x, options, ~{
      all(.x %in% names(.y))
    }
  )))
}

check.op_environment = function(x, ...) { # nolint
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(length(x) == 2)
  assertthat::assert_that(all(purrr::map_int(x, length) == 1))
  assertthat::assert_that(is.numeric(x[[1]]))
  assertthat::assert_that(is.character(x[[2]]))
  assertthat::assert_that(x[[2]] %in% c("Yes", "No", "Not Applicable - we have copies offsite"))
}

check.rep_and_refresh = function(x, ...) { # nolint
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(length(x) == 2)
  assertthat::assert_that(all(purrr::map_int(x, length) == 1))
  assertthat::assert_that(all(purrr::map_lgl(x, is.numeric)))
}

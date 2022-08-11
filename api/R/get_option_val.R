#' Get option val
#'
#' Generic function for getting allowed options for
#' single node responses.
#'
#' @param res typically single response node, may be a
#' numeric or character vector, or a list of such objects.
get_option_val = function(res) UseMethod("get_option_val")

get_option_val.default = function(res) { # nolint
  invisible(NULL)
}

get_option_val.technical_skills = function(res) { # nolint
  c(None = 0L, Basic = 3L, Intermediate = 6L, Advanced = 10L)
}

get_option_val.physical_disaster = function(res) { # nolint
  c(`Very Low` = 0.05, Low = 0.5, Medium = 2, High = 5)
}

get_option_val.system_security = function(res) { # nolint
  list(
    c(
      "No" = 0, "Cyber Essentials" = 10,
      "Cyber Essentials Plus" = 40, "ISO 27001" = 70
    ),
    c(
      "No test" = 0, "Critical issues outstanding" = 5,
      "Severe issues outstanding" = 10,
      "None, or only minor issues outstanding" = 15
    ),
    c(
      "Not achieved" = 0, "Level 1" = 2,
      "Level 2" = 4, "Level 3" = 7,
      "Level 4" = 10
    ),
    c(
      "No" = 0, "Yes" = 5
    )
  )
}

get_option_val.info_management = function(res) { # nolint
  list(
    c(
      "Not achieved" = 0, "Level 1" = 7,
      "Level 2" = 14, "Level 3" = 21, "Level 4" = 28
    ),
    c(
      "Not achieved" = 0, "Level 1" = 8,
      "Level 2" = 16, "Level 3" = 16, "Level 4" = 16
    ),
    c(
      "Minimal awareness" = 0, "Awareness" = 7,
      "Basic" = 14, "Managed" = 21, "Optimized" = 28
    )
  )
}

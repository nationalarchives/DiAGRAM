cases = c("mixed", "mixed_simple")

purrr::walk(
  cases, ~{
    test_that(
      glue::glue("build csv: {.x} data"), {
        req = test_req_csv(.x)$postBody
        parsed = jsonlite::parse_json(req)
        res = build_csv(parsed)
        expect_s3_class(res, "data.frame")
        expect_named(
          res,
          c(
            "name", "scenario", "notes", "topic",
            "question", "part", "response",
            "intellectual_control", "renderability"
          )
        )
      }
    )
  }
)

valid = jsonlite::parse_json(test_req_csv("mixed")$postBody)

test_that("key validation", {
  # set up invalid versions with missing keys
  keys = c(
    "model_name", "scenario", "notes",
    "intellectual_control", "renderability",
    "response", "advanced", "is_advanced"
  )
  invalid_versions = replicate(length(keys), valid[1])
  invalid_versions = purrr::map2(keys, invalid_versions, ~{
    .y[setdiff(names(.y), .x)]
  })
  purrr::walk(
    invalid_versions,
    ~expect_false(validate_keys(.x))
  )
  purrr::walk(valid, ~expect_true(validate_keys(.x)))
})

test_that("simple node validation", {
  purrr::walk(valid, ~expect_true(validate_simple_nodes(unpack_json(.x))))
})

test_that("validation works", {
  parsed_json = jsonlite::parse_json(test_req_csv("mixed")$postBody)
  valid = validate(unpack_json(parsed_json))
  expect_true(valid)
})

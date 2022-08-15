simple_req = test_req_score("simple")
parsed_json = jsonlite::parse_json(simple_req$postBody)
responses = extract_responses(parsed_json)[[1]]


test_that("default model loads", {
  x = load_default_model()
  expect_s3_class(x, "bn.fit")
})

test_that("is_grain", {
  x = load_default_model()
  expect_false(is_grain(x))
  x = ensure_grain_model(x)
  expect_true(is_grain(x))
})

test_that("ensure grain model", {
  x = load_default_model()
  x = ensure_grain_model(x)
  expect_s3_class(x, "grain")
})

test_that("model scoring for simple model", {
  model = load_default_model()
  score = score_model_(responses, model)
  expect_true(is.list(score))
  expect_named(score, c("intellectual_control", "renderability", "nodes"))
})

test_that("model scoring for advanced model", {
  model = load_default_model()
  advanced_req = test_req_score("advanced")
  parsed_json_full = jsonlite::parse_json(advanced_req$postBody)
  responses = extract_responses(parsed_json_full)[[1]]
  score = score_model_(responses, model)
  expect_true(is.list(score))
  expect_named(score, c("intellectual_control", "renderability", "nodes"))
})

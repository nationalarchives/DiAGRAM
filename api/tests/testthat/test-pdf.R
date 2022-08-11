test_that("pkg_resource", {
  required = c("tna-logo.png", "template.tex")
  non_existent = c("foo.bar")
  for (req in required) {
    out = pkg_resource(req)
    expect_type(out, "character")
    expect_gt(nchar(out), 0)
    expect_true(file.exists(out))
  }
  for (err in non_existent) {
    expect_error(pkg_resource(err))
  }
})

test_that("get questions returns data", {
  res = get_questions()
  expect_named(res, .user_nodes)
  expect_length(res, 9)
})

test_that("get questions tibble returns data", {
  res = get_questions_tibble()
  expect_s3_class(res, "tbl")
  expect_equal(nrow(res), 9)
  expect_equal(ncol(res), 2)
})

test_that("Can join questions and responses together", {
  q = get_questions_tibble()
  r = test_req_score("simple")$postBody %>%
    jsonlite::parse_json() %>%
    extract_simple_responses() %>%
    magrittr::extract2(1)
  res = join_questions_responses(q, r)
  expect_s3_class(res, "tbl")
})

test_that("pdf tables simple scenario response", {
  q = get_questions_tibble()
  responses = test_req_score("simple_scenario")$postBody %>%
    jsonlite::parse_json() %>%
    extract_simple_responses()
  for (response in responses) {
    res = join_questions_responses(q, response)
    for (row in seq_len(nrow(res))) {
      expect_s3_class(pdf_table_part(
        res$question[[row]],
        res$response[[row]],
        res$node[[row]]
      ), "tbl")
    }
  }
})

test_that("find scenario diffs", {
  responses = test_req_score("simple_scenario")$postBody %>%
    jsonlite::parse_json() %>%
    extract_simple_responses()
  res = find_scenario_diffs(responses)
  expect_length(res, 1)
  purrr::walk(res, ~expect_named(.x, c("data", "name", "notes")))
  purrr::walk(res, ~expect_s3_class(.x, "scenario_diff"))
})

test_that("join scenario questions and repsonses", {
  responses = test_req_score("simple_scenario")$postBody %>%
    jsonlite::parse_json() %>%
    extract_simple_responses()
  policies = find_scenario_diffs(responses)
  q = get_questions_tibble()
  res = join_questions_responses(q, policies[[1]])
  expect_s3_class(res, "tbl")
  expect_named(res, c("node", "question", "scenario", "base_model"))
})

responses = test_req_score("mixed_simple")$postBody %>%
    jsonlite::parse_json() %>%
    extract_responses()

test_that("previous errors", {
  data = test_previous_error("pdf_fail")$postBody %>%
    jsonlite::parse_json()
  responses = extract_simple_responses(data[-1])
  res = find_scenario_diffs(responses)
  expect_equal(nrow(res[["big disaster"]]$data), 1)
})

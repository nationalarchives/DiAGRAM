# turn off logging
# logger::log_threshold(logger::FATAL - logger::FATAL)
port = httpuv::randomPort()

simple_post = function() {
  post_data = test_req_score("simple_single")$postBody
  headers = httr::add_headers(
    Accept = "application/json",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/model/score")
  res = httr::POST(
    url, body = post_data, config = headers
  )
  res
}

pdf_post = function() {
  post_data = test_req_pdf("advanced")$postBody
  headers = httr::add_headers(
    Accept = "application/pdf",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/report/pdf")
  res = httr::POST(
    url, body = post_data, config = headers
  )
  res
}

csv_post = function() {
  post_data = test_req_csv("mixed_simple")$postBody
  headers = httr::add_headers(
    Accept = "text/csv",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/report/csv")
  res = httr::POST(
    url, body = post_data, config = headers
  )
  res
}

png_post = function() {
  post_data = test_req_plot("simple_scenario")$postBody
  headers = httr::add_headers(
    Accept = "image/png",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/chart/plot")
  res = httr::POST(
    url, body = post_data, config = headers
  )
  res
}

validate_post = function() {
  post_data = test_req_csv("mixed")$postBody
  headers = httr::add_headers(
    Accept = "application/json",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/validation/validate_json")
  res = httr::POST(
    url, body = post_data, config = headers
  )
  res
}

running_api = callr::r_bg(
  function(port) {
    # diagramAPI::setup_logger()
    dir = diagramAPI::get_internal_routes()
    routes = diagramAPI::create_routes(dir)
    api = diagramAPI::generate_api(routes)
    api = diagramAPI:::add_default_hooks(api)
    api$run(port = port, host = "0.0.0.0")
  }, list(port = port)
)

Sys.sleep(15)

test_that("API starts successully", {
  expect_true(running_api$is_alive())
})

# skip other tests if api is not alive
skip_dead_api = function() {
  testthat::skip_if_not(running_api$is_alive(), "Api not started")
}

test_that("is alive", {
  skip_dead_api()
  res = httr::GET(glue::glue("http://0.0.0.0:{port}/test/is_alive"))
  expect_equal(res$status_code, 200)
})

test_that("validation api endpoint", {
  skip_dead_api()
  res = validate_post()
  expect_equal(res$status_code, 200)
})

test_that("api score endpoint", {
  skip_dead_api()
  res = simple_post()
  parsed = httr::content(res, as = "parsed")
  expect_type(parsed, "list")
  expect_named(parsed, c("intellectual_control", "renderability", "nodes"), ignore.order = TRUE)
  expect_gte(parsed[[1]], 0)
  expect_gte(parsed[[2]], 0)
  expect_lte(parsed[[1]], 100)
  expect_lte(parsed[[2]], 100)
})

test_that("api score endpoint", {
  skip_dead_api()
  res = simple_post()
  parsed = httr::content(res, as = "parsed")
  expect_type(parsed, "list")
  expect_named(parsed, c("intellectual_control", "renderability", "nodes"), ignore.order = TRUE)
  expect_gte(parsed[[1]], 0)
  expect_gte(parsed[[2]], 0)
  expect_lte(parsed[[1]], 100)
  expect_lte(parsed[[2]], 100)
})

test_that("pdf endpoint", {
  skip_dead_api()
  skip_on_ci()
  res = pdf_post()
  expect_equal(res$status_code, 200)
  expect_equal(res$headers$`content-type`, "application/pdf")
})

test_that("api csv endpoint", {
  skip_dead_api()
  res = csv_post()
  parsed = httr::content(res, as = "parsed")
  expect_equal(res$status_code, 200)
  expect_match(res$headers$`content-type`, "text/csv")
})

test_that("api csv endpoint", {
  res = csv_post()
  parsed = httr::content(res, as = "parsed")
  expect_equal(res$status_code, 200)
})

test_that("api plot endpoint", {
  res = png_post()
  parsed = httr::content(res, as = "parsed")
  expect_equal(res$status_code, 200)
})

test_that("previous errors", {
  score_error_json = test_previous_error("score_error")$postBody
  headers = httr::add_headers(
    Accept = "application/json",
    "Content-Type" = "application/json"
  )
  url = glue::glue("http://0.0.0.0:{port}/model/score")
  res = httr::POST(
    url, body = score_error_json, config = headers
  )
  expect_equal(res$status_code, 200)

  advanced_error_json = test_previous_error("advanced_fail")$postBody
  parsed_json = jsonlite::parse_json(advanced_error_json)
  responses = extract_responses(parsed_json)
  res = httr::POST(
    url, body = score_error_json, config = headers
  )
  expect_equal(res$status_code, 200)
})

running_api$kill()

test_that(
  "score_model() gives appropriate response type", {
    req = test_req_score("simple_single")
    expect_type(score_model(req), "list")
  }
)

test_that(
  "score_model() errors if something other than json is passed", {
    expect_error(score_model(1))
  }
)

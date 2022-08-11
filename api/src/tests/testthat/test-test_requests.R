test_that("all test requests can be loaded", {
  possible = eval(formals(test_req_score)$type)
  for (type in possible) {
    out = test_req_score(type)
    expect_type(out, "environment")
  }
})

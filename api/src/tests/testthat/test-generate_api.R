test_that("generate_api() builds plumber router object", {
  dir = get_internal_routes()
  routes = create_routes(dir)
  apis = generate_api(routes)
  expect_s3_class(apis, "Plumber")

  route = c("test" = file.path(dir, "test.R"))
  api = generate_api(route)
  expect_s3_class(api, "Plumber")
  expect_true(names(api$mounts) == "/test/")
})

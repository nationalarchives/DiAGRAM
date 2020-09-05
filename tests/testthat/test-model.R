test_that("Utility calculation is correct", {
  model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagram"))
  scores = calculate_utility(model)
  expect_equal(scores$Intellectual_Control, 0.3336339, tolerance = 1e-4)
  expect_equal(scores$Renderability, 0.2421412, tolerance = 1e-4)
})

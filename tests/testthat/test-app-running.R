context("Testing the app runs")

app = shinytest::ShinyDriver$new("../../R/")

test_that("The app runs", {

  expect_equal(app$getTitle(), "DiAGRAM")

})

app$stop()

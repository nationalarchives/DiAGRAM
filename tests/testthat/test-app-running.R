context("Testing the app runs")

app = shinytest::ShinyDriver$new(system.file("example", package = "diagram"))

test_that("The app runs", {

  expect_equal(app$getTitle(), "DiAGRAM")

})

app$stop()

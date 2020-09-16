context("Testing the app runs")

app = shinytest::ShinyDriver$new(system.file("example", package = "diagramNAT"))

test_that("The app runs", {

  expect_equal(app$getTitle(), "DiAGRAM")

})

app$stop()

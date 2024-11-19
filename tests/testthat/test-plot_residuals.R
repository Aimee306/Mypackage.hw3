test_that("plot_residuals generates a plot without errors", {
  my_model <- myreg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  # Test that the function runs without error
  expect_silent(plot_residuals(my_model))
})

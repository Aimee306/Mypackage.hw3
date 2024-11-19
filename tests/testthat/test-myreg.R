test_that("myreg produces correct results", {
  my_model <- myreg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  lm_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  # Test coefficients
  expect_equal(my_model$coefficients, lm_model$coefficients)

  # Test adjusted R-squared
  expect_equal(my_model$adj.r.squared, summary(lm_model)$adj.r.squared)

  # Test F-statistic
  expect_equal(unname(my_model$fstatistic), unname(summary(lm_model)$fstatistic[1]), tolerance = 1e-8)

  # Test p-value
  expect_equal(unname(my_model$p_value), unname(pf(summary(lm_model)$fstatistic[1],
                                                   df1 = summary(lm_model)$fstatistic[2],
                                                   df2 = summary(lm_model)$fstatistic[3],
                                                   lower.tail = FALSE)), tolerance = 1e-8)
})

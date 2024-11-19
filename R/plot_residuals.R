#' Plot residuals vs. fitted values for model diagnosis
#'
#' @description
#' Plot_residuals function creates a plot of residuals against fitted values as
#' a diagnostic tool for a linear regression model fitted using the myreg function.
#' It helps to evaluate the following:
#'
#' 1) Linearity: The residuals should show no systematic pattern.
#'
#' 2) Homoscedasticity: The spread of residuals should remain consistent across fitted values.
#'
#' 3) Outliers: Points far from the others may indicate outliers or influential observations.

#' @param model An object of class "myreg" returned by the myreg function.
#' @examples
#' # Fit a linear regression model using the iris dataset
#' iris_model <- myreg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#'
#' # Plot residuals vs. fitted values for model diagnosis
#' plot_residuals(iris_model)
#' @export

plot_residuals <- function(model) {
  plot(
    model$fitted.values,
    model$residuals,
    main = "Residuals vs Fitted Values",
    xlab = "Fitted Values",
    ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)
}

#' Perform linear regression using the normal equation.
#'
#' @description
#' Myreg function performs linear regression using the normal equation method.
#' It calculates regression coefficients, fitted values, residuals, and key goodness-of-fit metrics
#' such as R-squared, adjusted R-squared, F-statistic, and p-values.
#'
#' @param formula A formula specifying the outcome and predictor variables.
#' @param data A data frame containing all the variables in the formula.
#'
#' @examples
#' # Fit a linear regression model using the iris dataset
#' iris_model <- myreg(Sepal.Length ~ Sepal.Width + Species, data = iris)
#'
#' # Access regression coefficients
#' iris_model$coefficients
#'
#' # Access the fitted values and residuals
#' head(iris_model$fitted.values)
#' head(iris_model$residuals)
#'
#' # Model diagnosis using residuals vs. fitted values plot
#' plot(iris_model$fitted.values, iris_model$residuals,
#'      main = "Residuals vs. Fitted Values",
#'      xlab = "Fitted Values",
#'      ylab = "Residuals")
#' @export

myreg <- function(formula, data) {
  X <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))

  # Calculate regression coefficients using the normal equation
  coef <- solve(t(X) %*% X) %*% t(X) %*% y

  # Calculate fitted values and residuals
  fitted <- X %*% coef
  residuals <- y - fitted

  # Calculate SST, SSE and their degree of freedoms
  sst <- sum((y - mean(y))^2)
  sse <- sum(residuals^2)
  df_total <- nrow(X) - 1
  df_residual <- nrow(X) - ncol(X)
  df_model <- ncol(X) - 1

  # Calculate R-squared and Adjusted R-squared values
  r_squared <- 1 - (sse / sst)
  adj_r_squared <- 1 - (sse / df_residual) / (sst / df_total)

  # Calculate F-statistic and its p-value
  f_stat <- ((sst - sse) / df_model) / (sse / df_residual)
  p_value <- pf(f_stat, df_model, df_residual, lower.tail = FALSE)

  # Calculate Standard errors
  mse <- sum(residuals^2) / df_residual
  std_err <- sqrt(mse * diag(solve(t(X) %*% X)))

  # Return the regression results as a classed object
  structure(
    list(
      coefficients = setNames(as.vector(coef), colnames(X)),
      fitted.values = as.vector(fitted),
      residuals = as.vector(residuals),
      std.error = std_err,
      r.squared = r_squared,
      adj.r.squared = adj_r_squared,
      fstatistic = f_value,
      p_value = p_value,
    ),
    class = "myreg"
  )
}

  #' Summarize a linear regression model
  #'
  #' @description
  #' Summary.myreg function provides a summary of the results from a linear regression model fitted using myreg function.
  #' The summary includes estimated coefficients, their standard errors, t-values, p-values,
  #' R-squared, adjusted R-squared, and the F-statistic with its p-value.
  #' @param model An object of class "myreg".
  #' @examples
  #' # Fit a regression model
  #' iris_model <- myreg(Sepal.Length ~ Sepal.Width + Species, data = iris)
  #'
  #' # Summarize the regression model
  #' summary(iris_model)
  #' @export

  summary.myreg <- function(model) {
    # Calculate t-values and p-values for coefficients
    t_values <- model$coefficients / model$std_err
    p_values <- 2 * pt(-abs(t_values), df = model$df_residual)

    # Create a table of coefficients
    coef_table <- data.frame(
      Coefficients = names(model$coefficients),
      Estimate = model$coefficients,
      Std.Error = model$std_err,
      t.value = t_values,
      p.value = p_values
    )

    # Print the summary output
    print(coef_table)
    cat("\nR-squared:", round(model$r_squared, 4), )
    cat("Adjusted R-squared:", round(model$adj_r_squared, 4), "\n")
    cat("F-statistic:", round(model$f_value, 2), "on", model$df_model, "and",
        model$df_residual, "DF,  p-value:", round(model$p_value, digits = 4), "\n")
    }


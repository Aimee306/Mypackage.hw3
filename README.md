[![R-CMD-check.yaml](https://github.com/Aimee306/Mypackage.hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Aimee306/Mypackage.hw3/actions/workflows/R-CMD-check.yaml)

[![codecov](https://codecov.io/gh/Aimee306/Mypackage.hw3/branch/main/graph/badge.svg)](https://codecov.io/gh/Aimee306/Mypackage.hw3)

## **Overview**
Mypackage.hw3 is an R package that implements linear regression using the normal equation. The myreg 
function simplifies the modeling process while providing detailed insights into regression coefficients, 
residuals, goodness-of-fit metrics, and model significance. The package also includes the plot_residuals
function, a diagnostic tool to evaluate linearity, homoscedasticity, and the presence of outliers. 

The idea behind Mypackage.hw3 is to provide a convenient, integrated approach to model diagnosis, catering 
to users who need more than just regression coefficients.

## **Usage**
```r
# Load the required library
library(Mypackage.hw3)

# Fit a linear regression model using the iris dataset
iris_model <- myreg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

# Access regression coefficients
coefficients <- iris_model$coefficients

# View the adjusted R-squared value
adj_r_squared <- iris_model$adj.r.squared

# Evaluate the model using F-statistic and p-value
f_stat <- iris_model$fstatistic  
p_value <- iris_model$p_value

# Generate a residuals vs. fitted values diagnostic plot
plot_residuals(iris_model)
```

# will be using a simple data frame and the iris data set to ensure that the package works well for both simple and complex data sets.
library(testthat)
test_that("plot_clusters works correctly", {
  # create a simple data frame and create the variable result to pass into the tests.
  data <- data.frame(x = 1:10, y = 11:20)
  k <- 3
  result <- kmeans_clustering(data, k)

  # Check if the plot is generated without errors
  expect_silent(plot_clusters(data, result))
  # for a more comprehensive test, will be using the iris data set
  data(iris)
  numeric_iris <- iris[, sapply(iris, is.numeric)] # ensures that we are only including the numeric data into the function so that it does not return an error
  result_iris <- kmeans_clustering(numeric_iris, 3)
  expect_silent(plot_clusters(numeric_iris, result_iris))

  # Test for errors with invalid input
  expect_error(plot_clusters(iris, result_iris), "Input data must be a numeric data frame") # Non-numeric data
  expect_error(plot_clusters(data, iris), "kmeans_result must be a kmeans object") # Not a kmeans object
  expect_error(plot_clusters(1:10, result), "Input data must be a numeric data frame") # Not a data frame
})

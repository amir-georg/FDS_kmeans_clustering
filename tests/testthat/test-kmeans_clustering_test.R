# will be using a simple data frame and the iris data set to ensure that the package works well for both simple and complex data sets.
library(testthat)
test_that("kmeans_clustering works correctly", {
  # create a simple data frame and create the variable result to pass into the tests.
  data <- data.frame(x = 1:10, y = 11:20)
  k <- 3
  result <- kmeans_clustering(data, k)

  # check if the result of kmeans is appropriate for the function.
  expect_s3_class(result, "kmeans")

  # check the number of cluster centers and whether it matches with the number of clusters
  expect_equal(nrow(result$centers), k)

  # check that we have the right number of clusters
  expect_equal(length(result$cluster), nrow(data))

  # check that the function can handle different k values without returning errors.
  expect_silent(kmeans_clustering(data, 2))
  expect_silent(kmeans_clustering(data, 5))
  # for a more comprehensive test, will be using the iris data set
  data(iris)
  numeric_iris <- iris[, sapply(iris, is.numeric)] # ensures that we are only including the numeric data into the function so that it does not return an error
  expect_silent(kmeans_clustering(numeric_iris, 3))

  # Test for errors with invalid input
  expect_error(kmeans_clustering(data, 0), "k must be a positive integer")
  expect_error(kmeans_clustering(data, -1), "k must be a positive integer")
  expect_error(kmeans_clustering(data, 1.5), "k must be a positive integer") # Non-integer k
  expect_error(kmeans_clustering(iris, 3), "Input data must be a numeric data frame") # Non-numeric data
  expect_error(kmeans_clustering(1:10, 3), "Input must be a data frame") # Not a data frame
})

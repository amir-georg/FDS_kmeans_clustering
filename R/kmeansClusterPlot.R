#First function is the clustering function.

#Roxygen2 documentation block to describe the function that displays when ?kmeansClustering

#' Perform K-means Clustering
#'
#' @param data A data frame containing numeric variables.
#' @param k The number of clusters.
#'
#' @return A list object of class `kmeans` (the result of the `kmeans()` function).
#' @export

#this creates a function kmeans_clustering that makes clusters by taking in data frame and number of clusters (k). IF the wrong type of data or illogical number of clusters (<1) error messages display
#!is means "is not", so !is.data.frame(data) means if "data" is not data frame, stop() function will occur and "input must be a data frame" will be displayed to guide user into what type of data is needed.
#all(sapply(data, is.numeric)) ensures that the information in the data frame must is numeric. The "||" is the or operator, ensuring that both conditions need to be met for the code to run.
#the second argument is similar in that it ensures that the data is numeric, an integer and greater than 1 (i.e., we can't have -1.3 clusters)
kmeans_clustering <- function(data, k) {
  if (!is.data.frame(data) || !all(sapply(data, is.numeric))) stop("Input must be a numeric data frame")  # Check numeric data
  if (!is.numeric(k) || k < 1 || k != as.integer(k)) stop("k must be a positive integer") # Check for integer k

  kmeans(data, centers = k) # Return the kmeans result directly
}


#Second function is the plotting function.

#Roxygen2 documentation block to describe the function that displays when ?plot_clusters is used
#' Plot K-means Clustering Results
#'
#' @param data A data frame containing numeric variables.
#' @param kmeans_result The result of the `kmeans_clustering()` function (a kmeans object).
#'
#' @return A scatter plot with clusters.
#' @export

#this creates a function plot_clusters that takes in input data, and the kmeans_result from above function.
#the first argument is same as above, ensure that the data is a dataframe and numeric.
#then cluster_assignments and cluster_centers are variables that are made in the function and the results of kmeans are applied to them.
plot_clusters <- function(data, kmeans_result) {  # Take kmeans_result as an argument
  if (!is.data.frame(data) || !all(sapply(data, is.numeric))) stop("Input data must be a numeric data frame")
  if (!inherits(kmeans_result, "kmeans")) stop("kmeans_result must be a kmeans object") # Type check

  cluster_assignments <- kmeans_result$cluster
  cluster_centers <- kmeans_result$centers

  plot(data, col = cluster_assignments, pch = 20, cex = 2,
       main = "K-means Clustering")
  points(cluster_centers, pch = 4, cex = 3, lwd = 3, col = "black")
}

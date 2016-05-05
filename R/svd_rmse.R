#' Calculate RMSE of SVD Approximation model
#'
#' @param model model object, returned by \code{\link{svd_build}} function
#' @param r (default 10) number of latent factors to take into account to reconstruct rating matrix and make prediction
#' @param rmse_type (default c("train", "validation", "test")) vector, which indicates matrixes for which RMSE should be calculated
#'
#' @return vector of RMSE based on \code{rmse_type}. Errors are returned in next order: "train", "validation", "test"
#'
#' @details
#' Function calculate RMSE for SVD Approximation Recommender model for train, validation and test sets
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#' svd_rmse(model, r = 10, rmse_type = c("validation"))
#' svd_rmse(model, r = 5, rmse_type = c("train", "validation"))
#'
#' @import Matrix
#'
#' @export

svd_rmse <- function(model, r = 10, rmse_type = c("train", "validation", "test")) {

  pred <- model$decomposed_matrix$u[, 1:r] %*% model$decomposed_matrix$d[1:r, 1:r] %*% t(model$decomposed_matrix$v[,1:r]) + model$user_average

  rmse <- NULL

  if ("train" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$train_matrix_ones - model$mtx$train_matrix) ^ 2) / sum(model$mtx$train_matrix_ones)))
  }
  if ("validation" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$valid_matrix_ones - model$mtx$valid_matrix) ^ 2) / sum(model$mtx$valid_matrix_ones)))
  }
  if ("test" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$test_matrix_ones - model$mtx$test_matrix) ^ 2) / sum(model$mtx$test_matrix_ones)))
  }
  return(rmse)
}

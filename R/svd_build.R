#' Build SVD Recommender Model
#'
#' @param mtx list of matrixes for training, validation and testing recommender, returned by \code{\link{split_ratings}} function
#'
#' @return List of 3 objects:
#' \itemize{
#'   \item decomposed_matrix - list of matrixes, returned by \code{\link{svd}} decomposition
#'   \item user_average - vector of average rating of each user
#'   \item mtx - list of train, validation and test sparse matrixes
#' }
#'
#' @details
#' Function performs SVD decomposition of user-item rating matrix
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#'
#' @import Matrix
#'
#' @export

svd_build <-  function(mtx) {

  train_matrix_user <- rowSums(mtx$train_matrix) / rowSums(mtx$train_matrix_ones)
  train_matrix_item <- colSums(mtx$train_matrix) / colSums(mtx$train_matrix_ones)
  train_matrix_item[which(is.na(train_matrix_item) == TRUE)] <- sum(mtx$train_matrix) / sum(mtx$train_matrix_ones)

  user_average <- matrix(rep(train_matrix_user, length(train_matrix_item)), ncol = length(train_matrix_item), byrow = FALSE)

  train <- mtx$train_matrix + matrix(rep(train_matrix_item, length(train_matrix_user)), nrow = length(train_matrix_user), byrow = TRUE) * (1 - mtx$train_matrix_ones)
  train <- train - user_average

  decomposed_matrix <- svd(train)

  d <- matrix(0, length(decomposed_matrix$d), length(decomposed_matrix$d))
  diag(d) <- decomposed_matrix$d

  decomposed_matrix$d <- d

  return(list(decomposed_matrix = decomposed_matrix, user_average = user_average, mtx = mtx))
}

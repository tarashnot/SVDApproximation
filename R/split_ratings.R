#' Split ratings data into train, validation and test sets
#'
#' @param ratings_table \code{\link{data.table}} or \code{\link{data.frame}} of ratings. Should contain 3 columns: user (id of user, integer), item (id of item, integer) and rating (rating of item by user, integer or numeric)
#' @param proportion (default c(0.7, 0.15, 0.15)) vector of 3 numeric values, which sums to 1 and indicate proportion of ratings to be set to train, validation and test sets
#'
#' @return List of 6 objects:
#' \itemize{
#'   \item train_matrix - sparse matrix with ratings for train
#'   \item valid_matrix - sparse matrix with ratings for validation
#'   \item test_matrix - sparse matrix with ratings for test
#'   \item train_matrix_ones - train_matrix with ratings replaced with ones
#'   \item valid_matrix_ones - valid_matrix with ratings replaced with ones
#'   \item test_matrix_ones - test_matrix with ratings replaced with ones
#' }
#'
#' @details
#' Function splits data into train, validation and test sets and transforms it into sparse matrix objects
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#'
#' @import Matrix recommenderlab
#'
#' @export

split_ratings <- function(ratings_table, proportion = c(0.7, 0.15, 0.15)) {

  in_train <- rep(TRUE, nrow(ratings))
  in_train[sample(1:nrow(ratings), size = round((1 - proportion[1]) * nrow(ratings), 0))] <- FALSE

  in_valid <- rep(FALSE, nrow(ratings))
  in_valid[sample(which(in_train == FALSE), size = round(proportion[2] * nrow(ratings), 0))] <- TRUE

  train_matrix <- sparseMatrix(i = ratings$user[in_train], j = ratings$item[in_train], x = ratings$rating[in_train],
                               dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = "") , paste("m", 1:length(unique(ratings$item)), sep = "")))

  valid_matrix <- sparseMatrix(i = ratings$user[in_valid], j = ratings$item[in_valid], x = ratings$rating[in_valid],
                               dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = "") , paste("m", 1:length(unique(ratings$item)), sep = "")))

  test_matrix <- sparseMatrix(i = ratings$user[!in_train & !in_valid], j = ratings$item[!in_train & !in_valid], x = ratings$rating[!in_train & !in_valid],
                              dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                              dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = "") , paste("m", 1:length(unique(ratings$item)), sep = "")))

  train_matrix_ones <- as(binarize(new("realRatingMatrix", data = train_matrix), minRating=1), "dgCMatrix")
  valid_matrix_ones <- as(binarize(new("realRatingMatrix", data = valid_matrix), minRating=1), "dgCMatrix")
  test_matrix_ones <- as(binarize(new("realRatingMatrix", data = test_matrix), minRating=1), "dgCMatrix")

  return (list(train_matrix = train_matrix, valid_matrix = valid_matrix, test_matrix = test_matrix, train_matrix_ones = train_matrix_ones, valid_matrix_ones = valid_matrix_ones, test_matrix_ones = test_matrix_ones))
}

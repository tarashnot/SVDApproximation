#' Split ratings data into train, validation and test sets
#'
#' @param ratings_table \code{\link{data.table}} or \code{\link{data.frame}} of ratings. Should contain 3 columns: user (id of user, integer), item (id of item, integer) and rating (rating of item by user, integer or numeric)
#' @param train_test_splitted (default FALSE) is dataset is already splitted into train and test set (column train with TRUE and FALSE should be in ratings_table)
#' @param proportion (default c(0.7, 0.15, 0.15)) vector of 3 numeric values for train_test_splitted=FALSE, which sums to 1 and indicate proportion of ratings to be set to train, validation and test sets. And of length 2 for train_test_splitted=TRUE, which indicates in what proportion should be splitted train set into train and validation
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
#' @import Matrix recommenderlab data.table
#'
#' @export

split_ratings <- function(ratings_table, train_test_splitted = FALSE, proportion = c(0.7, 0.15, 0.15)) {

  if (train_test_splitted == FALSE) {
    in_train <- rep(TRUE, nrow(ratings_table))
    in_train[sample(1:nrow(ratings_table), size = round((1 - proportion[1]) * nrow(ratings_table), 0))] <- FALSE

    in_valid <- rep(FALSE, nrow(ratings_table))
    in_valid[sample(which(in_train == FALSE), size = round(proportion[2] * nrow(ratings_table), 0))] <- TRUE

    ratings_table$train <- in_train
    ratings_table$validation <- in_valid
  }

  if (train_test_splitted == TRUE) {
    in_valid <- rep(FALSE, nrow(ratings_table))
    in_valid[sample(which(ratings_table$train == TRUE), size = round(proportion[2] * nrow(ratings_table), 0))] <- TRUE

    ratings_table$validation <- in_valid
    ratings_table[validation == TRUE, train := FALSE]
  }


  train_matrix <- sparseMatrix(i = ratings_table[train == TRUE]$user, j = ratings_table[train == TRUE]$item, x = ratings_table[train == TRUE]$rating,
                               dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))

  valid_matrix <- sparseMatrix(i = ratings_table[validation == TRUE]$user, j = ratings_table[validation == TRUE]$item, x = ratings_table[validation == TRUE]$rating,
                               dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))

  test_matrix <- sparseMatrix(i = ratings_table[train == FALSE & validation == FALSE]$user, j = ratings_table[train == FALSE & validation == FALSE]$item, x = ratings_table[train == FALSE & validation == FALSE]$rating,
                              dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                              dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))

  train_matrix_ones <- as(binarize(new("realRatingMatrix", data = train_matrix), minRating=1), "dgCMatrix")
  valid_matrix_ones <- as(binarize(new("realRatingMatrix", data = valid_matrix), minRating=1), "dgCMatrix")
  test_matrix_ones <- as(binarize(new("realRatingMatrix", data = test_matrix), minRating=1), "dgCMatrix")

  return (list(train_matrix = train_matrix, valid_matrix = valid_matrix, test_matrix = test_matrix, train_matrix_ones = train_matrix_ones, valid_matrix_ones = valid_matrix_ones, test_matrix_ones = test_matrix_ones))
}

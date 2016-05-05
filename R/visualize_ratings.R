#' Visualize statistics for all ratings, users' ratings and items' ratings
#'
#' @param ratings_table \code{\link{data.table}} or \code{\link{data.frame}} of ratings. Should contain 3 columns: user (id of user, integer), item (id of item, integer) and rating (rating of item by user, integer or numeric)
#' @param color_palett (default "Accent") name of palett to use to color graphs. See \code{\link{brewer.pal}}
#'
#' @return 5 graphs combined into one plot:
#' \itemize{
#'   \item count of different ratings
#'   \item histogram of users' average ratings
#'   \item histogram of items' average ratings
#'   \item histogram of number of rated items by user
#'   \item histogram of number of scores items have
#' }
#'
#' @details
#' Function combines 5 ggplot graphs into 1 plot and returns it
#'
#' @examples
#'
#' visualize_ratings(ratings_table = ratings)
#' visualize_ratings(ratings_table = ratings, color_palett = "Dark2")
#'
#' @import RColorBrewer Matrix recommenderlab ggplot2 gridExtra
#'
#' @export

visualize_ratings <- function(ratings_table, color_palett = "Accent") {

  #Select colors for graphs
  cols <- brewer.pal(3, color_palett)

  #Create sparse matrix
  ratings_sparse <- sparseMatrix(i = ratings$user, j = ratings$item, x = ratings$rating,
                                 dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                                 dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = "") , paste("m", 1:length(unique(ratings$item)), sep = "")))

  #Create real rating matrix object for recommenderlab
  matrix_sparse <- new("realRatingMatrix", data = ratings_sparse)
  matrix_ones <- binarize(matrix_sparse, minRating=1)

  data_plot <- data.frame(table(getRatings(matrix_sparse)))
  names(data_plot) <- c("Score", "Count")

  all_ratings <- ggplot(data_plot,aes(x = Score, y = Count)) +
    geom_bar(stat="identity",colour="white", fill = cols[1]) +
    labs(title = "Count of different ratings") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12))

  data_plot <- data.frame(rowMeans(matrix_sparse))
  names(data_plot) <- "UserAverageScore"
  users_ratings <- ggplot(data_plot,aes(x = UserAverageScore)) +
    geom_histogram(binwidth=0.1, colour = "white", fill = cols[2]) +
    geom_vline(xintercept=median(data_plot$UserAverageScore), color = "grey", size=2) +
    labs(title = "Histogram of Users' Average Ratings", x = "User Avearge Score", y = "Count") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = c(1:5, round(median(data_plot$UserAverageScore), 2)))

  data_plot <- data.frame(colMeans(matrix_sparse))
  names(data_plot) <- "ItemAverageScore"
  items_ratings <- ggplot(data_plot,aes(x = ItemAverageScore)) +
    geom_histogram(binwidth=0.1, colour = "white", fill = cols[3]) +
    geom_vline(xintercept=median(data_plot$ItemAverageScore), color = "grey", size=2) +
    labs(title = "Histogram of Items' Average Ratings", x = "Item Avearge Score", y = "Count") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = c(1:5, round(median(data_plot$ItemAverageScore), 2)))

  data_plot <- data.frame(rowSums(matrix_ones))
  names(data_plot) <- "UserRated"
  users_rated <- ggplot(data_plot,aes(x = UserRated)) +
    geom_histogram(binwidth=50, colour = "white", fill = cols[2]) +
    geom_vline(xintercept=median(data_plot$UserRated), color = "grey", size=2) +
    labs(title = "Histogram of Number of Rated items by user", x = "Number of Rated Items", y = "Users") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = round(c(seq(min(data_plot$UserRated),max(data_plot$UserRated), length.out = 5), median(data_plot$UserRated)), 0))

  data_plot <- data.frame(colSums(matrix_ones))
  names(data_plot) <- "Rated"
  item_rated <- ggplot(data_plot,aes(x = Rated)) +
    geom_histogram(binwidth=50, colour = "white", fill = cols[3]) +
    geom_vline(xintercept=median(data_plot$Rated), color = "grey", size=2) +
    labs(title = "Histogram of Number of Scores Items have", x = "Number of Scores Item has", y = "Items") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = round(c(seq(min(data_plot$Rated),max(data_plot$Rated), length.out = 5), median(data_plot$Rated)), 0))

  layout <- matrix(c(1, 1, 2, 3, 4, 5), ncol = 2, byrow = TRUE)
  return(grid.arrange(all_ratings, arrangeGrob(users_ratings, items_ratings, ncol=2), arrangeGrob(users_rated, item_rated, ncol=2), nrow = 3))
}

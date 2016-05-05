#' Tune SVD Recommender Model
#'
#' @param model model object, returned by \code{\link{svd_build}} function
#' @param r (default 2:50) vector of number of latent factors to take into account while reconstructing rating matrix. From these values will be selected one with the smallest RMSE on validation set
#' @param color_pallet (default "Accent") name of palett to use to color graphs. See \code{\link{brewer.pal}}
#'
#' @return List of 3 objects:
#' \itemize{
#'   \item r_best - best value of r which was selected based on validation error
#'   \item all_rmse - \code{\link{data.frame}} with train and validation errors for each r
#'   \item train_vs_valid - ggplot graph of train and validation errors for different r
#' }
#'
#' @details
#' Function selects the best value for number of latent factors to minimize validation error
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#' svd_tune(model, r = 2:5)
#'
#' @import reshape2 ggplot2 RColorBrewer
#'
#' @export


svd_tune <- function(model, r = 2:50, color_pallet = "Accent") {

  rmse <- data.frame(r = integer(), train = numeric(), validation = numeric())

  for (k in r) {
    rmse_r <- svd_rmse(model, r = k, rmse_type = c("train", "validation"))
    rmse <- rbind(rmse, data.frame(r = k, train = rmse_r[1], validation = rmse_r[2]))
    cat("Model is evaluated for r =", k, "\n")
  }

  #Visualization train vs Validation errors

  data_plot <- melt(rmse, id.vars = "r")
  names(data_plot) <- c("r", "Type", "Value")

  cols <- brewer.pal(3, color_pallet)

  train_vs_valid <- ggplot(data = data_plot,
                           aes(x = r, y = Value, color = Type)) +
    geom_point(shape = 21, size = 6, fill="white", alpha=6/10) +
    geom_line(size = 1.1) +
    geom_vline(xintercept = rmse[which.min(rmse[,3]),1], linetype = "longdash", colour = cols[3], size = 1.5) +
    scale_color_manual(values=cols[1:2]) +
    labs(title = "Train and Validation RMSE for SVD Approximation", x = "Number of Components", y = "RMSE") +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=17), title=element_text(size=15),
          legend.text = element_text(size=12))

  return(list(r_best = rmse[which.min(rmse[,3]),1], all_rmse = rmse, train_vs_valid = train_vs_valid))
}

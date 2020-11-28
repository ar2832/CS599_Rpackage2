#' @title BINSEG: Binary segmentation function for the square loss.
#'
#' @description This function performs binary segmentation function for the estimation of the number and locations of multiple change-points in sequence data.
#'
#' @param numeric_data Vector of numeric data
#' @param Kmax Maximum number of clusters
#'
#' @return Vector of square loss values, from 1 to the maximum number of segments.
#' @export
#'
#' @examples
#' BINSEG(numeric_data=one.sequence$logratio, Kmax=5)
#'
#' BINSEG(numeric_data=one.sequence$logratio, Kmax=10)
#'

BINSEG <- function(numeric_data, Kmax){
  loss_f <- c()
  for (i in (1:Kmax)){
    for (x in 1:length(numeric_data)){
      cost1 <- sum(numeric_data[1:x]^2)
      cost2 <- sum(numeric_data[x:length(numeric_data)^2])
      loss_function <- cost1 + cost2
    }
    model <- binsegRcpp::binseg_normal(numeric_data, Kmax)
    min_loss <- model$loss[i]
    loss_f[i] <- min_loss
  }
  loss_f
}


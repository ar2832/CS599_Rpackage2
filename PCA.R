#' @title PCA: Principal Component Analysis function.
#'
#' @description This function performs principal component analysis, a dimensionality-reduction method that is often used to reduce the dimensionality of large data sets.
#'
#' @param X High dimensional data
#'
#' @return Principal components matrix (output of eigen vectors)
#' @export
#'
#' @examples
#' PCA(X=zip.train)
#'
#' PCA(X=zip.test)
#'
PCA <- function(X){
  X <- scale(X, center=TRUE, scale=FALSE)
  s <- base::svd(X)
  s <- s$u %*% diag(s$d)
}

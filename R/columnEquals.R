#' @author
#' Nicolas Felipe Trujillo Montero
#'
#' @title
#' Function Column Equals: check the equality of 2 sparse set
#' IMPORTANT: X and Y must be column vector
#'
#' @param x
#' It is a sparse matrix
#'
#' @param y
#' It is a sparse matrix
#'
#' @return
#' Returns a boolean that express if the matrices are equals or not.
#'
#' @examples
#' .columnEquals(NULL,NULL) == TRUE
#' .columnEquals(Matrix(c(1,0),sparse=TRUE), Matrix(c(1,0),sparse=TRUE)) == TRUE
#' .columnEquals(Matrix(c(0,1),sparse=TRUE), Matrix(c(1,0),sparse=TRUE)) == FALSE

.columnEquals <- function(x, y){

  res <- NULL

  if(is.null(x) && is.null(y)){
    res <- TRUE
  } else if (is.null(x)){
    res <- FALSE
  } else if (is.null(y)){
    res <- FALSE
  } else {
    res <- ( all(dim(x) == dim(y)) && (all(x == y)) )
  }
  return (res)
}

#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the Min_Covers function in the FastD-Basis algorithm
#'
#' @param A
#' It is a set of items represented by a sparse matrix (column vector)
#'
#' @param B
#' It is a set of items represented by a sparse matrix (column vector)
#'
#' @param C
#' It is a set of items represented by a sparse matrix (column vector)
#'
#' @param gamma
#' It is a sparse matrix in which contains 3-tuples of elements represented by
#' 3 binded columns together.
#'
#' @param attr
#' It's a vector that contains the attributes of sigma
#'
#' @return
#' Returns an sparse matrix

.minCovers_FDB <- function(A,B,C,gamma, attr){

  # Check if arguments are correct
  if (is.null(gamma)) {
    stop("Gamma introduced in .minCovers_FDB is NULL")
  }

  # BE CAREFUL WITH A,B,C NULL IN FIRST ITERATION
  res <- .fix_FDB(A,B,C,gamma,attr)
  mnl <- res[[1]]
  gamma <- res[[2]]

  # Base case
  if(is.null(mnl) && is.null(gamma)){
    return(NULL)
  }

  phi <- NULL

  mult3_gamma <- dim(gamma)[2]/3


  for ( ind_mul3_gamma in 1:mult3_gamma ) {

    gamma_ind <-(ind_mul3_gamma-1) *3

    X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)

    isElement <- any(apply(mnl, MARGIN = 2,
                           FUN = function(col, X){
                                  col <- Matrix(col, sparse = TRUE);
                                  return( all(.columnEquals(col,X)) )
                                }, X))
    # any(.equal_sets(X,Y))
    # %~%

    if(isElement){

      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      min <- .minCovers_FDB(X,Y,Z,gamma,attr)

      if(!is.null(min)){
        psi <- cbind( cbind(X,Y,Z),min)
      } else {
        psi <- cbind(X,Y,Z)
      }

      phi <- .join_FDB(phi,psi)

    }

  }

  return(phi)
}

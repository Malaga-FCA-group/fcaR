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

  # BE CAREFUL WITH A,B,C NULL IN FIRST ITERATION
  res <- .fix_FDB(A,B,C,gamma,attr)
  mnl <- res[[1]]
  gamma <- res[[2]]

  phi <- NULL

  mult3_gamma <- dim(gamma)[2]/3


  if(!is.null(gamma)){

    for ( ind_mul3_gamma in 1:mult3_gamma ) {

      gamma_ind <-(ind_mul3_gamma-1) *3

      X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)

      if(isElementOf_FDB(X,mnl)){

        Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
        Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

        min <- .minCovers_FDB(X,Y,Z,gamma)

        if(!is.null(min)){
          psi <- .union( cbind(X,Y,Z),min)
        }

        phi <- .join_FDB(phi,psi)
      }

    }

  }

  return(phi)

}

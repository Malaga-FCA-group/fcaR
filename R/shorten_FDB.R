#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the Shorten function in the FastD-Basis algorithm
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
#' @return
#' Returns a 3-tuple simplified

# A,B,C CAN'T BE NULL
.shorten_FDB <- function (A,B,C, gamma) {

  if(is.null(A) || is.null(B) || is.null(C)){
    stop("A, B or C can't be NULL in shorten")
  }

  if(!is.null(gamma)){

    if (sum(A) != 1){

      mult3_gamma <- dim(gamma)[2]/3

      for ( ind_mul3_gamma in 1:mult3_gamma ) {


        gamma_ind <-(ind_mul3_gamma-1) *3

        X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
        Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
        Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

        if (all(.subset(X,A))) {
          B <- .difference2(B,Y)
        } else if (!all(.subset(A,X)) && all(.subset(X,C))) {
          B <- .difference2(B,Y)
        }

        if(sum(B)==0){
          return(NULL)
        }
      }

    }

  }

    return(cbind(A,B,C))
}

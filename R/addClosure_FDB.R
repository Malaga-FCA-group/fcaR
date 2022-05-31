#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the addClosure function in the FastD-Basis algorithm
#'
#' @param A
#' It is a set of items represented by a sparse matrix (column vector)
#'
#' @param gamma
#' It is a sparse matrix in which contains 3-tuples of elements represented by
#' 3 binded columns together.
#' IMPORTANT: it is required that all of the columns in gamma were different to
#' empty set.
#'
#' @return
#' Returns a sparse matrix gamma that it is applied the AddClosure function

.addClosure_FDB <- function (A, gamma) {

  # Check if arguments are correct
  if (is.null(A) || is.null(gamma)) {
    stop("Some argument introduced in .addClosure is NULL")
  }

  # Doesn't require initialize with Matrix Class
  B <- A
  C <- B

  repeat{

    # Doesn't require initialize with Matrix Class
    B_old <- B
    gamma_new <- NULL

    mult3_gamma <- dim(gamma)[2]/3

    for ( ind in 1:mult3_gamma ) {

      # Initialize X,Y,Z
      gamma_ind <-(ind-1) *3

      X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      # Ac1
      if (.matrixEquals(A,X)){
        B <- .union(B, .union(Y,Z) )
        C <- .union(C,Z)
      } else {

        # Ac2
        if (all(.subset(X,B))){
          B <- .union(B, .union(Y,Z) )
        }

        # Ac3
        if(all(.subset(A,X))){ # !(.matrixEquals(A,X)) not necessary because you check it above

          if(!all(.subset(Y,B))){
            gamma_new <- cbind(gamma_new, X, .difference2(Y,B), .union(Z,C))
          }

        # Ac4
        } else {
            gamma_new <- cbind(gamma_new, X, Y, Z)
        }
      }

      if (sum(A)==1){
        c <- B
      }

    }

    gamma <- gamma_new

    if (.matrixEquals(B_old,B)) {
      break
    }

  }

  #Ac5
  if (sum(A)==1){
    gamma <- cbind(gamma, A, .difference2(B,A), B)
  } else {
    gamma <- cbind(gamma, A, .difference2(B,C), C)
  }

  return(gamma)

}

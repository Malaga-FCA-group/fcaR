.shorten_FDB <- function (A,B,C, gamma) {

  # Check if arguments are correct
  if (is.null(gamma) || is.null(A) || is.null(B) || is.null(C)) {
    stop("Some argument introduced in shorten_FDB is NULL")
  }

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

    return(cbind(A,B,C))

  }

}

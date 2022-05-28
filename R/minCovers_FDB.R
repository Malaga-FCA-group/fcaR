.minCovers_FDB <- function(A,B,C,gamma){

  # Check if arguments are correct
  if (is.null(gamma)) {
    stop("Gamma introduced in minCovers_FDB is NULL")
  }

  res <- .fix_FDB(A,B,C,gamma)
  mnl <- res[1]
  gamma <- res[2]

  phi <- NULL

  mult3_gamma <- dim(gamma)[2]/3

  for ( ind_mul3_gamma in 1:mult3_gamma ) {

    gamma_ind <-(ind_mul3_gamma-1) *3

    X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)

    if(isElementOf_FDB(X,mnl)){

      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      psi <- .union( cbind(X,Y,Z), .minCovers_FDB(X,Y,Z,gamma) )

      phi <- .join_FDB(phi,psi)
    }

  }

  return(phi)


}

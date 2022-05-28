.join_FDB <- function (gamma1, gamma2) {


  gamma1_new <- NULL
  gamma2_new <- NULL

  if(!is.null(gamma1)){

    mult3_gamma1 <- dim(gamma1)[2]/3

    for ( ind_mul3_gamma1 in 1:mult3_gamma1 ) {

      gamma_ind <-(ind_mul3_gamma1-1) *3

      X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      gamma1_new <- .union(gamma1_new, .shorten_FDB(X,Y,Z,gamma2))

    }

  }

  if(!is.null(gamma2)){

    mult3_gamma2 <- dim(gamma2)[2]/3

    for ( ind_mul3_gamma2 in 1:mult3_gamma2 ) {


      gamma_ind <-(ind_mul3_gamma2-1) *3

      X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      gamma2_new <- .union(gamma2_new, .shorten_FDB(X,Y,Z,gamma1_new))

    }

  }

  return(.union(gamma1_new, gamma2_new))

}

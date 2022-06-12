#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the Join function in the FastD-Basis algorithm
#'
#' @param gamma1, @param gamma2
#' It is a sparse matrix in which contains 3-tuples of elements represented by
#' 3 binded columns together.
#'
#' @return
#' Returns an sparse matrix in which contains the union of the gamma1 and gamma2
#' simplified by shorten

.join_FDB <- function (gamma1, gamma2) {


  gamma1_new <- NULL
  gamma2_new <- NULL

  if(!is.null(gamma1)){

    mult3_gamma1 <- dim(gamma1)[2]/3

    for ( ind_mul3_gamma1 in 1:mult3_gamma1 ) {

      gamma_ind <-(ind_mul3_gamma1-1) *3

      X <- Matrix(gamma1[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma1[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma1[, gamma_ind+3], sparse = TRUE)

      sh <- .shorten_FDB(X,Y,Z,gamma2)

      if(!is.null(sh) && is.null(gamma1_new)){
        gamma1_new <- sh
      } else if (!is.null(sh)){
        gamma1_new <- .union(sh,gamma1_new)
      }

    }

  }

  if(!is.null(gamma2)){

    mult3_gamma2 <- dim(gamma2)[2]/3

    for ( ind_mul3_gamma2 in 1:mult3_gamma2 ) {

      gamma_ind <-(ind_mul3_gamma2-1) *3

      X <- Matrix(gamma2[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma2[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma2[, gamma_ind+3], sparse = TRUE)

      sh <- .shorten_FDB(X,Y,Z,gamma1_new)

      if(!is.null(sh) && is.null(gamma2_new)){
        gamma2_new <- sh
      } else if (!is.null(sh)){
        gamma2_new <- .union(sh,gamma2_new)
      }

    }

  }

  res <- NULL

  if(is.null(gamma1_new) && is.null(gamma2_new)){
    res <- NULL
  } else if(is.null(gamma1_new)){
    res <- gamma2_new
  } else if(is.null(gamma2_new)){
    res <- gamma1_new
  } else {
    res <- .union(gamma1_new,gamma2_new)
  }

  return(res)

}

.showTuple <- function(gamma, attributes){
  gam_toString <- "{"

  columns <- dim(gamma)[2]/3

  for ( ind_mul3_gamma in 1:columns ) {

    gamma_ind <- (ind_mul3_gamma-1) *3

    X <- as.logical(Matrix(gamma[, gamma_ind+1], sparse = TRUE))
    Y <- as.logical(Matrix(gamma[, gamma_ind+2], sparse = TRUE))
    Z <- as.logical(Matrix(gamma[, gamma_ind+3], sparse = TRUE))

    X <- paste(attributes[X], collapse = "")
    Y <- paste(attributes[Y], collapse = "")
    Z <- paste(attributes[Z], collapse = "")

    if(ind_mul3_gamma != columns){
      gam_toString <- paste(gam_toString, "<", X, ", ", Y, ", ", Z, ">", ", ", sep = "")
    } else {
      gam_toString <- paste(gam_toString, "<", X, ", ", Y, ", ", Z, ">", "}", sep = "")
    }



  }

  return (gam_toString)
}

# A, B y C son conjuntos de atributos seleccionados


# gamma = 1_x,1_y, 1_z, 2_x, 2_y, 2_Z !!!!! X, Y, Z different to  empty set math

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


      gamma_ind <-(ind-1) *3

      X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
      Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
      Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

      if (.matrixEquals(A,X)){
        B <- .union(B, .union(Y,Z) )
        C <- .union(C,Z)
      } else {

        if (all(.subset(X,B))){
          B <- .union(B, .union(Y,Z) )
        }

        if(!(.matrixEquals(A,X)) && all(.subset(A,X))){

          if(!all(.subset(Y,B))){
            gamma_new <- cbind(gamma_new, X, .difference2(Y,B), .union(Z,C))
          }

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

  if (sum(A)==1){
    gamma <- cbind(gamma, A, .difference2(B,A), B)
  } else {
    gamma <- cbind(gamma, A, .difference2(B,C), C)
  }

  return(gamma)

}

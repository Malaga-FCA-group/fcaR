
.fix_FDB <- function( A, B, C, gamma) {

  # Check if arguments are correct
  if (is.null(gamma)) {
    stop("Some argument introduced in fix_FDB is NULL")
  }

  gamma_new <- NULL
  minimals <- NULL
  mult3_gamma <- dim(gamma)[2]/3

  for ( ind_mul3_gamma in 1:mult3_gamma ) {

    gamma_ind <-(ind_mul3_gamma-1) *3

    X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
    Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
    Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

    X <- .union(A, .difference2(X, .union(B,C) ))
    Y <- .difference2(Y, .union(B,C) )
    Z <- .union(C, .difference2(Z,B) )

    if (sum(Y) != 0) {
      gamma_new <- cbind(gamma_new, X, Y, Z)
    }

    # Check if there is not exist
    stopComprobe <- TRUE
    cont <- 1
    len_Minimals <- dim(minimals)[2]

    while ( stopComprobe && (cont <= len_Minimals) ) {

      W <- Matrix( minimals[,cont], sparse = TRUE )

      if(.subset(W,X)){
        stopComprobe <- FALSE
      }

      cont <- cont + 1
    }

    if(stopComprobe == TRUE){

      minimals_aux <- NULL

      # Remove section

      if(len_Minimals != 0) {

        for(cont in 1:len_Minimals){

          V <- Matrix( minimals[,cont], sparse=TRUE )

          if(!(.subset(X,V))){
            minimals_aux <- cbind(minimals_aux,V)
          }

        }

        minimals <- minimals

      }

      # Add section

      minimals <- cbind(minimals,X)

    }

  }

  len_Minimals <- dim(minimals)[2]

  for(cont in 1:len_Minimals){
    X <- Matrix(minimals[,cont], sparse = TRUE)
    gamma_new <- .addClosure_FDB(X,gamma_new)
  }

  return(minimals,gamma_new)
}


.fix_FDB <- function( A, B, C, gamma) {

  # Check if arguments are correct
  if (is.null(gamma)) {
    stop("Some argument introduced in fix_FDB is NULL")
  }

  # Creation of attributes
  gamma_new <- NULL
  minimals <- NULL
  mult3_gamma <- dim(gamma)[2]/3

  # Initialize the first time A,B,C
  if(is.null(A)){
    A <- Matrix(matrix(0,dim(gamma)[1],1))
  }

  if(is.null(B)){
    B <- Matrix(matrix(0,dim(gamma)[1],1))
  }

  if(is.null(C)){
    C <- Matrix(matrix(0,dim(gamma)[1],1))
  }

  # We do the loop <X,Y,Z> E gamma
  for ( ind_mul3_gamma in 1:mult3_gamma ) {

    # As the matrix gamma is a set which element is a tuple of 3 elements
    # we use multiples of 3 to initialize X,Y,Z
    gamma_ind <-(ind_mul3_gamma-1) *3

    X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
    Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
    Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

    # We do the 3 initialize in F1
    X <- .union(A, .difference2(X, .union(B,C) ))
    Y <- .difference2(Y, .union(B,C) )
    Z <- .union(C, .difference2(Z,B) )

    # We do the if
    if (sum(Y) != 0) {
      gamma_new <- cbind(gamma_new, X, Y, Z)
    }

    # Check if there is not exist, F2
    stopComprobe <- TRUE
    cont <- 1
    len_Minimals <- dim(minimals)[2]

    # If minimals is NULL, we couldn't enter in the while loop
    if(!is.null(minimals)){

      while ( stopComprobe && (cont <= len_Minimals) ) {

        W <- Matrix( minimals[,cont], sparse = TRUE )

        if(all(.subset(W,X))){
          stopComprobe <- FALSE
        }

        cont <- cont + 1
      }

    } else {
      len_Minimals <- 0
    }


    # If stopComprobe is TRUE means that not exists W E Minimals ...
    if(stopComprobe == TRUE){

      minimals_aux <- NULL

      # Remove section (Remove certains columns is equivalent to create another matrix without them)

      if(len_Minimals != 0) {

        for(cont in 1:len_Minimals){

          V <- Matrix( minimals[,cont], sparse=TRUE )

          if(!all(.subset(X,V))){
            minimals_aux <- cbind(minimals_aux,V)
          }

        }

        minimals <- minimals_aux

      }

      # Add section

      minimals <- cbind(minimals,X)

    }

  }

  len_Minimals <- dim(minimals)[2]

  # The last for
  for(cont in 1:len_Minimals){
    X <- Matrix(minimals[,cont], sparse = TRUE)
    gamma_new <- .addClosure_FDB(X,gamma_new)
  }

  return(list(minimals,gamma_new))
}

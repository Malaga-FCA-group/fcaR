#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the Fix function in the FastD-Basis algorithm
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
#' @param attr
#' It's a vector that contains the attributes of sigma
#' @return
#' Returns a 2-tuple of sparse matrices (minimals(mx1) and gamma) resultant of apply the function

.fix_FDB <- function(A, B, C, gamma, attr) {

  # Check if arguments are correct. First iteration A, B,C are NULL
  if (is.null(gamma)) {
   stop("Gamma introduced in fix_FDB is NULL")
  }

  # Creation of attributes
  gamma_new <- NULL
  mnl <- NULL

  # Initialize the first time A,B,C
  if(is.null(A)){
    A <- Matrix(matrix(0,dim(gamma)[1],1), sparse = TRUE)
  }

  if(is.null(B)){
    B <- Matrix(matrix(0,dim(gamma)[1],1), sparse = TRUE)
  }

  if(is.null(C)){
    C <- Matrix(matrix(0,dim(gamma)[1],1), sparse = TRUE)
  }

  mult3_gamma <- dim(gamma)[2]/3

  # We do the loop <X,Y,Z> E gamma
  for ( ind_mul3_gamma in 1:mult3_gamma ) {

    # As the matrix gamma is a set which element is a tuple of 3 elements
    # we use multiples of 3 to initialize X,Y,Z
    gamma_ind <-(ind_mul3_gamma-1) *3

    X <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
    Y <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
    Z <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)

    # We do the 3 initialize in F1
    un <- .union(B,C)
    X  <- .union(A, .difference2(X, un))
    Y  <- .difference2(Y, un)
    Z  <- .union(C, .difference2(Z,B) )

    # We do the if
    if (sum(Y) != 0) {
      gamma_new <- cbind(gamma_new, X, Y, Z)
    }


    if(is.null(mnl)){
      mnl <- X
    } else{

      bool <- any(apply(mnl, MARGIN = 2,
                        FUN = function(col, X){
                          W <- Matrix(col, sparse = TRUE);
                          return( all(.subset(W,X)) )
                        }, X))

      # If bool is TRUE means that not exists W E Minimals ...
      if(!bool){

        # We see here which columns don't meet the condition to get them
        res <- apply(mnl, MARGIN = 2,
                     FUN = function(col, X){
                       V <- Matrix(col, sparse = TRUE);
                       return( !all(.subset(X,V)) )
                     }, X)

        # Here, we stay with the columns above and add the X at the minimals
        mnl <- cbind(mnl[,res],X)

      }

    }

  }

  len_mnl <- dim(mnl)[2]

  # The last for
  for(cont in 1:len_mnl){
    X <- Matrix(mnl[,cont], sparse = TRUE)
    gamma_new <- .addClosure_FDB(X,gamma_new, attr)
  }



  return(list(mnl,gamma_new))
}

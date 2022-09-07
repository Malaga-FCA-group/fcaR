#' @author
#' Nicolas Felipe Trujillo Montero
#'
#' @Title
#' Application of the simplification function in the Simplify algorithm
#'
#' @param sigma_lhs
#' Sparse Matrix equivalent to implications$get_LHS_matrix()
#'
#' @param sigma_rhs
#' Sparse Matrix equivalent to implications$get_RHS_matrix()
#'
#' @param attr
#' It's a vector that contains the attributes of sigma
#
#' @return
#' Implication base simplified
#'
#' @examples
#' See in test or vignettes.

.simplifyDOB <- function(sigma_lhs, sigma_rhs, attr) {

  # Check if arguments are correct
  if (is.null(sigma_lhs) || is.null(sigma_rhs) || is.null(attr)) {
    stop("Some argument introduced in simplifyDOB is NULL")
  }


  # 1. Initialize sigma again
  sigma_prim_lhs <- NULL
  sigma_prim_rhs <- NULL

  numImplicaciones <- dim(sigma_lhs)[2]

  for ( ind in 1:numImplicaciones ) {

    A <- Matrix(sigma_lhs[,ind], sparse=TRUE)
    B <- Matrix(sigma_rhs[,ind], sparse=TRUE)

    if( !(all(.subset(B,A))) ) {
        sigma_prim_lhs <- cbind(sigma_prim_lhs, A)
        sigma_prim_rhs <- cbind(sigma_prim_rhs, .difference2(B,A))
      }

  }

  sigma_lhs <- sigma_prim_lhs
  sigma_rhs <- sigma_prim_rhs

  repeat {

    # We use in each iteration of the repeat loop a system of flags
    # to check if the sSigma and the sigma are equal.
    flagEQ <- TRUE

    # Inicializacion de sigmaDO (Direct-Optimal Basis) y sigma
    sSigma_lhs <- sigma_lhs
    sSigma_rhs <- sigma_rhs

    sigma_lhs <- NULL
    sigma_rhs <- NULL

    numImplicaciones_S <- dim(sSigma_lhs)[2]

    # foreach A -> B
    for ( ind_s in 1:numImplicaciones_S ) {

      A <- Matrix(sSigma_lhs[,ind_s], sparse=TRUE)
      B <- Matrix(sSigma_rhs[,ind_s], sparse=TRUE)

      gamma_lhs <- NULL
      gamma_rhs <- NULL

      # Primera iteracion => 0 columnas sigma
      if(!is.null(sigma_lhs)){

        numImplicaciones <- dim(sigma_lhs)[2]

        for (ind in 1:numImplicaciones) {

          C <- Matrix(sigma_lhs[,ind], sparse=TRUE)
          D <- Matrix(sigma_rhs[,ind], sparse=TRUE)

          # 2
          if ( ( all( .subset(C,A) ) && all( .subset( A, .union(C,D) ) ) ) ||
               ( all( .subset(A,C) ) && all( .subset( C, .union(A,B) ) ) ) ) {

            int <- A*C
            uni <- .union(B,D)

            # We check if the "int = A" and "uni = B"
            if( (!.columnEquals(A,int)) || (!.columnEquals(B,uni)) ) {
             flagEQ <- FALSE
            }

            A <- int
            B <- uni

          } else {

              # 3
              if ( all( .subset(A,C) ) && !( .columnEquals(A,C) ) ) {

                if ( !( all(.subset(D,B)) ) ) {

                  diff1 <- .difference2(C,B)
                  diff2 <- .difference2(D,B)

                  # We check if the "diff1 = C" and "diff2 = D"
                  if( (!.columnEquals(C,diff1)) || (!.columnEquals(D,diff2)) ) {
                     flagEQ <- FALSE
                  }

                  gamma_lhs <- cbind( gamma_lhs, diff1 )
                  gamma_rhs <- cbind( gamma_rhs, diff2 )

                }

              }

              else {

                  # 4
                  if( all(.subset(C,A)) && !(.columnEquals(C,A)) ) {

                    diff1 <- .difference2(A,D)
                    diff2 <- .difference2(B,D)

                    # We check if the "diff1 = C" and "diff2 = D"
                    if(!.columnEquals(A,diff1) || !.columnEquals(B,diff2)){
                     flagEQ <- FALSE
                    }

                    A <- diff1
                    B <- diff2

                  }

                  gamma_lhs <- cbind( gamma_lhs, C )
                  gamma_rhs <- cbind( gamma_rhs, D )

              }

            }

          }

        }

      # 5
      if (sum(B) == 0) {

        sigma_lhs <- gamma_lhs
        sigma_rhs <- gamma_rhs

      } else {

        sigma_lhs <- cbind(gamma_lhs, A)
        sigma_rhs <- cbind(gamma_rhs, B)

      }

    }

   # We use a flag to check the equality because the equality in sets
   # isn't efficient (State if it's important the order)
   if ( flagEQ ){
     break
   }

   # if (.matrixEquals(sSigma_lhs, sigma_lhs) &&
   #     .matrixEquals(sSigma_rhs, sigma_rhs)){
   #      break
   #    }

  }

  return(list(sSigma_lhs, sSigma_rhs))

}

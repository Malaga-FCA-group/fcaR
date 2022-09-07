#' @author
#' Nicolás Felipe Trujillo Montero
#'
#' @Title
#' Application of the slGetDo function in the Simplify algorithm
#'
#' @param sigma_lhs
#' Sparse Matrix equivalent to implications$get_LHS_matrix()
#'
#' @param sigma_rhs
#' Sparse Matrix equivalent to implications$get_RHS_matrix()
#'
#' @param attr
#' It's a vector that contains the attributes of sigma
#'
#' @return
#' Equivalent direct-optimal implicational system
#'
#' @examples
#' See in test or vignettes.

.slGetDo <- function(sigma_lhs, sigma_rhs, attr) {

  # Check if arguments are correct
  if (is.null(sigma_lhs) || is.null(sigma_rhs) || is.null(attr)) {
    stop("Some argument introduced in SLGetDo is NULL")
  }

  listaSigma <- .simplifyDOB(sigma_lhs, sigma_rhs, attr)
  sigma_lhs <- listaSigma[[1]]
  sigma_rhs <- listaSigma[[2]]

  repeat {

    # We use in each iteration of the repeat loop a system of flags
    # to check if the SigmaDO and the sigma are equal.
    flagEQ <- TRUE

    # Inicializacion de sigmaDO (Direct-Optimal Basis) y sigma
    sigmaDO_lhs <- sigma_lhs
    sigmaDO_rhs <- sigma_rhs

    sigma_lhs <- NULL
    sigma_rhs <- NULL

    numImplicaciones_DO <- dim(sigmaDO_lhs)[2]

    for ( ind_DO in 1:numImplicaciones_DO ) {

        A <- Matrix(sigmaDO_lhs[,ind_DO], sparse=TRUE)
        B <- Matrix(sigmaDO_rhs[,ind_DO], sparse=TRUE)

        gamma_lhs <- NULL
        gamma_rhs <- NULL

        if(!is.null(sigma_lhs)){

          numImplicaciones <- dim(sigma_lhs)[2]

          for (ind in 1:numImplicaciones) {

            C <- Matrix(sigma_lhs[,ind], sparse=TRUE)
            D <- Matrix(sigma_rhs[,ind], sparse=TRUE)

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

                  if( all(.subset(C,A)) && !(.columnEquals(C,A))) {

                    diff1 <- .difference2(A,D)
                    diff2 <- .difference2(B,D)

                    # We check if the "diff1 = C" and "diff2 = D"
                    if(!.columnEquals(A,diff1) || !.columnEquals(B,diff2)){
                        flagEQ <- FALSE
                    }

                    A <- diff1
                    B <- diff2

                  }

                  lista  <- .add_sSimp(A,B,C,D, sigma_lhs, sigma_rhs)
                  lista2 <- .add_sSimp(C,D,A,B, sigma_lhs, sigma_rhs)

                  # We check if the previous flag with the returned flags
                  # of the add_sSimp
                  flagEQ <- flagEQ && lista[[2]] && lista2[[2]]

                  E1 <- lista[[1]][[1]]
                  F1 <- lista[[1]][[2]]
                  E2 <- lista2[[1]][[1]]
                  F2 <- lista2[[1]][[2]]

                  gamma_lhs <- cbind( gamma_lhs, C, E1, E2 )
                  gamma_rhs <- cbind( gamma_rhs, D, F1, F2 )

                }

            }

          }

        }


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
   if (flagEQ){
     break
   }

  # if (.matrixEquals(sigmaDO_lhs, sigma_lhs) &&
  #     .matrixEquals(sigmaDO_rhs, sigma_rhs)){
  #     break
  # }

  }

  return( list(sigmaDO_lhs, sigmaDO_rhs) )
}

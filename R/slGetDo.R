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
#' @return
#' Equivalent direct-optimal implicational system
#'
#' @examples
#' See in test or vignettes.
#'

.slGetDo <- function(sigma_lhs, sigma_rhs) {

  # Check if arguments are correct
  if (is.null(sigma_lhs) || is.null(sigma_rhs)) {
    stop("Some argument introduced in SLGetDo is NULL")
  }

  listaSigma <- .simplifyDOB(sigma_lhs, sigma_rhs)
  sigma_lhs <- listaSigma[[1]]
  sigma_rhs <- listaSigma[[2]]

  repeat {

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

            if ( ( all( .subset(C,A) ) && all( .subset( A, .union(C,D) ) ) ) || ( all( .subset(A,C) ) && all( .subset( C, .union(A,B) ) ) ) ) {

              int <- A*C
              uni <- .union(B,D)

              if( (!.matrixEquals(A,int)) || (!.matrixEquals(B,uni)) ) {
                flagEQ <- FALSE
              }

              A <- int
              B <- uni

            } else {

              if ( all( .subset(A,C) ) && !( .matrixEquals(A,C) ) ) {

                if ( all(!(.subset(D,B))) ) {

                  diff1 <- .difference2(C,B)
                  diff2 <- .difference2(D,B)

#                  if( (!.matrixEquals(C,diff1)) || (!.matrixEquals(D,diff2)) ) {
#                    flagEQ <- FALSE
#                  }

                  gamma_lhs <- cbind( gamma_lhs, diff1 )
                  gamma_rhs <- cbind( gamma_rhs, diff2 )

                } else {

                  if( all(.subset(C,A)) && !(.matrixEquals(C,A))) {

                    diff1 <- .difference2(A,D)
                    diff2 <- .difference2(B,D)

                    if(!.matrixEquals(A,diff1) || !.matrixEquals(B,diff2)){
                      flagEQ <- FALSE
                    }

                    A <- diff1
                    B <- diff2

                  }

                  # .add_sSimp <- function(A, B, C, D, sigma_lhs, sigma_rhs)
                  lista  <- .add_sSimp(A,B,C,D, sigma_lhs, sigma_rhs)
                  lista2 <- .add_sSimp(C,D,A,B, sigma_lhs, sigma_rhs)

                  if(!is.null(lista) || !is.null(lista2)){
                    flagEQ <- FALSE
                  }

                  gamma_lhs <- cbind( gamma_lhs, C, lista[[1]], lista2[[1]] )
                  gamma_rhs <- cbind( gamma_rhs, D, lista[[2]], lista2[[2]] )

                }

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


    if (flagEQ){
      break
    }


  }

  return( list(sigmaDO_lhs, sigmaDO_rhs) )

}

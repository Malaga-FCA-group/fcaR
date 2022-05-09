#' @Title
#' Application of the slGetDo function in the Simplify algorithm
#'
#' @param sigma_lhs
#' Sparse Matrix equivalent to implications$get_LHS_matrix()
#'
#' @param sigma_rhs
#' Sparse Matrix equivalent to implications$get_RHS_matrix()
#'
#' @param attributes
#' Los necesito???
#'
#' @return
#' Equivalent direct-optimal implicational system
#'
#' @examples
#' TODO
#'

.slGetDo <- function(sigma_lhs, sigma_rhs, attributes) {
  listaSigma <- .simplifyDOB(sigma_lhs, sigma_rhs, attributes)
  sigma_lhs <- listaSigma[1]
  sigma_rhs <- listaSigma[2]

  repeat {

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

      numImplicaciones <- dim(sigma_lhs)[2]

      if(!is.null(numImplicaciones)){
        for (ind in 1:numImplicaciones) {

          C <- Matrix(sigma_lhs[,ind], sparse=TRUE)
          D <- Matrix(sigma_rhs[,ind], sparse=TRUE)

          if ( ( all( .subset(C,A) ) && all( .subset( A, .union(C,D) ) ) ) || ( all( .subset(A,C) ) && all( .subset( C, .union(A,B) ) ) ) ) {

            A <- A*C
            B <- .union(B,D)

          } else {

            if ( all( .subset(A,C) ) && !( .matrixEquals(A,C) ) ) {

              if ( all(!(.subset(D,B))) ) {

                gamma_lhs <- cbind( gamma_lhs, .difference2(C,B) )
                gamma_rhs <- cbind( gamma_rhs, .difference2(D,B) )

              } else {

                if( all(.subset(C,A)) && !(.matrixEquals(C,A)) ) { # Quitar el equals aqui?

                  A <- .difference2(A,D)
                  B <- .difference2(B,D)

                }

                # .add_sSimp <- function(A, B, C, D, sigma_lhs, sigma_rhs)
                lista  <- .add_sSimp(A,B,C,D, sigma_lhs, sigma_rhs)

                lista2 <- .add_sSimp(C,D,A,B, sigma_lhs, sigma_rhs)


                gamma_lhs <- cbind( gamma_lhs, C, lista[1], lista2[1] )
                gamma_rhs <- cbind( gamma_rhs, D, lista[1], lista2[1] )

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

    # Until ¿Me fijo en el orden de las columnas?
    if ( ( .matrixEquals(sigmaDO_lhs, sigma_lhs) ) && ( .matrixEquals(sigmaDO_rhs, sigma_rhs) ) ){
      break
    }

  }

  return(sigmaDO_lhs, sigmaDO_rhs)

}

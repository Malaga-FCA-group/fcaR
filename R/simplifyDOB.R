#' @Title
#' Application of the simplification function in the Simplify algorithm
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
#' Implication base simplified
#'
#' @examples
#' TODO
#'

.simplifyDOB <- function(sigma_lhs, sigma_rhs, attributes) {



  # Check if arguments are correct
  if (is.null(sigma_lhs) || is.null(sigma_rhs) || is.null(attributes) ) {
    stop("Some argument introduced in simplifyDOB is NULL")
  }


  # 1
  sigma_prim_lhs <- NULL
  sigma_prim_rhs <- NULL

  numImplicaciones <- dim(sigma_lhs)[2]

  for ( ind in 1:numImplicaciones ) {

    A <- Matrix(sigma_lhs[,ind], sparse=TRUE)
    B <- Matrix(sigma_rhs[,ind], sparse=TRUE)

    if( all(!(.subset(B,A))) ) {
        sigma_prim_lhs <- cbind(sigma_prim_lhs,A)
        sigma_prim_rhs <- cbind(sigma_prim_rhs, .difference2(B,A))
      }

  }

  sigma_lhs <- sigma_prim_lhs
  sigma_rhs <- sigma_prim_rhs

  #2
  repeat {

    # Inicializacion de sigmaDO (Direct-Optimal Basis) y sigma
    sSigma_lhs <- sigma_lhs
    sSigma_rhs <- sigma_rhs

    sigma_lhs <- NULL
    sigma_rhs <- NULL

    numImplicaciones_S <- dim(sSigma_lhs)[2]

    for ( ind_s in 1:numImplicaciones_S ) {

      A <- Matrix(sSigma_lhs[,ind_s], sparse=TRUE)
      B <- Matrix(sSigma_rhs[,ind_s], sparse=TRUE)

      gamma_lhs <- NULL
      gamma_rhs <- NULL

      numImplicaciones <- dim(sigma_lhs)[2]

      # Primera iteracion = 0 longitud
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

                  gamma_lhs <- cbind( gamma_lhs, C )
                  gamma_rhs <- cbind( gamma_rhs, D )

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
        sigma_rhs <- cbind(gamma_rhs,B)

      }

    }


    # Until ¿Me fijo en el orden de las columnas?
    if ( ( .matrixEquals(sSigma_lhs, sigma_lhs) ) && ( .matrixEquals(sSigma_rhs, sigma_rhs) ) ){
      break
    }
  }

  return(list(sSigma_lhs, sSigma_rhs))

  }

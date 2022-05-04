#' @Title
#' Application of the add_sSimp function in the SLgetdo algorithm
#'
#' @param A
#' Antecedent in the implication A -> B, represented by a sparse matrix (column vector)
#'
#' @param B
#' Consequent in the implication A -> B, represented by a sparse matrix (column vector)
#'
#' @param C
#' Antecedent in the implication C -> D, represented by a sparse matrix (column vector)
#'
#' @param D
#' Consequent in the implication C -> D, represented by a sparse matrix (column vector)
#'
#' @param sigma_lhs
#' Sparse Matrix equivalent to implications$get_LHS_matrix()
#'
#' @param sigma_rhs
#' Sparse Matrix equivalent to implications$get_RHS_matrix()
#'
#' @return
#' A list of lhs and rhs "E -> F"
#'
#' @examples
#' TODO
#'
#' CTRL + SHIFT + ALT + R
#  CBIND NULL + LHS
#  isredundant


.add_sSimp <- function(A, B, C, D, sigma_lhs, sigma_rhs) {

  # Check if arguments are correct
  if (is.null(implicacion1) || is.null(implicacion2) || is.null(sigma) ) {
    stop("Some argument introduced in Add_sSimp is NULL")
  }


  inters <- B*C
  diff_aux <- .difference2( D, (.union(A,B)) )

  if ( !( .subset(A,C) ) && ( sum(B*C) != 0 ) && ( sum( diff_aux ) != 0 ) && ( .matrixEquals(inters, diff_aux) ) ) {

    E <- .union(A, .difference2(C,B))
    F <- diff_aux

    numImplicaciones <- dim(sigma_lhs)[2] # Se puede usar sigma_rhs tmbn

    for (ind in 1:numImplicaciones) {

      X <- Matrix(sigma_lhs[,ind], sparse = TRUE)
      Y <- Matrix(sigma_rhs[,ind], sparse = TRUE)

      # .subset = equals or contains
      if ( .subset(X,E) ) {

          if( .subset(F,Y) ) {

            return(NULL) # NULL = Empty

          } else {

            E <- .difference2(E,Y)
            F <- .difference2(F,Y)

          }

      }
    }

    # Return a list with lhs and rhs instead of a implication
    return (list(E,F))

  }


  return(NULL)
}

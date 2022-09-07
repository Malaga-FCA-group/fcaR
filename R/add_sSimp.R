#' @author
#' Nicolas Felipe Trujillo Montero
#'
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
#' A list of lhs and rhs "E -> F" and the flag
#'
#' @examples
#' See in test or vignettes.
#'
#' @note
#' CTRL + SHIFT + ALT + R to add the skeleton description
#' .subset = equals or contains
#' NULL == Empty set


.add_sSimp <- function(A, B, C, D, sigma_lhs, sigma_rhs) {

  # Check if arguments are correct
  if (is.null(A) || is.null(B) || is.null(C) || is.null(D)
      || is.null(sigma_lhs) || is.null(sigma_rhs) ) {
    stop("Some argument introduced in Add_sSimp is NULL")
  }

  # We make the two parameters
  inters <- B*C
  diff_aux <- .difference2( D, (.union(A,B)) )

  # Equals between inters and diff_aux doesn't require
  if ( !( all(.subset(A,C)) ) && ( sum(inters) != 0 ) &&
        ( sum( diff_aux ) != 0 ) ## && !(.columnEquals(inters,diff_aux))
       ) {

    # 1
    E <- .union(A, .difference2(C,B))

    # 2
    F <- diff_aux

    numImplicaciones <- dim(sigma_lhs)[2] # It could be possible use sigma_rhs too

    for (ind in 1:numImplicaciones) {

      X <- Matrix(sigma_lhs[,ind], sparse = TRUE)
      Y <- Matrix(sigma_rhs[,ind], sparse = TRUE)

      if ( all(.subset(X,E)) ) {

          if( all(.subset(F,Y)) ) {

            return(list(NULL,TRUE))

          } else {

            E <- .difference2(E,Y)
            F <- .difference2(F,Y)

          }

      }
    }

    # We apply a technique of using flags checking if the new implication
    # is different to a combination with the implication passed as parameters
    impl <- list(E,F)
    flag <- TRUE

    if(    (.columnEquals(E,A) && .columnEquals(F,B))
        || (.columnEquals(E,A) && .columnEquals(F,D))
        || (.columnEquals(E,C) && .columnEquals(F,B))
        || (.columnEquals(E,C) && .columnEquals(F,D))
        ) {
      flag <- FALSE
    }

    return(list(impl,flag))

  } else {
     return(list(NULL,TRUE))
  }


}

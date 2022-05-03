#' @Title
#' Application of the simplification function in the Simplify algorithm
#'
#' @param sigma_lhs
#' List of lhs of the set of implications
#'
#' @param sigma_rhs
#' List of rhs of the set of implications
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


  # 1 Plantearlo de nuevo sigma no es una lista sino una matriz lhs y rhs
  sigma_prim_lhs <- list()
  sigma_prim_rhs <- list()

  for ( ind in (1:length(sigma_lhs)) ) {

    A <- Matrix(sigma_lhs[,ind], sparse=TRUE)
    B <- Matrix(sigma_rhs[,ind], sparse=TRUE)

    if(!(.subset(B,A))) {
        sigma_prim_lhs <- append(sigma_prim_lhs,  A)
        sigma_prim_rhs <- append(sigma_prim_rhs, .difference2(B,A))
      }

  }

  sigma_lhs <- sigma_prim_lhs
  sigma_rhs <- sigma_prim_rhs

  #2
  repeat {

    sSigma_lhs <- sigma_lhs
    sSigma_rhs <- sigma_rhs

    sigma_lhs <- NULL
    sigma_rhs <- NULL

    for ( ind_s in ( 1:length(sigma_prim_lhs) ) ) {
      # Arreglar
      A <- sigma_prim_lhs[ind_s]
      B <- sigma_prim_rhs[ind_s]

      gamma_lhs <- NULL
      gamma_rhs <- NULL

      if (length(sigma_lhs)!=0){

      }

      if (sum(B) == 0) {
        sigma_lhs <- gamma_lhs
        sigma_rhs <- gamma_rhs
      } else {
        sigma_lhs <- cbind(gamma_lhs, A)
        sigma_rhs <- cbind(gamma_rhs,B)
      }
    }


    # Until
    if ( ( .matrixEquals(sSigma_lhs, sigma_lhs) ) && ( .matrixEquals(sSigma_rhs, sigma_rhs) ) ){
      break
    }
  }

  }

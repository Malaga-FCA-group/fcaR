#' Title
#'
#' @param A
#' @param B
#' @param C
#' @param D
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' CTRL + SHIFT + ALT + R
#  CBIND NULL + LHS
#  isredundant


.add_sSimp <- function(A, B, C, D, sigma) {

  # Implication1 = A -> B
  # Implication2 = c -> D
  # sigma = Implication set



  solution <- NULL

  # Check if arguments are correct
  if (is.null(implicacion1) | is.null(implicacion2) | is.null(sigma) ) {

    stop("Some argument introduced in Add_sSimp is NULL")

  }






  # Attribute in the left part of the first implication in form of Sparse Matrix
  A <- implication1$get_LHS_matrix()
  B <- implication1$get_RHS_matrix()
  C <- implication2$get_LHS_matrix()
  D <- implication2$get_RHS_matrix()



  if (!(.subset(A,C)) & (sum(B*C) != 0)) {}

  return(NULL)
}

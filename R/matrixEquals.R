#' @author
#' Nicolas Felipe Trujillo Montero
#'
#' @title
#' Function Column Equals: check the equality of 2 sparse set
#' IMPORTANT: X and Y must be column vector
#'
#' @param x
#' It is a sparse matrix
#'
#' @param y
#' It is a sparse matrix
#'
#' @return
#' Returns a boolean that express if the matrices are equals or not.
#'
#' @examples
#' X <- Matrix(c(1,0,0,1), nrow = 2, ncol=2, sparse=TRUE)
#' .matrixEquals(NULL,NULL) == TRUE
#' .matrixEquals(X, X) == TRUE
#' .matrixEquals(cbind(X,X), X) == FALSE
#' .matrixEquals(X, cbind(X,x)) == FALSE

.matrixEquals <- function(A, B){
  
  bool <- FALSE
  
  if(is.null(A) && is.null(B)){
    bool <- TRUE
  } else if (is.null(A)){
    bool <- FALSE
  } else if (is.null(B)){
    bool <- FALSE
  } else {

    numImplicacionesA <- dim(A)[2]
    numImplicacionesB <- dim(B)[2]
    
    if(numImplicacionesA == numImplicacionesB){
    
      for ( indA in 1:numImplicacionesA ) {
        
        X <- Matrix(A[,indA], sparse=TRUE)
        
        indB <- 1
        halt <- FALSE
        
        while(indB <= numImplicacionesB && !halt){
          Y <- Matrix(B[,indB], sparse=TRUE)
          
          if(.columnEquals(X,Y)){
            halt <- TRUE
            B <- Matrix(B[,-indB], sparse = TRUE)
            numImplicacionesB <- dim(B)[2]
          } else{
            indB <- indB+1
          }
          
        }
        
        if(!halt){
          break
        }
        
        if(halt && dim(B)[2]==0){
          bool <- TRUE
          break
        }
  
      }
      
    }
  
  }
  
  return(bool)
}
#' @author
#' Nicolas Felipe Trujillo Montero
#'
#' @Title
#' Application of the algorithm doSimp
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
#'

.doSimp <- function(sigma_lhs, sigma_rhs, attr) {
  
  # Check if arguments are correct
  if (is.null(sigma_lhs) || is.null(sigma_rhs) || is.null(attr)) {
    stop("Some argument introduced in doSimp is NULL")
  }
  
  # Stage 1: Generation of sigmar by reduction of sigma
  sigma_r_lhs <- NULL
  sigma_r_rhs <- NULL
  
  numImplicaciones <- dim(sigma_lhs)[2]
  
  for ( ind in 1:numImplicaciones ) {
    
    A <- Matrix(sigma_lhs[,ind], sparse=TRUE)
    B <- Matrix(sigma_rhs[,ind], sparse=TRUE)
    
    if( !(all(.subset(B,A))) ) {
      sigma_r_lhs <- cbind(sigma_r_lhs, A)
      sigma_r_rhs <- cbind(sigma_r_rhs, .difference2(B,A))
    }
    
  }
  
  # Stage 2: Generation of sigmasr by simplification of sigmar 
  
  sigma_sr_lhs <- sigma_r_lhs
  sigma_sr_rhs <- sigma_r_rhs
  
  repeat {
    
    # flagEQ <- TRUE
    sigma_bef_sr_lhs <- sigma_sr_lhs
    sigma_bef_sr_rhs <- sigma_sr_rhs
    
    numImplicaciones <- dim(sigma_sr_lhs)[2]
    
    for ( ind_sr1 in 1:numImplicaciones ) {
      
      A <- Matrix(sigma_sr_lhs[,ind_sr1], sparse=TRUE)
      B <- Matrix(sigma_sr_rhs[,ind_sr1], sparse=TRUE)

      for ( ind_sr2 in 1:numImplicaciones ) {
          
          C <- Matrix(sigma_sr_lhs[,ind_sr2], sparse=TRUE)
          D <- Matrix(sigma_sr_rhs[,ind_sr2], sparse=TRUE)
          
          if ( all(.subset(A,C))) {
            
            if( all( .subset(C, .union(A,B)) ) ){
              sigma_sr_lhs <- Matrix(sigma_sr_lhs[,-c(ind_sr1,ind_sr2)])
              sigma_sr_rhs <- Matrix(sigma_sr_lhs[,-c(ind_sr1,ind_sr2)])
              
              sigma_sr_lhs <- cbind(sigma_sr_lhs, A)
              sigma_sr_rhs <- cbind(sigma_sr_rhs, .union(B,D))
            
            } else if ( all(.subset(D,B)) ){
              
              sigma_sr_lhs <- Matrix(sigma_sr_lhs[,-ind_sr2])
              sigma_sr_rhs <- Matrix(sigma_sr_lhs[,-ind_sr2])
            
            } else {
              
              sigma_sr_lhs[, ind_sr2] <- .difference2(C,B)
              sigma_sr_rhs[, ind_sr2] <- .difference2(D,B)
            }

          }
        }
        
      }
   
    # ????????????????????????? No entiendo
    
      if (.matrixEquals(sigma_bef_sr_lhs, sigma_sr_lhs) &&
          .matrixEquals(sigma_bef_sr_rhs, sigma_sr_rhs)){
        break
      }
      
  }
  
  # Stage 3: Generation of sigma_dsr by completion of sigma_sr
  sigma_dsr_lhs <- sigma_sr_lhs
  sigma_dsr_rhs <- sigma_sr_rhs
  
  numImplicaciones <- dim(sigma_dsr_lhs)[2]
  
  for ( ind_dsr1 in 1:numImplicaciones ) {
    
    A <- Matrix(sigma_dsr_lhs[,ind_sr1], sparse=TRUE)
    B <- Matrix(sigma_dsr_rhs[,ind_sr1], sparse=TRUE)
    
    for ( ind_dsr2 in 1:numImplicaciones ) {
      
      C <- Matrix(sigma_dsr_lhs[,ind_sr2], sparse=TRUE)
      D <- Matrix(sigma_dsr_rhs[,ind_sr2], sparse=TRUE)
      
      if ( ( sum(B*C) != 0 ) && ( .difference2(D, .union(A,B)) ) ) {
        sigma_dsr_lhs <- cbind(sigma_dsr_lhs, .difference2( .union(A,C),B) )
        sigma_dsr_rhs <- cbind(sigma_dsr_rhs, .difference2( D, .union(A,B)) )
      }
    
    }
  
  }
  
  # Stage 4: Generation of ??do by optimization of ??dsr
  
  sigma_do_lhs <- NULL
  sigma_do_rhs <- NULL
  
  for ( ind_dsr1 in 1:numImplicaciones ) {
    
    A <- Matrix(sigma_dsr_lhs[,ind_sr1], sparse=TRUE)
    B <- Matrix(sigma_dsr_rhs[,ind_sr1], sparse=TRUE)
    
    for ( ind_dsr2 in 1:numImplicaciones ) {
      
      C <- Matrix(sigma_dsr_lhs[,ind_sr2], sparse=TRUE)
      D <- Matrix(sigma_dsr_rhs[,ind_sr2], sparse=TRUE)
  
      if (.columnEquals(C,A)) {
        B <- .union(B,D)
      }
      
      if (.subset(C,A)){ # Not necessary inequals
        B <- .difference2(B,D)
      }
    
    }
    
    if (sum(B) != 0){
      sigma_do_lhs <- cbind(sigma_do_lhs, A)
      sigma_do_rhs <- cbind(sigma_do_rhs, B)
    }
  
  }
  
    
    
  # if (flagEQ){
  #   break
  # }
    
  
  return( list(sigma_do_lhs, sigma_do_rhs) )
  
}

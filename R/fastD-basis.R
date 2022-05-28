.fDB <- function(sigma_lhs,sigma_rhs, attributes){

  if(is.null(sigma_lhs) || is.null(sigma_rhs) || is.null(attributes)){
    stop("Some argument introduced in FDB is NULL")
  }

  # 1.- Creation of gamma
  gamma <- NULL
  len <- dim(sigma_lhs)[2]

  for(index in 1:len){

    X <- Matrix(sigma_lhs[,index], sparse=TRUE)
    Y <- Matrix(sigma_rhs[,index], sparse=TRUE)

    if(!.subset(Y,X)){
      gamma <- cbind(gamma,X,.difference2(Y,X),X)
    }

  }

  # 2.- Creation of Phi
  phi <- .minCovers_FDB(NULL,NULL,NULL,gamma)

  # 3.- Creation of sigma bin and n
  mult3_phi <- dim(phi)[2]/3
  sigma_bin_lhs <- NULL
  sigma_bin_rhs <- NULL
  sigma_n_lhs <- NULL
  sigma_n_rhs <- NULL

  for ( index in 1:mult3_phi ) {

    gamma_ind <-(ind_mul3_gamma-1) *3

    A <- Matrix(gamma[, gamma_ind+1], sparse = TRUE)
    B <- Matrix(gamma[, gamma_ind+2], sparse = TRUE)
    C <- Matrix(gamma[, gamma_ind+3], sparse = TRUE)


    if(sum(A)==1){
      sigma_bin_lhs <- cbind(sigma_bin_lhs,A)
      sigma_bin_rhs <- cbind(sigma_bin_rhs,B)
    } else {
      sigma_n_lhs <- cbind(sigma_n_lhs,A)
      sigma_n_rhs <- cbind(sigma_n_rhs,B)
    }
  }

  return(sigma_bin_lhs,sigma_bin_rhs,sigma_n_lhs,sigma_n_rhs)

}

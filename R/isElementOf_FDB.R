isElementOf_FDB <- function (X, domain) {

  bool <- FALSE

  if( !(is.null(X) || is.null(domain)) ){
    size_domain <- dim(domain)[2]


    if(dim(domain)[1]==dim(domain)[1] ){
      for(index in 1:size_domain){
        elem <- Matrix(domain[,index], sparse=TRUE)

        if(.matrixEquals(X,elem)){
          bool <- TRUE
          break
        }

      }
    }

  }


  return(bool)


}

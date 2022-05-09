.matrixEquals <- function(x, y){

  res <- NULL

  if(is.null(x) && is.null(y)){
    res <- TRUE
  } else if (is.null(x)){
    res <- FALSE
  } else if (is.null(y)){
    res <- FALSE
  } else {
    res <- ( all(dim(x) == dim(y)) && (all(x == y)) )
  }
  return (res)
}

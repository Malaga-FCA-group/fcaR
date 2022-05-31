#------------------------------------------------------------------------------
# FastD-Basis
#------------------------------------------------------------------------------
# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)
A <- Matrix(c(1,0,0), sparse = TRUE)
gamma <- cbind(A,A,A)

res <- cbind(gamma,A,.difference2(A,A),A)

res_aux <- .addClosure_FDB(A,gamma)

#-------------------------------------------------------------------------------
A <- Matrix(c(1,1,1), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,0,1), sparse = TRUE)
gamma <- cbind(A,B,C,A,B,C)
res <- cbind(A,.difference2(B,C),C)

.addClosure_FDB(C,gamma)

#-------------------------------------------------------------------------------

A <- Matrix(c(1,1,1), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,0,1), sparse = TRUE)
gamma <- cbind(A,C,C,B,A,A)

.fix_FDB(A,B,C,gamma)

gamma_new <-Matrix(c(1,1,1,0,0,0,1,1,1),3,3, sparse = TRUE)

#-------------------------------------------------------------------------------

A <- Matrix(c(1,0,0), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,1,1), sparse = TRUE)
gamma <- cbind(A,C,C,B,A,A)

.shorten_FDB(A,B,C,gamma)

#-------------------------------------------------------------------------------
A <- Matrix(c(1,1,0), sparse = TRUE)
B <- Matrix(c(1,1,1), sparse = TRUE)
C <- Matrix(c(0,1,1), sparse = TRUE)
gamma <- cbind(A,B,C)

.join_FDB(gamma,NULL)

#-------------------------------------------------------------------------------

A <- Matrix(c(1,1,1), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,0,1), sparse = TRUE)
gamma <- cbind(A,C,C,B,A,A)

.minCovers_FDB(A,B,C,gamma)

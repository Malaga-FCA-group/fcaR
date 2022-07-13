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

A <- Matrix(c(1,0,0), sparse = TRUE)
gamma <- cbind(A,A,A)

.addClosure_FDB(NULL,NULL) # NULL
.addClosure_FDB(A,NULL) # , Matrix(c(1,0,0, 0,0,0, 1,0,0),3,3,sparse = TRUE))
.addClosure_FDB(NULL,gamma)# , Matrix(c(1,0,0, 0,0,0, 1,0,0),3,3,sparse = TRUE))

#-------------------------------------------------------------------------------
A <- Matrix(c(1,1,1), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,0,1), sparse = TRUE)
gamma <- cbind(A,B,C,A,B,C)
res <- cbind(A,.difference2(B,C),C)

.addClosure_FDB(C,gamma)

#-------------------------------------------------------------------------------

.fix_FDB(NULL,NULL,NULL,NULL)
gamma <- Matrix(c(1,0,0, 0,0,1, 0,1,1), 3, 3, sparse = TRUE)
.fix_FDB(NULL,NULL,NULL,gamma)

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

.join_FDB(NULL,NULL)
.join_FDB(gamma,NULL)
.join_FDB(NULL,gamma)

#-------------------------------------------------------------------------------

A <- Matrix(c(1,1,0), sparse = TRUE)
B <- Matrix(c(1,1,1), sparse = TRUE)
C <- Matrix(c(0,1,1), sparse = TRUE)
gamma <- cbind(A,B,C)

.join_FDB(gamma,gamma)

#-------------------------------------------------------------------------------

A <- Matrix(c(1,1,1), sparse = TRUE)
B <- Matrix(c(1,0,1), sparse = TRUE)
C <- Matrix(c(0,0,1), sparse = TRUE)
gamma <- cbind(A,C,C,B,A,A)

#.minCovers_FDB(A,B,C,gamma)

#-------------------------------------------------------------------------------

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

input2 <- system.file("Implications", "ex1", package = "fcaR")
imps2 <- parse_implications(input2)
imps2

sigma_lhs <- imps2$get_LHS_matrix() # ordered by attributes randomly
sigma_rhs <- imps2$get_RHS_matrix()


fil <- dim(sigma_lhs)[1]
col <- dim(sigma_lhs)[2]

# Redefinition of lhs
lhs <- Matrix(c(0,0,0,0,1, 0,1,1,0,0, 0,1,0,1,0, 0,0,1,1,0, 1,0,0,1,0, 0,1,0,0,1, 0,0,1,0,1, 1,0,0,0,1), fil, col, sparse = TRUE)
dimnames(lhs)[[1]] <- c('1','2','3','4','5')

# Redefinition of rhs

rhs <- Matrix(c(0,0,0,1,0, 0,0,0,1,0, 0,0,1,0,0, 0,1,0,0,0, 0,1,0,0,1, 1,0,1,0,0, 0,1,0,0,0, 0,1,0,0,0), fil, col, sparse = TRUE)
dimnames(rhs)[[1]] <- c('1','2','3','4','5')

attr <- imps2$get_attributes()

imp_simp <- .algorithm_FDB(lhs,rhs)
res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])

#------------------------------------------------------------
input <- system.file("Implications", "ex1", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output1 <- system.file("Implications", "ex1_bin", package = "fcaR")
imp_out_bin <- parse_implications(output1)
imp_out_bin

output2 <- system.file("Implications", "ex1_n", package = "fcaR")
imp_out_n <- parse_implications(output2)
imp_out_n







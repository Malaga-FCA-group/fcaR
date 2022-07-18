# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

input2 <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imps2 <- parse_implications(input2)
imps2

sigma_lhs <- imps2$get_LHS_matrix() # ordered by attributes randomly
sigma_rhs <- imps2$get_RHS_matrix()


# fil <- dim(sigma_lhs)[1]
# col <- dim(sigma_lhs)[2]
#
# # Redefinition of lhs
# lhs <- Matrix(c(0,0,0,0,1, 0,1,1,0,0, 0,1,0,1,0, 0,0,1,1,0, 1,0,0,1,0, 0,1,0,0,1, 0,0,1,0,1, 1,0,0,0,1), fil, col, sparse = TRUE)
# dimnames(lhs)[[1]] <- c('1','2','3','4','5')
#
# # Redefinition of rhs
#
# rhs <- Matrix(c(0,0,0,1,0, 0,0,0,1,0, 0,0,1,0,0, 0,1,0,0,0, 0,1,0,0,1, 1,0,1,0,0, 0,1,0,0,0, 0,1,0,0,0), fil, col, sparse = TRUE)
# dimnames(rhs)[[1]] <- c('1','2','3','4','5')

attr <- imps2$get_attributes()

imp_simp <- .doSimp(sigma_lhs,sigma_rhs,attr)

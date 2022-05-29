###############################################################################
# Prueba sin test
###############################################################################
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)
# profiz
dfPrueba <- FormalContext$new(planets)
dfPrueba$find_implications()
implicacionesPrueba <- dfPrueba$implications
lhs <- implicacionesPrueba$get_LHS_matrix()
rhs <- implicacionesPrueba$get_RHS_matrix()
attr <- implicacionesPrueba$get_attributes()

imp_simp <- .slGetDo(lhs,rhs)


# Build the ImplicationSet
res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])
res$cardinality()
#???


# ----------------

input2 <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imps2 <- parse_implications(input2)
imps2

#-------------------

sigma_lhs <- imps2$get_LHS_matrix()
sigma_rhs <- imps2$get_RHS_matrix()
attr <- imps2$get_attributes()

imp_simp <- .slGetDo(sigma_lhs,sigma_rhs)
res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])

#--------------------
ind1 <- 1
ind2 <- 2
A <- Matrix(sigma_lhs[,ind1], sparse=TRUE)
B <- Matrix(sigma_rhs[,ind1], sparse=TRUE)
C <- Matrix(sigma_lhs[,ind2], sparse=TRUE)
D <- Matrix(sigma_rhs[,ind2], sparse=TRUE)


aux <- .add_sSimp(A,B,C,D, sigma_lhs, sigma_rhs)
ImplicationSet$new(attributes = attr, lhs = aux[[1]], rhs = aux[[2]])

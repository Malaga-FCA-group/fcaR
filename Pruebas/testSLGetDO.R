###############################################################################
# Prueba sin test
###############################################################################
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(fcaR)
library(Matrix)
# profiz
dfPrueba <- FormalContext$new(planets)
dfPrueba$find_implications()
implicacionesPrueba <- dfPrueba$implications
lhs <- implicacionesPrueba$get_LHS_matrix()
rhs <- implicacionesPrueba$get_RHS_matrix()
attr <- implicacionesPrueba$get_attributes()

imp_simp <- .slGetDo(lhs,rhs,attr)


# Build the ImplicationSet
res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])
res$cardinality()
#???


# ----------------

input2 <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imps2 <- parse_implications(input2)
imps2

#-------------------

lhs <- imps2$get_LHS_matrix()
rhs <- imps2$get_RHS_matrix()
attr <- imps2$get_attributes()

imp_simp <- .slGetDo(lhs,rhs,attr)
res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])


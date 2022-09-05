###############################################################################
# Prueba sin test
###############################################################################
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)
# # profiz
# dfPrueba <- FormalContext$new(planets)
# dfPrueba$find_implications()
# implicacionesPrueba <- dfPrueba$implications
# lhs <- implicacionesPrueba$get_LHS_matrix()
# rhs <- implicacionesPrueba$get_RHS_matrix()
# attr <- implicacionesPrueba$get_attributes()
#
# imp_simp <- .slGetDo(lhs,rhs)
#
#
# # Build the ImplicationSet
# res <- ImplicationSet$new(attributes = attr, lhs = imp_simp[[1]], rhs = imp_simp[[2]])
# res$cardinality()
# #???


# ----------------
# Input
input <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imp_in <- parse_implications(input)
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output
output <- system.file("Implications", "ex_implicationsDBO_sol", package = "fcaR")
imp_out <- parse_implications(output)
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )


res <- .slGetDo(imp_in$get_LHS_matrix(), imp_in$get_RHS_matrix(), imp_in$get_attributes())
ImplicationSet$new(lhs=res[[1]],rhs=res[[2]],attributes=attrSorted)


#--------------------
ind1 <- 1
ind2 <- 2
A <- Matrix(sigma_lhs[,ind1], sparse=TRUE)
B <- Matrix(sigma_rhs[,ind1], sparse=TRUE)
C <- Matrix(sigma_lhs[,ind2], sparse=TRUE)
D <- Matrix(sigma_rhs[,ind2], sparse=TRUE)


aux <- .add_sSimp(A,B,C,D, sigma_lhs, sigma_rhs)
ImplicationSet$new(attributes = attr, lhs = aux[[1]], rhs = aux[[2]])

#--------------------
#
# # Example Paper Fast Direct-Optimal Basis
#
# # We have an example to check the output that corresponds to the written example
# # in the article "Formation of the D-basis from implicational
# # systems using Simplification logic"
#
# # Input
# input <- system.file("Implications", "ex_implicationsFDOB", package = "fcaR")
# imp_in <- parse_implications(input)
# imp_in
#
# # Output
# output <- system.file("Implications", "ex_implicationsFDOB_sol", package = "fcaR")
# imp_out <- parse_implications(output)
# imp_out
#
# # We have to prepare the data because the order of the attributes is incorrect
# # Input correct
# attrSorted <- sort(imp_in$get_attributes())
# sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
# sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
# imp_in_ex2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )
#
# # Output correct
# attrSorted <- sort(imp_out$get_attributes())
# sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
# sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
# imp_out_ex2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )
#
# res <- .slGetDo(imp_in_ex2$get_LHS_matrix(), imp_in_ex2$get_RHS_matrix(), imp_in_ex2$get_attributes())
# imp_fin <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attrSorted )
#
#
#
#


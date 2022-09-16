# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

# Input
input <- system.file("Implications", "ex_implicationsDOB", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex_implicationsDOB_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_DOB <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_DOB <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

imp_simp <- .slGetDo(sigma_lhs = imp_in_ex_DOB$get_LHS_matrix(),
                           sigma_rhs = imp_in_ex_DOB$get_RHS_matrix(),
                           attr = imp_in_ex_DOB$get_attributes())
res <- ImplicationSet$new(attributes = imp_in_ex_DOB$get_attributes(), lhs = imp_simp[[1]], rhs = imp_simp[[2]])

res  %~% imp_out_ex_DOB

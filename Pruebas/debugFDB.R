# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

# Input
input <- system.file("Implications", "ex_implicationsFDOB", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex_implicationsFDOB_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_FDOB <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_FDOB <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )


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



imp_simp <- .algorithm_FDB(sigma_lhs = imp_in_ex_FDOB$get_LHS_matrix(),
                           sigma_rhs = imp_in_ex_FDOB$get_RHS_matrix(),
                           attr = imp_in_ex_FDOB$get_attributes())


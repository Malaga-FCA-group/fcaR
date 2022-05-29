###############################################################################
# Prueba sin test
###############################################################################

# Libraries
library(Matrix)
library(usethis)
library(devtools)
# Execute this command line the first time: devtools::load_all()

# Initialize the Formal Context
df <- planets
fc <- FormalContext$new(df)
fc$find_implications()
sigma <- fc$implications
sigma_lhs <- sigma$get_LHS_matrix()
sigma_rhs <- sigma$get_RHS_matrix()
attributes <- sigma$get_attributes()

# Exec the algorithm
res <- .algorithm_FDB(sigma_lhs,sigma_rhs,attributes)
bin <- ImplicationSet$new(lhs = res[[1]], rhs = res[[2]], attributes = attributes)
n <- ImplicationSet$new(lhs = res[[3]], rhs = res[[4]], attributes = attributes)


# ----------------

input2 <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imps2 <- parse_implications(input2)
imps2

#-------------------

sigma_lhs <- imps2$get_LHS_matrix()
sigma_rhs <- imps2$get_RHS_matrix()
attr <- imps2$get_attributes()

imp_simp <- .algorithm_FDB(sigma_lhs,sigma_rhs,attr)

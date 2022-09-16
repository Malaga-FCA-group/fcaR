#' @author Nicolás Felipe Trujillo Montero

# In this test, we are going to check if all functions in the DOB algorithm,
# (Direct-Optimal basis and Fast Direct-Optimal basis) pass the unitary tests.

###############################################################################
# Libraries to Load                                                           #
###############################################################################

library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

###############################################################################
# Inputs and Outputs of Examples                                              #
###############################################################################

# Planets Dataframe

# 1.- Make a fc through a dataframe
fc <- FormalContext$new(planets)

# 2.- Find the implications based on the formal context
fc$find_implications()

# 3.- Save the implications, the left part (antecedent), the right
# part (consequent) and the attributes
imp_planets <- fc$implications

###############################################################################

# Example Paper Direct-Optimal Basis (ex_DOB)

# We have an example to check the output that corresponds to the written example
# in the article "Direct-optimal basis computation by means of the fusion of
# simplification rules"

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

###############################################################################

# Example Paper Fast Direct-Optimal Basis (ex_FDOB)

# We have an example to check the output that corresponds to the written example
# in the article "Formation of the D-basis from implicational
# systems using Simplification logic"

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

###############################################################################

# Example 1 (ex1)

# Input
input <- system.file("Implications", "ex1", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex1_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_1 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_1 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

###############################################################################

# Example 2 (ex2)

# Input
input <- system.file("Implications", "ex2", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex2_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

###############################################################################

# Example 3 (ex3)

# Input
input <- system.file("Implications", "ex3", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex3_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_3 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_3 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

###############################################################################

# Example 4 (ex_Inf_Sci)

# Input
input <- system.file("Implications", "ex_Inf_Sci", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex_Inf_Sci_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

# We have to prepare the data because the order of the attributes is incorrect
# Input correct
attrSorted <- sort(imp_in$get_attributes())
sigma_lhs_Sorted <- imp_in$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_in$get_RHS_matrix()[attrSorted,]
imp_in_ex_Inf_Sci <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex_Inf_Sci <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )


###############################################################################
# Test Add-sSimp                                                              #
###############################################################################

test_that("Add-sSimp stops the program if input arguments are incorrect.", {

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  A <- Matrix(lhs[,1], sparse=TRUE)
  B <- Matrix(rhs[,1], sparse=TRUE)
  C <- Matrix(lhs[,2], sparse=TRUE)
  D <- Matrix(rhs[,2], sparse=TRUE)

  expect_error(.add_sSimp(NULL,B,C,D,lhs,rhs))
  expect_error(.add_sSimp(A,NULL,C,D,lhs,rhs))
  expect_error(.add_sSimp(A,B,NULL,D,lhs,rhs))
  expect_error(.add_sSimp(A,B,C,NULL,lhs,rhs))
  expect_error(.add_sSimp(A,B,C,D,NULL,rhs))
  expect_error(.add_sSimp(A,B,C,D,lhs,NULL))

})

test_that("Comprobe that it is possible that add-sSimp returns NULL." ,{

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  A <- Matrix(lhs[,1], sparse=TRUE)
  B <- Matrix(rhs[,1], sparse=TRUE)
  C <- Matrix(lhs[,2], sparse=TRUE)
  D <- Matrix(rhs[,2], sparse=TRUE)

  expect_null(.add_sSimp(A,B,C,D,lhs,rhs)[[1]])
  expect_true(.add_sSimp(A,B,C,D,lhs,rhs)[[2]])

})

test_that("Comprobe that add-sSimp works fine.", {

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  A_p <- Matrix(c(1,0,0,1), sparse=TRUE)
  B_p <- Matrix(c(0,1,0,0), sparse=TRUE)
  C_p <- Matrix(c(0,1,1,0), sparse=TRUE)
  D_p <- Matrix(c(0,0,1,0), sparse=TRUE)

  expect_false(is.null(.add_sSimp(A_p,B_p,C_p,D_p,lhs,rhs)[[1]]))

  E_p <- Matrix(c(0,0,1,1), sparse=TRUE)
  F_p <- Matrix(c(0,0,1,0), sparse=TRUE)
  res <- list(E_p,F_p)

  expect_equal(.add_sSimp(A_p,B_p,C_p,D_p,lhs,rhs)[[1]], res)
})


###############################################################################
# Test Simplify and SLGetDo                                                   #
###############################################################################

test_that("Simplify stops the program if input arguments are incorrect.", {

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  expect_error(.simplifyDOB(NULL,NULL))
  expect_error(.simplifyDOB(lhs,NULL))
  expect_error(.simplifyDOB(NULL,rhs))

})

test_that("Comprobe that Simplify works fine.", {

  # Test with 1 implication {ac => bc}
  lhs <- Matrix(c(1,0,1), sparse = TRUE)
  rhs <- Matrix(c(0,1,1), sparse = TRUE)
  attr <- c('a','b','c')
  expect_equal(.simplifyDOB(lhs, rhs, attr), list(lhs, .difference2(rhs,lhs)))

})

test_that("SLGetDo stops the program if input arguments are incorrect.", {

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  expect_error(.slGetDo(NULL,NULL))
  expect_error(.slGetDo(lhs,NULL))
  expect_error(.slGetDo(NULL,rhs))

})

test_that("Comprobe that SLGetDo works fine.", {

  # Test with 1 implication
  lhs <- Matrix(c(1,0,1), sparse = TRUE)
  rhs <- Matrix(c(0,1,1), sparse = TRUE)
  attr <- c('a','b','c')
  expect_equal(.slGetDo(lhs, rhs, attr), list(lhs, .difference2(rhs,lhs)))

})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_DOB).", {
  lhs_ini <- imp_in_ex_DOB$get_LHS_matrix()
  rhs_ini <- imp_in_ex_DOB$get_RHS_matrix()
  attr <- imp_in_ex_DOB$get_attributes()

  lhs_fin <- imp_out_ex_DOB$get_LHS_matrix()
  rhs_fin <- imp_out_ex_DOB$get_RHS_matrix()

  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  expect_true(.matrixEquals(res[[1]],lhs_fin) && .matrixEquals(res[[2]],rhs_fin))
})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_FDOB).", {
 lhs_ini <- imp_in_ex_FDOB$get_LHS_matrix()
 rhs_ini <- imp_in_ex_FDOB$get_RHS_matrix()
 attr <- imp_in_ex_FDOB$get_attributes()

 res <- .slGetDo(lhs_ini, rhs_ini, attr)
 sol <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attr )
 expect_true(sol %~% imp_out_ex_FDOB)
})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_1).", {
  lhs_ini <- imp_in_ex_1$get_LHS_matrix()
  rhs_ini <- imp_in_ex_1$get_RHS_matrix()
  attr <- imp_in_ex_1$get_attributes()


  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  sol <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attr )
  expect_true( sol %~% imp_out_ex_1 )
})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_2).", {
  lhs_ini <- imp_in_ex_2$get_LHS_matrix()
  rhs_ini <- imp_in_ex_2$get_RHS_matrix()
  attr <- imp_in_ex_2$get_attributes()

  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  sol <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attr )
  expect_true( sol %~% imp_out_ex_2 )
})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_3).", {
  lhs_ini <- imp_in_ex_3$get_LHS_matrix()
  rhs_ini <- imp_in_ex_3$get_RHS_matrix()
  attr <- imp_in_ex_3$get_attributes()

  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  sol <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attr )
  expect_true( sol %~% imp_out_ex_3 )
})

test_that("Comprobe that the DOB algorithm works fine with an real example (ex_Inf_Sci).", {
  lhs_ini <- imp_in_ex_Inf_Sci$get_LHS_matrix()
  rhs_ini <- imp_in_ex_Inf_Sci$get_RHS_matrix()
  attr <- imp_in_ex_Inf_Sci$get_attributes()

  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  sol <- ImplicationSet$new(lhs=res[[1]], rhs=res[[2]], attributes = attr )
  expect_true( sol %~% imp_out_ex_Inf_Sci )
})

###############################################################################
# Test AddClosure                                                             #
###############################################################################

test_that("AddClosure works fine if input arguments are NULL.",{
  A <- Matrix(c(1,0,0), sparse = TRUE)
  gamma <- cbind(A,A,A)

  expect_error(.addClosure_FDB(NULL,NULL))
  expect_error(.addClosure_FDB(A,NULL))
  expect_error(.addClosure_FDB(NULL,gamma))

})

test_that("Comprobe that AddClosure works fine.",{
  A <- Matrix(c(1,0,0), sparse = TRUE)
  gamma <- cbind(A,A,A)

  res <- cbind(A,.difference2(A,A),A)

  expect_equal(.addClosure_FDB(A,gamma), res)

})

test_that("Comprobe that AddClosure works fine 2.",{
  A <- Matrix(c(1,1,1), sparse = TRUE)
  B <- Matrix(c(1,0,1), sparse = TRUE)
  C <- Matrix(c(0,0,1), sparse = TRUE)
  gamma <- cbind(A,B,C,A,B,C)
  res <- Matrix(c(1,1,1, 1,0,0, 0,0,1, 1,1,1, 1,0,0, 0,0,1, 0,0,1, 0,0,0, 0,0,1),3,9,sparse =TRUE)

  expect_equal(.addClosure_FDB(C,gamma), res)
})

###############################################################################
# Test Fix                                                                    #
###############################################################################

test_that("Fix works fine if gamma are NULL.",{

  expect_error(.fix_FDB(NULL,NULL,NULL,NULL))

})

test_that("Fix works fine if A,B,C are NULL.",{

  X <- Matrix(c(1,0,0,1),sparse = TRUE)

  gamma <- cbind(X,X,X,X,X,X)
  mnl <- X
  gamma_new <- .addClosure_FDB(X,gamma)

  expect_equal(.fix_FDB(NULL,NULL,NULL,gamma),list(mnl,gamma_new))

})

test_that("Comprobe that fix works fine.",{
  A <- Matrix(c(1,1,1), sparse = TRUE)
  B <- Matrix(c(1,0,1), sparse = TRUE)
  C <- Matrix(c(0,0,1), sparse = TRUE)
  gamma <- cbind(A,C,C,B,A,A)

  mnl <- Matrix(c(1,1,1), sparse = TRUE)
  gamma_new <-Matrix(c(1,1,1,0,0,0,1,1,1),3,3, sparse = TRUE)

  res <- list(mnl,gamma_new)

  expect_equal(res, .fix_FDB(A,B,C,gamma))
})

###############################################################################
# Test Shorten                                                                #
###############################################################################

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("Shorten works fine if input arguments are NULL.", {

 X <- Matrix(c(1,0,0), sparse = TRUE)
 expect_error(.shorten_FDB(NULL,X,X,X))
})

test_that("Comprobe that shorten works fine 1.",{
  A <- Matrix(c(1,0,0), sparse = TRUE)
  B <- Matrix(c(1,0,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)
  gamma <- cbind(A,C,C,B,A,A)

  expect_equal(cbind(A,B,C), .shorten_FDB(A,B,C,gamma))

})

test_that("Comprobe that shorten works fine 2.",{
  A <- Matrix(c(1,1,0), sparse = TRUE)
  B <- Matrix(c(1,1,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)
  gamma <- cbind(A,B,C)

  expect_equal(NULL, .shorten_FDB(A,B,C,gamma))
})

###############################################################################
# Test Join                                                                   #
###############################################################################

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("Join works fine if input arguments are NULL.",{
  A <- Matrix(c(1,1,0), sparse = TRUE)
  B <- Matrix(c(1,1,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)
  gamma <- cbind(A,B,C)

  expect_equal(.join_FDB(NULL,NULL),NULL)
  expect_equal(.join_FDB(gamma,NULL),gamma)
  expect_equal(.join_FDB(NULL,gamma),gamma)

})

test_that("Comprobe that join works fine.",{
  A <- Matrix(c(1,1,0), sparse = TRUE)
  B <- Matrix(c(1,1,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)
  gamma <- cbind(A,B,C)

  expect_equal(gamma, .join_FDB(gamma,NULL))
})

###############################################################################
# Test Min-Covers                                                             #
###############################################################################

library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("Min-Covers works fine if input arguments are NULL.",{
  A <- Matrix(c(1,1,0), sparse = TRUE)
  B <- Matrix(c(1,1,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)
  gamma <- cbind(A,B,C)

  expect_error(.minCovers_FDB(A,B,C,NULL))

})

test_that("Min-Covers works fine with base condition.",{
  A <- Matrix(c(0,0,1,0,1), sparse = TRUE)
  B <- Matrix(c(1,1,0,0,0), sparse = TRUE)
  C <- Matrix(c(0,0,1,1,1), sparse = TRUE)

  D <- Matrix(c(0,1,0,0,1), sparse = TRUE)
  E <- Matrix(c(1,0,1,0,0), sparse = TRUE)
  F <- Matrix(c(0,1,0,1,1), sparse = TRUE)

  G <- Matrix(c(1,0,0,0,1), sparse = TRUE)
  H <- Matrix(c(0,1,1,0,0), sparse = TRUE)
  I <- Matrix(c(1,0,0,1,1), sparse = TRUE)

  gamma <- cbind(A,B,C,D,E,F,G,H,I)

  expect_null(.minCovers_FDB(A,B,C,gamma))

})

###############################################################################
# Test FastDBasis Algorithm                                                      #
###############################################################################

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_DOB).", {
  lhs_ini <- imp_in_ex_DOB$get_LHS_matrix()
  rhs_ini <- imp_in_ex_DOB$get_RHS_matrix()
  attrSorted <- imp_in_ex_DOB$get_attributes()

  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_DOB)
})

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_FDOB).", {
  lhs_ini <- imp_in_ex_FDOB$get_LHS_matrix()
  rhs_ini <- imp_in_ex_FDOB$get_RHS_matrix()
  attrSorted <- imp_in_ex_FDOB$get_attributes()

  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_FDOB)
})

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_1).", {
  lhs_ini <- imp_in_ex_1$get_LHS_matrix()
  rhs_ini <- imp_in_ex_1$get_RHS_matrix()
  attrSorted <- imp_in_ex_1$get_attributes()


  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_1)
})

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_2).", {
  lhs_ini <- imp_in_ex_2$get_LHS_matrix()
  rhs_ini <- imp_in_ex_2$get_RHS_matrix()
  attrSorted <- imp_in_ex_2$get_attributes()

  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_2)
})

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_3).", {
  lhs_ini <- imp_in_ex_3$get_LHS_matrix()
  rhs_ini <- imp_in_ex_3$get_RHS_matrix()
  attrSorted <- imp_in_ex_3$get_attributes()

  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_3)
})

test_that("Comprobe that the FDOB algorithm works fine with an real example (ex_Inf_Sci).", {
  lhs_ini <- imp_in_ex_Inf_Sci$get_LHS_matrix()
  rhs_ini <- imp_in_ex_Inf_Sci$get_RHS_matrix()
  attrSorted <- imp_in_ex_Inf_Sci$get_attributes()

  imp_simp <- .algorithm_FDB(lhs_ini, rhs_ini, attrSorted)
  sol <- ImplicationSet$new(lhs=cbind(imp_simp[[1]],imp_simp[[3]]), rhs=cbind(imp_simp[[2]],imp_simp[[4]]), attributes = attrSorted )
  expect_true(sol %~% imp_out_ex_Inf_Sci)
})

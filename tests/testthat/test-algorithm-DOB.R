#' @author Nicolás Felipe Trujillo Montero

# In this test, we are going to check if all functions in the DBO algorithm,
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

# Example Paper Direct-Optimal Basis

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
imp_in_ex1 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex1 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

###############################################################################

# Example Paper Fast Direct-Optimal Basis

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
imp_in_ex2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )

# Output correct
attrSorted <- sort(imp_out$get_attributes())
sigma_lhs_Sorted <- imp_out$get_LHS_matrix()[attrSorted,]
sigma_rhs_Sorted <- imp_out$get_RHS_matrix()[attrSorted,]
imp_out_ex2 <- ImplicationSet$new(lhs=sigma_lhs_Sorted, rhs=sigma_rhs_Sorted, attributes = attrSorted )


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

  expect_null(.add_sSimp(A,B,C,D,lhs,rhs), NULL)

})

test_that("Comprobe that add-sSimp works fine.", {

  lhs <- imp_planets$get_LHS_matrix()
  rhs <- imp_planets$get_RHS_matrix()

  A_p <- Matrix(c(1,0,0,1), sparse=TRUE)
  B_p <- Matrix(c(0,1,0,0), sparse=TRUE)
  C_p <- Matrix(c(0,1,1,0), sparse=TRUE)
  D_p <- Matrix(c(0,0,1,0), sparse=TRUE)

  expect_false(is.null(.add_sSimp(A_p,B_p,C_p,D_p,lhs,rhs)))

  E_p <- Matrix(c(0,0,1,1), sparse=TRUE)
  F_p <- Matrix(c(0,0,1,0), sparse=TRUE)
  res <- list(E_p,F_p)

  expect_equal(.add_sSimp(A_p,B_p,C_p,D_p,lhs,rhs), res)
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

test_that("Comprobe that the DBO algorithm works fine with an real example.", {
  lhs_ini <- imp_in_ex1$get_LHS_matrix()
  rhs_ini <- imp_in_ex1$get_RHS_matrix()
  attr <- imp_in_ex1$get_attributes()

  lhs_fin <- imp_out_ex1$get_LHS_matrix()
  rhs_fin <- imp_out_ex1$get_RHS_matrix()

  res <- .slGetDo(lhs_ini, rhs_ini, attr)
  expect_true(.matrixEquals(res[[1]],lhs_fin) && .matrixEquals(res[[2]],rhs_fin))
})

# test_that("Comprobe that the DBO algorithm works fine with an real example 2.", {
#   lhs_ini <- imp_in_ex2$get_LHS_matrix()
#   rhs_ini <- imp_in_ex2$get_RHS_matrix()
#   attr <- imp_in_ex2$get_attributes()
#
#   lhs_fin <- imp_out_ex2$get_LHS_matrix()
#   rhs_fin <- imp_out_ex2$get_RHS_matrix()
#
#   res <- .slGetDo(lhs_ini, rhs_ini, attr)
#   expect_true(.matrixEquals(res[[1]],lhs_fin) && .matrixEquals(res[[2]],rhs_fin))
# })


###############################################################################
# Test AddClosure                                                             #
###############################################################################

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("AddClosure works fine if input arguments are NULL.",{
  A <- Matrix(c(1,0,0), sparse = TRUE)
  gamma <- cbind(A,A,A)

  expect_equal(.addClosure_FDB(NULL,NULL), NULL)

  res1 <- Matrix(c(1,0,0, 0,0,0, 1,0,0),3,3,sparse = TRUE)
  expect_equivalent(res1, .addClosure_FDB(A,NULL))

  res2 <- Matrix(c(1,0,0, 1,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0),3,6, sparse = TRUE)
  expect_equal(.addClosure_FDB(NULL,gamma), res2)

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

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("Fix works fine if input arguments are NULL.",{

  expect_equal(.fix_FDB(NULL,NULL,NULL,NULL), list(NULL,NULL))

  gamma <- Matrix(c(1,0,0, 0,0,1, 0,1,1), 3, 3, sparse = TRUE)
  mnl <- Matrix(c(1,0,0),3,1,sparse = TRUE)
  gamma_res <- Matrix(c(1,0,0, 0,1,1, 1,1,1), 3, 3, sparse = TRUE)

  expect_equal(.fix_FDB(NULL,NULL,NULL,gamma),list(mnl,gamma_res))

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

# Columnas vacias??????

###############################################################################
# Test Shorten                                                                #
###############################################################################

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

test_that("Shorten works fine if input arguments are NULL.", {
  A <- Matrix(c(1,0,0), sparse = TRUE)
  B <- Matrix(c(1,0,1), sparse = TRUE)
  C <- Matrix(c(0,1,1), sparse = TRUE)

  expect_error(.shorten_FDB(A,NULL,C,NULL))
  expect_equal(.shorten_FDB(A,B,C,NULL),cbind(A,B,C))
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

test_that("Shorten works fine if input arguments are NULL.",{
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


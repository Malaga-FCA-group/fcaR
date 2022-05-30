#' @author Nicolás Felipe Trujillo Montero

# In this test, we are going to check if all functions in the DBO algorithm,
# (Direct-Optimal basis and Fast Direct-Optimal basis) pass the unitary tests.

###############################################################################
# Test Add-sSimp                                                              #
###############################################################################

# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

# 1.- Make a fc through a dataframe
fc <- FormalContext$new(planets)

# 2.- Find the implications based on the formal context
fc$find_implications()

# 3.- Save the implications, the left part (antecedent), the right
# part (consequent) and the attributes
imp <- fc$implications
lhs <- imp$get_LHS_matrix()
rhs <- imp$get_RHS_matrix()

# res <- .add_sSimp(A,B,C,D,lhs,rhs) # res = NULL

test_that("Add-sSimp stops the program if input arguments are incorrect.", {

  expect_error(.add_sSimp(NULL,B,C,D,lhs,rhs))
  expect_error(.add_sSimp(A,NULL,C,D,lhs,rhs))
  expect_error(.add_sSimp(A,B,NULL,D,lhs,rhs))
  expect_error(.add_sSimp(A,B,C,NULL,lhs,rhs))
  expect_error(.add_sSimp(A,B,C,D,NULL,rhs))
  expect_error(.add_sSimp(A,B,C,D,lhs,NULL))

})

test_that("Comprobe that it is possible that add-sSimp returns NULL." ,{

  A <- Matrix(lhs[,1], sparse=TRUE)
  B <- Matrix(rhs[,1], sparse=TRUE)
  C <- Matrix(lhs[,2], sparse=TRUE)
  D <- Matrix(rhs[,2], sparse=TRUE)

  expect_null(.add_sSimp(A,B,C,D,lhs,rhs), NULL)

})

test_that("Comprobe that add-sSimp works fine.", {

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


# Initialize parameters
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)

# We have an example to check the output that corresponds to the written example
# in the article "Direct-optimal basis computation by means of the fusion of
# simplification rules"

# Input
input <- system.file("Implications", "ex_implicationsDBO", package = "fcaR")
imp_in <- parse_implications(input)
imp_in

# Output
output <- system.file("Implications", "ex_implicationsDBO_sol", package = "fcaR")
imp_out <- parse_implications(output)
imp_out

sigma_lhs <- imp_in$get_LHS_matrix()
sigma_rhs <- imp_in$get_RHS_matrix()
attr <- imp_in$get_attributes()


test_that("Simplify stops the program if input arguments are incorrect.", {

  expect_error(.simplifyDOB(NULL,NULL))
  expect_error(.simplifyDOB(sigma_lhs,NULL))
  expect_error(.simplifyDOB(NULL,sigma_rhs))

})

test_that("Comprobe that Simplify works fine.", {

  # Test with 1 implication
  A <- Matrix(c(1,0,1), sparse = TRUE)
  B <- Matrix(c(0,1,1), sparse = TRUE)
  expect_equal(.simplifyDOB(A,B), list(A,.difference2(B,A)))

})

test_that("SLGetDo stops the program if input arguments are incorrect.", {

  expect_error(.slGetDo(NULL,NULL))
  expect_error(.slGetDo(sigma_lhs,NULL))
  expect_error(.slGetDo(NULL,sigma_rhs))

})

test_that("Comprobe that SLGetDo works fine.", {

  # Test with 1 implication
  A <- Matrix(c(1,0,1), sparse = TRUE)
  B <- Matrix(c(0,1,1), sparse = TRUE)
  expect_equal(.slGetDo(A,B), list(A,.difference2(B,A)))

})

test_that("Comprobe that the DBO algorithm works fine with an real example.", {
  res <- .slGetDo(imp_in$get_LHS_matrix(), imp_in$get_RHS_matrix())
  expect_equal(res, list(imp_out$get_LHS_matrix(),imp_out$get_RHS_matrix()))
})

###############################################################################
# Test AddClosure                                                             #
###############################################################################


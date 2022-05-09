###############################################################################
# Prueba sin test
###############################################################################

dfPrueba <- FormalContext$new(planets)
dfPrueba$find_implications()
implicacionesPrueba <- dfPrueba$implications
lhs <- implicacionesPrueba$get_LHS_matrix()
rhs <- implicacionesPrueba$get_RHS_matrix()

A <- Matrix(lhs[,1], sparse=TRUE)
B <- Matrix(rhs[,1], sparse=TRUE)

C <- Matrix(lhs[,2], sparse=TRUE)
D <- Matrix(rhs[,2], sparse=TRUE)

res <- .add_sSimp(A,B,C,D,lhs,rhs) # res = NULL

A_p <- Matrix(list(1,0,0,1), sparse=TRUE)
B_p <- Matrix(list(0,1,0,0), sparse=TRUE)
C_p <- Matrix(list(0,1,1,0), sparse=TRUE)
D_p <- Matrix(list(0,0,1,0), sparse=TRUE)

res_p <- .add_sSimp(A_p, B_p, C_p, D_p, lhs, rhs)


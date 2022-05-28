###############################################################################
# Prueba sin test
###############################################################################

dfPrueba <- FormalContext$new(planets)
dfPrueba$find_implications()
implicacionesPrueba <- dfPrueba$implications
lhs <- implicacionesPrueba$get_LHS_matrix()
rhs <- implicacionesPrueba$get_RHS_matrix()
attr <- implicacionesPrueba$get_attributes()

imp_simp <- .simplifyDOB(lhs,rhs,attr)

# -----------------------------------------------------------------------------



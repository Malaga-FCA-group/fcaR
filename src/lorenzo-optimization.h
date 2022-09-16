#include <Rcpp.h>
#include "set_operations_galois.h"
#include "subsetting.h"
using namespace Rcpp;

S4 compute_closure_vector(S4 V, S4 I);

S4 compute_closure_matrix(S4 V, S4 I);

SparseVector S4toSparse_fix(S4 A); //It was wrong and Lorenzo fixed it (modified)

NumericVector as_vector_slow(SparseVector v, int nrow, int ncol);

NumericMatrix S4toNumericMatrix(S4 I);

NumericMatrix S4toNumericMatrix2(S4 I);

NumericVector sort_c(NumericVector v);

List compute_grades_c(NumericMatrix mat, int tam);

void populateMatches2(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper);

void populateMatches_binary(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper);

void populateMatchesEqual_binary(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper);

SEXP is_subset_C2(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);

SEXP is_subset_C_binary(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);

SEXP is_equal_set_C_binary(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);

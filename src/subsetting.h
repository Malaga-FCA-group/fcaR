/*
 C implementation of sparse matrix subset
 Author: Ian Johnson
 */

// #include <R.h>
// #include <Rdefines.h>
#include <Rcpp.h>
using namespace Rcpp;


void populateMatches(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper);

void populateMatchesEqual(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper);

IntegerVector self_intersection_C(IntegerVector x_i,
                                  IntegerVector x_p,
                                  IntegerVector y_i,
                                  IntegerVector y_p);

void populateMatchesIntersect(int* matches_for_y, int* x_i, int* x_p, int* y_p, int* y_i, int y_index, int num_rows);

int copyMatches(int* y_matches, int** output_i, int* output_i_length, int* output_i_last);

SEXP is_subset_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);

SEXP intersects_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP OUT_P);

SEXP is_equal_set_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);

IntegerVector which_at_col(IntegerVector x_i, IntegerVector x_p, int col);

NumericVector flatten_sparse_C(IntegerVector p,
                               IntegerVector i,
                               NumericVector x,
                               NumericVector dims);

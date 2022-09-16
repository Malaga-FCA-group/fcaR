#include <Rcpp.h>
#include "lorenzo-optimization.h"
using namespace Rcpp;


//Created by Lorenzo
//Computes the closure using vectors
// [[Rcpp::export]]
S4 compute_closure_vector(S4 V, S4 I) {

  NumericMatrix temp = S4toNumericMatrix(I);

  SparseVector R = S4toSparse_fix(V);

  SparseVector R2 = compute_closure(R, temp);

  freeVector(&R);

  S4 res = SparseToS4_fast(R2);

  freeVector(&R2);

  return res;

}


//Created by Lorenzo
//Computes the closure using matrices
// [[Rcpp::export]]
S4 compute_closure_matrix(S4 V, S4 I) {

  NumericMatrix temp = S4toNumericMatrix2(I);

  SparseVector R = S4toSparse_fix(V);

  SparseVector R2 = compute_closure(R, temp);

  freeVector(&R);

  S4 res = SparseToS4_fast(R2);

  freeVector(&R2);

  return res;

}


//Created by Lorenzo
//Turns a S4 object into a SparseVector
//The original method S4toSparse_fix contained errors, so I fixed them
SparseVector S4toSparse_fix(S4 A) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  SparseVector V;
  initVector(&V, adims[0]);

  for (size_t i = 0; i < ai.size(); i++) {

    insertArray(&(V.i), ai[i]);
    insertArray(&(V.x), ax[i]);

  }
  for (size_t i = 0; i < ap.size(); i++) {

    insertArray(&(V.p), ap[i]);

  }

  return V;

}


//Created by Lorenzo
//Converts S4 object into NumericMatrix, using vectors inside
NumericMatrix S4toNumericMatrix(S4 I) {

  std::vector<int> i = I.slot("i");
  std::vector<int> p = I.slot("p");
  std::vector<double> x = I.slot("x");
  std::vector<int> adims = I.slot("Dim");
  //NumericVector vect(adims[0]*adims[1]);
  std::vector<double> vect(adims[0]*adims[1]);

  int it = 0;
  for(long long unsigned int k=0; k<p.size()-1; k++) {
    int ant = p[k];
    int post = p[k+1];
    int aux = post - ant;
    int cont = 0;

    for(int j=0; j<adims[0]; j++) {
      if(cont<aux) {
        if(j==i[ant]) {
          vect[it] = x[ant];
          ant++;
          cont++;
        } else{
          vect[it] = 0;
        }
      } else {
        vect[it] = 0;
      }
      it++;
    }
  }
  //print(vect);
  //NumericVector s = wrap(vect);
  NumericMatrix res(adims[0], adims[1], vect.begin());


  return(res);

}


//Created by Lorenzo
//Converts S4 object into NumericMatrix, using matrices inside
NumericMatrix S4toNumericMatrix2(S4 I) {

  std::vector<int> i = I.slot("i");
  std::vector<int> p = I.slot("p");
  std::vector<double> x = I.slot("x");
  std::vector<int> adims = I.slot("Dim");
  NumericMatrix mat(adims[0],adims[1]);

  int it = 0;
  for(long long unsigned int k=0; k<p.size()-1; k++) {
    int ant = p[k];
    int post = p[k+1];
    int aux = post - ant;
    int cont = 0;

    for(int j=0; j<adims[0]; j++) {
      if(cont<aux) {
        if(j==i[ant]) {
          mat(j,k) = x[ant];
          ant++;
          cont++;
        } else{
          mat(j,k) = 0;
        }
      } else {
        mat(j,k) = 0;
      }
      it++;
    }
  }
  //print(vect);

  return(mat);

}


//Created by Lorenzo
//converts SparseVector into NumericVector
NumericVector as_vector_slow(SparseVector v, int nrow, int ncol) {

  NumericVector x(nrow*ncol);

  int it = 0;
  for(long long unsigned int i=0; i<v.p.used-1; i++) {
    int ant = v.p.array[i];
    int post = v.p.array[i+1];
    int aux = post - ant;
    int cont = 0;

    for(int j=0; j<(int)v.length; j++) {
      if(cont<aux) {
        if(j==v.i.array[ant]) {
          x[it] = v.x.array[ant];
          ant++;
          cont++;
        } else{
          x[it] = 0;
        }
      } else {
        x[it] = 0;
      }
      it++;
    }
  }

  return(x);

}


//Created by Lorenzo
//Sorting a NumericVector in C is faster
// [[Rcpp::export]]
NumericVector sort_c(NumericVector v) {

  return v.sort();
}


//Created by Lorenzo
//Equivalent to compute_grades wrote in R language by the authors, but much faster (x3)
// [[Rcpp::export]]
List compute_grades_c(NumericMatrix mat) {

  List res;

  for(int i=0; i<mat.cols(); i++) {

    NumericVector v = mat(_,i);
    v.push_back(1);
    v.push_front(0);
    v = sort_unique(v);
    res.push_back(v);
  }

  return res;
}


//Created by Lorenzo
//Trying accelerate this algorithm
void populateMatches2(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

    curr_col = y_start_index;

    if (curr_col >= y_end_index) continue;

    // recorremos las filas de x e y en paralelo
    //comprueba si x es subconjunto de y
    while(loc < end_loc){

      if (y_i[loc] == x_i[curr_col]) {

        if (y[loc] >= x[curr_col]) {

          curr_col++;

        } else break;

      }
      //if(curr_col >= y_end_index) break;

      loc++;
      if(x_i[curr_col] < y_i[loc]) break;
    }


    if(curr_col == y_end_index){
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}


//Created by Lorenzo
//Trying accelerate this algorithm
void populateMatches_binary(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

    curr_col = y_start_index;

    if (curr_col >= y_end_index) continue;

    // recorremos las filas de x e y en paralelo
    //comprueba si x es subconjunto de y
    while(loc < end_loc){

      if (y_i[loc] == x_i[curr_col]) {

        //if (y[loc] >= x[curr_col]) {

        curr_col++;

      } //else break;
      //if(curr_col >= y_end_index) break;

      loc++;
      if(x_i[curr_col] < y_i[loc]) break;
    }


    if(curr_col == y_end_index){
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}


//Created by Lorenzo
//Trying accelerate this algorithm
void populateMatchesEqual_binary(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1];

    // To be equal, they have to have the same number of nnz rows.
    if (end_loc - loc != y_end_index - y_start_index) continue;

    bool all_equal = true;

    for (int idx = 0; idx < end_loc - loc; idx++) {

      if (x_i[y_start_index + idx] != y_i[loc + idx]) {

        all_equal = false;
        break;

      }
      //para pasar a binario quitar este if
      // if (x[y_start_index + idx] != y[loc + idx]) {
      //
      //   all_equal = false;
      //   break;
      //
      //}

    }

    if (all_equal){
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}


//Created by Lorenzo
//Trying accelerate this algorithm
// [[Rcpp::export]]
SEXP is_subset_C2(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  double* x = REAL(X);
  double* y = REAL(Y);

  int proper = LOGICAL(PROPER)[0];

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  /* MFH: unused
   * int y_i_max    = INTEGER(Y_DIM)[0];
   */

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatches2(y_matches, x_i, x_p, x, y_p, y_i, y, y_index, y_p_length, proper);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}



// [[Rcpp::export]]
SEXP is_subset_C_binary(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  double* x = REAL(X);
  double* y = REAL(Y);

  int proper = LOGICAL(PROPER)[0];

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  /* MFH: unused
   * int y_i_max    = INTEGER(Y_DIM)[0];
   */

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatches_binary(y_matches, x_i, x_p, x, y_p, y_i, y, y_index, y_p_length, proper);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}


//Created by Lorenzo
//Trying accelerate this algorithm
// [[Rcpp::export]]
SEXP is_equal_set_C_binary(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  double* x = REAL(X);
  double* y = REAL(Y);

  int proper = LOGICAL(PROPER)[0];

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  /* MFH: unused
   * int y_i_max    = INTEGER(Y_DIM)[0];
   */

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatchesEqual_binary(y_matches, x_i, x_p, x, y_p, y_i, y, y_index, y_p_length, proper);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}

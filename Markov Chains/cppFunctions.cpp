#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
int sampleC(Rcpp::NumericVector prob){
  // por si el usuario da probabilidades que no suman 1
  prob = prob / Rcpp::sum(prob);
  Rcpp::NumericVector cumprob = Rcpp::cumsum(prob);
  // runif (Rcpp) regresa un NumericVector que es un arreglo de doubles
  double u = Rcpp::runif(1)[0];
  int i=0;
  while(cumprob[i] < u){
    i++;
  }
  // vamos a trabajar con índices que empiecen en uno como en R y no C++
  return i+1;
}


// [[Rcpp::export]]
int mc_transition(int current_state, Rcpp::NumericMatrix TM){
  Rcpp::NumericVector prob = TM(current_state-1, _);
  int new_state = sampleC(prob);
  return new_state;
}


// [[Rcpp::export]]
Rcpp::NumericVector mc_trajectory(int init_state, int nobs,  NumericMatrix TM){
  NumericVector trajectory(nobs + 1);
  trajectory[0] = init_state;
  for(int i=0; i < nobs; i++){
    trajectory[i+1] = mc_transition(trajectory[i], TM);
  }
  return trajectory;
}



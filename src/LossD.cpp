#include "MyPerson.pb.h"
#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//



//' LossD
//'
//' \loadmathjax This function evaluates the following quadratic loss function
//' \mjsdeqn{L(\beta) = (X\beta - Y)^2}
//' @param b [vector] the vector of \mjseqn{\beta} parameters in the formula above.
//' @param X [matrix] the design matrix \code{X} in the formula above.
//' @param Y [vector] the response vector \code{Y} in the formula above.
//' 
//' @return [double] the function evaluation
//' @export
// [[Rcpp::export]]
double LossD(arma::vec b, arma::mat X, arma::vec Y) {
  arma::vec XbY = X * b - Y;
  arma::mat res = XbY.t() * XbY;
  return res(0);
}







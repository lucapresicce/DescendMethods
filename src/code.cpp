#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double try_rcpp(double x){
  return x*2;
}
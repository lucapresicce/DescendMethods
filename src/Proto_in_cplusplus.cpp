// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <RcppEigen.h>

#include "MySave.pb.h"
#include "GSL_wrappers.h"
#include "Rcpp.h"


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//



//' Proto_in_cplusplus
//'
//' @export
// [[Rcpp::export]]
Rcpp::RawVector Proto_in_cplusplus() {
  
  MyNamespace::MySave output1; //create object
  
  // Fill the message
  output1.set_name("Mario");
  output1.set_salary(1500.56);
  
  // Use fields in the message
  Rcpp::Rcout<<"The name is: "<<output1.name()<<std::endl;
   
  // Print serialized message
  std::string msg = output1.SerializeAsString();
  
  Rcpp::Rcout << "Serialized message: "<<msg<<std::endl;
  
  // Return serialized object to R
  Rcpp::RawVector out(msg.size());
  std::copy(msg.begin(), msg.end(), out.begin());
  
  // Random number generation
  double x = R::rnorm(0,1);
  Rcpp::Rcout << "x = "<<x<<std::endl;
  Rcpp::Vector xx = Rcpp::rnorm(3,0,1);
  Rcpp::Rcout << "xx = "<<xx<<std::endl;
  return out;
}








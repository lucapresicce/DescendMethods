// [[Rcpp::depends(RcppEigen)]]
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <RcppEigen.h>

#include "MySave.pb.h"
#include "MyMatrix.pb.h"
#include "SamplerState.pb.h"
#include "GSL_wrappers.h"
#include "Rcpp.h"
#include <Eigen/LU>


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


//' BayesLM
//'
//' @export
// [[Rcpp::export]]
std::vector<Rcpp::RawVector>
                BayesLM(Eigen::MatrixXd const & data, unsigned int const & niter, unsigned int const & burnin,
                        Eigen::VectorXd const & mu0, Eigen::MatrixXd const & Lambda0, double const & b,Eigen::MatrixXd const & D,
                        Eigen::VectorXd const & mu_init, Eigen::MatrixXd const & Sigma_init){

  std::vector<Rcpp::RawVector> out;
  out.reserve(niter-burnin);
  unsigned int p = mu_init.size();
  unsigned int n = data.rows();
  sample::rmvnorm rmv;
  
  Eigen::VectorXd ones(Eigen::VectorXd::Constant(p,1.0));
  Eigen::MatrixXd Id(Eigen::MatrixXd::Identity(p,p));
 
  Eigen::VectorXd mu(mu_init);
  Eigen::MatrixXd Sigma(Sigma_init);
  Eigen::MatrixXd K(Sigma_init.partialPivLu().solve(Id));
  
  Eigen::VectorXd invLambda0mu0(Lambda0.partialPivLu().solve(mu0));
  Eigen::MatrixXd invLambda0(Lambda0.partialPivLu().solve(Id));
  Eigen::VectorXd data_mean(data.colwise().sum()/n);

  // Start MCMC
  for(std::size_t i = 0; i < niter; i++){
    Eigen::MatrixXd Lambda_n = (invLambda0 + n*K).partialPivLu().solve(Id) ;
    Eigen::VectorXd mu_n = Lambda_n * (invLambda0mu0 + n*K) ;
    mu = rmv(mu_n,Lambda_n);

    Eigen::MatrixXd U(Eigen::MatrixXd::Constant(p,p,0.0));
    for(std::size_t j = 0; j<n; j++){
      U += (data.row(j)-mu.transpose()).transpose()*(data.row(j)-mu.transpose());
    }
    Eigen::MatrixXd DU(D+U);
    K = sample::rwish<Eigen::MatrixXd, sample::isChol::False>()((double)(b+n), DU);

    if(i >= burnin){

      /* 
      Example: how to save a matrix
      MyMatrix PrecSave; //create object
      
      // Fill the message
      PrecSave.set_rows(K.rows());
      PrecSave.set_cols(K.cols());
      *PrecSave.mutable_data() = {K.data(), K.data()+K.size()}; // save a buffer
      std::string Kser = PrecSave.SerializeAsString();
      */

      // Save
      State State_it;  //Create state message
      *State_it.mutable_mu() = {mu.data(), mu.data()+mu.size()}; //save mean


      Prec *p;  // Create pointer to nested message
      p = State_it.add_prec(); // add object of nested message
      p->set_rows(K.rows());
      p->set_cols(K.cols());
      *(p->mutable_data()) = {K.data(), K.data()+K.size()}; //save matrix

      std::string s = State_it.SerializeAsString(); // serialization
      //Rcpp::Rcout << "Serialized message (2): "<<s<<std::endl; 

      // Cast string into RawVector
      Rcpp::RawVector out_raw(s.size());
      std::copy(s.begin(), s.end(), out_raw.begin());

      // Fill return object
      out.push_back(out_raw);
    }
  }
  
  return out;
}





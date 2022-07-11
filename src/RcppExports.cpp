// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Proto_in_cplusplus
Rcpp::RawVector Proto_in_cplusplus();
RcppExport SEXP _DescendMethods_Proto_in_cplusplus() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(Proto_in_cplusplus());
    return rcpp_result_gen;
END_RCPP
}
// BayesLM
Rcpp::RawVector BayesLM(Eigen::MatrixXd const& data, unsigned int const& niter, unsigned int const& burnin, Eigen::VectorXd const& mu0, Eigen::MatrixXd const& Lambda0, double const& b, Eigen::MatrixXd const& D, Eigen::VectorXd const& mu_init, Eigen::MatrixXd const& Sigma_init);
RcppExport SEXP _DescendMethods_BayesLM(SEXP dataSEXP, SEXP niterSEXP, SEXP burninSEXP, SEXP mu0SEXP, SEXP Lambda0SEXP, SEXP bSEXP, SEXP DSEXP, SEXP mu_initSEXP, SEXP Sigma_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd const& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< unsigned int const& >::type niter(niterSEXP);
    Rcpp::traits::input_parameter< unsigned int const& >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd const& >::type mu0(mu0SEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd const& >::type Lambda0(Lambda0SEXP);
    Rcpp::traits::input_parameter< double const& >::type b(bSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd const& >::type D(DSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd const& >::type mu_init(mu_initSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd const& >::type Sigma_init(Sigma_initSEXP);
    rcpp_result_gen = Rcpp::wrap(BayesLM(data, niter, burnin, mu0, Lambda0, b, D, mu_init, Sigma_init));
    return rcpp_result_gen;
END_RCPP
}
// try_rcpp
double try_rcpp(double x);
RcppExport SEXP _DescendMethods_try_rcpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(try_rcpp(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DescendMethods_Proto_in_cplusplus", (DL_FUNC) &_DescendMethods_Proto_in_cplusplus, 0},
    {"_DescendMethods_BayesLM", (DL_FUNC) &_DescendMethods_BayesLM, 9},
    {"_DescendMethods_try_rcpp", (DL_FUNC) &_DescendMethods_try_rcpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_DescendMethods(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

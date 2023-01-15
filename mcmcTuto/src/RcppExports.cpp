// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// log_likelihood
double log_likelihood(NumericVector dat, double mu, double sd);
RcppExport SEXP _mcmcTuto_log_likelihood(SEXP datSEXP, SEXP muSEXP, SEXP sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    rcpp_result_gen = Rcpp::wrap(log_likelihood(dat, mu, sd));
    return rcpp_result_gen;
END_RCPP
}
// log_prior_mu
double log_prior_mu(double mu, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_log_prior_mu(SEXP muSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(log_prior_mu(mu, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}
// log_prior_sd
double log_prior_sd(double sd);
RcppExport SEXP _mcmcTuto_log_prior_sd(SEXP sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    rcpp_result_gen = Rcpp::wrap(log_prior_sd(sd));
    return rcpp_result_gen;
END_RCPP
}
// log_prior_total
double log_prior_total(double mu, double sd, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_log_prior_total(SEXP muSEXP, SEXP sdSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(log_prior_total(mu, sd, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}
// log_posterior
double log_posterior(Rcpp::NumericVector dat, double mu, double sd, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_log_posterior(SEXP datSEXP, SEXP muSEXP, SEXP sdSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(log_posterior(dat, mu, sd, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}
// mcmc
Rcpp::DataFrame mcmc(int n_iter, double init_mu, double init_sd, double sd_proposal_mu, double sd_proposal_sd, Rcpp::NumericVector dat, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_mcmc(SEXP n_iterSEXP, SEXP init_muSEXP, SEXP init_sdSEXP, SEXP sd_proposal_muSEXP, SEXP sd_proposal_sdSEXP, SEXP datSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n_iter(n_iterSEXP);
    Rcpp::traits::input_parameter< double >::type init_mu(init_muSEXP);
    Rcpp::traits::input_parameter< double >::type init_sd(init_sdSEXP);
    Rcpp::traits::input_parameter< double >::type sd_proposal_mu(sd_proposal_muSEXP);
    Rcpp::traits::input_parameter< double >::type sd_proposal_sd(sd_proposal_sdSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(mcmc(n_iter, init_mu, init_sd, sd_proposal_mu, sd_proposal_sd, dat, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}
// move_mu
List move_mu(double curr_mu, double curr_sd, double sd_proposal_mu, Rcpp::NumericVector dat, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_move_mu(SEXP curr_muSEXP, SEXP curr_sdSEXP, SEXP sd_proposal_muSEXP, SEXP datSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type curr_mu(curr_muSEXP);
    Rcpp::traits::input_parameter< double >::type curr_sd(curr_sdSEXP);
    Rcpp::traits::input_parameter< double >::type sd_proposal_mu(sd_proposal_muSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(move_mu(curr_mu, curr_sd, sd_proposal_mu, dat, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}
// move_sd
List move_sd(double curr_mu, double curr_sd, double sd_proposal_sd, Rcpp::NumericVector dat, double prior_mu, double prior_sd);
RcppExport SEXP _mcmcTuto_move_sd(SEXP curr_muSEXP, SEXP curr_sdSEXP, SEXP sd_proposal_sdSEXP, SEXP datSEXP, SEXP prior_muSEXP, SEXP prior_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type curr_mu(curr_muSEXP);
    Rcpp::traits::input_parameter< double >::type curr_sd(curr_sdSEXP);
    Rcpp::traits::input_parameter< double >::type sd_proposal_sd(sd_proposal_sdSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dat(datSEXP);
    Rcpp::traits::input_parameter< double >::type prior_mu(prior_muSEXP);
    Rcpp::traits::input_parameter< double >::type prior_sd(prior_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(move_sd(curr_mu, curr_sd, sd_proposal_sd, dat, prior_mu, prior_sd));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mcmcTuto_log_likelihood", (DL_FUNC) &_mcmcTuto_log_likelihood, 3},
    {"_mcmcTuto_log_prior_mu", (DL_FUNC) &_mcmcTuto_log_prior_mu, 3},
    {"_mcmcTuto_log_prior_sd", (DL_FUNC) &_mcmcTuto_log_prior_sd, 1},
    {"_mcmcTuto_log_prior_total", (DL_FUNC) &_mcmcTuto_log_prior_total, 4},
    {"_mcmcTuto_log_posterior", (DL_FUNC) &_mcmcTuto_log_posterior, 5},
    {"_mcmcTuto_mcmc", (DL_FUNC) &_mcmcTuto_mcmc, 8},
    {"_mcmcTuto_move_mu", (DL_FUNC) &_mcmcTuto_move_mu, 6},
    {"_mcmcTuto_move_sd", (DL_FUNC) &_mcmcTuto_move_sd, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_mcmcTuto(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

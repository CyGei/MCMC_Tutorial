#ifndef CPP_MODEL_H
#define CPP_MODEL_H

#include <Rcpp.h>

using namespace Rcpp;

double log_likelihood(NumericVector dat, double mu, double sd);
double log_prior_mu(double mu, double prior_mu, double prior_sd);
double log_prior_sd(double sd);
double log_prior_total(double mu, double sd, double prior_mu, double prior_sd);
double log_posterior(Rcpp::NumericVector dat, double mu, double sd, double prior_mu, double prior_sd);

#endif

#ifndef MOVE_H
#define MOVE_H

#include <Rcpp.h>
using namespace Rcpp;

List move_mu(double curr_mu, double curr_sd, double sd_proposal_mu, Rcpp::NumericVector dat, double prior_mu, double prior_sd);
List move_sd(double curr_mu, double curr_sd, double sd_proposal_sd, Rcpp::NumericVector dat, double prior_mu, double prior_sd);

#endif

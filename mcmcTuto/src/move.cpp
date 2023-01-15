
#include <Rcpp.h>
#include "cpp_model.h"
using namespace Rcpp;

// [[Rcpp::export]]
List move_mu(double curr_mu, double curr_sd, double sd_proposal_mu, Rcpp::NumericVector dat, double prior_mu, double prior_sd) {
  
  // Moving MU
  Rcpp::Rcout << "MOVING MU: " << std::endl;
  
  // Print out the current values of the mean and standard deviation
  Rcpp::Rcout << "curr_mu: " << curr_mu << std::endl;
  Rcpp::Rcout << "curr_sd: " << curr_sd << std::endl;
  
  //declaring accept outside of if statement
  int accept ;
  
  // Sample a new value for the mean from a normal distribution with mean equal to the current mean and standard deviation equal to sd_proposal_mu
  double new_mu = R::rnorm(curr_mu, sd_proposal_mu);
  
  // Print out the new value of the mean
  Rcpp::Rcout << "new_mu: " << new_mu << std::endl;
  
  // Calculate the ratio of the posterior probabilities for the new and current values of the mean
  double ratio_post = log_posterior(dat, new_mu, curr_sd, prior_mu, prior_sd) - log_posterior(dat, curr_mu, curr_sd, prior_mu, prior_sd);
  
  // Initialize the correction term to 0
  double correction = 0.0;
  
  // Calculate the acceptance probability for the new value of the mean
  double p_accept = ratio_post + correction;
  
  Rcpp::Rcout << "p_accept: " << p_accept << std::endl;
  
  // Sample a uniform random value between 0 and 1
  double tmp = log(Rcpp::runif(1)[0]);
  Rcpp::Rcout << "random draw: " << tmp << std::endl;
  
  
  // If the random value is less than the acceptance probability, accept the new value of the mean
  if (tmp < p_accept) {
    
    curr_mu = new_mu;
    accept = 1.0;
    
    // Print out that the new value of the mean was accepted
    Rcpp::Rcout << "accept" << std::endl << std::endl;
    
    
  } else { 
    
    // Otherwise, reject the new value of the mean and keep the current value
    accept = 0.0;
    
    // Print out that the new value of the mean was rejected
    Rcpp::Rcout << "reject" << std::endl << std::endl;
  }
  
  // Return a pair containing the updated value of the mean and a binary value indicating whether the new value was accepted or rejected
  return Rcpp::List::create(Rcpp::Named("updated_mu") = curr_mu, Rcpp::Named("accept") = accept);
}






// [[Rcpp::export]]
List move_sd(double curr_mu, double curr_sd, double sd_proposal_sd, Rcpp::NumericVector dat, double prior_mu, double prior_sd) {
  
  // Moving SD
  Rcpp::Rcout << "MOVING SD: " << std::endl;
  
  // Print out the current values of the mean and standard deviation
  Rcpp::Rcout << "curr_mu: " << curr_mu << std::endl;
  Rcpp::Rcout << "curr_sd: " << curr_sd << std::endl;
  
  //declaring accept outside of if statement
  int accept ;
  
  // Sample a new value for the standard deviation from a normal distribution with mean equal to the current standard deviation and standard deviation equal to sd_proposal_sd
  double new_sd = R::rnorm(curr_sd, sd_proposal_sd);
  
  // Print out the new value of the standard deviation
  Rcpp::Rcout << "new_sd: " << new_sd << std::endl;
  
  // Calculate the ratio of the posterior probabilities for the new and current values of the standard deviation
  double ratio_post = log_posterior(dat, curr_mu, new_sd, prior_mu, prior_sd) - log_posterior(dat, curr_mu, curr_sd, prior_mu, prior_sd);
  
  // Initialize the correction term to 0
  double correction = 0;
  
  // Calculate the acceptance probability for the new value of the standard deviation
  double p_accept = ratio_post + correction;
  
  // Print out the acceptance probability
  Rcpp::Rcout << "p_accept: " << p_accept << std::endl;
  
  // Sample a uniform random value between 0 and 1
  double tmp = log(Rcpp::runif(1)[0]);
  
  // If the random value is less than the acceptance probability, accept the new value of the sd
  if (tmp < p_accept) {
    
    curr_sd = new_sd;
    
    accept = 1.0;
    
    // Print out that the new value of the sd was accepted
    Rcpp::Rcout << "accept" << std::endl << std::endl;
    
  } else { // Otherwise, reject the new value of the sd and keep the current value
    
    accept = 0.0;
    
    // Print out that the new value of the sd was rejected
    Rcpp::Rcout << "reject" << std::endl << std::endl;
  }
  
  // Return a pair containing the updated value of the sd and a binary value indicating whether the new value was accepted or rejected
  
  return Rcpp::List::create(Rcpp::Named("updated_sd") = curr_sd, Rcpp::Named("accept") = accept);
}


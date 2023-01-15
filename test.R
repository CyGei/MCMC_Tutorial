library(tidyverse)
library(firatheme);theme_set(theme_fira())
library(mcmcTuto)

          

true_mu = 43.21
true_sd = 1.2
prior_mu = 30
prior_sd = 1.5


data <- rgamma(1000, shape=(true_mu/true_sd)^2, scale = true_sd^2/true_mu)

set.seed(123)
output <- mcmcTuto::mcmc(
  n_iter = 2000,
  init_mu = 30,
  init_sd = 2,
  sd_proposal_mu = 0.1 ,
  sd_proposal_sd = 0.1 ,
  prior_mu = prior_mu,
  prior_sd = prior_sd,
  dat = data
) %>%
  as_tibble()



( sum(output$accept_mu, na.rm = T)/nrow(output) ) *100

( sum(output$accept_sd, na.rm = T)/nrow(output) ) *100


trace_plot(clean_chain(output)) #no thinning, no burnin 
trace_plot(clean_chain(output, thinning =  50, burnin = 0))


clean_chain(output, thinning =  1, burnin = 500) %>% 
  filter(param == "mu_chain") %>% 
  trace_plot()

clean_chain(output, thinning =  1, burnin = 500) %>% 
  filter(param == "sd_chain") %>% 
  trace_plot()

clean_chain(output, thinning =  50, burnin = 0) %>%
  ggplot(aes(x = iter, y = loglike_chain))+
  geom_line()+
  labs(title = "log LOGLIKELIHOOD")

clean_chain(output, thinning =  50, burnin = 0) %>%
  ggplot(aes(x = iter, y = logpost_chain))+
  geom_line()+
  labs(title = "log POSTERIOR")



clean_chain(output, thinning = 50, burnin = 200) %>% 
  group_by(param) %>% 
  summarise(mean = mean(value),
            lower95 = quantile(value, probs = 0.025),
            upper95 = quantile(value, probs = 0.975)) %>% 
  ungroup() %>% 
  mutate(true_value = c(true_mu, true_sd)) %>% 
  mutate_if(is.numeric, round, 2) 





# Plotting ----------------------------------------------------------------


sd = 0.5
tibble(xval = seq(true_mu/2,true_mu*2, 0.1)) %>% 
  mutate( truth = dgamma(xval, shape=(true_mu/true_sd)^2, scale = true_sd^2/true_mu, log = TRUE),
          prior = map(.x = xval, ~log_prior_total(.x, sd = sd, prior_mu, prior_sd)),
          like = map(.x = xval, ~log_likelihood(dat = data, mu = .x, sd = sd)),
          post = map(.x = xval, ~log_posterior(dat = data, mu = .x, sd = sd, prior_mu, prior_sd))
  ) %>% 
  unnest(cols = -c(xval, truth)) %>%
  mutate(across(-xval, ~. / max(.))) %>% 
  #mutate(across(-c(xval), ~exp(.))) %>% 
  ggplot()+
  aes(x = xval )+
  geom_line(aes(y = truth, col = "truth")) +
  geom_line(aes(y = prior, col = "prior"))+
  geom_line(aes(y = like, col = "likelihood"))+
  geom_line(aes(y = post, col = "posterior"))+
  scale_y_reverse()+
  labs(y = "log probability", x = "mean", title = paste0("sd set to ", sd ) )



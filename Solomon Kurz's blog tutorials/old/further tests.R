sim_data_fit <- function(seed, n_player) {
  
  update(fit1,
         newdata = d,
         seed = seed) %>% 
    posterior_samples() %>% # extract posterior draws
    transmute(p = inv_logit_scaled(b_Intercept)) %>%# convert back from log-odds scale
    tidybayes::median_qi() %>% #
    select(.lower:.upper)
  
}
################
n_trials <- 1
prob_hit <- .25
n_player=50

set.seed(seed)

d <- tibble(y = rbinom(n    = n_player, 
                       size = n_trials, 
                       prob = prob_hit))
head(d)

########################
fit1 <-
  brm(data = d, 
      family = binomial,
      y | trials(1) ~ 1,
      prior(normal(0, 2), class = Intercept),
      seed = 3)


brms::posterior_samples(fit1) %>% # extract posterior draws
  transmute(p = inv_logit_scaled(b_Intercept)) %>%# convert back from log-odds scale to a probability metric
  tidybayes::median_qi() %>% #
  select(.lower:.upper)

# working line-by-line version of the above from the function to show what each command does individually
head(posterior_samples(fit1)) # list of posterior draws. NOT HDI's; these are actual medians
test=as_tibble(posterior_samples(fit1)) %>% transmute(p = inv_logit_scaled(b_Intercept))
median_qi(test)


library(rstanarm)
data(wells)
wells$dist100 <- wells$dist / 100
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit2_rstan <- stan_glm(switch ~ dist100, data = wells,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)

fit3_rstandata_with_brms=brm(data = wells, 
                             family = binomial,
                             y (1) ~ 1,
                             prior(student(0, 2), class = Intercept),
                             seed = 3)

# working line-by-line version of function, with an rstanarm model
head(posterior_samples(fit2_rstan)) # list of posterior draws. NOT HDI's; these are actual medians
test=as_tibble(posterior_samples(fit2_rstan) %>% rename("Intercept"="(Intercept)")) %>% transmute(p = inv_logit_scaled(Intercept))
median_qi(test)



CIs <- bayestestR::hdi(fit1, ci = c(0.89))
plot(CIs)
tidy_posterior(fit1)







###### Examples of brms

fit <-
  brm(data = my_data, 
      family = bernoulli, #can be either 'bernouli' or 'binomial and resutls will be the same, as long as the data is not aggregated
      y ~ 1 + x1 + x2,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 2), class = b)))


fit <-
  brm(data = my_data, 
      family = binomial,
      y | trials(1) ~ 1 + x1 + x2,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 2), class = b)))

############

# Trying it with my stuff

get_prior(Accept_Reject~ 0 + Intercept + Discount + Guilt + PTS,
          data=pubdata,family = binomial())


brms_model <-brm(data = pubdata, 
                 family = binomial,
                 Accept_Reject | trials(1) ~ 0 + Intercept + Discount + Guilt + PTS,
                 prior =c(set_prior(student_t(5, 0, 2), class = b, coef = Discount1),
                          set_prior(student_t(5, 0, 2), class = b, coef = Discount2),
                          set_prior(student_t(5, 0, 2), class = b, coef = Guilt1),
                          set_prior(student_t(5, 0, 2), class = b, coef = PTS1),
                          set_prior(student_t(5, 0, 2.5), class = b,coef = Intercept)) )

# vs. rstanarm
rstan_model=stan_glm(Accept_Reject~ Discount + Guilt + PTS, 
                     family = binomial(link = "logit"), 
                     data=pubdata, 
                     prior = Priors_MEmodel,
                     #prior_intercept = normal(), 
                     #prior_PD = FALSE, 
                     algorithm = c("sampling"), 
                     mean_PPD = TRUE,
                     adapt_delta = 0.95, 
                     #QR = FALSE, 
                     #sparse = FALSE,
                     chains=3,iter=1000,cores=3)

describe_prior(rstan_model)

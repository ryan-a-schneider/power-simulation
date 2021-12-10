source("C:/Users/rschn/OneDrive/R archive/Ryans Script Archive/Utilities.r")
pacman::p_load(bayestestR,tidyverse,rstanarm,brms,tidybayes,broom,broom.mixed)
load(file = "pubdata.RData")
load(file = "sonadata_clean.RData")
source("power functions.R")

########## Part III.b: Redefining power and using binary data###########

# Create the data

set.seed(3)

d <- tibble(y = rbinom(n = 50, size = 1, prob = .25))

# my version
d=tibble(y = rbinom(n = 100, size = 1, prob = .75)) # 75% chance of pleading guilty

tibble::glimpse(d)

#plot of data
theme_set(theme_gray() + theme(panel.grid = element_blank()))

d %>% 
  mutate(y = factor(y)) %>% 
  
  ggplot(aes(x = y)) +
  geom_bar()

#### Model the data ####

# 1. Decide on prior

#Priors_MEmodel<- student_t(df=5, location = c(0, 0, 0),scale = c(2, 2, 2), autoscale = FALSE)
Priors_MEmodel<- student_t(df=5, location = 0, scale = 2, autoscale = FALSE)

# run model
Thesis_Model=stan_glm(Accept_Reject~Guilt, 
                      family = binomial(link = "logit"), 
                      data=pubdata, 
                      prior = Priors_MEmodel,
                      prior_intercept = normal(0,3), 
                      #prior_PD = FALSE, 
                      algorithm = c("sampling"), 
                      mean_PPD = TRUE,
                      adapt_delta = 0.95, 
                      #QR = FALSE, 
                      #sparse = FALSE,
                      chains=2,iter=500,cores=2)

describe_posterior(Thesis_Model) %>% mutate(OR=exp(Median))
# simulated people ten times as likely to plead guilty



# 3. extract the posterior draws
posterior_samples(Thesis_Model) %>% 
  rename("Intercept"="(Intercept)") %>% 
  # transform from the log-odds to a probability metric
  transmute(p = inv_logit_scaled(Intercept)) %>% 
  
  # plot the posterior draws to see it and the HDI's
  ggplot(aes(x = p)) +
  geom_density(fill = "grey25", size = 0) +
  scale_x_continuous("probability of a pleading", limits = c(0, 1)) +
  scale_y_continuous(NULL, breaks = NULL)

#looks like the NH (of 0.5) isn't plausable here; reject NH


#### Conduct power analysis ####

sim_data_fit <- function(seed, n_player) {
  
  n_trials <- 1
  prob_hit <- .25
  
  set.seed(seed)
  
  d <- tibble(y = rbinom(n    = n_player, 
                         size = n_trials, 
                         prob = prob_hit))
  
  update(Thesis_Model,
         data = pubdata,
         seed = seed) %>% 
    posterior_samples() %>% 
    rename("Intercept"="(Intercept)") %>% # extract posterior draws
    transmute(p = inv_logit_scaled(Intercept)) %>%# convert back from log-odds scale
    tidybayes::median_qi() %>% #
    select(.lower:.upper)
  
}


# simulate
sim1 <-
  tibble(seed = 1:100) %>% # the UL of seed is the number of iterations/simulations
  mutate(ci = map(seed, sim_data_fit, n_player = 50)) %>% 
  unnest()


# plot intervals
sim1 %>% 
  ggplot(aes(x = seed, ymin = .lower, ymax = .upper)) +
  geom_hline(yintercept = c(.25, .5), color = "white") +
  geom_linerange() +
  xlab("seed (i.e., simulation index)") +
  scale_y_continuous("probability of hitting the ball", limits = c(0, 1))

# summarize results
sim1 %>% 
  mutate(width = .upper - .lower) %>% 
  summarise(`conventional power` = mean(.upper < .5),
            `mean width`         = mean(width),
            `width below .25`    = mean(width < .25))

# check if the Null (0.5 in this exampel) is within these intervals

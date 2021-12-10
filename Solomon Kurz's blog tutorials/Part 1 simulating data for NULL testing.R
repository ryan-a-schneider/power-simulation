pacman::p_load(broom, broom.mixed, brms, rstanarm)
pacman::p_load(parameters, insight, report, bayestestR)
library(legaldmlab) #load my package

set.seed(1)


n=50 # define number of participants
# define the means for your two populations
mu_c <- 0
mu_t <- 0.5

# Create data frame
d <-
  tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
  mutate(treatment = ifelse(group == "control", 0, 1),
         y         = ifelse(group == "control", 
                            rnorm(n, mean = mu_c, sd = 1),
                            rnorm(n, mean = mu_t, sd = 1)))

glimpse(d)


# decide on priors
get_prior(data = d,
          family = gaussian,
          y ~ 0 + Intercept + treatment)

# fit model and run
fit <-
  brm(data = d,
      family = gaussian,
      y ~ 0 + Intercept + treatment,
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1)

model_parameters(fit) # broom::tidy() is depricated!!! USE THIS INSTEAD!!!!!

# This was one simple practice run
# Now to simulate many, many times










#### Method 1 ####

# starts off similar to above, but creating a function that allows the above to be run n times at once

sim_d <- function(seed, n) {
  
  mu_t <- .5
  mu_c <- 0
  
  set.seed(seed)
  
  tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
}

sim_d(seed = 123, n = 2) # n is the number of people in each condition



# how many simulations would you like?
n_sim <- 100

# this will help us track time
t1 <- Sys.time()

#### here's the main event! This is the data simulation.

s <- # create "nested data frame"
  tibble(seed = 1:n_sim) %>% # construct a data frame. Put in it a column called "seed" that has numbers 1 through n_sim (which is specified above)
  mutate(d    = map(seed, sim_d, n = 50)) %>% # add a new column called "d", supply it with the seed value from above, and perform the 'sim_d' command repeatedly
  mutate(fit  = map2(d, seed, ~update(fit, newdata = .x, seed = .y)))

t2 <- Sys.time()

# This creates a nested tibble with all simulated data in the 'd' column, and all brms objects in the 'fit' column.
# now use broom::tidy() and some other stuff to extract parameters of interest (here, 'b_treatment')

s %>% # this code is supposed to do describe_posterior() and extract the b_treatment parameter from each simulation
  mutate(treatment = map(fit, model_parameters, prob = .95)) %>% 
  unnest(treatment) %>% 
  filter(term == "b_treatment") %>% 
  head()


# examine these graphed, in bulk
# In this example, we know the true parameter/effect should be 0.5. The 0 line represents the NH of no difference
s %>% 
  mutate(treatment = map(fit, tidy, prob = .95)) %>% 
  unnest(treatment) %>% 
  filter(term == "b_treatment") %>% 
  
  ggplot(aes(x = seed, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

# and check on average how many Credible Intervals contain zero (i.e., your power level)
s %>% 
  mutate(treatment = map(fit, parameters::model_parameters)) %>% 
  unnest(treatment) %>% 
  filter(term == "treatment") %>% 
  mutate(check = ifelse(CI_lower > 0, 1, 0)) %>% # code all instances where LL of HDI is >0 as 1, all else 0, saved as variable "check"
  summarise(power = mean(check)) #take the mean of the above, which is your Bayesian power estimate (i.e. the number out of the 100 simulations  where the HDI did not straddle 0)


# in this example, n= 50 was sufficient enough to produce a Bayesian 95% HDI that did not straddle 0



#### Method 2 ####

# Method 2

# Check the Rhat values of all the models at once
s %>% 
  mutate(rhat = map(fit, rhat)) %>% 
  unnest(rhat) %>% 
  
  ggplot(aes(x = rhat)) +
  geom_histogram(bins = 20)


# Redo the model calculations and storing from Method 1, but now add a step that tells it to ditch what we don't need
s2 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(d    = map(seed, sim_d, n = 50)) %>% 
  # here's the new part
  mutate(tidy = map2(d, seed, ~update(fit, newdata = .x, seed = .y) %>% 
                       tidy(prob = .95) %>% 
                       filter(term == "b_treatment")))


# Check results
s2 %>% 
  unnest(tidy) %>% 
  
  ggplot(aes(x = seed, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))
# Same summary as in Method 1, but now takes up substantially less memory


#### Method 3: Getting "even stingier" with saving memory ####

# For our purposes here just checking power, we don't care about saving the simulated data.
# We don't ever need to go back to the data and (e.g. check random subsets of it)
# If you're willing to forgo the luxury of inspecting your data sims, it might make sense to run our power analysis in a way that avoids saving them.
# One way to do this is just to wrap the data simulation and model fitting all in one function
# We shall call this function, 'sim_d_and_fit()'


# This function "no longer (holds) onto the data simulations or the brms fit objects." It only retains the parameter summaries and the seed.
sim_d_and_fit <- function(seed, n) {
  
  mu_t <- .5
  mu_c <- 0
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    tidy(prob = .95) %>% 
    filter(term == "b_treatment")
}



# Now iterate 
t5 <- Sys.time()

s3 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(tidy = map(seed, sim_d_and_fit, n = 50)) %>% 
  unnest(tidy)

t6 <- Sys.time()

# inspect
head(s3)

# View graph
s3 %>% 
  ggplot(aes(x = seed, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

#estimate power
s3 %>% 
  mutate(check = ifelse(lower > 0, 1, 0)) %>% 
  summarise(power = mean(check))



#### Recap from part 1 

# 1. Simulate a single data set and fit an initial model

# define the means
mu_c <- 0
mu_t <- 0.5

# determine the group size
n <- 50

# simulate the data
set.seed(1)
d <-
  tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
  mutate(treatment = ifelse(group == "control", 0, 1),
         y         = ifelse(group == "control", 
                            rnorm(n, mean = mu_c, sd = 1),
                            rnorm(n, mean = mu_t, sd = 1)))
# fit the model
fit <-
  brm(data = d,
      family = gaussian,
      y ~ 0 + intercept + treatment,
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1)


# 2. Make custom function that both simulated data sets and used the update() function to update that initial fit

sim_d_and_fit <- function(seed, n) {
  
  mu_c <- 0
  mu_t <- 0.5
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    tidy(prob = .95) %>% 
    filter(term == "b_treatment")
}


# Iterate over n_sim= 100 times

n_sim <- 100

s3 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(tidy = map(seed, sim_d_and_fit, n = 50)) %>% 
  unnest(tidy)


# 3. Examine results

theme_set(theme_grey() +
            theme(panel.grid = element_blank()))

s3 %>% 
  ggplot(aes(x = seed, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

#estimate power
s3 %>% 
  mutate(check = ifelse(lower > 0, 1, 0)) %>% 
  summarise(power = mean(check))

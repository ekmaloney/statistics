#CHAPTER 4 NOTES

#4.1.1 Normal by addition
library(tidyverse)

set.seed(1000)
pos <- replicate(100, runif(16, -1, 1)) %>%
       as_tibble() %>%
       rbind(0, .) %>%
       mutate(step = 0:16) %>%
       gather(key, value, -step) %>%
       mutate(person = rep(1:100, each = 17)) %>%
       group_by(person) %>%
       mutate(position = cumsum(value)) %>%
       ungroup()

glimpse(pos)
ggplot(pos, mapping = aes(x = step, y = position, group = person)) +
         geom_vline(xintercept = c(4, 8, 16), linetype = 2) +
         geom_line(aes(color = person < 2, alpha = person < 2)) +
         scale_color_manual(values = c("skyblue", "black")) +
         scale_alpha_manual(values = c(1/5, 1)) +
         scale_x_continuous("step number", breaks = c(0, 4, 8, 12, 16)) + 
         theme(legend.position = "none")

#4.1.2 Normal by Multiplication
set.seed(1)
tibble(a = 1,
       b = runif(12, 0, 0.1)) %>%
       mutate(c = a + b) %>% 
       summarise(p = prod(c))

set.seed(1)
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1))) %>% 
          as_tibble()

ggplot(data = growth, aes(x = value)) + geom_density()

#comparing multiplicative effects with big and small differences
set.seed(412)
big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))

set.seed(412)
small <- replicate(10000, prod(1 + runif(12, 0, 0.1)))

tibble(samples = c(big, small),
       distribution = rep(c("big", "small"), each = 10000)) %>% 
  ggplot(aes(x = samples)) +
  geom_density(fill = "black", color = "transparent") + 
  facet_wrap(~distribution, scales = "free")

#4.3 A Gaussian model of height
library(rethinking)
data(Howell1)
d <- Howell1
rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)

glimpse(d)

#filter to only adults
d2 <- d %>% filter(age >= 18)

ggplot(d2, aes(x = height)) + geom_density()

#PLOTTING PRIORS
#prior for mu = N(178, 20)
ggplot(data = tibble(x = seq(from = 100, to = 250, by = .1)),
       aes(x = x, y = dnorm(x, mean = 178, sd = 20))) +
       geom_line() + 
       ylab("density")

#prior for sigma = U(0, 50)
tibble(x = seq(from = -10, to = 60, by = .1)) %>% 
      ggplot(aes(x = x, y = dunif(x, min = 0, max = 50))) +
      geom_line() +
      scale_y_continuous(NULL, breaks = NULL) +
      theme(panel.grid = element_blank())

#sample from both priors to get a prior probability dist of heights
n <- 1e4
set.seed(432)
tibble(sample_mu = rnorm(n, mean = 178, sd = 20),
       sample_sigma = runif(n, min = 0, max = 50)) %>% 
       mutate(x = rnorm(n, mean = sample_mu, sd = sample_sigma)) %>% 
       
       ggplot(aes(x = x)) +
       geom_density(fill = "black", size = 0) +
       scale_y_continuous(NULL, breaks = NULL) +
       labs(subtitle = expression(paste("Prior predictive distribution for ", italic(h[i]))),
            x = NULL)

#grid approximatino of the posterior distribution 
n <- 200
#creates a tibble containing every possible combination of mu and sigma across specified values
d_grid <- tibble(mu = seq(from = 140, to = 160, length.out = n),
                 sigma = seq(from = 4, to = 9, length.out = n)) %>% 
  expand(mu, sigma)

head(d_grid)

grid_function <- function(mu, sigma) {
  dnorm(d2$height, mean = mu, sd = sigma, log = T) %>% 
       sum()
}

d_grid <- d_grid %>% 
          mutate(log_lh = map2(mu, sigma, grid_function)) %>% 
          unnest() %>% 
          mutate(prior_mu = dnorm(mu, mean = 178, sd = 20, log = T),
                 prior_sigma = dunif(sigma, min = 0, max = 50, log = T)) %>% 
          mutate(product = log_lh + prior_mu + prior_sigma) %>% 
          mutate(probability = exp(product - max(product)))

head(d_grid)

#making contour plot
d_grid %>% ggplot(aes(x = mu, y = sigma, z = probability)) +
           geom_contour() +
           labs(x = expression(mu),
                y = expression(sigma)) +
           coord_cartesian(xlim = range(d_grid$mu),
                           ylim = range(d_grid$sigma))

#making heat map
d_grid %>% ggplot(aes(x = mu, y = sigma)) +
           geom_raster(aes(fill = probability)) +
           scale_fill_viridis_c() +
           labs(x = expression(mu),
              y = expression(sigma))

#sampling from the posterior
d_grid_samples <- d_grid %>% sample_n(size = 1e4, replace = T, weight = probability)

d_grid_samples %>% ggplot(aes(x = mu, y = sigma)) +
                   geom_point(size = 0.9, alpha = 1/15) +
                    scale_fill_viridis_c() +
                    labs(x = expression(mu[samples]),
                         y = expression(sigma[samples]))

#getting the HDI for mu and sigma
library(tidybayes)
d_grid_samples %>% select(mu, sigma) %>% 
                   gather() %>% 
                   group_by(key) %>% 
                   mode_hdi(value)

#fitting the model with brm()
b4.1 <- brm(data = d2, family = gaussian, height ~ 1,
            prior = c(prior(normal(178, 20), class = Intercept),
                      prior(uniform(0, 50), class = sigma)),
            iter = 31000, warmup = 30000, chains = 4, cores = 4)

plot(b4.1)
summary(b4.1)

#changing the prior to make it super narrow
b4.2 <- brm(data = d2, family = gaussian,
            height ~ 1,
            prior = c(prior(normal(178, 0.1), class = Intercept),
                      prior(uniform(0, 50), class = sigma)),
            iter = 3000, warmup = 2000, chains = 4, cores = 4)
plot(b4.2)
summary(b4.2)

#sampling from a brm() fit
post <- posterior_samples(b4.1)
cov(post[,1:2]) #provides a variance-covariance matrix 

#decomposing into variances and the correlation matrix
post[,1:2] %>% 
          cov() %>% 
          diag()

post %>% select(b_Intercept, sigma) %>% cor()

summary(post[,1:2])

#4.4 Adding a predictor
ggplot(d2, aes(x = weight, y = height)) +
      geom_point(shape = 1, size = 2) + 
      theme_bw()

#modeling with brms
b4.3 <- brm(data = d2, family = gaussian,
            height ~ 1 + weight, 
            prior = c(prior(normal(156,100), class = Intercept),
                      prior(normal(0, 10), class = b),
                      prior(cauchy(0, 1), class = sigma)),
            iter = 41000, warmup = 40000, chains = 4, cores = 4)
plot(b4.3)

#interpreting model fit
#1. tables of estimates
posterior_summary(b4.3)[1:3,]

#to get the var-cov matrix, have to first put chains in a data frame
posterior_samples(b4.3) %>% 
        select(-lp__) %>% 
        cor() %>% 
        round(digits = 2)

#sometimes want to center to reduce the correlations among the parameters
d2 <- d2 %>% mutate(weight_c = weight - mean(weight))

#running the model again
#modeling with brms
b4.4 <- brm(data = d2, family = gaussian,
            height ~ 1 + weight_c, 
            prior = c(prior(normal(178,100), class = Intercept),
                      prior(normal(0, 10), class = b),
                      prior(uniform(0, 50), class = sigma)),
            iter = 46000, warmup = 45000, chains = 4, cores = 4,
            control = list(adapt_delta = 0.8,
                           max_treedepth = 10))
plot(b4.4)
posterior_summary(b4.4)[1:3,]
posterior_samples(b4.4) %>% 
  select(-lp__) %>% 
  cor() %>% 
  round(digits = 2)

#plotting the posterior inference against the data
#superimposing the model info over the height and weight data
d2 %>% 
   ggplot(aes(x = weight, y = height)) +
   geom_abline(intercept = fixef(b4.3)[1],
               slope = fixef(b4.3)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue")
#adding uncertainty around the mean
#by extracting samples :) 
post <- posterior_samples(b4.3)
post %>% slice(1:5)

N <- 10
b10 <- brm(data = d2 %>% slice(1:N),
           family = gaussian,
           height ~ 1 + weight,
           prior = c(prior(normal(178,100), class = Intercept),
                     prior(normal(0, 10), class = b),
                     prior(cauchy(0, 1), class = sigma)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4)
N <- 50 
b50 <- brm(data = d2 %>% slice(1:N),
                  family = gaussian,
                  height ~ 1 + weight,
                  prior = c(prior(normal(178,100), class = Intercept),
                            prior(normal(0, 10), class = b),
                            prior(cauchy(0, 1), class = sigma)),
                  iter = 2000, warmup = 1000, chains = 4, cores = 4)
N <- 150
b150 <- brm(data = d2 %>% slice(1:N),
                   family = gaussian,
                   height ~ 1 + weight,
                   prior = c(prior(normal(178,100), class = Intercept),
                             prior(normal(0, 10), class = b),
                             prior(cauchy(0, 1), class = sigma)),
                   iter = 2000, warmup = 1000, chains = 4, cores = 4)

N <- 352 
b352 <- brm(data = d2 %>% slice(1:N),
                   family = gaussian,
                   height ~ 1 + weight,
                   prior = c(prior(normal(178,100), class = Intercept),
                             prior(normal(0, 10), class = b),
                             prior(cauchy(0, 1), class = sigma)),
                   iter = 2000, warmup = 1000, chains = 4, cores = 4)

#putting chains into data frames
post10 <- posterior_samples(b10)
post50 <- posterior_samples(b50)
post150 <- posterior_samples(b150)
post325 <- posterior_samples(b352)

#plots
#just doing the first one for the moment
p10 <- 
      ggplot(data = d2[1:10, ],
             aes(x = weight, y = height)) + 
      geom_abline(intercept = post10[1:20, 1],
                  slope = post10[1:20,2],
                  size = 1/3, alpha = 0.3) +
      geom_point(shape = 1, size = 2, color = "royalblue")
p10    

#plotting regression intervals and contours
mu_at_50 <- post %>% 
            transmute(mu_at_50 = b_Intercept + b_weight * 50)
head(mu_at_50)
mean_hdi(mu_at_50[,1], .width = c(0.89, 0.95))

mu <- fitted(b4.3, summary = F)
str(mu)

weight_seq <- tibble(weight = seq(from = 25, to = 70, by = 1))
mu <- fitted(b4.3,
             summary = F,
             newdata = weight_seq) %>% 
      as_tibble() %>% 
      mutate(Iter = 1:4000) %>% 
      select(Iter, everything())
str(mu)

#change from wide to long format
mu <- mu %>% 
      gather(key, value, V1:V46) %>% 
      mutate(key = str_extract(key, "\\d+") %>% as.integer()) %>% 
      rename(weight = key,
             height = value) %>% 
      mutate(weight = weight + 24)

mu_summary <- fitted(b4.3, newdata = weight_seq) %>% as_tibble() %>% bind_cols(weight_seq)

#first 100 valyes in dist of mu at each weight value
d2 %>% ggplot(aes(x = weight, y = height)) +
       geom_point(data = mu %>% filter(Iter < 101),
                  alpha = 0.1)

#height data with the 95% HPDI of the mean as shaded region
d2 %>% ggplot(aes(x = weight, y = height)) +
       geom_ribbon(data = mu_summary,
                   aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
                   fill = "grey70") +
       geom_line(data = mu_summary,
                 aes(y = Estimate)) +
       geom_point(color = "navyblue", shape = 1, size = 1.5, alpha = 2/3)

##PREDICTION INTERVALS
#need to include info about sigma 
pred_height <- predict(b4.3, newdata = weight_seq) %>% 
               as_tibble() %>% 
               bind_cols(weight_seq)
pred_height %>% slice(1:6)

#COME BACK TO MAKE THIS PLOT

#4.5 Polynomial Regression
#standardizing variables to help with later interpretation
d <- d %>% mutate(weight_s = (weight- mean(weight))/sd(weight))

b4.5 <- brm(data = d, family = gaussian,
           height ~ 1 + weight_s + I(weight_s^2),
           prior = c(prior(normal(178,100), class = Intercept),
                     prior(normal(0, 10), class = b),
                     prior(cauchy(0, 1), class = sigma)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4)
plot(b4.5)
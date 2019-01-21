####SAMPLING THE IMAGINARY#######

library(tidyverse)

#typical vampire test example with dplyr
tibble(pposv = 0.95,
        pposm = 0.01,
        pv = 0.001) %>%
  mutate(ppos = pposv * pv + pposm * (1-pv),
         pvpos = pposv * pv / ppos) %>%
  glimpse()

#sampling from a grid-like approximate posterior
n <- 1000
n_success <- 6
n_trials <- 9

(d <- tibble(p_grid = seq(from = 0, to = 1, length.out = n),
            prior = 1) %>%
     mutate(lh = dbinom(n_success, size = n_trials, prob = p_grid),
            post = lh * prior,
            post = post/sum(post)))

#drawing samples
samples <- tibble(samples = sample(d$p_grid, prob = d$post, size = n, replace = T)) %>%
           mutate(sample_n = 1:n())
head(samples)

#plotting
samples %>%
            ggplot(aes(x = sample_n, y = samples)) +
            geom_line(size = 1/10) + 
            labs(x = "sample number",
                 y = "proportion of water (p)")

samples %>%
          ggplot(aes(x = samples)) +
          geom_density(fill = "black") + 
          coord_cartesian(xlim = 0:1) + 
          xlab("proportion of water (p)")

#sampling to summarize
#TYPE 1: intervals of defined boundaries
    #add up posterior probability where p < 0.5
d %>% filter(p_grid < 0.5) %>%
      summarise(sum = sum(post))

#that method not as easy once there are multiple parameters
#use samples from posterior instead
samples %>% filter(samples < 0.5) %>%
            summarise(sum = n()/n)

#how much post prob lies between 0.5 and 0.75
samples %>% filter(samples > 0.5 & samples < 0.75) %>%
            summarise(sum = n()/n)

#TYPE 2: intervals of a defined mass
#i.e. confidence interval
#what are the boundaries of the lower 80% post prob? 
q_80 <- quantile(samples$samples, prob = 0.8)
q_80

#what about middle 80% interval?
samples %>%
        summarise("10th percentile" = quantile(samples, p = 0.1),
                  "90th percentile" = quantile(samples, p = 0.9))

#in base r
q_10_90 <- quantile(samples$samples, prob = c(0.1, 0.9))

#different prior
        n <- 1000
        n_success <- 3
        n_trials <- 3
        
        (d <- tibble(p_grid = seq(from = 0, to = 1, length.out = n),
                     prior = rep(1,1000)) %>%
            mutate(lh = dbinom(n_success, size = n_trials, prob = p_grid),
                   post = lh * prior,
                   post = post/sum(post)))
        
        samples <- tibble(samples = sample(d$p_grid, prob = d$post, size = n, replace = T)) %>%
                   mutate(sample_n = 1:n())
        head(samples)
        
        samples %>%
                    ggplot(aes(x = samples)) +
                    geom_density(fill = "black") + 
                    coord_cartesian(xlim = 0:1) + 
                    xlab("proportion of water (p)")

#rethinking PI function instead of quantiles
library(rethinking)
PI(samples$samples, prob = 0.5)
#highest posterior density interval
HPDI(samples$samples, prob = 0.5)

#tidybayes package
library(tidybayes)
median_qi(samples$samples, .width = 0.5)
median_qi(samples$samples, .width = c(0.5, 0.8, 0.99))
mode_hdi(samples$samples, .width = 0.5)

#TYPE 3: point estimates
    #maximum a posteriori (MAP)
    d %>%
          arrange(desc(post)) %>%
          slice(1)
    
    #chainmode 
     chainmode(samples$samples, adj = 0.01)

    #loss function
     d %>%
          mutate(loss = post * abs(0.5-p_grid)) %>%
          summarise('ex_loss' = sum(loss))

    #applying the function to every possible decision
     make_loss <- function(our_d) {
       d %>%
            mutate(loss = post * abs(our_d - p_grid)) %>%
            summarise("ex_loss" = sum(loss))
     }

    #using the map functions in purrr
     (
       l <- d %>%
                  select(p_grid) %>%
                  rename(decision = p_grid) %>%
                  mutate(weighted_avg_loss = purrr::map(decision, make_loss)) %>%
                  unnest()
     )     
     
     #making the graph of the loss function
     min_loss <- l %>%
                      filter(ex_loss == min(ex_loss)) %>%
                      as.numeric()
     
     l %>%
            ggplot(aes(x = decision)) + 
            geom_ribbon(aes(ymin = 0, ymax = ex_loss), fill = "grey75") +
            geom_vline(xintercept = min_loss[1], color = "white", linetype = 3) +
            geom_hline(yintercept = min_loss[2], color = "white", linetype = 3) +
            ylab("expected proportion loss") +
            theme(panel.grid = element_blank())
     
     #min_loss corresponds to the median of the posterior
     samples %>%
                summarise(post_med = median(samples))
     min_loss[1]     

     #what about a different loss function?
     #quadratic loss
     make_loss <- function(our_d) {
       d %>%
         mutate(loss = post * abs(our_d - p_grid)^2) %>%
         summarise("ex_loss" = sum(loss))
     }
     
     #using the map functions in purrr
     (
       l <- d %>%
         select(p_grid) %>%
         rename(decision = p_grid) %>%
         mutate(weighted_avg_loss = purrr::map(decision, make_loss)) %>%
         unnest()
     )     
     
     #making the graph of the loss function
     min_loss <- l %>%
       filter(ex_loss == min(ex_loss)) %>%
       as.numeric()
     
     l %>%
       ggplot(aes(x = decision)) + 
       geom_ribbon(aes(ymin = 0, ymax = ex_loss), fill = "grey75") +
       geom_vline(xintercept = min_loss[1], color = "white", linetype = 3) +
       geom_hline(yintercept = min_loss[2], color = "white", linetype = 3) +
       ylab("expected proportion loss") +
       theme(panel.grid = element_blank())
     
     #corresponds to the mean of the post\
     samples %>%
       summarise(post_mean = mean(samples))
     min_loss[1] 

     
#SAMPLING TO SIMULATE PREDICTION
     tibble(n = 2,
            p = 0.7,
            w = 0:2) %>%
       mutate(density = dbinom(w, size = n, prob = p))

     #setting seed makes results reproducible
     set.seed(331)
     rbinom(10, size = 2, prob = 0.7)     

    #generating 100,000 dummy observations
     n_draws <- 1e5
     set.seed(331)     
     d <- tibble(draws = rbinom(n_draws, size = 9, prob = 0.7))     
     d %>%
          group_by(draws) %>%
          count() %>%
          mutate(proportion = n / nrow(d))
     
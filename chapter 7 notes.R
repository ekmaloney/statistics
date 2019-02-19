library(rethinking)
library(tidyverse)

data(rugged)
d <- rugged

d <- d %>% 
     mutate(log_gdp = log(rgdppc_2000))

dd <- d %>% filter(!is.na(rgdppc_2000))

d1 <- dd %>% filter(cont_africa == 1)
d2 <- dd %>% filter(cont_africa == 0)

#african nations
m7.1 <- rethinking::map(
            alist(
            log_gdp ~ dnorm(mu, sigma),
            mu <- a + bR*rugged,
            a ~ dnorm(8, 100),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)
            ),
            data = d1)

#non african nations
m7.2 <- rethinking::map(
            alist(
              log_gdp ~ dnorm(mu, sigma),
              mu <- a + bR*rugged,
              a ~ dnorm(8, 100),
              bR ~ dnorm(0, 1),
              sigma ~ dunif(0, 10)
            ),
            data = d2)

precis(m7.1)
precis(m7.2)

#interaction effect
m7.5 <- rethinking::map(
              alist(
                log_gdp ~ dnorm(mu, sigma),
                mu <- a + gamma*rugged + bA*cont_africa,
                gamma <- bR + bAR*cont_africa,
                a ~ dnorm(8, 100),
                bA ~ dnorm(0, 1),
                bR ~ dnorm(0, 1),
                bAR ~ dnorm(0, 1),
                sigma ~ dunif(0, 10)
              ),
              data = dd)

precis(m7.5)

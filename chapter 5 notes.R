#################CHAPTER 5######################
#section 1 - spurious associations

#load data
library(rethinking)
library(tidyverse)
data("WaffleDivorce")
d <- WaffleDivorce

#standardize predictor
d <- d %>% mutate(medagestd =
                    (MedianAgeMarriage - mean(MedianAgeMarriage, na.rm = T))/sd(MedianAgeMarriage),
                  marriagestd = 
                    (Marriage - mean(Marriage, na.rm = T))/sd(Marriage))

#fit model
m5.1 <- rethinking::map(
        alist(
          Divorce ~ dnorm(mu, sigma),
          mu <- a + bA*medagestd,
          a ~ dnorm(10, 10),
          bA ~ dnorm(0, 1),
          sigma ~ dunif(0, 10)
        ), 
        data = d)

m5.2 <- rethinking::map(
          alist(
            Divorce ~ dnorm(mu, sigma),
            mu <- a + bR*marriagestd,
            a ~ dnorm(10, 10),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)
          ), 
          data = d)

precis(m5.1)
precis(m5.2)

#putting together
m5.3 <- rethinking::map(
          alist(
            Divorce ~ dnorm(mu, sigma),
            mu <- a + bA*medagestd + bR*marriagestd,
            a ~ dnorm(10, 10),
            bA ~ dnorm(0, 1),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)
          ), 
          data = d)
precis(m5.3)
plot(precis(m5.3))

#Predictor residual plots
m5.4 <- rethinking::map(
        alist(
          marriagestd ~ dnorm(mu, sigma),
          mu <- a + b*medagestd,
          a ~ dnorm(10, 10),
          b ~ dnorm(0, 1),
          sigma ~ dunif(0, 10)
        ),
        data = d)

#compute expected value at MAP for each state
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$medagestd
#compute residual for each state
mr_residual <- d$marriagestd - mu

plot(marriagestd ~ medagestd, d, col = rangi2)
abline(m5.4)
for(i in 1:length(mr_residual)) {
  x <- d$medagestd[i]
  y <- d$marriagestd[i]
  lines(c(x,x), c(mu[i], y), lwd=0.5, col = col.alpha("black", 0.7))
}

#counterfactual plots
#prepare new counterfactual data
A.avg <- mean(d$medagestd)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(marriagestd = R.seq,
                        medagestd = A.avg)

#compute counterfactial mean divorce
mu <- link(m5.3, data = pred.data)
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

#simluate counterfactual divorce outcomes
R.sim <- sim(m5.3, data = pred.data, n = 1e4)
R.pi <- apply(R.sim, 2, PI)

#display predictions, hiding raw data bc counterfactual
plot(Divorce ~ marriagestd, data = d, type = "n")
mtext("Med Age Marriage = 0")
lines(R.seq, mu_mean)
shade(mu_PI, R.seq)
shade(R.pi, R.seq)

#posterior prediction plots
mu <- link(m5.3)
#summarise samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
#simulate obs
divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

plot(mu.mean ~ d$Divorce, col = rangi2, ylim = range(mu.PI),
      xlab = "Observed Divorce", ylab = "Predicted Divorce")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(d)) {
  lines(rep(d$Divorce[i],2), c(mu.PI[1,i], mu.PI[2,1]),
        col = rangi2)
}

#5.2 MASKED RELATIONSHIP
data(milk)
d <- milk
str(d)

#model 1
dcc <- d %>% filter(complete.cases(d))
m5.5 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc
)
precis(m5.5, digits = 3)

#model 2
#first add variable for log(mass)
dcc <- dcc %>% mutate(logmass = log(mass))
m5.6 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*logmass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc
)
precis(m5.6, digits = 3)

#putting both together
m5.7 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*logmass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc
)
precis(m5.7, digits = 3)

#problems with adding too many covariates
#1. Multicollinearity
m5.10 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf*perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

m5.11 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.10, digits = 3)
precis(m5.11, digits = 3)

#putting both together
m5.12 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose + bf*perc.fat,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.12, digits = 3)

pairs( ~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

#2. post-treatment bias
#didn't work this example

#3. Categorical variables
d <- d %>% mutate(nwm = ifelse(clade == "New World Monkey", 1, 0),
                  str = ifelse(clade == "Strepsirrhine", 1, 0),
                  owm = ifelse(clade == "Old World Monkey", 1, 0))

m5.16 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.nwm*nwm + b.owm*owm + b.str*str,
    a ~ dnorm(0.6, 10),
    b.nwm ~ dnorm(0, 1),
    b.owm ~ dnorm(0, 1),
    b.str ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.16)

#sample posterior
post <- extract.samples(m5.16)

#compute averages for each category
mu.ape <- post$a
mu.nwm <- post$a + post$b.nwm
mu.owm <- post$a + post$b.owm
mu.str <- post$a + post$b.str

precis(data.frame(mu.ape, mu.nwm, mu.owm, mu.str))

#can go back and look at unique intercepts if you want


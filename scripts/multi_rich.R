library(tidyverse)
library(rstan)
library(rethinking)

beta1 <- 3
beta2 <- 5
alpha <- 5
std_dev <- 5

what1 <- .2
what2 <- 1

x1_data <- rep(1:50, each = 3) + runif(50, -.2, .2)
x2_data <- seq(from = .1, to = .5, length.out = 150)
x3_data <- seq(from = -20, to = -100, length.out = 150)
y_data <- beta1 * x1_data + beta2 * x2_data + alpha + rnorm(length(x_data), 0, std_dev)
dat <- data.frame(x1 = x1_data, x2 = x2_data, x3 = x3_data, y = y_data) # tibbles don't work with rethinking

ggplot(dat) + geom_point(aes(x, y))


m1 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x1 + a,
    b1 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b2 * x2  + a,
    b2 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m3 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x1 + b2 * x2 + a,
    b1 ~ dnorm(0, 2),
    b2 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m4 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x1 + b2 * x2 + b3 * x3 + a,
    b1 ~ dnorm(0, 2),
    b2 ~ dnorm(0, 2),
    b3 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)



par(mfrow = c(2, 2))
mu <- link(m1)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x1, data = dat, main = "m1")
lines(dat$x1, mu_mean)
shade(mu_pi, dat$x1)

mu <- link(m2)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x1, data = dat, main = "m2")
lines(dat$x1, mu_mean)
shade(mu_pi, dat$x1)

mu <- link(m3)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x1, data = dat, main = "m3")
lines(dat$x1, mu_mean)
shade(mu_pi, dat$x1)

mu <- link(m4)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x1, data = dat, main = "m4")
lines(dat$x1, mu_mean)
shade(mu_pi, dat$x1)

par(mfrow = c(1, 1))

compare(m1, m2, m3, m4)

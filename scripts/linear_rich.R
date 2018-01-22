library(tidyverse)
library(rstan)
library(rethinking)

beta <- 3
alpha <- 5
std_dev <- 15

what1 <- .2
what2 <- 1

x_data <- rep(1:50, each = 5)
y_data <- beta * x_data + alpha + rnorm(length(x_data), 0, std_dev)
z_data <- what1 * log(x_data) ^ 2 - what2 * x_data^2 + rnorm(length(x_data), 0, std_dev)
dat <- data.frame(x = x_data, y = y_data, z = z_data) # tibbles don't work with rethinking

ggplot(dat) + geom_point(aes(x, y))
  

m1 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + a,
    b1 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + a,
    b1 ~ dnorm(0, 2),
    b2 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m3 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b2 * x ^ 2 + a,
    b2 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m4 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b3 * z + a,
    b1 ~ dnorm(0, 2),
    b3 ~ dnorm(0, 2),
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m5 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)

m6 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + b3 * x ^ 3 + 
      b4 * x ^ 4 +  a,
    a ~ dnorm(0, 5),
    b1 ~ dnorm(0, 5),
    b2 ~ dnorm(0, 5),
    b3 ~ dnorm(0, 5),
    b4 ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ),
  data = dat
)


par(mfrow = c(3, 2))
mu <- link(m1)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m1")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m2)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m2")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m3)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m3")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m4)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m4")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m5)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m5")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m6)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m6")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

par(mfrow = c(1, 1))

compare(m1, m2, m3, m4, m5, m6)

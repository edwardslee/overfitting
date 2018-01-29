library(tidyverse)
library(rstan)
library(rethinking)

beta <- 3
alpha <- 5
std_dev <- 25



x_data <- rep(seq(from = 10, to = 50, by = 10), each = 1)
y_data <- beta * x_data + alpha + rnorm(length(x_data), 0, std_dev)
dat <- data.frame(x = x_data, y = y_data) # tibbles don't work with rethinking

ggplot(dat) + geom_point(aes(x, y))


# constant
m_const <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(80, 10),
    sigma ~ dcauchy(0, 3)
  ),
  data = dat
)

# linear
m_linear <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + a,
    b1 ~ dnorm(0, 10),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 5)
  ),
  data = dat
)

m_quad <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + a,
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 5)
  ),
  data = dat
)

m_3rd <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + b3 * x ^ 3 +  a,
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 5)
  ),
  data = dat
)

m_4th <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + b3 * x ^ 3 + b4 * x ^ 4 + a,
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    b4 ~ dnorm(0, 10),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 5)
  ),
  data = dat
)

m_5th <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x + b2 * x ^ 2 + b3 * x ^ 3 + b4 * x ^ 4 + b5 * x ^ 5 + a,
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    b4 ~ dnorm(0, 10),
    b5 ~ dnorm(0, 10),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 5)
  ),
  data = dat
)


par(mfrow = c(3, 2))
mu <- link(m_const)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m1")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m_linear)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m2")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m_quad)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m3")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m_3rd)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m4")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m_4th)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m5")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

mu <- link(m_5th)
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, HPDI, prob = .89)
plot(y ~ x, data = dat, main = "m6")
lines(dat$x, mu_mean)
shade(mu_pi, dat$x)

par(mfrow = c(1, 1))

compare(m_const, m_linear, m_quad, m_3rd, m_4th, m_5th)


rm(list = ls())

asset_paths <- function(s0, mu, sigma, nsims, periods = c(0, 1)) {
  
  s0 <- as.vector(s0)
  nsteps <- length(periods)
  dt <- c(periods[1], diff(periods))
  
  drift <- mu - 0.5 * sigma^2
  
  if(nsteps == 1) {
    s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
  } else {
    temp <- matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
    for(i in 2:nsteps) {
      temp[i,] <- temp[i,] * temp[(i-1),]
    } 
    s0 * temp
  }
}

daily_ret <- asset_paths(s0 = 50, mu = 0.1, sigma = 0.3, nsims = 10000, periods = seq(from = 0, to = 1, by = 1/365))
monthly_ret <- asset_paths(s0 = 50, mu = 0.1, sigma = 0.3, nsims = 10000, periods = seq(from = 0, to = 1, by = 1/12))
quarterly_ret <- asset_paths(s0 = 50, mu = 0.1, sigma = 0.3, nsims = 10000, periods = seq(from = 0, to = 1, by = 1/4))
yearly_ret <- asset_paths(s0 = 50, mu = 0.1, sigma = 0.3, nsims = 10000, periods = seq(from = 0, to = 1, by = 1))

dev.off()
par(mfrow = c(2,2))
matplot(daily_ret, type = "l")
matplot(monthly_ret, type = "l")
matplot(quarterly_ret, type = "l")
matplot(yearly_ret, type = "l")

mean(daily_ret[366,])
mean(monthly_ret[13,])
mean(quarterly_ret[5,])
mean(yearly_ret[2,])

sd(daily_ret[366,])
sd(monthly_ret[13,])
sd(quarterly_ret[5,])
sd(yearly_ret[2,])

dev.off()
yearly_ret_10 <- asset_paths(s0 = 50, mu = 0.1, sigma = 0.3, nsims = 10000, periods = seq(from = 0, to = 10, by = 1))
matplot(yearly_ret_10, type = "l")
mean(yearly_ret_10[11,])
max(yearly_ret_10[11,])
min(yearly_ret_10[11,])





library(MASS)
sigma <- c(0.11,0.16)
rho <- 0.63
cov.matrix <- sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
mvrnorm(20, rep(0, 2), cov.matrix)



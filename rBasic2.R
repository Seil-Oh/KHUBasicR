
# Genetic algorithms (GAs) ------------------------------------------------
install.packages("GA")
library(GA)

# Function optimisation in one dimension
f <- function(x)  (x^2+x)*cos(x)
min <- -10
max <- 10
curve(f, min, max, n = 1000)

GA <- ga(type = "real-valued", fitness = f, min = min, max = max, 
         monitor = FALSE)
summary(GA)

plot(GA)

curve(f, min, max, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)


# Function optimisation in two dimensions
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, color.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors)

GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         min = c(-5.12, -5.12), max = c(5.12, 5.12), 
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)

plot(GA)

filled.contour(x1, x2, f, color.palette = bl2gr.colors, 
               plot.axes = { axis(1); axis(2); 
                 points(GA@solution[,1], GA@solution[,2], 
                        pch = 3, cex = 2, col = "white", lwd = 2) })


# Asset Paths -------------------------------------------------------------
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



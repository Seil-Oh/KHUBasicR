

# Start -------------------------------------------------------------------
# comments

ls()

# remove
rm(list = ls())

# Working Directory
getwd()
setwd()

# Vector ------------------------------------------------------------------
x_c <- c(5,6,7,8)
x_c_1 <- 5:8
# Index
x_c[1]
x_c[2]
x_c[4]
x_c[1:3]
x_c[c(1,3,4)]
x_c[1] <- 1
x_c
x_c[-1]
x_c[-(1:3)]

# Operations with vector
x_c * 10
x_c / 10
x_c^2
cos(x_c)

# Number of elements in the vector
length(x_c)

# mean, standard deviation, variance, min, max, sum, cumsum, range, prod, cumprod, summary
mean(x_c)
sd(x_c)
var(x_c)
min(x_c)
max(x_c)
sum(x_c)
cumsum(x_c)
range(x_c)
prod(x_c)
cumprod(x_c)
summary(x_c)

# Generating sequence
x_seq_1 <- seq(from = 1, to = 10)
x_seq_2 <- seq(from = 1, to = 10, length.out = 5)
x_seq_3 <- seq(by = 0.45, from = 2.7, to = 6.7)
x_seq_4 <- seq(from = -pi, to = pi, length.out = 12)
x_seq_1[1]

# Repeating value
y <- 2
rep(y, times = 5)
w <- c(2,5)
rep(w, times = 5)
rep(w, each = 5)

# Sequences of random numbers
runif(n = 6, min = -2, max = 2)   # Uniform distribution
rnorm(n = 6, mean = 0, sd = 1)    # Normal distribution
set.seed(123)
runif(n = 6, min = -2, max = 2)   # Uniform distribution
rnorm(n = 6, mean = 0, sd = 1)    # Normal distribution

# Logical vector
v <- runif(n = 10, min = -8, max = 10)
v > 0
which(v > 0)
v[which(v > 0)]
v[v > 0]


# Matrices ----------------------------------------------------------------
# Matrix
x_mat <- matrix(data = c(1,2,3,4,
                         5,6,7,8), nrow = 2, ncol = 4)

x_mat <- matrix(data = seq(1,8), nrow = 2, ncol = 4)

x_mat <- matrix(data = c(1,2,3,4,
                         5,6,7,8), nrow = 2, ncol = 4, byrow = T)

rownames(x_mat) <- c("Row.1","Row.2")
colnames(x_mat) <- c("Col.1","Col.2","Col.3","Col.4")

x_mat <- matrix(data = c(1,2,3,4,
                         5,6,7,8), nrow = 2, ncol = 4, byrow = T, 
                dimnames = list(c("Row_1","Row_2"),
                                c("Col_1","Col_2","Col_3","Col_4")))
# Index
x_mat[2,3]
x_mat[2,]
x_mat[,3]
x_mat[,"Col_1"]
x_mat["Row_1",]
x_mat[c("Row_1","Row_2"),]

# Bind
x_row <- 1:4
y_row <- 6:9
z_mat_rbind <- rbind(x_row, y_row)
z_mat_cbind <- cbind(x_row, y_row)

# Diagonal matrix
diag(c(4,5,6))
diag(rep(1,3))

# Arithmetic operations on matrices
x_mat - 1
x_mat / 2
x_mat * 3
sqrt(x_mat)
options(digits = 3)
sqrt(x_mat)

# Matrix multiplication
set.seed(321)
m1 <- matrix(data = runif(n = 9, min = 0, max = 10), nrow = 3, ncol = 3)
m2 <- matrix(data = runif(n = 9, min = -9, max = 30), nrow = 3, ncol = 3)
m3 <- m1 %*% m2

# Transpose and determinant
t(m1)
det(m1)

# crossprod & tcrossprod
crossprod(m1, m2)
t(m1) %*% m2
tcrossprod(m1, m2)
m1 %*% t(m2)


# Programming and functions ----------------------------------------------------------------
# Contitional execution: if and ifelse

# if (condition 1) {
#   result 1
# } else if (condition 2) {
#   result 2
# } else {
#   result 3
# }

x <- -3

if (x < -1) {
  y <- -1
} else if (x < 0) {
  y <- 0
} else {
  y <- 1
}

# ifelse
x <- -1
y_1 <- ifelse(x > 0, 10, 20)
if (x > 0) {
  y_2 <- 10
} else {
  y_2 <- 20
}

# Loops
sum <- 0
for (i in 1:10) {
  sum <- sum + 1
}
sum_1 <- 0
for (i in 1:10) {
  cat(i, "번째 실행\n")
  sum_1 <- sum_1 + 1
  print(sum)
}

# while & repeat
x <- 0
n <- 0
set.seed(333)
while (x <= 10) {
  n <- n+1
  x <- x + rnorm(n = 1, mean = 0.5, sd = 1)
}
print(paste("n = ", n, "x = ", x))
print(paste("n = ", n, "x = ", round(x,2)))
print(paste0("n = ", n, "x = ", round(x,2)))
print(paste0("n = ", n, " x = ", round(x,2)))


x <- 0
n <- 0
set.seed(333)
repeat {
  n <- n + 1
  dx <- rnorm(n = 1, mean = 0.5, sd = 1)
  if (dx < -1) 
    next
  x <- x + dx
  if (x > 10)
    break
}
print(paste0("n = ", n, ", x = ", round(x, 2)))

# User-defined function
powers_func <- function(x) {
  
  matrix(c(x, x^2, x^3), nrow = length(x), ncol = 3)
  
}
v <- 1:5
powers_func(v)

powers_func_y <- function(x) {
  
  y <- matrix(c(x, x^2, x^3), nrow = length(x), ncol = 3)
  return(y)
  
}
powers_func_y(v)

mod_calc <- function(x, y) {
  
  w <- x%/%y
  z <- x%%y
  res <- c(w,z)
  return(res)
  
}
mod_calc(17,3)

randwalk <- function(N) {
  
  walk <- rep(0, N+1)
  
  for (i in 2:(N+1)) {
    
    x <- runif(1)
    
    if (x <= 0.5) {
      
      walk[i] <- walk[i-1] - 1
      
    } else {
      
      walk[i] <- walk[i-1] + 1
      
    }
    
  }
  
  return(walk[N])
  
}

multiwalks <- c()
for (k in 1:100) {
  
  multiwalks[k] <- randwalk(100)
  
}

mean(multiwalks)
sd(multiwalks)
hist(multiwalks)
plot(density(multiwalks))


sample(x = 1:100, size = 20)
sample(x = c(-1,1), size = 10, replace = TRUE, prob = c(0.5, 0.5))
randwalk2 <- function(N) {
  
  res <- sum(sample(x = c(-1,1), size = N, replace = TRUE, prob = c(0.5, 0.5)))
  return(res)
  
}
multiwalks2 <- c()
for (k in 1:100) {
  
  multiwalks2[k] <- randwalk2(100)
  
}
mean(multiwalks2)
sd(multiwalks2)
hist(multiwalks2)
plot(density(multiwalks2))


# Graphing ----------------------------------------------------------------
x_1 <- c(0,cumsum(sample(x = c(-1,1), size = 100, replace = TRUE, prob = c(0.5, 0.5))))
x_2 <- c(0,cumsum(sample(x = c(-1,1), size = 100, replace = TRUE, prob = c(0.5, 0.5))))

dev.off()
plot(x_1)
?plot
plot(x_1, type = "l")
plot(x_1, type = "b")
plot(x_1, type = "s")
plot(x_1, type = "l", lwd = 1)
plot(x_1, type = "l", lwd = 3)
plot(x_1, type = "l", xlab = "Time", ylab = "Value")
plot(x_1, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk")
lines(x_2, col = "red")
plot(x_1, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk", ylim = c(min(x_1,x_2),max(x_1,x_2)))
lines(x_2, col = "red")
abline(h = 0, lty = 3)
grid()
legend("topright", legend = c("X_1","X_2"), col = 1:2, fill = 1:2)

par(mfrow = c(2,1))
plot(x_1, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk: X_1")
plot(x_2, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk: X_2", col = "red")

par(mfrow = c(1,2))
plot(x_1, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk: X_1")
plot(x_2, type = "l", xlab = "Time", ylab = "Value", main = "Random Walk: X_2", col = "red")


x_1 <- c(0,cumsum(sample(x = c(-1,1), size = 1000, replace = TRUE, prob = c(0.5, 0.5))))
x_2 <- c(0,cumsum(sample(x = c(-1,1), size = 1000, replace = TRUE, prob = c(0.5, 0.5))))
x_3 <- c(0,cumsum(sample(x = c(-1,1), size = 1000, replace = TRUE, prob = c(0.5, 0.5))))
x_4 <- c(0,cumsum(sample(x = c(-1,1), size = 1000, replace = TRUE, prob = c(0.5, 0.5))))
x_5 <- c(0,cumsum(sample(x = c(-1,1), size = 1000, replace = TRUE, prob = c(0.5, 0.5))))
x_paths <- cbind(x_1, x_2, x_3, x_4, x_5)

dev.off()
matplot(x_paths, type = "l", 
        xlab = "Time", ylab = "Value", main = "Random Walks")
grid()
legend("topleft", legend = paste0("X_", 1:5), title = "Paths", col = 1:5, fill = 1:5)

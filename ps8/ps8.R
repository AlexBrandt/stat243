# Question 1

# a) In order to simulate a bootstrap, we begin with our two models, the "control" which
# which we assert is the control (stadnard linear regression) and our black box method ("the
# bootstrap").  From these two methods, we want to compare the absolute predictive error
# as well as the predictiv interal coverage.  Initially, 

# b) I suck.

# Question 2

pareto <- function(x, alpha, beta) {
  return(beta * (alpha^beta) / (x^(beta+1)))
}

inverse_cdf_pareto <- function(p, alpha, beta) {
  return( alpha * ((1 - p) ^ (-1 / beta)) )
}

expd <- function(x, lambda, xshift) {
  return( (x >= 2) * lambda * exp( -lambda * (x - xshift) ))
}

inverse_cdf_myexp <- function(p) {
  return(2 - log(1-p))
}
# ALPHA IS 2.  BETA IS 3.
q <- integrate(pareto, alpha = 2, beta = 3, lower = 0, upper = 1000000)
q$value

# a) The exponential decays faster.

# b.1)

# b.2)

alpha <- 2
beta <- 3

m <- 10000
us <- runif(n=m)

vals <- inverse_cdf_pareto(us, alpha=2, beta=3)

f <- expd(vals, lambda = 1, xshift=2) 
g <- pareto(vals, alpha=alpha, beta=beta)

my_weights <- f / g
ex_vals <- vals * my_weights
ex_2_vals <- vals^2 * my_weights
EX <- mean(ex_vals)
EX2 <- mean(ex_2_vals)

hist(ex_vals)
hist(ex_2_vals)

hist(my_weights)

EX
EX2

# c)

m <- 10000
us <- runif(n=m)

vals <- inverse_cdf_myexp(us)

g <- expd(vals, lambda = 1, xshift=2) 
f <- pareto(vals,alpha=alpha,beta=beta)

my_weights <- f / g
ex_vals <- vals * my_weights
ex_2_vals <- vals^2 * my_weights
EX <- mean(ex_vals)
EX2 <- mean(ex_2_vals)

hist(ex_vals)
hist(ex_2_vals)

hist(my_weights)

EX
EX2

# Question 3

beta <- c(0,0,0,0)
X <- cbind(1, rnorm(n = 100), rnorm(n = 100), rnorm(n = 100))
# y <- sample(0:1, 100, replace=TRUE)

# beta <- solve(t(X) %*% X) %*% t(X) %*% y
# Z ~ N(Xbeta, 1)
mean <- X %*% beta
Z <- rnorm(mean,sd=1) # + matrix(rnorm(n=nrow(X),sd=sig),ncol=1)
y <- (Z > 0)
iter <- 0

converge <- FALSE

while (!converge) {
  Ez <- mean - (1-y)*dnorm(mean)/pnorm(-mean) + y*dnorm(mean)/(1-pnorm(-mean))
  
  # To check for convergence later
  
  former_beta <- beta
  
  beta <- solve(t(X) %*% X) %*% t(X) %*% Ez
  mean <- X %*% beta

  # print(beta)
  iter <- iter + 1

  # print(max(abs(beta - former_beta)))
  
  if (max(abs(beta - former_beta)) < .00001 ) {
    converge <- TRUE
  }
}

print(iter)
print(beta)

llp <- function(beta, x, y) {
  mean <- x %*% beta
  loglike <- sum(y * log(pnorm(mu)) + (1-Y) * log(pnorm(-mu)))
}

glm_probit <- glm(y ~ X[,2] + X[,3] + X[,4], family = binomial(link = "probit"))
glm_probit$coefficients

# b) Propose a reasonable starting values for beta?

std <- function(x) sd(x)/sqrt(length(x))

# c) done above (why would I ever use "lm"?)

# d) how to check answer by maximizing the log-likelihood

optim( method="BFGS")

# Question 4

helical_R_filename = "~/Dropbox/Berkeley Classes/Stat245/state243-fall-2015/ps/helical.R"

theta <- function(x1,x2) atan2(x2, x1)/(2*pi)

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2+x[2]^2)-1)
  f3 <- x[3]
  return(f1^2+f2^2+f3^2)
}

x=seq(-10,10,1)
y=seq(-10,10,1)
x_len= length(x)
y_len = length(y)
z=array(0,dim=c(x_len,y_len))

for (i in 1:x_len) for (j in 1:y_len) z[i,j] = f(c(x[i],y[j],-3))

persp(x, y, z, phi = 20, theta = 40)
contour(x, y, z)
image(x, y, z)

install.packages("optimx")
library("optimx")


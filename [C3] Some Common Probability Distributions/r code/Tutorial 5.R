library(stats);
library(prob);
library(mosaicCalc);

Q1 <- function() {
  X <- c(2, 3, 4, 5, 6);
  prob_X <- c(0.01, 0.25, 0.40, 0.30, 0.04);
  
  # calculate first-moment
  first_moment = 0;
  for (i in 1:length(X)) {
    first_moment = first_moment + (X[i] * prob_X[i]);
  }
  print(first_moment);
  
  # calculate second-moment
  second_moment = 0;
  for (i in 1:length(X)) {
    second_moment = second_moment + (X[i]^2 * prob_X[i]);
  }
  print(second_moment);
  
  # find variance of X using definition of variance
  # ----- find variance -> DISCRETE-CASE -----
  var1 = 0;
  mean = first_moment;
  for (i in 1:length(X)) {
    var1 = var1 + ( (X[i] - mean)^2  * prob_X[i]);
  }
  print(var1);
  
  # find variance of X using 𝑉(𝑋) = 𝐸(𝑋2) − [𝐸(𝑋)]2
  var2 = second_moment - (first_moment)^2;
  print(var2);
  
  # find mean and variance of Z = 3X - 2
  Z <- c();
  for (i in 1:length(X)) {
    Z[i] = 3 * X[i]  - 2;
  }
  prob_Z = prob_X;
  
  # calculate first-moment (mean)
  first_moment_Z = 0;
  for (i in 1:length(Z)) {
    first_moment_Z = first_moment_Z + (Z[i] * prob_Z[i]);
  }
  print(first_moment_Z);
  
  # calculate second-moment
  second_moment_Z = 0;
  for (i in 1:length(Z)) {
    second_moment_Z = second_moment_Z + (Z[i]^2 * prob_Z[i]);
  }
  # calculate variance
  var_Z = second_moment_Z - (first_moment_Z)^2;
  
  print(var_Z);
  
  # find probability (mass) function of Z
  OUTCOME_Z = Z;
  PROBABILITY_Z = prob_Z;
  ps_Z = probspace(x=OUTCOME_Z, probs=PROBABILITY_Z);
  print(ps_Z)
}

Q1d <- function() {
  X <- c(2, 3, 4, 5, 6);
  prob_X <- c(0.01, 0.25, 0.40, 0.30, 0.04);
  
  # a better way of transforming probability-space?
  ps <- probspace(x=X, probs=prob_X);
  ps = addrv(ps, z=3*x-2);
  
  mean = 0;
  for (row in rownames(ps)) {
    value = ps[row, "z"] * ps[row, "probs"];
    mean = mean + value;
  }
  
  variance = 0;
  for (row in rownames(ps)) {
    value = (ps[row, "z"] - mean)^2 * ps[row, "probs"];
    variance = variance + value;
  }  
  
  print(mean);
  print(variance);
}

Q1d <- function() {
  # given that: W = aZ + b
  
  # MEAN of W
  # E[W] = E[aZ + b] = a * E[Z] + b = 10.33a + b;
  
  # VARAINCE of W
  # Var[W] = Var[aZ + b] = a^2 * Var[Z];
}

Q2 <- function() {
  X <- c(0, 1, 2, 3, 4, 5);
  prob_X <- c(1/15, 2/15, 2/15, 3/15, 4/15, 3/15);
  
  ps <- probspace(x=X, probs=prob_X);
  
  # find the profit in each scenario
  ps = addrv(ps, profit=x*(1.65-1.20) - (5-x)*(1.20*1/4));
  
  # calculate the expected-value of the profits
  mean = 0;
  for (row in rownames(ps)) {
    value = ps[row, "profit"] * ps[row, "probs"];
    mean = mean + value;
  }
  
  print(mean);
}

Q3a <- function() {
  # ----- skipped -----
}

Q3b <- function() {
  ps <- iidspace(x=1:6, ntrials=3, probs=NULL) # same as rolldie(3), since probs=NULL so equal-chances assumed
  
  ps = addrv(ps, FUN=min, name="M");
  # print(tail(S))
  
  # calculate the expected-value of M
  mean = 0;
  for (row in rownames(ps)) {
    value = ps[row, "M"] * ps[row, "probs"];
    mean = mean + value;
  } 
  print(mean)
}

Q4a <- function() {
  # find mean of X
  integrand_mean <- function(x) {
    ( x ) * ( 2*(1-x) ) # ( x ) * ( pdf for x )
  };
  mean = integrate(f=integrand_mean, lower=0, upper=1);
  
  # find variance of X
  integrand_variance <- function(x) {
    # ----- REQUIRED PARAMETER --------------------------------------------
    mean = 1/3;
    # ---------------------------------------------------------------------
    
    ( x - mean )^2 * ( 2*(1-x) ) # ( x - mean )^2 * ( pdf for x )
  };
  variance = integrate(f=integrand_variance, lower=0, upper=1);
  
  print(mean);
  print(variance);
}

Q4b <- function() {
  # find mean of Y
  integrand_mean <- function(x) {
    ( 3*x-2 ) * ( 2*(1-x) ) # ( y ) * ( pdf for y )
  };
  mean = integrate(f=integrand_mean, lower=0, upper=1);
  
  # find variance of Y
  integrand_variance <- function(x) {
    # ----- REQUIRED PARAMETER --------------------------------------------
    mean = -1; # copy and paste value of "mean" here
    # ---------------------------------------------------------------------
    
    ( 3*x-2 - mean )^2 * ( 2*(1-x) ) # ( y - mean )^2 * ( pdf for y )
  };
  variance = integrate(f=integrand_variance, lower=0, upper=1);
  
  print(mean);
  print(variance);
}

Q5 <- function() {
  eq1 <- antiD(a+b*(x^2) ~ x);
  print(eq1);
  
  eq2 <- antiD((a*x+b*(x^3)) ~ x);
  print(eq2);
  
  # don't think it can be completely done in R
}

Q6 <- function() {
  # ----- skipped -----
}

Q7 <- function() {
  # ----- skipped -----
}

Q8a <- function() {
  pdf <- function(x) { 6*x*(1-x) }
  
  # find mean of X
  mean_integrand <- function(x) {
    x * pdf(x);
  }
  mean = integrate(f=mean_integrand, lower=0, upper=1)$value;
  
  # find variance of X
  variance_integrand <- function(x) {
    # ----- PARAMETER ------------------------------
    mean = 0.5;
    # ----------------------------------------------
    
    (x-mean)^2 * pdf(x);
  }
  variance = integrate(f=variance_integrand, lower=0, upper=1)$value;
  stddev = sqrt(variance);
  
  print(mean);
  print(stddev);
}

Q8b <- function() {
  # from Q8a
  mean = 0.5;
  stddev = 0.2236068;
  
  lower_lim = mean - (2*stddev);
  upper_lim = mean + (2*stddev);
  # print(lower_lim); print(upper_lim);
  
  pdf <- function(x) { 6*x*(1-x) }
  prob = integrate(f=pdf, lower=lower_lim, upper=upper_lim)$value;
  
  print(prob);
}

Q8c <- function() {
  # ----- skipped -----
}

Q8d <- function() {
  # ----- skipped -----
}

Q9 <- function() {
  # ----- skipped -----
}


# Q1();
# Q1d();
# Q2();
# Q3b()
# Q4a();
# Q4b();
# Q5();
# Q8a()
# Q8b()
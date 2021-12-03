library(stats)

Q1 <- function() {
  prob_atleast10 = 1 - pbinom(q=9, size=20, prob=0.30, lower.tail=TRUE);
  prob_atmost4 = pbinom(q=4, size=20, prob=0.30, lower.tail=TRUE);
  prob_exactly5 = dbinom(x=5, size=20, prob=0.30)
  
  prob_atmost4_diffprob = pbinom(q=4, size=20, prob=0.60, lower.tail=TRUE);
  
  print(prob_atleast10)
  print(prob_atmost4)
  print(prob_exactly5)
  print(prob_atmost4_diffprob)
}

Q2 <- function() {
  prob_0 = dbinom(x=0, size=15, prob=0.25)
  prob_atleast8 = 1 - pbinom(q=7, size=15, prob=0.25)
  mean = 15 * 0.25; # mean = np
  
  # By Chebyshev's Inequality, Pr(|x - μ| <= k*σ) >= (1-1/k^2)
  # prob = 0.75 = (1-1/k^2)
  #   k = 2
  k = 2;
  variance = 15*0.25*(1-0.25); # variance = np(1-p)
  stddev = sqrt(variance);
  lower_lim = -(k*stddev) + mean;
  upper_lim = (k*stddev) + mean;
  
  print(prob_0)
  print(prob_atleast8)
  print(mean)
  
  print(ceiling(lower_lim)); print(floor(upper_lim)); # not really sure how this works tbh
}

Q3 <- function() {
  n = 10000;
  p = 1/1000;
  
  prob_678 = dbinom(x=6, size=n, prob=p) +  dbinom(x=7, size=n, prob=p) +  dbinom(x=8, size=n, prob=p)
  
  mean = n*p;
  variance = n*p*(1-p);
  
  print(prob_678)
  print(mean); print(variance);
  
  # By Chebyshev's Inequality, Pr(|x - μ| <= k*σ) >= (1-1/k^2)
  # prob = 8/9 = (1-1/k^2)
  #   k = 3
  k = 3;
  stddev = sqrt(variance);
  lower_lim = -(k*stddev) + mean;
  upper_lim = (k*stddev) + mean;
  
  print(ceiling(lower_lim)); print(floor(upper_lim));
}

Q4 <- function() {
  p = 0.3;
  r = 5; # required number of successes
  n_trials_so_far = 10;
  n = n_trials_so_far - r;
  
  prob = dnbinom(x=n, size=r, prob=p)
  
  print(prob)
}

Q5 <- function() {
  p = 0.5;
  r = 2; # required number of successes
  n_trials_so_far = 7;
  n = n_trials_so_far - r;  
  
  prob = dnbinom(x=n, size=r, prob=p)
  
  print(prob)
}

Q6 <- function() {
  p = 1-(0.5^3 + 0.5^3); # Pr(HHH) + Pr(TTT)
  
  prob = pgeom(q=2, prob=p); # q: number of failures in a sequence of Bernoulli trials before success occurs
  
  print(prob)
}

Q7 <- function() {
  lambda = 2; # average number of successes in the given time-interval
  variance = lambda; # V(X) = lambda
  
  prob_4orMore = 1 - ppois(q=3, lambda=lambda, lower.tail=TRUE);
  prob_noErrors = ppois(q=0, lambda=lambda, lower.tail=TRUE);
  
  print(prob_4orMore);
  print(prob_noErrors);
}

Q8 <- function() {
  lambda = 5;
  
  prob_0 = ppois(q=0, lambda=lambda, lower.tail=TRUE);
  prob_moreThan10 = 1 - ppois(q=10, lambda=lambda, lower.tail=TRUE);
  
  updated_lambda = lambda * 3;
  prob_moreThan20_during3hrShift = 1 - ppois(q=20, lambda=updated_lambda, lower.tail=TRUE);
  
  print(prob_0);
  print(prob_moreThan10);
  print(prob_moreThan20_during3hrShift);
}

Q9 <- function() {
  n = 10000;
  p = 0.05/100;
  
  mean = n*p;
  variance = n*p*(1-p);
  stddev = sqrt(variance);
  
  print(mean); print(stddev);
  
  prob_atleast10 = 1 - pbinom(q=9, size=n, prob=p);
  prob_zero = pbinom(q=0, size=n, prob=p);
  
  print(prob_atleast10);
  print(prob_zero);
}

Q10 <- function() {
  # pdf: f(x) = 1/(b-a), where a <= x <= b
  
  # ----- find Pr(x >= 3) -----
  # use Mathematica:
  #   Integrate[(1/4), {x, 3, 4}]
  #     output: 1/4
  
  # ----- find E[X] -----
  # use Mathematica:
  #   Integrate[x*(1/4), {x, 0, 4}]
  #     output: 2
  
  # ----- find Var[X] -----
  # use Mathematica:
  #   Integrate[(x - 2)^2*(1/4), {x, 0, 4}]
  #     output: 4/3
}

Q11 <- function() {
  # using Exponent Distribution
  mean = 4;
  alpha = 1/mean;
  
  prob_morethan3 = 1 - pexp(q=3, rate=alpha);
  prob_lessthan3 = pexp(q=3, rate=alpha);
  
  # now use Binomial Distribution
  p = prob_lessthan3
  n = 6;
  
  prob_lessthan3_atleast4ofnext6 = 1 - pbinom(q=3, size=n, prob=p);
  
  print(prob_morethan3)
  print(prob_lessthan3);
  print(prob_lessthan3_atleast4ofnext6)
}

# Q1();
# Q2();
# Q3();
# Q4();
# Q5();
# Q6()
# Q7()
# Q8()
# Q9()
# Q11()
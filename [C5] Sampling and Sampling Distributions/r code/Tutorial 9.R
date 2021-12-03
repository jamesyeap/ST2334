library(stats)

Q1 <- function() {
  alpha = 1; # infer from question
  
  mean = 1/alpha;
  variance = 1/(alpha^2);
  stddev = sqrt(variance);
  
  print(mean);
  print(stddev);
  
  prob_leq4 = pexp(q=4, rate=alpha);
  prob_geq2_leq5 = pexp(q=5, rate=alpha) - pexp(q=2, rate=alpha);
  
  print(prob_leq4);
  print(prob_geq2_leq5)
}

Q2 <- function() {
  mean = 25000;
  alpha = 1/mean;
  
  prob_atleast20000 = 1 - pexp(q=20000, rate=alpha); # last 20000 hours <==> last at least 20000 hours
  prob_atmost30000 = pexp(q=30000, rate=alpha);
  prob_between_20000_30000 = pexp(q=30000, rate=alpha) - pexp(q=20000, rate=alpha);
  
  variance = 1/(alpha^2);
  stddev = sqrt(variance);
  target = mean + 2*stddev;
  
  prob = 1 - pexp(q=target, rate=alpha);
  
  print(prob_atleast20000);
  print(prob_atmost30000)
  print(prob_between_20000_30000)
  print(prob);
}

Q3 <- function() {
  mean = 2;
  alpha = 1/mean;
  
  variance = 1/(alpha^2);
  print(variance);
  
  # using Binomial-Distribution now,
  p = pexp(q=1, rate=alpha);
  n = 100;
  
  prob = pbinom(q=30, size=n, prob=p);
  print(prob)
}

Q4 <- function() {
  lower_tail = pnorm(q=-3, mean=0, sd=1);
  upper_tail = pnorm(q=3, mean=0, sd=1, lower.tail=FALSE);
  
  prob = 1 - lower_tail - upper_tail;
  
  print(prob);
}

Q5 <- function() {
  mean = 200;
  stddev = 15;
  
  prob_morethan224 = 1 - pnorm(q=224, mean=mean, sd=stddev);
  prob_between191_209 = pnorm(q=209, mean=mean, sd=stddev) - pnorm(q=191, mean=mean, sd=stddev);
  
  # using Binomial Distribution, where:
  n = 1000;
  p = 1 - pnorm(q=230, mean=mean, sd=stddev);
  nCups_overflow = ceiling(n*p); # round up as number of cups is an integer value
    
  prob_smallest25percent = qnorm(p=0.25, mean=mean, sd=stddev);
  
  print(prob_morethan224)
  print(prob_between191_209)
  print(nCups_overflow)
  print(prob_smallest25percent)
}

Q6 <- function() {
  mean = 24;
  stddev = 3.8;
  
  prob_atleast30 = 1 - pnorm(q=30, mean=mean, sd=stddev);
  
  # he has 15 minutes to travel -> find probability of time-taken exceeding 15 mins
  prob_atleast15 = 1 - pnorm(q=15, mean=mean, sd=stddev);
  
  # using Binomial Distribution,
  p = prob_atleast30;
  n = 3;
  prob = dbinom(x=2, size=n, prob=p);
  
  print(prob_atleast30);
  print(prob_atleast15);
  print(prob);
}

Q7 <- function() {
  n = 400;
  p = 0.5;
  
  prob_between_185_210 = pbinom(q=210, size=n, prob=p) - pbinom(q=184, size=n, prob=p)
  prob_205 = dbinom(x=205, size=n, prob=p);
  
  prob_lessthan176 = pbinom(q=175, size=n, prob=p) # note: use 176 - 1
  prob_morethan227 = 1 - pbinom(q=227, size=n, prob=p)
  
  prob = prob_lessthan176 + prob_morethan227;
  
  print(prob_between_185_210);
  print(prob_205);
  print(prob)
}

Q8 <- function() {
  n = 400;
  p = 1/10;
  
  prob_lessthan32 = pbinom(q=31, size=n, prob=p); # note: use 32-1
  prob_morethah49 = 1 - pbinom(q=49, size=n, prob=p)
  prob_atleast35_lessthan47 = pbinom(q=46, size=n, prob=p) - pbinom(q=34, size=n, prob=p);
  
  print(prob_lessthan32);
  print(prob_morethah49);
  print(prob_atleast35_lessthan47);
}

Q9 <- function() {
  n = 100;
  p = 1 - 0.95;
  
  prob_morethan2 = 1 - pbinom(q=2, size=n, prob=p);
  prob_morethan10 = 1 - pbinom(q=10, size=n, prob=p);
  
  print(prob_morethan2)
  print(prob_morethan10)
}

Q10 <- function() {
  # ----- using Microsoft Excel, -----
  pop_mean = 5.3;
  pop_variance = 0.81;
  # ----------------------------------
  
  n = 36;
  mean_of_sample_mean = pop_mean;
  variance_of_sample_mean = pop_variance/n;
  
  prob = pnorm(q=5.5, mean=mean_of_sample_mean, sd=sqrt(variance_of_sample_mean));
  
  print(mean_of_sample_mean);
  print(variance_of_sample_mean);
  print(prob)
}

Q11 <- function() {
  n = 25;
  mean_of_sampleMean = 7950;
  stddev_of_sampleMean = 100/sqrt(25);
  
  prob_a = 1 - pnorm(q=7950, mean=mean_of_sampleMean, sd=stddev_of_sampleMean)
  
  print(prob_a)
  
  prob_b = 1 - pnorm(q=7960, mean=mean_of_sampleMean, sd=stddev_of_sampleMean)
  
  print(prob_b)
  # output: 0.3085
    # conclusion: NO -> becuz not statistically-significant enufff (usually must be <0.05)
}

# Q1()
# Q2()
# Q3()
# Q4()
# Q5()
# Q6()
# Q7()
# Q8()
# Q9()
# Q10()
# Q11()
  
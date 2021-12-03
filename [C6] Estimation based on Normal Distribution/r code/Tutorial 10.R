library(prob);

Q1 <- function() {
  population_mean = 20;
  population_var = NULL; # unknown

  # using Central-Limit Theorem,
  mean_sampleMean = population_mean;
  nSamples = 9;
  stddev_sampleMean = 4.1;

  t = (24 - population_mean)/(stddev_sampleMean/sqrt(nSamples));

  prob = 1- pt(q=t, df=nSamples-1);
  print(round(prob, 4)) # outputs 0.0095

  # Conclusion: No, it is unlikely that one would obtain such a sample,
  # as prob = 0.0095 < 0.05
}

Q2a <- function() {
  mean_sampleMean_A = 4.5;
  mean_sampleMean_B = 4.7;

  nSamples_A = 36;
  nSamples_B = 36;

  population_variance = 1; # ASKED TO ASSUME

  # then, use NORMAL-DISTRIBUTION, with parameters:
  mean = 0; # assume both POPULATION means are equal -> popmean_A - popmeanB = 0
  stddev = sqrt((population_variance/nSamples_A) + (population_variance/nSamples_B));

  r = 1 - pnorm(q=0.2, mean=mean, sd=stddev);
  print(round(r, 4))
}

Q2b <- function() {
  # Conclusion: NO, the probability of obtaining such a sample was computed
  # to be around 0.1981, which is greater than 0.05.

  # Thus, it is likely that such a sample was obtained due to chance
  # and not sufficiently unlikely enough to suggest that the assumption
  # was false.
}

Q3a <- function() {
  nSamples = 25;
  population_variance = 6;

  # CHISQUARE-DISTRIBUTION parameters:
  df = nSamples-1;

  # "normalise" to the standard chisq random-variable
  chisq_STATISTIC = (nSamples-1)*(9.1)/population_variance

  # calculate the RIGHT-TAIL probability
  r = 1 - pchisq(
    q=chisq_STATISTIC,
    df=df
  );

  print(round(r, 4));
}

Q3b <- function() {
  nSamples = 25;
  population_variance = 6;

  # CHISQUARE-DISTRIBUTION parameters:
  df = nSamples-1;

  # calculate the LOWER-LIM probability
  lower_chisq_STATISTIC = (nSamples-1)*(3.462)/population_variance;
  lower_prob = pchisq(
    q=lower_chisq_STATISTIC,
    df=df
  )

  # calculate the UPPER-LIM probability
  upper_chisq_STATISTIC = (nSamples-1)*(10.745)/population_variance;
  upper_prob = pchisq(
    q=upper_chisq_STATISTIC,
    df=df
  )

  # calculate the range in-between
  r = upper_prob - lower_prob;

  print(round(r, 4));
}

# F-DISTRIBUTION
Q4 <- function() {
  nSamples_1 = 8;
  nSamples_2 = 12;

  # GIVEN: sample 1 and sample 2 are both from populations with EQUAL variance
  # f_STATISTIC

  p = pf(q=4.89, df1=nSamples_1-1, df2=nSamples_2-1);
  print(round(p, 4))
}

Q5 <- function() {
  mine1_data <- c(8260, 8130, 8350, 8070, 8340);
  mine2_data <- c(7950, 7890, 7900, 8140, 7920, 7840);

  # to determine if their population-variances are equal, we need to
  #   use F-distribution

  # ASSUME: that their population-variances are equal, then see if
  # its likely

  mine1_sample_var = var(mine1_data);
  mine2_sample_var = var(mine2_data);

  mine1_pop_var = 1; # dummy-value
  mine2_pop_var = mine1_pop_var; # dummy-value

  # "normalize" to get the F statistic
  F_statistic = (mine1_sample_var/mine1_pop_var) / (mine2_sample_var/mine2_pop_var);

  prob = pf(
    q=F_statistic,
    df1=length(mine1_data)-1,
    df2=length(mine2_data)-1,
    lower.tail=FALSE
  )

  r = prob
  print(round(r, 4)); # outputs 0.3436
}

# --------- COMPUTING CONFIDENCE-INTERVAL ---------
Q7a <- function() {
  population_stddev = 0.75;

  nSamples = 20;
  sample_mean = 4.85;

  ci = 0.95;
  bothTailProb = (1-ci)/2; # probability of LOWER or UPPER tail (need to divide by 2)
  z_STATISTIC = qnorm(p=bothTailProb);
  error = (z_STATISTIC * population_stddev)/sqrt(nSamples)

  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;

  print(round(lower_limit, 4));
  print(round(upper_limit, 4));
}

ci_z <- function() {
  
}

Q7b <- function() {
  # CONSTRAINT: we want total-error to be = 0.40
  error = 0.40/2; # error on each side

  population_stddev = 0.75;
  sample_mean = 4.85;

  ci = 0.95;
  bothTailProb = (1-ci)/2; # probability of LOWER or UPPER tail (need to divide by 2)
  z_STATISTIC = qnorm(p=bothTailProb);

  # then, we just re-arrange the equation that we used to calculate error in
  #   Q7a to find nSamples needed
  nSamples = ( (z_STATISTIC * population_stddev) / error )^2;

  print(ceiling(nSamples)); # outputs 55
}

Q7c <- function() {
  # given
  nSamples = 20;
  sample_mean = 4.85;
  sample_stddev = 0.75;

  # find error
  ci = 0.95;
  bothTailProb = (1-ci)/2;
  t_STATISTIC = qt(p=bothTailProb, df=nSamples-1); # <-- CANNOT use Z-distribution this time!

  error = (t_STATISTIC * sample_stddev)/sqrt(nSamples);

  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;

  print(round(lower_limit, 4));
  print(round(upper_limit, 4));
}

Q8a <- function() {
  # given
  nSamples = 75;
  sample_mean = 0.310;
  population_stddev = 0.0015; # ASKED TO ASSUME

  # find error
  ci = 0.95;
  both_tail_prob = (1-ci)/2;
  z_STATISTIC = qnorm(p=both_tail_prob);

  error = (z_STATISTIC * population_stddev) / sqrt(nSamples);

  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;

  print(round(lower_limit, 5));
  print(round(upper_limit, 5));
}

Q8b <- function() {
  # CONSTRAINT
  error = 0.0005;

  # given
  nSamples = 75;
  sample_mean = 0.310;
  population_stddev = 0.0015; # ASKED TO ASSUME

  # find number of samples needed to achieve the above-mentioned error
  ci = 0.95;
  both_tail_prob = (1-ci)/2;
  z_STATISTIC = qnorm(p=both_tail_prob);

  nSamples = ( (z_STATISTIC * population_stddev) / error )^2;

  print(ceiling(nSamples))
}

Q9 <- function() {
  # given
  n_samples = 12;
  sample_mean = 48.5;
  sample_stddev = 1.5;

  # calculate error
  ci = 0.9;
  both_tail_prob = (1-ci)/2;
  t_STATISTIC = abs(qt(p=both_tail_prob, df=n_samples-1)); # <--- note: CANNOT USE Z-DISTRIBUTION!

  error = (t_STATISTIC * sample_stddev)/sqrt(n_samples);

  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;

  print(round(lower_limit, 4));
  print(round(upper_limit, 4));
}

Q10 <- function() {
  # given
  n_samples_1 = 25;
  n_samples_2 = 36;

  sample_1_mean = 80;
  sample_2_mean = 75;

  population_1_stddev = 5; population_1_variance = population_1_stddev^2;
  population_2_stddev = 3; population_2_variance = population_2_stddev^2;

  # find error
  ci = 0.94;
  both_tail_prob = (1-ci)/2;
  z_STATISTIC = abs(qnorm(p=both_tail_prob));
  standard_error = sqrt( (population_1_variance/n_samples_1) + (population_2_variance/n_samples_2) )

  error = z_STATISTIC * standard_error;

  # get lower and upper limits of the confidence-interval
  point_estimate = sample_1_mean - sample_2_mean;

  lower_lim = point_estimate - error;
  upper_lim = point_estimate + error;

  print(round(lower_lim, 4));
  print(round(upper_lim, 4));
}

Q11 <- function() {
  # given
  n_samples_CONTROL = 100; # without treatment
  n_samples_TREATMENT = 200;

  sample_mean_CONTROL = 12.2
  sample_mean_TREATMENT = 9.1

  sample_stddev_CONTROL = 1.1; sample_variance_CONTROL = sample_stddev_CONTROL^2;
  sample_stddev_TREATMENT = 0.9; sample_variance_TREATMENT = sample_stddev_TREATMENT^2;

  # find error
  ci = 0.98;
  both_tail_prob = (1-ci)/2;
  z_STATISTIC = abs(qnorm(p=both_tail_prob)); # <--- note: still can use Z-DISTRIBUTION
  standard_error = sqrt( (sample_variance_CONTROL/n_samples_CONTROL) + (sample_variance_TREATMENT/n_samples_TREATMENT) )

  error = z_STATISTIC * standard_error;

  # calculate limits
  point_estimate = sample_mean_CONTROL-sample_mean_TREATMENT;

  lower_limit = point_estimate - error;
  upper_limit = point_estimate + error;

  print(round(lower_limit, 4));
  print(round(upper_limit, 4));

  # Conclusion: YES, it is likely, as the entire range of the 98% confidence-interval
  # is POSITIVE -> suggesting that the amount of metal removed for CONTROL is very
  # likely to be HIGHER than that for TREATMENT
}


# uncomment to see answers to the questions
# Q1()
# Q2a()
# Q3a()
# Q3b()
# Q4()
# Q5()
# Q7a()
# Q7b()
# Q7c()
# Q8a()
# Q8b()
# Q9()
# Q10()
# Q11()











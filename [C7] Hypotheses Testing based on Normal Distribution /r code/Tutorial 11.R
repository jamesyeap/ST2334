# Tutorial 11
Q1a <- function() {
 pop_mean = 800;
 pop_stddev = 40;
 
 n_samples = 30;
 sample_mean = 788;
 
 mean_sampleMean = pop_mean;
 stddev_sampleMean = pop_stddev / sqrt(n_samples);
 
 # calculate Z-score
 z = (sample_mean-pop_mean)/stddev_sampleMean;
 p_value = pnorm(q=z, mean=0, sd=1) * 2; # remember to mulltiply by 2: two-sided test
 
 print(z);
 print(p_value);
}

Q1b <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 788
  population_variance = 40^2
  ci = 0.95
  n_samples = 30
  # ========================================================
  
  # find Z  
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # find standard-error
  sample_stddev = sqrt(population_variance);
  standard_error = sample_stddev/sqrt(n_samples);
  
  # find error
  error = z * standard_error;
  
  # find lower and upper-limits of Confidence Interval
  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;
  
  print(round(lower_limit, 5));
  print(round(upper_limit, 5));
}

Q1c <- function() {
  # FINDING TYPE II ERROR FOR TWO-SIDED TEST
  
  a <- 800; # population-mean under H0
  s <- 40; # standard-deviation
  n <- 30;
  alpha <- 0.05;
  
  error <- qnorm(1-(alpha/2))*s/sqrt(n);
  left <- a-error;
  right <- a+error;
  
  # print(left); print(right);
  
  assumed <- 790; # population-mean under H1 -> the value referenced by "actually..."
  Zleft <- (left-assumed)/(s/sqrt(n));
  Zright <- (right-assumed)/(s/sqrt(n));
  type2error <- pnorm(Zright) - pnorm(Zleft);
  
  print(type2error) 
  
  power = 1-type2error;
  
  print(power);
}

Q2a <- function() {
  alpha = 0.01;
  
  sample_DATA <- c(10.2, 9.7, 10.1, 10.3, 10.1, 9.8, 9.9, 10.4, 10.3, 9.8);
  
  sample_mean = mean(sample_DATA);
  sample_variance = var(sample_DATA);
  
  print(sample_mean); print(sample_variance);
  
  t = abs( (sample_mean - 10)/sqrt(sample_variance/length(sample_DATA)) ) ;
  print(t); # output: 0.771
  
  p_value = pt(q=-t, df=length(sample_DATA)-1) * 2 # remember to times 2 -> since its a TWO-SIDED test
  print(p_value) # output: 0.4600
}

# Confidence-Interval for Variance
  # Case 2: Unknown Mean
Q2c <- function() {
  data <- c(10.2, 9.7, 10.1, 10.3, 10.1, 9.8, 9.9, 10.4, 10.3, 9.8);
  n_sample = 10;

  ci = 0.95;
  alpha = 1-ci;
  alpha_half = alpha/2;

  sample_variance = var(data);

  chisq_STATISTIC_lower = qchisq(p=alpha_half, df=n_sample-1);
  lower_limit = ((n_sample-1)*sample_variance) / chisq_STATISTIC_lower;

  chisq_STATISTIC_upper = qchisq(p=1-alpha_half, df=n_sample-1); # note that p = 1-alpha/2
  upper_limit = ((n_sample-1)*sample_variance) / chisq_STATISTIC_upper;

  print(lower_limit);
  print(upper_limit, 4);
}

Q3 <- function() {
  # ONE-SIDED TEST --> VARIANCE
  
  sample_variance = 2.03;
  n_samples = 25;
  h0_variance = 1.15; # h1_variance > 1.15
  
  chisq_STATISTIC = ( (n_samples-1)*sample_variance ) / h0_variance;
  p_value = pchisq(q=chisq_STATISTIC, df=n_samples-1, lower.tail=FALSE); # p_value ==> RIGHT-HAND SIDE PROBABILITY
  
  print(p_value); # OUTPUT: 0.0117
  
  # CONCLUSION: p_value < 0.05 ==> REJECT h0_variance in FAVOUR of h1_variance! 
      # ==> quite likely that the machine is out of control
}

Q4 <- function() {
  # ONE-SIDED TEST --> DIFFERENCE BETWEEN MEANS
  
  n_samples_groupA = 50;
  sample_mean_groupA = 86.7;
  population_variance_groupA = 6.28^2;
  
  n_samples_groupB = 50;
  sample_mean_groupB = 77.8;
  population_variance_groupB = 5.61^2;
  
  h0_differenceInMean = 12;
  z_OBSERVED = ( (sample_mean_groupA - sample_mean_groupB ) - h0_differenceInMean ) / sqrt( (population_variance_groupA/n_samples_groupA) + (population_variance_groupB/n_samples_groupB) );
  
  print(z_OBSERVED) # output: -2.603
  
  z_ALPHA = qnorm(p=0.05, mean=0, sd=1, lower.tail=FALSE);
  print(z_ALPHA); # output: 1.644
  
  
  # CONCLUSION: DO NOT REJECT h0 ===> as z_OBSERVED is NOT GREATER than z_ALPHA ==> NOT INSIDE THE CRITICAL-REGION!
}

# Confidence-Interval for Difference Between Means
  # Case 2b: Unknown but EQUAL variances
Q5a <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 12
    sample_mean_group1 = 84 
    sample_stddev_group1 = 4
    
    n_samples_group2 = 18
    sample_mean_group2 = 77
    sample_stddev_group2 = 6
    
    ci = 0.99
    # ========================================================
  
  # find t
  alpha_half = (1-ci)/2;
  t = abs(qt(p=alpha_half, df=n_samples_group1+n_samples_group2-2));
  
  # find POOLED-variance from the two samples
  pooled_sample_variance = ( ((n_samples_group1-1)*sample_stddev_group1^2) + ((n_samples_group2-1)*sample_stddev_group2^2) ) / (n_samples_group1 + n_samples_group2 - 2);
  
  # calculate standard error
  standard_error = sqrt(pooled_sample_variance) * sqrt( (1/n_samples_group1) + (1/n_samples_group2) );
  
  # calculate error
  error = t * standard_error;
  
  # calculate upper and lower limits of the confidence interval
  sample_mean_diff = sample_mean_group1 - sample_mean_group2; # "point-estimate"
  upper_limit = sample_mean_diff + error;
  lower_limit = sample_mean_diff - error;
  
  print(round(upper_limit, 5));
  print(round(lower_limit, 5));
}

# Hypothesis-Test
#   Difference Between Means - One-Sided
Q5b <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 12
  sample_mean_group1 = 84 
  sample_stddev_group1 = 4
  
  n_samples_group2 = 18
  sample_mean_group2 = 77
  sample_stddev_group2 = 6
  # ========================================================
  
  # small sample-case -> use t-distribution
  h0_difference = 0; # h1_difference > 0
  pooled_sample_variance = ( ((n_samples_group1-1)*sample_stddev_group1^2) + ((n_samples_group2-1)*sample_stddev_group2^2) ) / (n_samples_group1 + n_samples_group2 - 2);
  t_OBSERVED = ( (sample_mean_group2 - sample_mean_group1) - h0_difference )  / ( sqrt(pooled_sample_variance) * sqrt((1/n_samples_group1 + 1/n_samples_group2)) );
  
  significance_level = 0.05;
  t_CRITICAL = abs(qt(0.05, df=n_samples_group1 + n_samples_group2 - 2)); # critical t-value is POSITIVE; since h1_difference > h0_difference
  
  print(t_OBSERVED); 
  print(t_CRITICAL);
}

# Confidence-Interval for Difference Between Means
  # Case 3: Paired Data
Q6a <- function() {
  radial_data = c(4.2, 4.7, 6.6, 7, 6.7, 4.5, 5.7, 6, 7.4, 4.9, 6.1, 5.2);
  belted_tires = c(4.1, 4.9, 6.2, 6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6, 4.9);
  differences = vector(mode="logical", length=length(radial_data));
  
  for (i in 1:length(differences)) {
    differences[i] = radial_data[i] - belted_tires[i];
  }
  
  # Calculate point-estimate of mean of differences
  pointEstimate_meanDiff = 0;
  for (i in 1:length(radial_data)) {
    pointEstimate_meanDiff = pointEstimate_meanDiff + differences[i];
  }
  pointEstimate_meanDiff = pointEstimate_meanDiff/length(differences);
  
  # Calculate point-estimate of variance of differences
  pointEstimate_varDiff = 0;
  for (i in 1:length(radial_data)) {
    pointEstimate_varDiff = pointEstimate_varDiff + (differences[i] - pointEstimate_meanDiff)^2;
  }
  pointEstimate_varDiff = pointEstimate_varDiff/(length(differences)-1);
  
  ci = 0.95;
  alpha = 1-ci;
  alpha_half = alpha/2;
  
  t_STATISTIC = abs(qt(p=alpha_half, df=length(radial_data)-1));
  
  standard_error = sqrt(pointEstimate_varDiff)/sqrt(length(radial_data));
  error = t_STATISTIC * standard_error;
  
  upper_lim = pointEstimate_meanDiff + error;
  lower_lim = pointEstimate_meanDiff - error;
  
  print(round(upper_lim, 4));
  print(round(lower_lim, 4));
}

Q6b <- function() {
  # use t-distribution
  radial_data = c(4.2, 4.7, 6.6, 7, 6.7, 4.5, 5.7, 6, 7.4, 4.9, 6.1, 5.2);
  belted_tires = c(4.1, 4.9, 6.2, 6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6, 4.9);
  differences = vector(mode="logical", length=length(radial_data));
  
  for (i in 1:length(differences)) {
    differences[i] = radial_data[i] - belted_tires[i];
  }
  
  # Calculate point-estimate of mean of differences
  pointEstimate_meanDiff = 0;
  for (i in 1:length(radial_data)) {
    pointEstimate_meanDiff = pointEstimate_meanDiff + differences[i];
  }
  pointEstimate_meanDiff = pointEstimate_meanDiff/length(differences);
  
  # Calculate point-estimate of variance of differences
  pointEstimate_varDiff = 0;
  for (i in 1:length(radial_data)) {
    pointEstimate_varDiff = pointEstimate_varDiff + (differences[i] - pointEstimate_meanDiff)^2;
  }
  pointEstimate_varDiff = pointEstimate_varDiff/(length(differences)-1);
  
  h0_meanDiff = 0; # h0: no difference; h1: difference is >0
  t_OBSERVED = abs( (pointEstimate_meanDiff - h0_meanDiff) / ( sqrt(pointEstimate_varDiff) / sqrt(length(radial_data)) ) )
  p_value = pt(q=t_OBSERVED, df=length(radial_data)-1, lower.tail=FALSE);
  
  print(p_value); # output: 0.015 < 0.05 ===> INSIDE CRITICAL-REGION!
  
  # CONCLUSION: Reject h0 in favour of h1
}

Q7 <- function() {
  h0_ratio = 1; # h0: variance of men EQUAL variance of women
                # h1: variance of men GREATER THAN variance of women ===> h1_ratio > 1
  
  variance_MEN = 6.1^2;
  nsamples_MEN = 11;
  
  variance_WOMEN = 5.3^2;
  nsamples_WOMEN = 14;
  
  significance_level = 0.05;
  
  f_OBSERVED = variance_MEN / variance_WOMEN;
  f_CRITICAL = abs( qf(p=significance_level, df1=nsamples_MEN-1, df2=nsamples_WOMEN-1, lower.tail = FALSE) )
  
  print(f_OBSERVED)
  print(f_CRITICAL)
}

# Confidence-Interval for Ratio of Variances
Q8a <- function() {
  company1_data <- c(102, 86, 98, 109, 92);
  company2_data <- c(81, 165, 97, 134, 92, 87, 114)
  
  company1_variance = var(company1_data);
  n_samples_company1 = length(company1_data);
  
  company2_variance = var(company2_data);
  n_samples_company2 = length(company2_data);
  
  ci = 0.95;
  alpha = 1 - ci;
  alpha_half = alpha/2;
  
  ratio_between_sampleVariances = company1_variance/company2_variance;
  
  # calculate lower and upper limits
  F_STATISTIC_lower = qf(p=alpha_half, df1=n_samples_company1-1, df2=n_samples_company2-1, lower.tail=FALSE);
  lower_lim = ratio_between_sampleVariances * (1/F_STATISTIC_lower);
  
  F_STATISTIC_upper = qf(p=alpha_half, df1=n_samples_company2-1, df2=n_samples_company1-1, lower.tail=FALSE);
  upper_lim = ratio_between_sampleVariances * F_STATISTIC_upper;
  
  print(round(lower_lim, 4));
  print(round(upper_lim, 4));
}

# Q1a();
# Q1b();
# Q1c()
# Q2a()
# Q2c();
# Q3()
# Q4()
# Q5a();
# Q5b()
# Q6a();
# Q6b()
# Q7()
Q8a();
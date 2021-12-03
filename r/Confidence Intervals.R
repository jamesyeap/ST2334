# Confidence-Interval for the Mean
# Case 1a: KNOWN variance and population is NORMAL
ci_mean_1a <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 
  population_variance =
  ci = 
  n_samples =
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

# Case 1b: KNOWN variance and LARGE sample-size >= 30 (exactly the same as Case 1a)
ci_mean_1b <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 
  population_variance =
  ci = 
  n_samples =
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

# Case 2a: UNKNOWN variance and SMALL sample-size < 30
ci_mean_2a <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 
  sample_variance =
  ci = 
  n_samples =
  # ========================================================
  
  # find t
  alpha_half = (1-ci)/2;
  t = abs(qt(p=alpha_half, df=n_samples-1));
  
  # find standard-error
  sample_stddev = sqrt(sample_variance);
  standard_error = sample_stddev/sqrt(n_samples);
  
  # find error
  error = t * standard_error;
  
  # find lower and upper-limits of Confidence Interval
  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;
  
  print(round(lower_limit, 5));
  print(round(upper_limit, 5));
}

# Case 2b: UNKNOWN variance and LARGE sample-size >= 30
ci_mean_2b <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 
  sample_variance =
  ci = 
  n_samples =
  # ========================================================
  
  # find z
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # find standard-error
  sample_stddev = sqrt(sample_variance);
  standard_error = sample_stddev/sqrt(n_samples);
  
  # find error
  error = z * standard_error;
  
  # find lower and upper-limits of Confidence Interval
  lower_limit = sample_mean - error;
  upper_limit = sample_mean + error;
  
  print(round(lower_limit, 5));
  print(round(upper_limit, 5));
}

# Confidence-Interval for Difference Between Means
# Case 1a: both variances KNOWN and both populations are NORMAL
ci_meanDiff_1a <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 
  sample_mean_group1 = 
  population_sttdev_group1 = 
    
  n_samples_group2 = 
  sample_mean_group2 = 
  population_stddev_group2 = 
    
  ci =
  # ========================================================
  
  # find z
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # calculate standard error
  standard_error = sqrt( (population_stddev_group1^2/n_samples_group1) + (population_stddev_group2^2/n_samples_group2) );
  
  # calculate error
  error = z * standard_error;
  
  # calculate upper and lower limits of the confidence interval
  sample_mean_diff = sample_mean_group1 - sample_mean_group2; # "point-estimate"
  upper_limit = sample_mean_diff + error;
  lower_limit = sample_mean_diff - error;
  
  print(round(upper_limit, 5));
  print(round(lower_limit, 5));
}

# Case 1b: both variances KNOWN and both sample-size LARGE >= 30
ci_meanDiff_1b <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 
  sample_mean_group1 = 
  population_sttdev_group1 = 
    
  n_samples_group2 = 
  sample_mean_group2 = 
  population_stddev_group2 = 
    
  ci =
  # ========================================================
  
  # find z
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # calculate standard error
  standard_error = sqrt( (population_stddev_group1^2/n_samples_group1) + (population_stddev_group2^2/n_samples_group2) );
  
  # calculate error
  error = z * standard_error;
  
  # calculate upper and lower limits of the confidence interval
  sample_mean_diff = sample_mean_group1 - sample_mean_group2; # "point-estimate"
  upper_limit = sample_mean_diff + error;
  lower_limit = sample_mean_diff - error;
  
  print(round(upper_limit, 5));
  print(round(lower_limit, 5));
}

# Case 2: both variances UNKNOWN and NOT SURE IF EQUAL and both sample-size LARGE >= 30
ci_meanDiff_2 <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 
  sample_mean_group1 = 
  sample_sttdev_group1 = 
    
  n_samples_group2 = 
  sample_mean_group2 = 
  sample_stddev_group2 = 
    
  ci =
  # ========================================================
  
  # find z
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # calculate standard error
  standard_error = sqrt( (sample_stddev_group1^2/n_samples_group1) + (sample_stddev_group2^2/n_samples_group2) );
  
  # calculate error
  error = z * standard_error;
  
  # calculate upper and lower limits of the confidence interval
  sample_mean_diff = sample_mean_group1 - sample_mean_group2; # "point-estimate"
  upper_limit = sample_mean_diff + error;
  lower_limit = sample_mean_diff - error;
  
  print(round(upper_limit, 5));
  print(round(lower_limit, 5));
}

# Case 3a: both variances UNKNOWN but EQUAL and both sample-size SMALL < 30
ci_meanDiff_3a <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 
  sample_mean_group1 = 
  sample_stddev_group1 = 
  
  n_samples_group2 = 
  sample_mean_group2 = 
  sample_stddev_group2 = 
  
  ci =
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

# Case 3b: both variances UNKNOWN but EQUAL and both sample-size LARGE >= 30
ci_meanDiff_3b <- function() {
  # ====================== PARAMETERS ======================
  n_samples_group1 = 
  sample_mean_group1 = 
  sample_stddev_group1 = 
    
  n_samples_group2 = 
  sample_mean_group2 = 
  sample_stddev_group2 = 
    
  ci =
  # ========================================================
  
  # find z
  alpha_half = (1-ci)/2;
  z = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  # find POOLED-variance from the two samples
  pooled_sample_variance = ( ((n_samples_group1-1)*sample_stddev_group1^2) + ((n_samples_group2-1)*sample_stddev_group2^2) ) / (n_samples_group1 + n_samples_group2 - 2);
  
  # calculate standard error
  standard_error = sqrt(pooled_sample_variance) * sqrt( (1/n_samples_group1) + (1/n_samples_group2) );
  
  # calculate error
  error = z * standard_error;
  
  # calculate upper and lower limits of the confidence interval
  sample_mean_diff = sample_mean_group1 - sample_mean_group2; # "point-estimate"
  upper_limit = sample_mean_diff + error;
  lower_limit = sample_mean_diff - error;
  
  print(round(upper_limit, 5));
  print(round(lower_limit, 5));
}

# Case 4: paired (dependent) data
ci_meanDiff_4 <- function() {
  # ====================== PARAMETERS ======================
  # EXAMPLE: group1_data = c(4.2, 4.7, 6.6, 7, 6.7, 4.5, 5.7, 6, 7.4, 4.9, 6.1, 5.2);
  group1_data = 
  # EXAMPLE: group2_data = c(4.1, 4.9, 6.2, 6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6, 4.9);
  group2_data = 
    
  ci = 
  # =======================================================
    
  differences_data = vector(mode="logical", length=length(group1_data));
  for (i in 1:length(differences_data)) {
    differences_data[i] = group1_data[i] - group2_data[i];
  }
  
  # Calculate point-estimate of mean of differences
  pointEstimate_meanDiff = 0;
  for (i in 1:length(differences_data)) {
    pointEstimate_meanDiff = pointEstimate_meanDiff + differences_data[i];
  }
  pointEstimate_meanDiff = pointEstimate_meanDiff/length(differences);
  
  # Calculate point-estimate of variance of differences
  pointEstimate_varDiff = 0;
  for (i in 1:length(differences_data)) {
    pointEstimate_varDiff = pointEstimate_varDiff + (differences_data[i] - pointEstimate_meanDiff)^2;
  }
  pointEstimate_varDiff = pointEstimate_varDiff/(length(differences_data)-1);
  
  # find t
  alpha_half = (1-ci)/2;
  t = abs(qt(p=alpha_half, df=length(differences_data)-1));
  
  # calculate standard-error
  standard_error = sqrt(pointEstimate_varDiff)/sqrt(length(radial_data));
  
  # calculate error
  error = t * standard_error;
  
  upper_lim = pointEstimate_meanDiff + error;
  lower_lim = pointEstimate_meanDiff - error;
  
  print(round(upper_lim, 5));
  print(round(lower_lim, 5));
}

# Confidence-Interval for Variances (just square-root both sides to get for Standard-Deviation)
# Case 1: KNOWN mean and population is NORMAL/APPROX-NORMAL


# Case 2: UNKNOWN mean and population is NORMAL/APPROX-NORMAL
ci_var_2 <- function() {
  # ====================== PARAMETERS ======================
  n_sample = 
  sample_variance = 
  ci = 
  # =======================================================
  
  alpha = 1-ci;
  alpha_half = alpha/2;
  
  chisq_STATISTIC_lower = qchisq(p=alpha_half, df=n_sample-1);
  lower_limit = ((n_sample-1)*sample_variance) / chisq_STATISTIC_lower;
  
  chisq_STATISTIC_upper = qchisq(p=1-alpha_half, df=n_sample-1); # note that p = 1-alpha/2
  upper_limit = ((n_sample-1)*sample_variance) / chisq_STATISTIC_upper;
  
  print(lower_limit);
  print(upper_limit);
}

# Confidence-Interval for Ratio of Variances (just square-root both sides to get for Ratio of Standard-Deviations)
# Case: both means UNKNOWN and both populations are NORMAL/APPROX-NORMAL
ci_ratioVar <- function() {
  # ====================== PARAMETERS ======================
  variance_group1 = 
  nsamples_group1 = 
    
  variance_group2 = 
  nsamples_group2 = 
  
  ci = 
  # =======================================================
  
  alpha = 1 - ci;
  alpha_half = alpha/2;
  
  ratio_between_sampleVariances = variance_group1/variance_group2;
  
  # calculate lower and upper limits
  F_STATISTIC_lower = qf(p=alpha_half, df1=nsamples_group1-1, df2=nsamples_group2-1, lower.tail=FALSE);
  lower_lim = ratio_between_sampleVariances * (1/F_STATISTIC_lower);
  
  F_STATISTIC_upper = qf(p=alpha_half, df1=nsamples_group2-1, df2=nsamples_group1-1, lower.tail=FALSE);
  upper_lim = ratio_between_sampleVariances * F_STATISTIC_upper;
  
  print(lower_lim);
  print(upper_lim);
}

# ===== HELPER FUNCTIONS (copy and paste on top of your own code-base where needed) =====
calculate_pooled_sample_variance <- function(n_samples_group1, sample_stddev_group1, n_samples_group2, sample_stddev_group2) {
  ( ((n_samples_group1-1)*sample_stddev_group1^2) + ((n_samples_group2-1)*sample_stddev_group2^2) ) / (n_samples_group1 + n_samples_group2 - 2);
}


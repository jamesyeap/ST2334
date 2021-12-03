Q2 <- function() {
  # coincident = x, where x: AT LEAST how many people fall into the same class
  prob_a = pbirthday(n=5, classes=12, coincident=2);
  
  prob_b = pbirthday(n=5, classes=12^2, coincident=2)
  
  print(prob_a)
  print(prob_b)
}

Q4a <- function() {
  mean = 3;
  alpha = 1/mean;
  
  prob_below1 = pexp(q=1, rate=alpha);
  prob_between_1_3 = pexp(q=3, rate=alpha) - pexp(q=1, rate=alpha);
  
  expected_refund_peritem = (300) * prob_below1 + (300/2) * prob_between_1_3;
  total_expected_refund = 200 * expected_refund_peritem;
  
  print(total_expected_refund)
}

Q4c <- function() {
  DATA = c(37.4, 48.8, 46.9, 55.0, 44.0);
  variance = var(DATA);
  
  # variance of sample is the point-estimate for population-variance
  print(variance);
  
  # ====================== PARAMETERS ======================
  sample_mean = mean(DATA)
  sample_variance = variance
  ci = 0.90
  n_samples = length(DATA);
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

Q5a <- function() {
  # use Mathematica:
  #   "Solve[Integrate[k*(x^2 + (x*y/2)), {x, 0, 1}, {y, 0, 2}] == 1, k]"
  #     output: 6/7
  #       so k=6/7
  
  # marginal density function for X:
  #   "Integrate[(6/7)*(x^2 + (x*y/2)), {y, 0, 2}]"
  #     output: (6 x)/7 + (12 x^2)/7
}

Q5b <- function() {
  alpha = 0.01;
  alpha_half = alpha/2;
  
  z_CRITICAL_oneTailed = abs(qnorm(p=alpha, mean=0, sd=1));
  z_CRITICAL_twoTailed = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  print(z_CRITICAL_twoTailed);
  print(z_CRITICAL_oneTailed);
}

Q6 <- function() {
  # h0: difference_in_populationMean = 0
  # h1: difference_in_populationMean > 0
  
  alpha = 0.05;
  
  # ===== PARAMETERS =====
  sampleA_mean = 4.163
  sampleA_stddev = 0.9562
  sampleA_nSamples = 12
    
  sampleB_mean = 5.105
  sampleB_stddev = 1.6098
  sampleB_nSamples = 8
  # ======================
  
  # small sample-sizes and unknown population-variances
  #   ---> have to assume that both samples come from NORMAL/APPROX-NORMAL populations
  #        with EQUAL variances
  #        ---> can relax this ONLY when sample-sizes are >= 30 for BOTH samples
  #   also,
  #   ---> use t-distribution
  
  pooled_sample_variance = ( (sampleA_nSamples-1) * (sampleA_stddev^2) + (sampleB_nSamples-1) * (sampleB_stddev^2) ) / 
    (sampleA_nSamples + sampleB_nSamples - 2)
  
  h0_difference_in_populationMean = 0;
  
  t_OBSERVED = ( (sampleB_mean - sampleA_mean) - h0_difference_in_populationMean ) / 
    sqrt( pooled_sample_variance * ((1/sampleA_nSamples) + (1/sampleB_nSamples)) )
  
  t_CRITICAL = abs(qt(p=alpha, df=sampleA_nSamples+sampleB_nSamples-2))
  
  print(t_OBSERVED);
  print(t_CRITICAL);
  
  p_value_OBSERVED = pt(q=t_OBSERVED, df=sampleA_nSamples+sampleB_nSamples-2, lower.tail = FALSE);
  p_value_CRITICAL = pt(q=t_CRITICAL, df=sampleA_nSamples+sampleB_nSamples-2, lower.tail = FALSE);
  
  print(p_value_OBSERVED);
  print(p_value_CRITICAL);
}

# Q2()
# Q4a()
# Q4c()
# Q5b()
Q6()
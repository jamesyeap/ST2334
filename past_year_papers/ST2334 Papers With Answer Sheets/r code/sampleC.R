Q1 <- function() {
  # (i)
  z = qnorm(p=0.8413, mean=0, sd=1);
  print(z)
  
  # double-check answer
  print(pnorm(q=58.5, mean=54, sd=4.5))
  
  # Pr(51 < X < 60)
  prob_between_51_60 = pnorm(q=60, mean=54, sd=4.5) - pnorm(q=51, mean=54, sd=4.5)
  
  prob_all4 = (prob_between_51_60)^4;
  print(prob_all4)
  
  nsamples = 4;
  population_mean = 54;
  population_stddev = 2.25;
  
  z_LOWER = round((51 - population_mean) / population_stddev, 2)
  z_HIGHER = round((60 - population_mean) / population_stddev, 2)
  
  prob_sampleMean_between_51_60 = pnorm(q=z_HIGHER, mean=0, sd=1) - pnorm(q=z_LOWER, mean=0, sd=1);
  print(prob_sampleMean_between_51_60)
}

Q2a_i <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 4.5
  sample_variance = 0.75^2
  ci = 0.95
  n_samples = 49
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

Q2a_ii <- function() {
  # ====================== PARAMETERS ======================
  sample_mean = 4.5
  sample_variance = 0.75^2
  ci = 0.95
  n_samples = 49
  # ========================================================
  
  h0_mean = 4.3; # h1_mean != 4.3
  
  alpha_half = (1-ci)/2;
  z_CRITICAL_LOWER = -abs(qnorm(p=alpha_half, mean=0, sd=1))
  z_CRITICAL_HIGHER = abs(qnorm(p=alpha_half, mean=0, sd=1));
  
  z_OBSERVED = (sample_mean - h0_mean) / sqrt(sample_variance / n_samples );
  
  print(z_CRITICAL_LOWER); print(z_CRITICAL_HIGHER); # output: -1.959964 and 1.959964
  print(z_OBSERVED) # output: 1.866667
  
  # observation: NOT WITHIN ANY OF THE 2 CRITICAL-REGIONS
  #   conclusion: DO NOT REJECT h0
}

Q3a <- function() {
  # Hypothesis-Testing ===> differences between means ===> 1-sided
  
  # ========== Parameters ==========
  # Group A: NOT agitated
  nsamples_A = 13;
  sampleMean_A = 30.59
  sampleVariance_A = 2.13^2
  
  # Group B: ARE agitated
  nsamples_B = 13;
  sampleMean_B = 27.78
  sampleVariance_B = 1.73^2
  # ================================
  
  # ASSUMPTIONS MADE:
  #   both populations are normal
  #   both populations have EQUAL variance
  
  alpha = 0.01;
  
  h0_diff = 0; # h1_diff > 0; where h1_diff = populationMean_A - populationMean_B
  
  pooled_sample_variance = ( ((nsamples_A-1)*sampleVariance_A) + ((nsamples_B-1)*sampleVariance_B) ) / (nsamples_A + nsamples_B - 2);
  
  t_CRITICAL = abs(qt(p=alpha, df=nsamples_A + nsamples_B-2));
  t_OBSERVATION = ( (sampleMean_A-sampleMean_B) - h0_diff ) / ( sqrt(pooled_sample_variance*(1/nsamples_A + 1/nsamples_B)) )
  
  print(t_CRITICAL);    # output: 2.492159
  print(t_OBSERVATION); # output: 3.692209
  
  # t_OBSERVATION > t_CRITICAL
  #   INSIDE critical-region
  #     conclusion: enough evidence to REJECT h0 in favour of h1
  
  p_value = pt(q=t_OBSERVATION, df=nsamples_A + nsamples_B-2, lower.tail=FALSE);
  print(p_value);
}

Q3b <- function() {
  oneSide_pvalue = 0.03/2;
  z_OBSERVATION = abs( qnorm(p=oneSide_pvalue, mean=0, sd=1) );
  
  print(z_OBSERVATION);
  
  # for ci=95%, 2-sided
  z_SCORE = abs( qnorm(p=(1-0.95)/2, mean=0, sd=1) );
  
  print(z_SCORE);
}

Q5a <- function() {
  # === to find c ===
    # in Mathematica,
    #   Solve[Integrate[(3/(1 + x)^3), {x, 0, c}] == 1]
    #     output: -1 + sqrt(3) = 0.732
  
  # === to find E(X) ===
    # in Mathematica,
    #   Integrate[x*(3/(1 + x)^3), {x, 0, 0.732}]
    #     output: 0.267928
}

Q6 <- function() {
  # ====================================================================
  # Find the probability that neither facility is busy more than 
  # one-quarter of the time. 
  # ====================================================================
    # in Mathematica,
    #   Integrate[(2/3) (x + 2 y), {x, 0, 1/4}, {y, 0, 1/4}]
    #     output: 1/64
  
  # ====================================================================
  # Find the probability that the drive-up facility is busy more than 
  # one-quarter of the time but less than three quarters of the time.
  # ====================================================================
    # in Mathematica,
    #   Integrate[(2/3) (x + 2 y), {x, 1/4, 3/4}, {y, 0, 1}]
    #     output: 1/2
  
  # ====================================================================
  # Given that the drive-up facility is busy 80% of the time, what is the 
  # probability that the walk-in facility is busy at most half the time?
  # ====================================================================
  
  # first, find marginal-probability function of X
    # Integrate[(2/3) (x + 2 y), {y, 0, 1}]
    #   output: 2/3 + (2 x)/3
  
  # then, the conditional-probability function of Y, given that X = 0.8, is equal to
  #   pdf(x=0.8, y) / marginal_probability_function_X(x=0.8)
  
  # in Mathematica, (make sure you replace x with the given-value, in this case x=0.5), for example:
  #     [BEFORE]:
  #       Integrate[(2/3) (x + 2 y) / (2/3 + (2 *x)/3), {y, 0, 0.5}]
  #     [AFTER] replacing x with 0.8:
  #       Integrate[(2/3) (0.8 + 2 y)/(2/3 + (2 *0.8)/3), {y, 0, 0.5}]
  #     then, running the command in Mathematica, we get:
  #       output: 0.361
  
  # ====================================================================
  # Given that the drive-up facility is busy 80% of the time, what is the 
  # expected proportion of time that the walk-in facility is busy?
  # ====================================================================
  
  # Integrate[y*(2/3) (0.8 + 2 y)/(2/3 + (2 *0.8)/3), {y, 0, 1}]
  #   output: 0.592593
  # (extra stuff)
  #     to get the fraction form: 
  #       Rationalize[0.592593]
  #         output: 16/27
}

# Q1()
# Q2a_i()
# Q2a_ii()
# Q3a()
# Q3b()
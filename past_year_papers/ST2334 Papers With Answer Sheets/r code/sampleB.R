library(prob);
library(combinat);
library(utils);

Q2a <- function() {
  number_of_rolls = 2;
  
  ps <- iidspace(x=1:6, ntrials=number_of_rolls, probs=NULL) # same as rolldie(3), since probs=NULL so equal-chances assumed
  
  ps = addrv(ps, FUN=min, name="M");
  
  # calculate the probability that the minimum-roll = 2
  prob_equal = Prob(x=ps, M==2);
  
  # calculate the expected-value of M
  mean = 0;
  for (row in rownames(ps)) {
    value = ps[row, "M"] * ps[row, "probs"];
    mean = mean + value;
  }
  
  print(prob_equal)
  print(mean)
}

Q3a <- function() {
  # in Mathematica,
    # Solve[Integrate[c*x*Exp[-x], {x, 0, Infinity}] == 1, c]
    #   output: 1
    #     so, c = 1
  
  # to find CDF,
    # Integrate[1*x*Exp[-x], x]
    #   output: E^-x * (-1 - x)
  
  # to find probability,
    # Integrate[1*x*Exp[-x], {x, 2, 5}]
    #   output: 0.365578
}

Q4a <- function() {
  pdf = function(x,y) { (1/25) * (x^2 + y^2) }
  
  # P(X > Y)
  #   only happens for these (x,y) pairs:
  #     (1,0), (2,0), (2,1)
  
  prob_a = pdf(1,0) + pdf(2,0) + pdf(2,1);
  
  # P(X+Y <= 2)
  #   only happens for these (x,y) pairs:
  #     (1,0), (1,1), (2,0)
  
  prob_b = pdf(1,0) + pdf(1,1) + pdf(2,0);
  
  print(prob_a);
  print(prob_b);
}

Q4b <- function() {
  pdf = function(x,y) { (1/25) * (x^2 + y^2) }
  
  marginal_pdf_X <- function(x) { pdf(x,0) + pdf(x,1) + pdf(x,2) }
  marginal_pdf_Y <- function(y) { pdf(1,y) + pdf(2,y) }
  
  X_outcome = c(1,2)
  X_marginal_prob = c(marginal_pdf_X(1), marginal_pdf_X(2));
  
  Y_outcome = c(0,1,2);
  Y_marginal_prob = c(marginal_pdf_Y(0), marginal_pdf_Y(1), marginal_pdf_Y(2));
  
  print(X_outcome); print(X_marginal_prob);
  print(Y_outcome); print(Y_marginal_prob);
  
  multiply_marginal_probs = c(marginal_pdf_X(1) * marginal_pdf_Y(0), marginal_pdf_X(1) * marginal_pdf_Y(1), marginal_pdf_X(1) * marginal_pdf_Y(2), marginal_pdf_X(2) * marginal_pdf_Y(0), marginal_pdf_X(2) * marginal_pdf_Y(1), marginal_pdf_X(2) * marginal_pdf_Y(2))
  joint_probs = c(pdf(1,0), pdf(1,1), pdf(1,2), pdf(2,0), pdf(2,1), pdf(2,2))
  
  # check if both are the same
  print(multiply_marginal_probs);
  print(joint_probs)
  
  # observation: ( marginal_pdf_X(1) * marginal_pdf_Y(0) ) IS NOT EQUAL TO ( pdf(1,0) )
  #   conclusion: X and Y are NOT independent
  
  # conditional distribution of X given Y = 1
  conditional_pdf_X_givenYequal1 <- function(x) {
    y = 1; # given Y = 1
    
    pdf(x,y) / marginal_pdf_Y(y);
  }
  
  conditional_prob = c(conditional_pdf_X_givenYequal1(1), conditional_pdf_X_givenYequal1(2))
  print(X_outcome); print(conditional_prob)
  
  # E(X | Y=1)
  expected = X_outcome[1]*conditional_prob[1] + X_outcome[2]*conditional_prob[2]
  
  print(expected) # output: 1.714
}

Q5b <- function() {
  n=100;
  p=0.5;
  
  prob = 1-pbinom(q=54, size=n, prob=p);
}

Q6a <- function() {
  # HYPOTHESIS-TESTING --> DIFFERENCE IN MEANS --> ONE-SIDED
  
  h0_diff = 0; # no difference
  # h1_diff < 0 ===> mu_A - mu_B < 0
  
  # small-sample sizes
  #   have to assume that population-variances are EQUAL
  
  # ===== PARAMETERS =====
  sampleA_mean = 72.9
  sampleA_stddev = 25.6
  sampleA_nSamples = 13
  
  sampleB_mean = 81.7
  sampleB_stddev = 28.3
  sampleB_nSamples = 16
  
  alpha = 0.05
  # ======================
  
  pooled_sample_variance = ( (sampleA_nSamples-1) * (sampleA_stddev^2) + (sampleB_nSamples-1) * (sampleB_stddev^2) ) / 
    (sampleA_nSamples + sampleB_nSamples - 2)
  
  t_OBSERVED = ( (sampleA_mean - sampleB_mean) - h0_diff ) / 
    sqrt( pooled_sample_variance * ((1/sampleA_nSamples) + (1/sampleB_nSamples)) )
  
  t_CRITICAL = qt(p=0.05, df=sampleA_nSamples+sampleB_nSamples-2);
  
  print(t_OBSERVED); # output: -0.8685893
  print(t_CRITICAL); # output: -1.703288
  
  # t_CRITICAL < t_OBSERVED
  #   CONCLUSION: DO NOT REJECT h0 at alpha=0.05
  
  p_value = pt(q=t_OBSERVED, df=sampleA_nSamples+sampleB_nSamples-2);
  
  print(p_value)
}

Q6b <- function() {
  h0_mean = 100; # h1_mean != 100
  
  # two-sided hypothesis test concerning population-mean
  alpha = 0.05;
  alpha_half = alpha/2;
  
  
}

# Q2a()
# Q4a()
Q4b()
# Q5b()
# Q6a()
# Q6b()
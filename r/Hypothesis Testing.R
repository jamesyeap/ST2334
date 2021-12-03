# ==================== HELPER FUNCTIONS =======================================
calculate_pooled_sample_variance <- function(n_samples_group1, sample_stddev_group1, n_samples_group2, sample_stddev_group2) {
  ( ((n_samples_group1-1)*sample_stddev_group1^2) + ((n_samples_group2-1)*sample_stddev_group2^2) ) / (n_samples_group1 + n_samples_group2 - 2);
}
# =============================================================================

# ****************************** CONCERNING MEAN ******************************
mean <- function() {
  # ===== PARAMETERS ==========================================================
  sampleMean = 
  nsamples = 
  
  h0_populationMean = 
  ci = 
  
  # is population-variance known? (use ONLY 1 and comment out the other-one)
  populationVariance = # CASE 1: yes => use z-distribution!
  sampleVariance =     # CASE 2: no => use t-distribution!
  # ===========================================================================
  
  # ----------- CASE 1: if population-variance IS known: -------------------------------------------------------
  z_OBSERVED = (sampleMean - h0_populationMean) / ( sqrt(populationVariance) / sqrt(nsamples) );
      # CASE 1A: 1-sided test?
      z_CRITICAL_1side = abs( qnorm(p=1-ci, mean=0, sd=1) );
      
        # CASE 1A-1: h0_populationMean > h1_populationMean
        z_CRITICAL_1side_lower = -z_CRITICAL_1side;
        
        # CASE 1A-2: h0_populationMean < h1_populationMean
        z_CRITICAL_1side_higher = z_CRITICAL_1side;
  
      # CASE 1B: 2-sided test?
      z_CRITICAL_2sided = abs( qnorm(p=(1-ci)/2, mean=0, sd=1) );
  # -------------------------------------------------------------------------------------------------------------
      
  # ----------- CASE 2: if population-variance NOT known: -------------------------------------------------------
  t_OBSERVED = (sampleMean - h0_populationMean) / ( sqrt(sampleVariance) / sqrt(nsamples) );
    # CASE 2A: 1-sided test?
    t_CRITICAL_1side = abs( qt(p=1-ci, df=nsamples-1) );
  
      # CASE 2A-1: h0_populationMean > h1_populationMean
      t_CRITICAL_1side_lower = -t_CRITICAL_1side;
  
      # CASE 2A-2: h0_populationMean < h1_populationMean
      t_CRITICAL_1side_higher = t_CRITICAL_1side;
  
    # CASE 2B: 2-sided test?
    t_CRITICAL_2sided = abs( qt(p=(1-ci)/2, df=nsamples-1) );
  # -------------------------------------------------------------------------------------------------------------
}

# ****************************** CONCERNING DIFFERENCE BETWEEN TWO MEANS ******************************

# CASE 3: Unknown But EQUAL Variances (Small Samples) 
differencesBetweenMeans_3 <- function() {
  # ===== PARAMETERS =============================================================
  nsamples_Y = 
  sampleMean_Y = 
  sampleStddev_Y = 
  
  nsamples_X = 
  sampleMean_X = 
  sampleStddev_X = 
    
  h0_diff = 0; # h1_diff < 0; where h1_diff = Y - X
  ci = 
  # ==============================================================================
  
  pooled_sample_variance = calculate_pooled_sample_variance(nsamples_Y, sampleStddev_Y, nsamples_X, sampleStddev_X);
  
  t_OBSERVED = ( (sampleMean_Y-sampleMean_X) - h0_diff ) / (sqrt(pooled_sample_variance*((1/nsamples_Y) + (1/nsamples_X))));
  t_CRITICAL = -abs( qt(p=0.01, df=nsamples_X+nsamples_Y-2) );
  
  print(t_OBSERVED);
  print(t_CRITICAL);  
}
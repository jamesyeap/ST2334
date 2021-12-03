library(RcppAlgos)
library(dplyr)
library(prob)
library(mosaicCalc)

Q1a <- function() {
  box_data <- c(1, 1, 1, 1, 5, 5);
  
  # get total possible combinations -> each row of the matrix represents a possible combination
  possible_combinations = comboGeneral(v=box_data, m=3, repetition=FALSE);
  
  possible_amounts = rowSums(possible_combinations);
  possible_amounts = unique(possible_amounts);
  
  print(possible_amounts);
}

Q1b <- function() {
  box_data <- c(1, 1, 1, 1, 5, 5);
  
  # get total possible combinations -> each row of the matrix represents a possible combination
  possible_combinations = comboGeneral(v=box_data, m=3, repetition=FALSE);
  
  # sum up the total-value of the coins in each possible combination
  possible_amounts = rowSums(possible_combinations);
  # count the frequency of each occurrence of total-value
  numOccurrences_of_possible_amounts = as.data.frame(table(possible_amounts)); # table(...) counts the number of unique values for you
  
  # count the probability of obtaining each possible total-value
  numOccurrences_of_possible_amounts = numOccurrences_of_possible_amounts %>% 
    select(possible_amounts, Freq) %>%
    mutate(prob = Freq / sum(numOccurrences_of_possible_amounts$Freq));
  
  print(probspace(possible_amounts))
  
  print(numOccurrences_of_possible_amounts)
}

# NOT IN TUTORIAL ---> for my own testing
test <- function() {
  box_data <- c(1, 1, 1, 1, 5, 5);
  
  # generate probability-space for:
  #   selecting 3 coins at random (m=3)
  #   without replacement (repetition=FALSE)
  possible_combinations = as.data.frame(comboGeneral(v=box_data, m=3, repetition=FALSE));
  possible_values = rowSums(x=possible_combinations);
  
  # count the probability of obtaining each possible total-value
  table1 = as.table(table(possible_values));
  table2 = prop.table(table1); # prop.table(...) gets the probabilities
                                      
  # convert OUTCOME and their associated PROBABILITIES to a probability-space
  df_table2 = as.data.frame(table2)
  OUTCOMES = as.numeric(as.character((df_table2$possible_values)));
  PROBABILITIES = df_table2$Freq;
  ps_possible_values = probspace(x=OUTCOMES, prob=PROBABILITIES);
  
  # describe the event (ie x>11, x == 11, etc) and get its probability
  print(Prob(x=ps_possible_values, event=(x>=11)));
}

Q2a <- function() {
  S <- tosscoin(times=3, makespace = TRUE);
  S = S %>% mutate(
    numHeads = ifelse(toss1=='H', 1, 0) + ifelse(toss2=='H', 1, 0) + ifelse(toss3=='H', 1, 0),
    numTails = ifelse(toss1=='T', 1, 0) + ifelse(toss2=='T', 1, 0) + ifelse(toss3=='T', 1, 0),
    W = numHeads - numTails
  );
  
  print(unique(S$W))
}

Q2b <- function() {
  S <- tosscoin(times=3, makespace = TRUE);
  S = S %>% mutate(
    numHeads = ifelse(toss1=='H', 1, 0) + ifelse(toss2=='H', 1, 0) + ifelse(toss3=='H', 1, 0),
    numTails = ifelse(toss1=='T', 1, 0) + ifelse(toss2=='T', 1, 0) + ifelse(toss3=='T', 1, 0),
    W = numHeads - numTails
  );

  # count the probability of obtaining each possible total-value
  S_grouped = S %>% group_by(W) %>% summarise(total_probs = sum(probs))
  # print(S_grouped)
  
  # convert OUTCOME and their associated PROBABILITIES to a probability-space
  OUTCOMES = S_grouped$W;
  PROBABILITIES = S_grouped$total_probs
  
  ps_possible_values = probspace(x=OUTCOMES, prob=PROBABILITIES);
  
  print(ps_possible_values);
}

Q2c <- function() {
  S <- iidspace(c("H", "T"), ntrials=3, probs=c(2/3, 1/3));
  S = S %>% mutate(
    numHeads = ifelse(X1=='H', 1, 0) + ifelse(X2=='H', 1, 0) + ifelse(X3=='H', 1, 0),
    numTails = ifelse(X1=='T', 1, 0) + ifelse(X2=='T', 1, 0) + ifelse(X3=='T', 1, 0),
    W = numHeads - numTails
  );
  
  # count the probability of obtaining each possible total-value
  S_grouped = S %>% group_by(W) %>% summarise(total_probs = sum(probs))
  
  # convert OUTCOME and their associated PROBABILITIES to a probability-space
  OUTCOMES = S_grouped$W;
  PROBABILITIES = S_grouped$total_probs

  ps_possible_values = probspace(x=OUTCOMES, prob=PROBABILITIES);

  print(ps_possible_values);
}

Q3 <- function() {
  c = 0.0333; # guess and check -> turns out c = 0.0333 is correct
  
  valid_values <- c(0, 1, 2, 3);
  
  total_prob = 0;
  for (x in valid_values) {
    value = c * (x^2 + 4);
    total_prob = total_prob + value;
  }
  
  print(total_prob); # if total_prob == 1, then the value chosen for "c" is correct
}

Q4 <- function() {
  # TODO
}

Q5 <- function() {
  # TODO
}

Q6a <- function() {
  # trial-and-error
  pdf <- function(x) {
    k = 1.5; # vary this constant
    
    k*sqrt(x);
  }
  integrate(f=pdf, lower=0, upper=1); # if the result == 1, then the constant, k, is correct
}

Q6b <- function() {
  k = 1.5; # now that you've found the value of the constant k, you now have the complete PDF
  cdf <- antiD(k*sqrt(x) ~ x); # then, compute the CDF from the PDF (the CDF is just the anti-derivative of the PDF)
  
  # Pr(0.3 < 𝑋 < 0.6)
  print(cdf(0.6) - cdf(0.3));
}

Q7a <- function() {
  pdf <- function(x) {
    (3/4)*(1-x^2)
  }
  
  prob = integrate(f=pdf, lower=-0.5, upper=0.5);
  print(prob);
}

Q7b <- function() {
  pdf <- function(x) {
    (3/4)*(1-x^2)
  }
  
  prob_lower = integrate(f=pdf, lower=-1, upper=-0.25);
  prob_upper = integrate(f=pdf, lower=0.25, upper=1);
  
  # print(str(prob_lower));
  prob = prob_lower$value + prob_upper$value;
  
  print(prob)
}

Q8a <- function() {
  cdf <- function(x) {
    1-exp(-8*x);
  }
  prob = cdf(12/60); # watch out, x is in hours
  
  print(prob);
}

Q8b <- function() {
  # usage: D(expr ~ wrt-which-variable-inside-expr)
  pdf <- D(1-exp(-8*x) ~ x);
  
  print(pdf);
}

# Q1a();
# Q1b();
# test();
# Q2a();
# Q2b();
# Q2c();
# Q3()
# Q6a();
# Q6b();
# Q7a();
# Q7b();
# Q8a();
# Q8b();

library(prob)
library(stats);
library(combinat)

# Rolling Die(s)

min_roll <- function() {
  number_of_rolls = 2;
  
  ps <- iidspace(x=1:6, ntrials=number_of_rolls, probs=NULL) # same as rolldie(3), since probs=NULL so equal-chances assumed
  
  ps = addrv(ps, FUN=min, name="M");
  
  # calculate the expected-value of M
  mean = 0;
  for (row in rownames(ps)) {
    value = ps[row, "M"] * ps[row, "probs"];
    mean = mean + value;
  }
  
  # calculate the probability that the minimum-roll = 2
  prob_equal = Prob(x=ps, M==2);
  
  print(tail(ps))
  print(mean)
  print(prob_equal)
}

numberfacingup_morethan <- function() {
  number_of_rolls = 2;
  
  ps <- iidspace(x=1:6, ntrials=number_of_rolls, probs=NULL) # same as rolldie(3), since probs=NULL so equal-chances assumed
  
  ps = addrv(ps, X={ifelse(X1 > 3, 1, 0) + ifelse(X2 > 3, 1, 0)})
  
  # calculate the expected-value of M
  mean = 0;
  for (row in rownames(ps)) {
    value = 1/(2+ps[row, "X"]) * ps[row, "probs"];
    mean = mean + value;
  }
  
  # print(tail(ps))
  print(mean)
}

# min_roll()
numberfacingup_morethan()
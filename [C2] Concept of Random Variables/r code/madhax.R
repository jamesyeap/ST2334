library(stats);
library(prob);

Q1 <- function() {
  n = 6;
  p = 0.5;
  
  prob = pbinom(q=2, size=n, prob=p);
  print(prob);
}

Q2 <- function() {
  p = 1/3;
  mean = 1/p; # mean = 3;
  
  prob = pgeom(q=2, prob=p);
  print(prob);
}

Q3 <- function() {
  lambda = 3;
  
  prob = ppois(q=2, lambda=lambda);
  print(prob);
}

pmf <- function(x) {
  cos(x*pi/180) * ( choose(5, x) * 0.3^(5-x) * 0.7^(x) ) ;
}

Q4 <- function() {
  dist <- c(
    pmf(0), pmf(1), pmf(2), pmf(3), pmf(4), pmf(5) 
  );
  
  
  
  print(dist)
  print(sum(dist));
}

Q21 <- function() {
  n = 100;
  p = 0.1;
  
  prob = pbinom(q=11, size=n, prob=p);
  print(prob);
}

# Q21()

Q4();



# Q1();
# Q2();
# Q3();
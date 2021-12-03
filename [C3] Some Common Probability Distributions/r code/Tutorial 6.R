library(stats);
library(prob);
library(combinat);
library(mosaicCalc);

Q1 <- function() {
  # ----- skipped -> think use MICROSOFT EXCEL easier -----
}

Q2a <- function() {
  fruits_DATA <- c("ORANGE", "ORANGE", "ORANGE", "APPLE", "APPLE", "BANANA", "BANANA", "BANANA");
  ps <- probspace(x=urnsamples(x=fruits_DATA, size=4, replace=FALSE, ordered = FALSE));
  
  ps = addrv(ps, X={ifelse(X1 == "ORANGE", 1, 0) + ifelse(X2 == "ORANGE", 1, 0) + ifelse(X3 == "ORANGE", 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == "APPLE", 1, 0) + ifelse(X2 == "APPLE", 1, 0) + ifelse(X3 == "APPLE", 1, 0)});
  
  # print(marginal(space=ps, vars="X"));
  # print(marginal(space=ps, vars="Y"));
}

Q2b <- function() {
  fruits_DATA <- c("ORANGE", "ORANGE", "ORANGE", "APPLE", "APPLE", "BANANA", "BANANA", "BANANA");
  ps <- probspace(x=urnsamples(x=fruits_DATA, size=4, replace=FALSE, ordered = FALSE));
  
  ps = addrv(ps, X={ifelse(X1 == "ORANGE", 1, 0) + ifelse(X2 == "ORANGE", 1, 0) + ifelse(X3 == "ORANGE", 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == "APPLE", 1, 0) + ifelse(X2 == "APPLE", 1, 0) + ifelse(X3 == "APPLE", 1, 0)});
  
  print(marginal(ps, vars=c("X", "Y"))); # from table output -> probs = 0.2571
}

Q2c <- function() {
  fruits_DATA <- c("ORANGE", "ORANGE", "ORANGE", "APPLE", "APPLE", "BANANA", "BANANA", "BANANA");
  ps <- probspace(x=urnsamples(x=fruits_DATA, size=4, replace=FALSE, ordered = FALSE));
  
  ps = addrv(ps, X={ifelse(X1 == "ORANGE", 1, 0) + ifelse(X2 == "ORANGE", 1, 0) + ifelse(X3 == "ORANGE", 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == "APPLE", 1, 0) + ifelse(X2 == "APPLE", 1, 0) + ifelse(X3 == "APPLE", 1, 0)});
  
}

Q2d <- function() {
  fruits_DATA <- c("ORANGE", "ORANGE", "ORANGE", "APPLE", "APPLE", "BANANA", "BANANA", "BANANA");
  ps <- probspace(x=urnsamples(x=fruits_DATA, size=4, replace=FALSE, ordered = FALSE));
  
  ps = addrv(ps, X={ifelse(X1 == "ORANGE", 1, 0) + ifelse(X2 == "ORANGE", 1, 0) + ifelse(X3 == "ORANGE", 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == "APPLE", 1, 0) + ifelse(X2 == "APPLE", 1, 0) + ifelse(X3 == "APPLE", 1, 0)});
  
  print(marginal(space=ps, vars="X"));
}

Q2e <- function() {
  fruits_DATA <- c("ORANGE", "ORANGE", "ORANGE", "APPLE", "APPLE", "BANANA", "BANANA", "BANANA");
  ps <- probspace(x=urnsamples(x=fruits_DATA, size=4, replace=FALSE, ordered = FALSE));
  
  ps = addrv(ps, X={ifelse(X1 == "ORANGE", 1, 0) + ifelse(X2 == "ORANGE", 1, 0) + ifelse(X3 == "ORANGE", 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == "APPLE", 1, 0) + ifelse(X2 == "APPLE", 1, 0) + ifelse(X3 == "APPLE", 1, 0)});
  
  # Prob doesn't work properly here for some reason so gotta do it manually
  print(marginal(ps));              # look for the column where X==2 AND Y == 0 -> 0.12857143
  print(marginal(ps, vars=c("X"))); # look for the column where X==2 -> 0.42857143
  
  print(0.12857143 / 0.42857143);
}

Q3a <- function() {
  ps <- rolldie(times=2, makespace=TRUE);

  ps = addrv(ps, X={ifelse(X1 == 4, 1, 0) + ifelse(X2 == 4, 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == 5, 1, 0) + ifelse(X2 == 5, 1, 0)});
  
  print(marginal(ps, vars=c("X", "Y")));
}

Q3b <- function() {
  ps <- rolldie(times=2, makespace=TRUE);
  
  ps = addrv(ps, X={ifelse(X1 == 4, 1, 0) + ifelse(X2 == 4, 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == 5, 1, 0) + ifelse(X2 == 5, 1, 0)});
  
  prob = Prob(ps, 2*X + Y < 3);
  print(prob)
}

Q3c <- function() {
  ps <- rolldie(times=2, makespace=TRUE);
  
  ps = addrv(ps, X={ifelse(X1 == 4, 1, 0) + ifelse(X2 == 4, 1, 0)});
  ps = addrv(ps, Y={ifelse(X1 == 5, 1, 0) + ifelse(X2 == 5, 1, 0)});
  
  print(marginal(ps, vars=c("X", "Y")));
  
  # Prob(Y==2 && X==2) = 0 // obviously
  # but, Prob(Y==2) * Prob(X==2) is NOT equals to 0!
  #     therefore, X and Y are NOT independent
}

Q4a <- function() {
  # run the following command in Mathematica (without quotations):
      # "Solve[Integrate[k*(x^2 + y^2), {x, 3, 5}, {y, 3, 5}] == 1]"
  
  # note: Mathematica is free for NUS students
}

Q4b <- function() {
  # run the following command in Mathematica (without quotations):
      # "Integrate[3/392*(x^2 + y^2), {x, 3, 4}, {y, 4, 5}]"
}

Q4c <- function() {
  # ---------- TO GET THE MARGINAL DENSITY EQUATION FOR X ------------
  # run the following command in Mathematica (without quotations):
      # "Integrate[3/392*(x^2 + y^2), {y, 3, 5}]"
  
  # ---------- TO GET THE PROBABILITY Pr(3.5 < 𝑋 < 4) ---------------
  # run the following command in Mathematica (without quotations):
      #  "Integrate[3/392*(x^2 + y^2), {y, 3, 5}, {x, 3.5, 4}]"
}

Q5 <- function() {
  # ---------- TO GET THE MARGINAL DENSITY EQUATION FOR Y ------------
  # run the following command in Mathematica (without quotations):
      # "Integrate[24*x*y, {y, 0, 1 - x}]"
      #   output: 12 (1 - x)^2 x
  
  # ---------- TO GET THE MARGINAL DENSITY EQUATION FOR X ------------
  # run the following command in Mathematica (without quotations):
      # "Integrate[24*x*y, {x, 0, 1 - y}]"
      #   output: 12 (1 - y)^2 y
    
  # ---------- FIND THE PROBABILITY THAT THE WEIGHT OF THE TOFFEES IN A BOX... ----------
  #   Integrate[(24*y*(3/4))/(12 (1 - (3/4))^2 *(3/4)), {y, 0, 1/8}]
  #     output: 1/4
}


# Q2a()
# Q2b();
# Q2c();
# Q2d();
# Q2e()
# Q3a();
# Q3b()
# Q3c();

library(stats)

# Mathematica - Trigonometric Functions:
  # Sin ▪  Cos ▪  Tan ▪  Csc ▪  Sec ▪  Cot
  #   ArcSin ArcCos ArcTan

Q4 <- function() {
  n = 5;
  
  p1 = 0.8;
  prob_a = dbinom(x=5, size=5, prob=p1);
  
  p2 = 0.6
  prob_b = dbinom(x=4, size=5, prob=p2);
  
  p3 = 0.3;
  prob_c = pbinom(q=1, size=5, prob=p3);
  
  print(prob_a);
  print(prob_b);
  print(prob_c);
}

Q5 <- function() {
  p_a = 0.25;
  n_successes_a = 1; # size is the required number of successes
  n_prior_failures_a = 3-n_successes_a;   # q is the number of failures before the required number of successes is achieved
  prob_a = pnbinom(q=n_prior_failures_a, size=n_successes_a, prob=p_a);
  
  p_b = 0.7;
  n_successes_b = 1;
  n_prior_failures_b = 5-n_successes_b;
  prob_b = 1 - pnbinom(q=n_prior_failures_b, size=n_successes_b, prob=p_b);
  
  print(prob_a)
  print(prob_b)
}

Q6 <- function() {
  # Solve[Sum[(c/(2^x)), {x, 1, Infinity}] == 1, c]
  #   output: 1
  #     so, c = 1
  
  # Sum[(1/(2^x)), {x, 2, Infinity}]
  #   output: 1/2
  #     so, Pr(X>1) = 1/2
  
  # Solve[Sum[(c*(2^x)/(x!)), {x, 1, Infinity}] == 1, c]
  #   output: 1/(-1 + E^2)
  #     so, c = 1/(-1 + E^2)
  
  # Sum[((1/(-1 + E^2))*(2^x)/(x!)), {x, 2, Infinity}]
  #   output: (-3 + E^2)/(-1 + E^2)
  #     so, Pr(X>1) = (-3 + E^2)/(-1 + E^2)
}

Q9 <- function() {
  # Solve[Sum[(c*2^(x + y))/((x!)*(y!)), {x, 0, Infinity}, {y, 0,Infinity}] == 1, c]
  #   output: 1/E^4
  #     so, c = 1/(e^4)
  
  # ===== MARGINAL PROBABILITY OF Y =====
  # Sum[((1/E^4)*2^(x + y))/((x!)*(y!)), {x, 0, Infinity}]
  #   output: 2^y/(E^2 y!)
  
  # ===== MARGINAL PROBABILITY OF X =====
  # Sum[((1/E^4)*2^(x + y))/((x!)*(y!)), {y, 0, Infinity}]
  #   output: 2^x/(E^2 x!)
  
  # ===== CHECK INDEPENDENCE =====
  # Expand[(2^y/(E^2 y!))*(2^x/(E^2 x!))] == Expand[((1/E^4)*2^(x + y))/((x!)*(y!))]
  #   output: True
  #     so, X and Y are independent
}

Q11 <- function() {
  # Integrate[x * D[(1/(a - b))*((a*(x^b)) - (b*(x^a))), x], {x, 0, 1}]
  #   output: ConditionalExpression[(a b)/(1 + a + b + a b), Re[a] > -1 && Re[b] > -1]
}

Q12 <- function() {
  # ===== FIND JOINT-PDF =====
  # D[(1 - Exp[-x])*((1/2) + (1/Pi)*(ArcTan[y])), x, y]
  #   output: E^-x/(\[Pi] (1 + y^2))
  
  # ===== FIND MARGINAL-PROBABILITY FUNCTION FOR X =====
  # Integrate[(E^-x/(\[Pi] (1 + y^2))), {y, -Infinity, Infinity}]
  #   output: E^-x
  
  # ===== FIND MARGINAL-PROBABILITY FUNCTION FOR Y =====
  # Integrate[(E^-x/(\[Pi] (1 + y^2))), {x, 0, Infinity}]
  #   output: 1/(\[Pi] + \[Pi] y^2)
  
  # ===== TEST INDEPENDENCE =====
  # Expand[(E^-x)*(1/(\[Pi] + \[Pi] y^2))] == Expand[E^-x/(\[Pi] (1 + y^2))]
  #   output: E^-x/(\[Pi] + \[Pi] y^2) == E^-x/(\[Pi] (1 + y^2))
  #     so, X and Y are independent.
}

Q13 <- function() {
  # ===== FIND C =====
  # Solve[Integrate[(c*x*(1 - y)), {x, 0, 1}, {y, 0, 1}] == 1, c]
  #   output: 4
  #     so, c = 4
  
  # ===== FIND MARGINAL-PROBABILITY FUNCTION FOR Y =====
  # Integrate[(4*x*(1 - y)), {x, 0, 1}]
  #   output: 2 (1 - y)
  
  # ===== FIND MARGINAL-PROBABILITY FUNCTION FOR X =====
  # Integrate[(4*x*(1 - y)), {y, 0, 1}]
  #   output: 2 x
  
  # ===== TEST INDEPENDENCE =====
  # Expand[(2 x)*(2 (1 - y))] == Expand[(4*x*(1 - y))]
  #   output: True
  
  # ===== FIND PROBABILITY =====
  # theRegion = ImplicitRegion[0 < x < y < 1, {x, y}];
  # Integrate[(4*x*(1 - y)), {x, y} \[Element] theRegion]
  #   output: 1/6
}

Q14 <- function() {
  # Integrate[24*x*y, {y, 0, 1 - x}]
  #   output: 12 (1 - x)^2 x
}

Q16 <- function() {
  # Integrate[2*(1 - Exp[(-2 x)])*(6*Exp[-6 x]), {x, 0, Infinity}]
  #   output: 1/2
}

# Q4()
# Q5()
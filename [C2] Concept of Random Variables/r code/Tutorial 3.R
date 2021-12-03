Q1 <- function() {
  prob_A = 0.75
  prob_B_g_A = 0.9;
  prob_B_g_nA = 0.8;
  prob_C_g_AB = 0.8;
  prob_C_g_nAB = 0.7; # Pr(𝐶|𝐴′ ∩ 𝐵)
  
  # Pr(𝐴 ∩ 𝐵 ∩ 𝐶)
  prob_AB = prob_B_g_A * prob_A;
  prob_ABC = prob_C_g_AB * prob_AB;
  print(prob_ABC);
  
  # Pr(B)
  prob_BnA = prob_B_g_nA * (1-prob_A);
  prob_B = prob_AB + prob_BnA;
  print(prob_B);
  
  # Pr(A|B)
  prob_A_g_B = prob_AB / prob_B;
  print(prob_A_g_B);
  
  # Pr(B ∩ C)
  prob_nABC = prob_C_g_nAB * (prob_B_g_nA * (1-0.75));
  prob_BC = prob_ABC + prob_nABC;
  print(prob_BC);
  
  # Pr(A | B ∩ C)
  prob_A_g_BC = prob_ABC / prob_BC;
  print(prob_A_g_BC);
}

Q2 <- function() {
  prob_AB = 0.05;
  prob_A = 0.18;
  prob_B = prob_A;
  
  # If product B is profitable, what is the probability that product A becomes profitable?
  prob_A_g_B = prob_AB / prob_B;
  print(prob_A_g_B)
  
  # If at least one of the products will be profitable, what is the probability that the profitable product is A?
  prob_C = prob_A + prob_B - prob_AB; # let C: probability that at least 1 profitable
  prob_A_g_C = prob_A / prob_C;
  
  print(prob_A_g_C);
}

Q3 <- function() {
  # let A: implemented TQM and let B: increase in sales
  prob_A = 30/100;
  prob_B = 60/100;
  prob_A_g_B = 20/60;
  
  print(prob_A);
  print(prob_B);
  
  # Are the two events {TQM implemented} and {Sales increased} independent or dependent? Explain.
  
  # -------------------- INDEPENDENCE TEST FOR 2 EVENTS (PAIRWISE) ------------------------------
  prob_AB = prob_A_g_B * prob_B;
  sprintf("%g | %g", prob_AB, prob_A * prob_B);
}

# Suppose that instead of 20 TQM-implementers among the 60 firms reporting sales
# increases, there were 18. 
# Now are the events {TQM implemented} and {Sales increased} independent or dependent? Explain.
Q3c <- function() {
  # let A: implemented TQM and let B: increase in sales
  prob_A = 30/100;
  prob_B = 60/100;
  prob_A_g_B = 18/60;
  
  print(prob_A);
  print(prob_B);
  
  # Are the two events {TQM implemented} and {Sales increased} independent or dependent? Explain.
  
  # -------------------- INDEPENDENCE TEST FOR 2 EVENTS (PAIRWISE) ------------------------------
  prob_AB = prob_A_g_B * prob_B;
  sprintf("%g | %g", prob_AB, prob_A * prob_B);
}

Q4 <- function() {
  prob_A1 = 0.5;
  prob_A2 = 0.3;
  prob_A3 = 0.2;
  
  prob_R_g_A1 = 0.05;
  prob_R_g_A2 = 0.08;
  prob_R_g_A3 = 0.1;
  
  # Law of Total-Probability
  prob_RA1 = prob_A1 * prob_R_g_A1
  prob_RA2 = prob_A2 * prob_R_g_A2;
  prob_RA3 = prob_A3 * prob_R_g_A3;
  
  prob_R = prob_RA1 + prob_RA2 + prob_RA3;
  prob_A1_g_R = prob_RA1 / prob_R;
  prob_A2_g_R = prob_RA2 / prob_R;
  prob_A3_g_R = prob_RA3 / prob_R;
  
  print(prob_A1_g_R)
  print(prob_A2_g_R)
  print(prob_A3_g_R)
}

Q5 <- function() {
  prob_A1 = 2/4;
  prob_A2 = 2/4;
  prob_A3 = 2/4;
  
  # -------------------- INDEPENDENCE TEST FOR 2 EVENTS (PAIRWISE INDEPENDENCE) ------------------------------
  prob_A1A2 = prob_A1A2A3 = 1/4;
  
  # sprintf("%g | %g", prob_A1A2, prob_A1 * prob_A2);
  # sprintf("%g | %g", prob_A1A2, prob_A1 * prob_A3);
  # sprintf("%g | %g", prob_A1A2, prob_A2 * prob_A3);
  
  
  # -------------------- INDEPENDENCE TEST FOR n EVENTS (MUTUAL INDEPENDENCE) ------------------------------
  sprintf("%g | %g", prob_A1A2A3, prob_A1 * prob_A2 * prob_A3);
}

Q6 <- function() {
  # let W: entire system works
  prob_A = 0.95;
  prob_B = 0.7;
  prob_C = 0.8;
  prob_D = 0.9;
  
  prob_BC = prob_B * prob_C;
  prob_B_or_C = prob_B + prob_C - prob_BC;
  
  prob_W = prob_A * prob_D * prob_B_or_C;
  
  print(prob_W)
  
  prob_BnC = prob_B_or_C - prob_C;
  prob_nC_g_W = (prob_A * prob_D * prob_BnC) / prob_W;
  print(prob_nC_g_W)
}

Q7 <- function() {
  # let A: vehicle passes inspection
  prob_A = 0.6;
  
  # let B: all of next 3 vehicles pass inspection
  prob_B = prob_A^3;
  
  # let C: at least one of the next three vehicles inspected fails.
  prob_C = 1 - prob_B;
  
  # let D: all three pass, given that at least one of the next three vehicles passes inspection
  prob_nonePassInspection = (1-prob_A)^3;
  prob_atLeastOnePass = 1 - prob_nonePassInspection;
  
  prob_D = (prob_B) / prob_atLeastOnePass # note: Pr(#pass = 3 ∩ #pass ≥ 1) = Pr(#pass = 3), so don't multiply!
  
  print(prob_D);
}

Q8 <- function() {
  # let A: house is locked
  prob_A = 1 - 0.3;
  
  # let B: agent has correct key to a house
  
  # ----- I think this step is unnecessary -----
  # a = 1/8;
  # b = (7/8)*(1/7);
  # c = (7/8)*(6/7)*(1/6);
  # d = (7/8)*(6/7)*(5/6)*(1/5);  
  # prob_B = a + b + c + d;
  
  prob_B = 3/8;
  prob_AB = prob_A * prob_B;
  
  # let C: agent can enter house
  prob_C = (1-prob_A) + prob_AB;
  
  print(prob_C);
}


# Q1();
# Q2();
# Q3();
# Q3c()
# Q4()
# Q5()
# Q6()
# Q7();
# Q8()

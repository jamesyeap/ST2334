Q1 <- function() {
  # ----- skipped -> use Microsoft Excel -----
}

Q2 <- function() {
  # ----- skipped -> use Microsoft Excel -----
}

Q3a <- function() {
  # ----- MARGINAL PROBABILITY FUNCTION FOR Y ----- 
  # use Mathematica:
    # "Integrate[(2/3)*(x + 2*y), {x, 0, 1}]"
  
  # ----- MARGINAL PROBABILITY FUNCTION FOR X ----- 
  # use Mathematica:
    # "Integrate[(2/3)*(x + 2*y), {y, 0, 1}]"
  
  # ----- INDEPENDENCE CHECK USING MARGINAL PROBABILITY FUNCTIONS -----
  # use Mathematica:
    # "Integrate[(2/3)*(x + 2*y), {y, 0, 1}] === Integrate[(2/3)*(x + 2*y), {x, 0, 1}]"
    # output: FALSE
  
  # thus, X and Y are NOT independent
}

Q3b <- function() {
  # use Mathematica
  # ----- find E(X) -----
  # Integrate[x*(2/3)*(x + 2*y), {x, 0, 1}, {y, 0, 1}]
  #   output: 5/9
  
  # ----- find E(X^2) -----
  # Integrate[x^2*(2/3)*(x + 2*y), {x, 0, 1}, {y, 0, 1}]
  #   output: 7/18
  
  # Var[X] = 7/18 - (5/9)^2 = 13/162;
}


Q3c <- function() {
  # ----- find E(Y) -----
  # Integrate[y*(2/3)*(x + 2*y), {x, 0, 1}, {y, 0, 1}
  #   output: 11/18
  # ----- find E(Y^2) -----
  # Integrate[y^2*(2/3)*(x + 2*y), {x, 0, 1}, {y, 0, 1}]
  #   output: 4/9
  
  # Var[Y] = 4/9- (11/18)^2 = 23/324
}

Q3d <- function() {
    # ----- find E(XY) -----
    # use Mathematica:
      # Integrate[(x*y)*(2/3)*(x + 2*y), {x, 0, 1}, {y, 0, 1}]
      #   output: 1/3
    
    # ----- Cov(X,Y) = E(XY) - E(X)E(Y) -----
    # Cov(X,Y) = 1/3 - (5/9)(11/18) = -1/162
}

Q4a <- function() {
  # use Mathematica:
    # ----- given JOINT_PDF, ------ 
    #     (1/2 + (3 x^2)/2) (1/2 + (3 y^2)/2)
  
  # ----- find MARGINAL PROBABILITY FUNCTION FOR X, MARG_PDF_X ----
    # Integrate[(3/2)*(x^2 + y^2), {y, 0, 1}]
    #   output: 1/2 + (3 x^2)/2
  
  # ----- find MARGINAL PROBABILITY FUNCTION FOR Y, MARG_PDF_Y ----
    # Integrate[(3/2)*(x^2 + y^2), {x, 0, 1}]
    #   output: 1/2 + (3 y^2)/2
    
  # ----- plot JOINT_PDF against (MARG_PDF_X * MARG_PDF_Y), and see if they overlap 100% across the valid PDF range -----
    # Plot3D[{(1/2 + (3 x^2)/2) (1/2 + (3 y^2)/2), (3/2)*(x^2 + y^2)}, 
    #     {x, 0, 0.1}, {y, 0, 1}]
  
    #   what is seen in the output: NOPE, DON'T OVERLAP 100%
    #   conclusion: X and Y are NOT INDEPENDENT
}

Q4b <- function() {
  # ----- find E[X] ----- 
    # Integrate[(x)*(3/2)*(x^2 + y^2), {x, 0, 1}, {y, 0, 1}]
    #   output: 5/8
  
  # ----- find E[X^2] -----
    # Integrate[(x^2)*(3/2)*(x^2 + y^2), {x, 0, 1}, {y, 0, 1}]
    #   output: 7/15
  
  # Var[X] = 7/15 - (5/8)^2 = 73/960
}

Q4c <- function() {
  # ----- find E[Y] ----- 
  # Integrate[(y)*(3/2)*(x^2 + y^2), {x, 0, 1}, {y, 0, 1}]
  #   output: 5/8
  
  # ----- find E[Y^2] -----
  # Integrate[(y^2)*(3/2)*(x^2 + y^2), {x, 0, 1}, {y, 0, 1}]
  #   output: 7/15
  
  # Var[Y] = 7/15 - (5/8)^2 = 73/960
}

Q4d <- function() {
  # ----- find E[XY] -----
  # Integrate[(y)*(3/2)*(x^2 + y^2), {x, 0, 1}, {y, 0, 1}]
  #   output: 3/8
  
  # Cov(X,Y) = E[XY] - (E[X]*E[Y]) = 3/8 - (5/8)(5/8) = -1/64
}

Q4e <- function() {
  # E[X + Y] = E[X] + E[Y] 
  #   = 5/8 + 5/8 
  #   = 5/4
  
  # Var[X + Y] = Var[X] + Var[Y] + 2 * Cov(X,Y)
  #   = 73/960 + 73/960 + (2*(-1/64))
  #   = 29/240
}

Q5a <- function() {
  # find E[XY]
    # Integrate[(x*y) (x + y), {x, 0, 1}, {y, 0, 1}]
  
  # find E[X]
    # Integrate[(x) (x + y), {x, 0, 1}, {y, 0, 1}]
  
  # find E[Y]
    # Integrate[(y) (x + y), {x, 0, 1}, {y, 0, 1}]
  
  # then, find Cov(X,Y)
    # Integrate[(x*y) (x + y), {x, 0, 1}, {y, 0, 1}] - (Integrate[(x) (x + y), {x, 0, 1}, {y, 0, 1}] * Integrate[(y) (x + y), {x, 0, 1}, {y, 0, 1}])
    #   output: -1/144
}

Q5b <- function() {
  # ----- skipped -> kinda tedious -> workings are in Mathematica -----
}

Q5c <- function() {
  # ----- skipped -> kinda tedious -> workings are in Mathematica -----
}



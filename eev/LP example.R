# As an illustrative example, lets walkthrough a portfolio investment problem. 
#   In the example, we want to find an optimal way to allocate the proportion of
#   asset in our investment portfolio.
#     StockA gives 5% return on average
#     StockB gives 4% return on average
#     StockC gives 6% return on average

# To set some constraints, lets say my investment in C must be less than sum of
#   A, B.  Also, investment in A cannot be more than twice of B.  Finally, at 
#   least 10% of investment in each stock.

# To formulate this problem:
  
#  Variable: x1 = % investment in A, x2 = % in B, x3 = % in C

# Maximize expected return: f(x1, x2, x3) = x1*5% + x2*4% + x3*6%

# Subjected to constraints:
#  10% < x1, x2, x3 < 100%
#  x1 + x2 + x3 = 1
#  x3 < x1 + x2
#  x1 < 2 * x2

library(lpSolve)
library(lpSolveAPI)

# set the number of variables
model = make.lp(0, 3)
# define the objective function: for minimize, use -ve (wtf?)
set.objfn(model, c(-0.05, -0.04, -0.06))
# add constraints
add.constraint(model, c(1,  1,  1), "=", 1)
add.constraint(model, c(1,  1, -1), ">", 0)
add.constraint(model, c(1, -2,  0), "<", 0)
# set upper and lower bounds
set.bounds(model, lower=c(0.1, 0.1, 0.1), upper=c(1, 1, 1))
# compute model
solve(model)
print(model)
# check the final values, that is, what is the allocation to use to max return?
cat("solved variables are:")
print(get.variables(model))
# check the final objective function value: the max return possible
cat("solved obj function result:")
print(get.objective(model))
# get constraints, that is, what did we constrain the systems of equations to?
#get.constraints(model)

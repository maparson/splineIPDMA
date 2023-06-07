#--------------------------------#
# FP script
#--------------------------------#

#- Evaluate one part of the FP2 function
FPone <- function(FP, beta = 1, x) {
  if(FP == 0) {
    toReturn = beta*log(x)
  } else {
    toReturn = beta*x^(FP)
  }
  return(toReturn)
}
#- Evaluate both parts together and return the value
FPeval <- function(FPs, betas = c(1,1), x, intercept = 0) {
  if(FPs[1] != FPs[2]) {
    num1 = FPone(FP = FPs[1], beta = betas[1], x)
    num2 = FPone(FP = FPs[2], beta = betas[2], x)
    toReturn = num1 + num2 + intercept
  } else {
    num1 = FPone(FP = FPs[1], beta = betas[1], x)
    num2.1 = FPone(FP = FPs[1], beta = betas[2], x)
    num2 = log(x)*num2.1
    toReturn = num1 + num2 + intercept 
  }
  return(toReturn)
}

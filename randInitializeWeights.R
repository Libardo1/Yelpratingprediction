randInitializeWeights <- function(L_in, L_out){
  #randInitializeWeights initialize weights in a hidden layer of neurons randomly with a parameter epsilon of 0.12
  
  W <- runif(L_in * L_out, min = -0.12, max = 0.12)
  W <- matrix(data = W, nrow = L_in, ncol =L_out)
    
}
stochasticNeuralNetEnvironments <- function(usersKeys, businessKeys, y, theta1, theta2, alpha, users.env, business.env, checkin.env, lambda){
  
  require(e1071)
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/sigmoidGradient.R')
  
  m <- length(y)
  
  b <- diag(max(y))
  y <- t(sapply(y, anonfun <- function(a, Diagonal){
    Diagonal[a, ]
  }, b))
  
  #theta1_grad <- matrix(data = 0, nrow = nrow(theta1), ncol = ncol(theta1))
  #theta2_grad <- matrix(data = 0, nrow = nrow(theta2), ncol = ncol(theta2))
    
  for (i in 1:m){  
    
    X1 <- try(get(as.character(usersKeys[iter]), envir = users.env), silent = TRUE)
    if (class(X1) == "try-error"){X1 <- get("mean", envir = users.env)}
    X2 <- try(get(as.character(businessKeys[iter]), envir = business.env), silent = TRUE)
    if (class(X2) == "try-error"){X2 <- get("mean", envir = business.env)}
    X3 <- try(get(as.character(businessKeys[iter]), envir = checkin.env), silent = TRUE)
    if (class(X3) == "try-error"){X3 <- 0 } #if the key is not found in the environment it is assumend that there is zero check-ins
    
    X <- c(1, X1, X2, X3)   
    
    #Feedforward Pass
    hidden <- sigmoid(X %*% theta1)
    out <- sigmoid(c(1, hidden) %*% theta2)
    
    #Backpropagation
    delta3 <- out - y[i, ]
    delta3Theta2 <- delta3 %*% t(theta2)
    delta2 <- delta3Theta2[ , 2:ncol(delta3Theta2)] * sigmoidGradient(X %*% theta1)
    
    #The important stuff 
    #Regularization term
    #regGrad1 <-  (lambda/m) * theta1[2:nrow(theta1), ]                                           
    #regGrad2 <-  (lambda/m) * theta2[2:nrow(theta2), ]                                                             
    regGrad1 <-  lambda * theta1[2:nrow(theta1), ]                                           
    regGrad2 <-  lambda * theta2[2:nrow(theta2), ]   
    
    #theta1_grad <- 1/m * (t(delta2) %*% X)
    #theta2_grad <- 1/m * (t(delta3)) %*% c(1, hidden)
    theta1_grad <- (t(delta2) %*% X)
    theta2_grad <- (t(delta3)) %*% c(1, hidden)
    
    
    theta1 <- theta1 - rbind(theta1_grad[ ,1], t(theta1_grad[, 2:ncol(theta1_grad)]) + regGrad1)
    theta2 <- theta2 - rbind(theta2_grad[ ,1], t(theta2_grad[ ,2:ncol(theta2_grad)]) + regGrad2)          
    
    #theta1_grad <- rbind(theta1_grad[ ,1], t(theta1_grad[, 2:ncol(theta1_grad)]) + regGrad1)
    #theta2_grad <- rbind(theta2_grad[ ,1], t(theta2_grad[ ,2:ncol(theta2_grad)]) + regGrad2)          
    
    print(paste(i, "/" , m, "Iterations" ))
    
    
  }
  
  return(list(theta1, theta2))
}
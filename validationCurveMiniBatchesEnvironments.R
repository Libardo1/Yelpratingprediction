validationCurveMiniBatchesEnvironments <- function(usersKeys, businessKeys, y, theta1, theta2, alpha, users.env, business.env, checkin.env, trainIndices, valIndices){
  
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/predictNetsFromEnvironments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/miniBatchNeuralNetEnvironments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSEEnvironments.R')
  
  lambdaVec = c(10, 3, 1, 0, 0.1, 0.3)
  
  errorTrain <- rep(0, length(lambdaVec))
  errorVal <- rep(0, length(lambdaVec))
  
  for (i in 1:length(lambdaVec)){
    lambda <- lambdaVec[i]
    model <- miniBatchNeuralNetEnvironments(usersKeys[trainIndices], businessKeys[trainIndices], 
                                             y[trainIndices], theta1, theta2, alpha, users.env, business.env, checkin.env, lambda, 1000)
    
    
    errorTrain[i] <- RMSEEnvironments(predictNetsFromEnvironments(usersKeys[trainIndices], businessKeys[trainIndices],
                                                                  model[[1]], model[[2]], users.env, business.env, checkin.env), y[trainIndices])
    
    errorVal[i] <- RMSEEnvironments(predictNetsFromEnvironments(usersKeys[valIndices], businessKeys[valIndices],
                                                                model[[1]], model[[2]], users.env, business.env, checkin.env), y[valIndices])
    
  }
  return(list(errorTrain, errorVal))  
  
}
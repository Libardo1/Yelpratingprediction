learningCurveMiniBatches <- function(usersKeys, businessKeys, y, theta1, theta2, alpha, users.env, business.env, checkin.env, trainIndices, valIndices, lambda){
  
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/predictNetsFromEnvironments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/miniBatchNeuralNetEnvironments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSEEnvironments.R')
  
  m = length(trainIndices)
  
  errorTrain <- rep(0, m)
  errorVal <- rep(0, m)
  
  for (i in 1:m){
    
    indicesVector <- trainIndices[1:i]
    model <- miniBatchNeuralNetEnvironments(usersKeys[indicesVector], businessKeys[indicesVector], 
                                             y[indicesVector], theta1, theta2, alpha, users.env, business.env, checkin.env, lambda, 1000)    
    
    errorTrain[i] <- RMSEEnvironments(predictNetsFromEnvironments(usersKeys[indicesVector], businessKeys[indicesVector],
                                                                  model[[1]], model[[2]], users.env, business.env, checkin.env), y[indicesVector])
    
    errorVal[i] <- RMSEEnvironments(predictNetsFromEnvironments(usersKeys[valIndices], businessKeys[valIndices],
                                                                model[[1]], model[[2]], users.env, business.env, checkin.env), y[valIndices])
  }
  
  return(list(errorTrain, errorVal))  
}
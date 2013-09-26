#Yelp Business Rating Prediction 
#This ain't over baby

#####################################
#Init
rm(list = ls())
ls()

setwd("D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/")

#####################################
#Read Id Look Up Table
#Not Valid anymore
#IdsTable <- read.csv("IdLookupTable.csv")

##############################
#Reading Training Set

library(RJSONIO)
Lines <- readLines("yelp_training_set_business.json")
business <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_user.json")
users <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_checkin.json")
checkin <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_review.json")
review <- as.data.frame(t(sapply(Lines, fromJSON, USE.NAMES = FALSE)))

#########################################
#Read Test Set
Lines <- readLines("yelp_test_set_business.json")
businessTestNOT <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_user.json")
usersTestNOT <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_checkin.json")
checkinTestNOT <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_review.json")
reviewTest <- as.data.frame(t(sapply(Lines, fromJSON, USE.NAMES = FALSE)))


#########################################
#Read FINAL Test Set HERP DERP
Lines <- readLines("final_test_set_business.json")
businessTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("final_test_set_user.json")
usersTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("final_test_set_checkin.json")
checkinTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("final_test_set_review.json")
reviewTest <- as.data.frame(t(sapply(Lines, fromJSON, USE.NAMES = FALSE)))

rm(Lines) #Delete element called "Lines"

#Define Targets
y <- as.numeric(review$stars)


#############################################
#Preprocess business columns
source('D:/Wacax/Kaggle Data Analysis/Yelp/extractContent.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/toBinary.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/Normalize.R')

latitude <- as.numeric(c(business$latitude, businessTest$latitude)) - 33
longitude <- as.numeric(c(business$longitude, businessTest$longitude)) + 113
numberOfClusters <- 500
numDiagonal <- diag(numberOfClusters)
LocationCluster <- kmeans(cbind(longitude, latitude), numberOfClusters)
LocationCluster <- t(sapply(LocationCluster$cluster, anonfun <- function(a, Diagonal){
  Diagonal[a, ]
}, numDiagonal))


businessCategories <- extractContent(as.character(c(business$categories, businessTest$categories), use.names=FALSE),
                                     unique(unlist(c(business$categories, businessTest$categories), use.names=FALSE)))

meanBusinessStars <- rep(mean(as.numeric(business$stars)), nrow(businessTest))


BusinessMatrix <- cbind(businessCategories,
                        toBinary(c(business$city, businessTest$city)), 
                        toBinary(c(business$state, businessTest$state)), 
                        as.numeric(c(business$open, businessTest$open)), 
                        Normalize(as.numeric(c(business$review_count, businessTest$review_count))), 
                        as.numeric(c(business$stars, meanBusinessStars)),
                        LocationCluster
                        
)

rownames(BusinessMatrix) <- NULL

###########################################################
#Checkin preprocesing

checkinOrderedTrain <- Normalize(as.numeric(sapply(checkin$checkin_info, sum, USE.NAMES = FALSE)))
checkinOrderedTest <- Normalize(as.numeric(sapply(checkinTest$checkin_info, sum, USE.NAMES = FALSE)))


#############################################
#Preprocess users columns

meanUsersVotes <- apply(as.data.frame(users$votes), 1, mean)
meanUsersVotes <- matrix(data = rep(meanUsersVotes, nrow(usersTest)), nrow = nrow(usersTest) , ncol = 3, byrow = TRUE)

meanUsersStars <- rep(mean(as.numeric(users$average_stars)), nrow(usersTest))

UsersMatrix <- cbind(rbind(t(as.data.frame(users$votes)), meanUsersVotes),
                     Normalize(as.numeric(c(users$review_count, usersTest$review_count))), 
                     as.numeric(c(users$average_stars, meanUsersStars))
)

UsersMatrix[ ,1] <- Normalize(UsersMatrix[ ,1])
UsersMatrix[ ,2] <- Normalize(UsersMatrix[ ,2])
UsersMatrix[ ,3] <- Normalize(UsersMatrix[ ,3])

rownames(UsersMatrix) <- NULL

#################################################
#Alternaive Users Preprocessing

meanUsersStars <- rep(mean(as.numeric(users$average_stars)), nrow(usersTest))

OtherUsersMatrix <- cbind(Normalize(as.numeric(c(users$review_count, usersTest$review_count))), 
                          as.numeric(c(users$average_stars, meanUsersStars))
)

rownames(OtherUsersMatrix) <- NULL

#################################################
#Second Alternaive Users Preprocessing

OtherUsersMatrix3 <- cbind(Normalize(as.numeric(c(users$review_count, usersTest$review_count))) 
                          )

rownames(OtherUsersMatrix3) <- NULL

########################################################
#Assign values to environments

source('D:/Wacax/Kaggle Data Analysis/Yelp/assignToEnvironment.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/getVectors.R')

#Assign business values environment
business.env <- new.env()
assignToEnvironment(as.character(c(business$business_id, businessTest$business_id)), BusinessMatrix, business.env)

#Assign users values environment
users.env <- new.env()
assignToEnvironment(as.character(c(users$user_id, usersTest$user_id)), UsersMatrix, users.env)

#Assign other users values environment
users2.env <- new.env()
assignToEnvironment(as.character(c(users$user_id, usersTest$user_id)), OtherUsersMatrix, users2.env)

#Assign other users values environment
users3.env <- new.env()
assignToEnvironment(as.character(c(users$user_id, usersTest$user_id)), OtherUsersMatrix3, users3.env)

#Assign checkin values to environment
checkin.env <- new.env()
assignToEnvironment(as.character(c(checkin$business_id, checkinTest$business_id)), c(checkinOrderedTrain, checkinOrderedTest), checkin.env)

#Calculate Means
meanBusiness <- apply(BusinessMatrix, 2, mean)
meanUsers <- apply(UsersMatrix, 2, mean)
otherMeanUsers <- apply(OtherUsersMatrix, 2, mean)
otherMeanUsers3 <- mean(OtherUsersMatrix3)
meanCheckin <- mean(c(checkinOrderedTrain, checkinOrderedTest))

#Assign means to their respective environments
assign("mean", meanBusiness, envir = business.env)
assign("mean", meanUsers, envir = users.env)
assign("mean", otherMeanUsers, envir = users2.env)
assign("mean", otherMeanUsers3, envir = users3.env)
assign("mean", meanCheckin, envir = checkin.env)


#end of preprocessing
##################################################
#Machine Learning
##################################################
#Validation Curves
#Sampling the data
source('D:/Wacax/Kaggle Data Analysis/Yelp/RMSLEEnvironments.R')

alpha <- 0.03
fullDataShuffled <- sample(length(y))
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsAlphas <- validationCurveEnvironments(usersIdTrain, businessIdTrain, starsTrain,
                                            dateReviewed, y, alpha, users.env,
                                            business.env, checkin.env, trainIndices, valIndices)

errorTest <-  RMSLEEnvironments(predictFromEnvironments(usersIdTrain[testIndices], businessIdTrain[testIndices], 
                                                        
                                                        model, users.env, business.env, checkin.env), y[testIndices])


####################################################
#Ensemble of linears second try
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/stochasticLinearGradienteENVIRONMENTS.R')
thetaLength <- length(get("mean", envir = users.env)) + 
               length(get("mean", envir = business.env)) +
               length(get("mean", envir = checkin.env)) + 1 #extra column of ones

theta <- rep(0, thetaLength)
alpha <- 0.3
lambda <- 3

numberOfShuffles <- 30
ensembleStochastics <- matrix(data = rep(0, thetaLength * numberOfShuffles), nrow = numberOfShuffles, ncol = thetaLength)

for(ii in 1:numberOfShuffles){
  
  indicesModel <- sample(1:length(usersIdTrain), length(usersIdTrain))
  modelStochastic <- stochasticLinearGradient(usersIdTrain[indicesModel], businessIdTrain[indicesModel], y[indicesModel], theta, alpha, users.env, business.env, checkin.env, lambda)
  
  ensembleStochastics[ii, ] <- t(modelStochastic)
  
  print(paste(ii, "/" , numberOfShuffles, "Model" ))
  
  
}
########################################
#Prediction for the Ensemble

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/predictFromEnvironments.R')

user_id <- IdsTable[, 1]
business_id <- IdsTable[, 2]

predictionsBlock <- matrix(data = rep(0, length(user_id) * numberOfShuffles), nrow = length(user_id), ncol = numberOfShuffles)

for (i in 1:nrow(ensembleStochastics)){
  predictionsBlock[ ,i] <- predictFromEnvironments(user_id, business_id, ensembleStochastics[i, ], users.env, business.env, checkin.env)
  
  print(paste(i, "/" , nrow(ensembleStochastics), "Prediction" ))
  
}

stars <- apply(floor(predictionsBlock), 1, mean)
RecommendationId <- 1:length(stars)

write.csv(cbind(RecommendationId, stars), file = "prediction.csv", row.names = FALSE)
##################################################
#                                                #
##################################################

#################################################
#Validation Curves for neural nets
#Sampling the data
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSLEEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/validationCurveEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')

alpha <- 0.03

numberOfStars <- max(y)
hiddenUnits <- 200
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)

validationAmount <- 500
fullDataShuffled <- sample(1:length(y), validationAmount)
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsLambdas <- validationCurveEnvironments(trainUsers, trainBusiness, y, initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, trainIndices, valIndices)

errorTest <-  RMSLEEnvironments(predictFromEnvironments(trainUsers[testIndices], trainBusiness[testIndices],
                                                        initWeightsTheta1, initWeightsTheta2, users.env, business.env, checkin.env), y[testIndices])

#####################################################
#Learning Curve
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSLEEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/learningCurve.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')


alpha <- 0.03

numberOfStars <- max(y)
hiddenUnits <- 50
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)

learningCurveAmount <- 6000
fullDataShuffled <- sample(1:length(y), learningCurveAmount)
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsAlgorithm <- learningCurve(trainUsers, trainBusiness, y, initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, trainIndices, valIndices, 3)


####################################################

#Stochastic Neural Net Learning

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/stochasticNeuralNetEnvironments.R')

lambda <- 3
#lambda <- 1000

numberOfStars <- max(y)
hiddenUnits <- 350
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)

model <- stochasticNeuralNetEnvironments(trainUsers, trainBusiness, y, initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, 3)

#####################################
#Prediction From Neural Nets

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/predictNetsFromEnvironments.R')

stars <- predictNetsFromEnvironments(as.character(IdsTable[, 1]), as.character(IdsTable[, 2]), model[[1]], model[[2]], users.env, business.env, checkin.env)
RecommendationId <- 1:length(stars)

write.csv(cbind(RecommendationId, stars), file = "prediction.csv", row.names = FALSE)

#####################################################
#####################################################
#DOES NOT WORK
#Learning Curve Mini Batches
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSLEEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/learningCurveMiniBatches.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')


alpha <- 0.03

numberOfStars <- max(y)
hiddenUnits <- 50
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)

learningCurveAmount <- 50000
fullDataShuffled <- sample(1:length(y), learningCurveAmount)
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsAlgorithm <- learningCurveMiniBatches(trainUsers, trainBusiness, y, initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, trainIndices, valIndices, 3)


####################################################
#DOES NOT WORK
#Validation Curves for stochastic neural nets with mini batches
#Sampling the data

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/RMSLEEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/validationCurveMiniBatchesEnvironments.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')

alpha <- 0.03

numberOfStars <- max(y)
hiddenUnits <- 150
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)

validationAmount <- 50000 
fullDataShuffled <- sample(1:length(y), validationAmount)
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsLambdas <- validationCurveMiniBatchesEnvironments(trainUsers, trainBusiness, y, initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, trainIndices, valIndices)

errorTest <-  RMSLEEnvironments(predictFromEnvironments(trainUsers[testIndices], trainBusiness[testIndices],
                                                        initWeightsTheta1, initWeightsTheta2, users.env, business.env, checkin.env), y[testIndices])


###################################
#Stochastic Mini-Batch Train

#reorder batches
probabilities <- sapply(1:5, anonFun <- function(number){sum(y == number) / length(y)})

batchedprobabilites <- rep(0, length(y))
batchedprobabilites[y==1] <- probabilities[1]/sum(y==1)
batchedprobabilites[y==2] <- probabilities[2]/sum(y==2)
batchedprobabilites[y==3] <- probabilities[3]/sum(y==3)
batchedprobabilites[y==4] <- probabilities[4]/sum(y==4)
batchedprobabilites[y==5] <- probabilities[5]/sum(y==5)

indicesForBatches <- sample(1:length(y), length(y), replace = FALSE, prob = batchedprobabilites)

#Training

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/randInitializeWeights.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/miniBatchNeuralNetEnvironments.R')

lambda <- 1
#lambda <- 1000

numberOfStars <- max(y)
hiddenUnits <- 200
MiniBatch <- 1000
featuresLength <- length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 1 #extra column of ones

initWeightsTheta1 <- randInitializeWeights(featuresLength, hiddenUnits)
initWeightsTheta2 <- randInitializeWeights(hiddenUnits + 1 , numberOfStars)


model <- miniBatchNeuralNetEnvironments(trainUsers[indicesForBatches], trainBusiness[indicesForBatches], y[indicesForBatches], initWeightsTheta1, initWeightsTheta2, 0.01, users.env, business.env, checkin.env, 3, MiniBatch)

#####################################
#Prediction From Neural Nets with mini-batches

source('D:/Wacax/Kaggle Data Analysis/Yelp Business Rating Prediction/predictNetsFromEnvironments.R')

IdsTable <- cbind(reviewTest[, 1], reviewTest[, 3])

stars <- predictNetsFromEnvironments(as.character(IdsTable[, 1]), as.character(IdsTable[, 2]), model[[1]], model[[2]], users.env, business.env, checkin.env)
review_id <- 1:length(stars)

write.csv(cbind(review_id, stars), file = "predictionIII.csv", row.names = FALSE)

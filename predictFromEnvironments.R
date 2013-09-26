predictFromEnvironments <- function(usersKeys, businessKeys, model, users.env, business.env, checkin.env){
  
  m <- length(usersKeys)
  
  prediction <- rep(0, m)
  
  for (iter in 1:m){  
    
    X1 <- try(get(as.character(usersKeys[iter]), envir = users.env), silent = TRUE)
    if (class(X1) == "try-error"){X1 <- get("mean", envir = users.env)}
    X2 <- try(get(as.character(businessKeys[iter]), envir = business.env), silent = TRUE)
    if (class(X2) == "try-error"){X2 <- get("mean", envir = business.env)}
    X3 <- try(get(as.character(businessKeys[iter]), envir = checkin.env), silent = TRUE)
    if (class(X3) == "try-error"){X3 <- 0 } #if the key is not found in the environment it is assumend that there is zero check-ins
    
    X <- c(1, X1, X2, X3)    
    
    prediction[iter] <- X %*% model 
    
    print(paste(iter, "/" , m, "Iteration" ))
    
    
  }
  
  return(prediction)
}

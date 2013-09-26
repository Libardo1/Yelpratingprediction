RMSEEnvironments <- function(predictions, y){
  
  predictions <- apply(predictions, 1, which.max)
  
  require(matlab)
  
  RMSEcost = sqrt(sum((predictions - y) ^ 2)/numel(predictions))
  
}

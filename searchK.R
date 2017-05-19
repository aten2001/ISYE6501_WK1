k_helper <- function(learn, valid, k) {
  model <- kknn(R1 ~ ., learn, valid, k = k)
  
  return(model)
}

searchK <- function(data, maxK = 10){
  m <- nrow(data)
  
  # index for validation set
  val <- sample(1:m, size = ceiling(m/10))
  
  data.learn <- data[-val]
  data.valid <- data[val]
  
  for (i in 1:maxK) {
    model <- k_helper(data.learn, data.valid, i)
    
    
  }
}
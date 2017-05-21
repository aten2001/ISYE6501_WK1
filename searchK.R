k_helper <- function(learn, valid, k) {
    ans <- list()
    
    model <- kknn(R1 ~ ., learn, valid, k = k)
    
    acc <- sum(model$fitted.values == valid[,11])/nrow(valid)
    
    ans <- list(model = model, acc = acc, k = k)
    
    return(ans)
}

searchK <- function(data, maxK = 10){
    m <- nrow(data)
    
    # index for validation set
    val <- sample(1:m, size = ceiling(m/10))
    
    data.learn <- data[-val,]
    data.valid <- data[val,]
    
    ans <- list(acc = 0)
    for (i in 1:maxK) {
        temp <- k_helper(data.learn, data.valid, i)
        
        if (temp$acc > ans$acc) {
            ans <- temp
        }
    }
    
    return(ans)
}
# Compute accuracy
helper <- function(train_set, test_set, c_val) {
    
    model <- ksvm(R1 ~ ., data = train_set, type="C-svc",kernel="vanilladot",C=c_val,scaled=TRUE)
    
    pred <- predict(model, test_set[,1:10])
    
    ans <- list(c_val = c_val, acc = sum(pred == test_set[,11])/nrow(test_set), model = model)
    
    print(paste('C =', c_val, 'Acc:', ans$acc))
    
    return(ans)
}

# data: data frame to be used for model building. Last column is the response.
# range: floating point in the form c(lower, upper). Search C in [lower, upper]. lower > 0!
# grid_n: number of grid points (including start and end points). Number of intervals = grid_n - 1.
# level: int indicating which level of grid search to perform (set to start at 1 initially)
searchC <- function(train_set, test_set, range = c(0.0001, 1), grid_n, level = 1) {
    
    # grid search for maximum acc & corresponding index
    c_vals <- seq(range[1], range[2], length.out = grid_n)
    
    max_ind <- 1
    ans <- list(acc = 0)
    
    for (i in 1:grid_n) {
        temp <- helper(train_set, test_set, c_vals[i])
        
        if (temp$acc > ans$acc) {
            max_ind <- i
            ans <- temp
        }
    }
    
    if (level < 2) {
        
        if (max_ind == 1) {
            range <- c(c_vals[max_ind], c_vals[max_ind + 2])
        } else if (max_ind == length(c_vals)) {
            range <- c(c_vals[max_ind - 2], c_vals[max_ind])
        } else {
            range <- c(c_vals[max_ind - 1], c_vals[max_ind + 1])
        }
        
        return(searchC(train_set, test_set, range, grid_n, level+1))
        
    } else {
        
        return(ans)
    }
}
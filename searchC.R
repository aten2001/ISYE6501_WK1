require('kernlab')

# Compute accuracy
helper <- function(data, c_val) {
    rows = nrow(data)
    cols = ncol(data)
    
    model <- ksvm(as.matrix(data[,1:cols-1]), as.factor(data[,cols]), type="C-svc",kernel="vanilladot",C=c_val,scaled=TRUE)
    
    pred <- predict(model, data[,1:cols-1])
    
    ans <- list(c_val = c_val, acc = sum(pred == data[,cols])/rows, model = model)
    
    return(ans)
}

# data: data frame to be used for model building. Last column is the response.
# range: floating point in the form c(lower, upper). Search C in [lower, upper]. lower > 0!
# grid_n: number of grid points (including start and end points). Number of intervals = grid_n - 1.
# level: int indicating which level of grid search to perform (set to start at 1 initially)
searchC <- function(data, range = c(1, 101), grid_n, level = 1) {
    
    # grid search for maximum acc & corresponding index
    c_vals <- seq(range[1], range[2], length.out = grid_n)
    
    max_acc <- 0
    max_ind <- 1
    ans <- list()
    for (i in 1:grid_n) {
        ans <- helper(data, c_vals[i])
        
        if (ans$acc[1] > max_acc) {
            max_ind <- i
            max_acc <- ans$acc
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
        
        return(searchC(data, range, grid_n, level+1))
        
    } else {
        
        return(ans)
    }
}
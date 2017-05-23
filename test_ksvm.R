# predictors_SCL <- scale(as.matrix(credit_data[,1:10]))
# 
# pred <- sign(t(a) %*% t(predictors_SCL) + svm_model@b)
# 
# pred_a0 <- sign(t(a) %*% t(predictors_SCL) + a0)
# 
# sum(pred == fitted(model))/nrow(credit_data)
# 
# sum(pred_a0 == fitted(model))/nrow(credit_data)


data <- read.table('credit_card_data-headers.txt', header = TRUE)

data[data[,11] == 0 ,11] = -1

data[,11] = as.factor(data[,11])


n <- nrow(data)
intrain <- sample(1:n, ceiling(n*8/10))

data.train <- data[intrain,]
data.test <- data[-intrain,]

model <- ksvm(R1 ~., data = data.train, type = 'C-svc', C = 100, kernel = 'vanilladot', scaled = TRUE, cross = 10)


pred = predict(model, data.test[, 1:10])

acc <- sum(pred == data.test[,11])/nrow(data.test)

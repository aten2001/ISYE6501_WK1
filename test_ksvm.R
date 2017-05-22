predictors_SCL <- scale(as.matrix(credit_data[,1:10]))

pred <- sign(t(a) %*% t(predictors_SCL) + svm_model@b)

pred_a0 <- sign(t(a) %*% t(predictors_SCL) + a0)

sum(pred == fitted(model))/nrow(credit_data)

sum(pred_a0 == fitted(model))/nrow(credit_data)
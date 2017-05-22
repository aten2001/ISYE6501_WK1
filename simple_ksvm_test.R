# use a decison boundary of y=x with symmetric data
x = c(100,100,100,100,200,200,200,300,300,400,200,300,300,400,400,400,500,500,500,500)
y = c(200,300,400,500,300,400,500,400,500,500,100,100,200,100,200,300,100,200,300,400)
predictors = as.matrix(cbind(x,y));  classes = c(rep("-1", 10), rep("1", 10))

# plot the data and add the known decision boundary
plot(x, y, col=as.factor(classes), pch=20, cex=3);    abline(0,1, lty=4, lwd=3)

# call vanilladot ksvm
model = ksvm(predictors, classes, type="C-svc", kernel="vanilladot", C=100, scaled=TRUE) 
# calculate a1.am   
a <- colSums(predictors[model@SVindex,1:2] * model@coef[[1]]); a
# calculate a0
a0 <- sum(a*predictors[1,1:2]) - model@b; a0
# see what the model predicts
pred <- predict(model,predictors); pred

#    scale predictors
predictors_SCL = scale(predictors)

# use calculated 'a0' get predicted values and then predicted signs
predictedValues_a0 = colSums(t(predictors_SCL)*a) + a0
predictedSigns_a0 = sign(predictedValues_a0)

# use model attribute 'b' to get predicted values and then predicted signs
predictedValues_b = colSums(t(predictors_SCL)*a) + model@b
predictedSigns_b = sign(predictedValues_b)

table('a0 preds' = predictedSigns_a0, ModelFits = fitted(model))
table('b preds' = predictedSigns_b, ModelFits = fitted(model))

# plot the "decision boundary" implied by a0
slope = -(a[1]/a[2]);   intercept_a0 = a0/a[2];   abline(intercept_a0, slope)

# calculate the lower gutter similarly and plot
aLower = -sum(a*predictors[1,1:2]) + model@b
abline(aLower/a[2], slope)
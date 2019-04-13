View(mtcars)
attach(mtcars)
logit <- glm(vs~factor(am)+mpg+cyl+disp+hp+drat+wt+qsec+gear+carb, family = binomial, data = mtcars)
summary(logit)
exp(coef(logit))
prob <- predict(logit, type = c("response"), mtcars)
prob
confusion <- table(prob>0.5, mtcars$vs)
confusion
Accuracy <- sum(diag(confusion) / sum(confusion))
Accuracy
summary(mtcars)
n <- nrow(mtcars)
n
n1 <- floor(n*(0.7))
n1
n2 <- n-n1
n2
train <- sample(1:n, n1)
train
ptrain <- glm(vs~factor(am)+mpg+cyl+disp+hp+drat+wt+qsec+gear+carb, family = binomial, data = mtcars[train,])
ptrain
summary(ptrain)
p2train <- predict(ptrain, newdata = mtcars[-train,], type = "response")
p2train
confusion1 <- table(p2train>0.5, mtcars$vs[-train])
confusion1
Accuracy1 <- sum(diag(confusion1))/ sum(confusion1)
Accuracy1

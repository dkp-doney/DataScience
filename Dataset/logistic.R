attach(claimants)
logit <- glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS, family = binomial, data = claimants)
summary(logit)

exp(coef(logit))

prob <- predict(logit, type = c("response"), claimants)
prob

confusion <- table(prob>0.5, claimants$ATTORNEY)
confusion

Accuracy <- sum(diag(confusion) / sum(confusion))
Accuracy
  

summary(claimants)
set.seed(1)
n <- nrow(claimants)
n

n1 <- floor(n*(0.7))
n1

n2 <- n-n1

train <- sample(1:n, n1)
train

mtrain <- glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS, family = binomial, data = claimants[train,])
summary(mtrain)

ptrain <- predict(mtrain, newdata = claimants[-train,], type = "response")
ptrain

confusion1 <- table(ptrain>0.5, claimants$ATTORNEY[-train])
confusion1

Accuracy1 <- sum(diag(confusion1))/ sum(confusion1)
Accuracy1
pairs(Cars)


#Hierarchical Clusterig
summary(Universities)
data <- scale(Universities[,2:7])
d <- dist(data, method = "euclidean")

fit <- hclust(d, method = "complete")
fit

plot(fit)
plot(fit, hang = -1)

groups <- cutree(fit, k=3)
groups

rect.hclust(fit, k=3, border = "red")



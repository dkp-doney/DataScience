x <- runif(50)
y <- runif(50)
data <- cbind(x,y)
data

plot(data)
plot(data, type = "n")
text(data, rownames(data))

km <- kmeans(data,5)
str(km)
f
km <- kmeans.ani(data, 5)
km$centers

x <- runif(50)
y <- runif(50)
x
y
data <- cbind(x,y)
data

plot(data)
plot(data, type = "n")
text(data, rownames(data))

km <- kmeans(data,5)
str(km)
km <- kmeans.ani(data, 5)
km$centers

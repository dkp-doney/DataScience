attach(Universities)
data<-scale(Universities[,2:7])
data
plot(data)
plot(data, type = "n")
text(data, rownames(data))

km <- kmeans(data,5)
str(km)
km <- kmeans.ani(data, 5)

km$centers



#multiple linear regression
pairs(wine)
attach(wine)
m1=lm(log(Wine) ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine)
summary(m1)
m3=lm(Wine ~log(Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline),data=wine)
summary(m3)

m=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine)
summary(m)
influenceIndexPlot(m)

m4=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine[-c(122,74,44),])
summary(m4)

influenceIndexPlot(m4)
m5=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine[-c(122,74,44,70,93,59,108,127),])
summary(m5)
influenceIndexPlot(m5)

m6=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine[-c(122,74,44,70,93,59,108,127,58,91,105),])
summary(m6)
influenceIndexPlot(m6)

m7=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine[-c(122,74,44,70,93,59,108,127,58,91,105,57,89,92,102),])
summary(m7)
influenceIndexPlot(m7)


m8=lm(Wine ~Alcohol+Malic.acid+Ash+Acl+Mg+Phenols+Flavanoids+Nonflavanoid.phenols+Proanth+Color.int+Hue+OD+Proline,data=wine[-c(122,74,44,70,93,59,108,127,58,91,105,57,89,92,102,56,86,98,79,81,55,84,95,90,78,80,107,76,78,22),])

summary(m8)
influenceIndexPlot(m8)
p1=predict(m7,data.frame(Alcohol=13.86,Malic.acid=1.35,Ash=2.27,Acl=16,Mg=98,Phenols=2.98,Flavanoids=3.15,Nonflavanoid.phenols=.22,Proanth=1.85,Color.int=7.22,Hue=1.010,OD=3.55,Proline=1045),interval = "pre")
p1

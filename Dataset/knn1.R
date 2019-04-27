
attach(wbcd)
dim(wbcd)
wbcd=wbcd[,-1] 
dim(wbcd)
table(wbcd$diagnosis)


round(prop.table(table(wbcd$diagnosis))*100,digits = 1)
prop.table(table(wbcd$diagnosis))*100


normalize_data=function(x){
  return((x-min(x))/(max(x)-min(x)))}
wbcd_n=as.data.frame(lapply(wbcd[,2:31],normalize_data))


wbcd_train=wbcd_n[1:469,]
wbcd_test=wbcd_n[470:569,]

wbcd_knn=knn(wbcd_train,wbcd_test,c1=wbcd$diagnosis[1:469],k=15) 

class(wbcd_knn)


conf=table(wbcd_knn,wbcd$diagnosis[470:569])


acc=sum(diag(conf)/sum(conf))
acc
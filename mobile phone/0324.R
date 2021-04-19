### peak

x<- seq(0, 1, len = 1024)
pos<- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.65, 0.76, 0.78, 0.81)
hgt<- c(4, 5, 4, 5, 6, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
wdt<-c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)
pSignal<- numeric(length(x))
for (i in seq(along=pos)){
  pSignal<-pSignal+hgt[i]/(1+abs((x-pos[i])/wdt[i]))^4
}
plot(pSignal, type="l", col="navy")

library(pracma)
findpeaks(pSignal, npeaks = 3, threshold = 3, sortstr = TRUE)
x<- findpeaks(pSignal, npeaks = 3, threshold = 4, sortstr = TRUE)
points(x[,2], x[,1], pch=20, col='maroon')

mean(diff(sort(x[,2])))
std(diff(sort(x[,2])))

mag<- function(df, column){
  df[,str_c("mag", column)]<- with(df, sqrt(get(str_c(column, ".x"))^2+get(str_c(column,".y"))^2+get(str_c(column, ".z"))^2))
  return(df)
}
library(dplyr)
library(stringr)
Peak_rslt<-data.frame()
a<- mag(a, "rotationRate")


for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)}


for(d in fls){
  f<-get(d)

  p<-findpeaks(f$magrotationRate,threshold = 4)
  Peak_rslt<-rbind(Peak_rslt, data.frame(d,
  
  f_n=ifelse(!is.null(p),dim(p)[1],0),
  p_interval=ifelse(!is.null(p),ifelse(dim(p)[1]>2,mean(diff(p[,2])),0),0),
  p_interval_dtd=ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(diff(p[,2])),0),0),
  p_mean=ifelse(!is.null(p),mean(p[,1]),0),
  p_max=ifelse(!is.null(p),max(p[,1]),0),
  p_min=ifelse(!is.null(p),min(p[,1]),0),
  p_std=ifelse(!is.null(p),std(p[,1]),0)))}


temp<-get(fls[1])
plot(temp$magrotationRate)

Peak_rslt

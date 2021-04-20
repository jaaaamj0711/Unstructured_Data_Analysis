t<-seq(0,200,by=0.1)

x<-cos(2*pi*t/16)+0.75*sin(2*pi*t/5)
plot(x, type="l")

x.spen<- spectrum(x)
str(x.spen)

spx<-x.spen$freq*1/0.1

spy<-2*x.spen$spec

plot(spy~spx, type='l')


spx[which(spy %in% sort(spy, decreasing = TRUE)[1:2])]

r.spec<- spectrum(`dws_1/sub_1.csv`$magrotation, plot=TRUE, log="no", span=10)

r.spec<- spectrum(`sit_13/sub_1.csv`$magrotation, plot=TRUE)

r.spec<- spectrum(`jog_16/sub_1.csv`$magrotation, plot=TRUE)

r.spec$freq
r.spec$spec

a<-get(fls[1])
spectrum(a$magrotationRate, span =10, log = "no")

spx<-x.spen$freq*1/50

fr<-r.spec$freq*1/50
sp<-2*r.spec$spec


for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)}# d는 객체명 f는 객체야!!!


## 어디가 틀렸을까?
f<-get(fls[1])
r.spec<-spectrum(f$magrotationRate, plot = TRUE)
r.spec_2<-spectrum(f$magrotationRate, log = "no", span=10, plot = TRUE)

fr<-r.spec$freq*50
sp<-r.spec$spec*2

fr2<-r.spec_2$freq*50
sp2<-r.spec_2$spec*2
plot(sp~fr, type='l')
plot(sp2~fr2, type='l')

fr[1:5];sp[1:5]
fr2[1:5];sp2[1:5]


freq_rslt<- data.frame()

for(d in fls){
  f<-get(d)
  r.spec<-spectrum(f$magrotationRate)
  
  fr<-r.spec$freq*50
  sp<-r.spec$spec*2
  
  freq_rslt<-rbind(freq_rslt, as.data.frame(t(c(d = d, fr[1:5],sp[1:5]))))
  
}


str(freq_rslt) # 데이터 타입이 character로 바뀜
freq_rslt[ ,c(2:11)] = lapply(freq_rslt[ ,c(2:11)], as.numeric)

# exp, id, activity 추출
id_f<-function(x){
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x, "\\_"))[1]
  return(cbind(exp_no, id, activity))
}

temp<-data.frame()
for(i in 1:nrow(freq_rslt)){
  temp<-rbind(temp, id_f(freq_rslt$d[i]))
}

freq_rslt<-cbind(freq_rslt, temp)
library(RWeka)
RF<- make_Weka_classifier("weka/classifiers/trees/RandomForest")
Bayes_net<-make_Weka_classifier("wake/classifiers/bayes/BayesNet")

activity_freq<- freq_rslt %>% select(-d, -exp_no, -id)

m<-RF(as.factor(activity)~., data=activity_freq)
e<- evaluate_Weka_classifier(m, numFolds=10, complexity = TRUE, class = TRUE)
e



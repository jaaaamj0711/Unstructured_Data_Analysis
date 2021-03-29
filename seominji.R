## library load

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(RWeka)
library(fBasics)
library(pracma)
library(signal)
library(seewave)
library(e1071)

# 경로 설정
setwd("/Users/seominji/Desktop/Unstruct_DA/A_DeviceMotion_data")
d<-getwd()
fls <-dir(d,recursive = TRUE)

# 객체 생성
for(f in fls){
  a<- file.path(str_c(d,"/",f))
  temp<- read.csv(a)
  assign(f,temp)
}

## 통계치 관련 변수 생성 ##
# 전체 데이터 생성(피크변수와 merge를 안전하게 하기 위해 d(파일 이름)을 추가)
HAR_total<-data.frame()
i<-0
for(f in fls){
  temp<-get(f)
  print(f)
  i<-i+1
  print(i)
  
  HAR_total_d <-rbind(HAR_total,
                      temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                    id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2],
                                    activity=unlist(str_split(f,"\\_"))[1],d=f))
}

# mag 함수 정의
mag<- function(df, column){
  df[,str_c("mag", column)]<- with(df, sqrt(get(str_c(column, ".x"))^2+get(str_c(column,".y"))^2+get(str_c(column, ".z"))^2))
  return(df)
}

# skewness 함수
skewness <- function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}

# skewness 함수 정의
rss<-function(x) rms(x)*(length(x))*0.5

# mag 적용(gravity 추가)
HAR_total_d<- mag(HAR_total_d, "userAcceleration")
HAR_total_d<- mag(HAR_total_d, "rotationRate")
HAR_total_d<-mag(HAR_total_d, "gravity")

# 통계치 구하기("maggravity","attitude.roll","attitude.pitch","attitude.yaw" 추가)
HAR_summary_extend<- HAR_total %>% group_by(id,exp_no,activity,d) %>% 
  summarise_at(.vars=c("maguserAcceleration","magrotationRate", "maggravity","attitude.roll","attitude.pitch","attitude.yaw"), 
               .funs=c(mean, min, max ,sd ,skewness, rms, rss, IQR, e1071::kurtosis))

# 변수별로 range값을 도출해 새로운 변수를 생성
HAR_summary_extend_d$magrotationRate_range<- HAR_summary_extend_d$magrotationRate_fn3 - HAR_summary_extend_d$magrotationRate_fn2
HAR_summary_extend_d$maguserAcceleration_range<- HAR_summary_extend_d$maguserAcceleration_fn3 - HAR_summary_extend_d$maguserAcceleration_fn2
HAR_summary_extend_d$attitude.roll_range<- HAR_summary_extend_d$attitude.roll_fn3 - HAR_summary_extend_d$attitude.roll_fn2
HAR_summary_extend_d$attitude.pitch_range<- HAR_summary_extend_d$attitude.pitch_fn3 - HAR_summary_extend_d$attitude.pitch_fn2
HAR_summary_extend_d$attitude.yaw_range<- HAR_summary_extend_d$attitude.yaw_fn3 - HAR_summary_extend_d$attitude.yaw_fn2
HAR_summary_extend_d$maggravity_range<-HAR_summary_extend_d$maggravity_fn3 - HAR_summary_extend_d$maggravity_fn2

## 피크 관련 변수 생성 ##
Peak_rslt<-data.frame()

for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)}# d는 객체명 f는 객체가 들어간다

# 피크 관련 변수 생성
for(d in fls){
  f<-get(d)
  
  p_ro<-findpeaks(f$magrotationRate,threshold = 4)
  p_ac<-findpeaks(f$maguserAcceleration,threshold = 1)
  Peak_rslt<-rbind(Peak_rslt, data.frame(d,
  p_ro_n=ifelse(!is.null(p_ro),dim(p_ro)[1],0),
  p_ro_interval=ifelse(!is.null(p_ro),ifelse(dim(p_ro)[1]>2,mean(diff(p_ro[,2])),0),0),
  p_ro_interval_std=ifelse(!is.null(p_ro),ifelse(dim(p_ro)[1]>2,std(diff(p_ro[,2])),0),0),
  p_ro_mean=ifelse(!is.null(p_ro),mean(p_ro[,1]),0),
  p_ro_max=ifelse(!is.null(p_ro),max(p_ro[,1]),0),
  p_ro_min=ifelse(!is.null(p_ro),min(p_ro[,1]),0),
  p_ro_range=ifelse(!is.null(p_ro),diff(range(p_ro[,1])),0),
  p_ro_std=ifelse(!is.null(p_ro),std(p_ro[,1]),0),
  p_ro_be_mean=ifelse(!is.null(p_ro),mean(f$magrotationRate[p_ro[,2]-1]),0),
  p_ro_be_max=ifelse(!is.null(p_ro),max(f$magrotationRate[p_ro[,2]-1]),0),
  p_ro_be_min=ifelse(!is.null(p_ro),min(f$magrotationRate[p_ro[,2]-1]),0),
  p_ro_be_std=ifelse(!is.null(p_ro),std(f$magrotationRate[p_ro[,2]-1]),0),
  p_ro_af_mean=ifelse(!is.null(p_ro),mean(f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_af_max=ifelse(!is.null(p_ro),max(f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_af_min=ifelse(!is.null(p_ro),min(f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_af_std=ifelse(!is.null(p_ro),std(f$magrotationRate[p_ro[,2]+1]),0),
  p_ac_n=ifelse(!is.null(p_ac),dim(p_ac)[1],0),
  p_ac_interval=ifelse(!is.null(p_ac),ifelse(dim(p_ac)[1]>2,mean(diff(p_ac[,2])),0),0),
  p_ac_interval_std=ifelse(!is.null(p_ac),ifelse(dim(p_ac)[1]>2,std(diff(p_ac[,2])),0),0),
  p_ac_mean=ifelse(!is.null(p_ac),mean(p_ac[,1]),0),
  p_ac_max=ifelse(!is.null(p_ac),max(p_ac[,1]),0),
  p_ac_min=ifelse(!is.null(p_ac),min(p_ac[,1]),0),
  p_ac_range=ifelse(!is.null(p_ac),diff(range(p_ac[,1])),0),
  p_ac_std=ifelse(!is.null(p_ac),std(p_ac[,1]),0),
  p_ac_be_mean=ifelse(!is.null(p_ac),mean(f$maguserAcceleration[p_ac[,2]-1]),0),
  p_ac_be_max=ifelse(!is.null(p_ac),max(f$maguserAcceleration[p_ac[,2]-1]),0),
  p_ac_be_min=ifelse(!is.null(p_ac),min(f$maguserAcceleration[p_ac[,2]-1]),0),
  p_ac_be_std=ifelse(!is.null(p_ac),std(f$maguserAcceleration[p_ac[,2]-1]),0),
  p_ac_af_mean=ifelse(!is.null(p_ac),mean(f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_af_max=ifelse(!is.null(p_ac),max(f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_af_min=ifelse(!is.null(p_ac),min(f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_af_std=ifelse(!is.null(p_ac),std(f$maguserAcceleration[p_ac[,2]+1]),0)
                                         
  ))}


temp<- data.frame()
for(d in fls){
  f<-get(d)
  f<-f %>% select(magrotationRate, maguserAcceleration)
  cfR<- crest(f$magrotationRate, 50, plot=TRUE)
  cfA<- crest(f$maguserAcceleration, 50, plot=TRUE)
  temp<- rbind(temp, data.frame(d, cfR=cfR$C, cfA=cfA$C))
}

Peak_final<- merge(Peak_rslt, temp, by="d")

id_f<-function(x){
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x, "\\_"))[1]
  return(cbind(exp_no, id, activity))
}

temp<-data.frame()
for(i in 1:nrow(Peak_final)){
  temp<-rbind(temp, id_f(Peak_final$d[i]))
}

Peak_final2<-cbind(Peak_final,temp)

# 널값 확인 후 0으로 변경
colSums(is.na(final2))
final2[is.na(final2)] <- 0

## 통계 변수 & 피크 변수 합치기 ##
static<- HAR_summary_extend_d %>% ungroup() %>% select(-exp_no,-id,-activity) # peak와 동일하여 제거
peak<-Peak_final2 %>% ungroup() %>% select(-exp_no,-id) 
final<- merge(static, peak, by="d") # d(파일명)을 기준으로 merge
final2<-final %>% ungroup() %>% select(-d)

## 모델 학습(randomForest 사용)
# 교차 검증을 10번 진행
k_fold<- createFolds(final2$activity, k=10, list=TRUE, returnTrain = FALSE)     
set.seed(5123512)
for(i in 1:length(k_fold)) {
  valid_index <- k_fold[[i]]
  
  # K-fold의 test 
  valid_set <- final2[valid_index,]
  
  # K-fold의 train 
  train_set <- final2[-valid_index,]
  
  # Decision Tree 모델 생성
  rf_model <- randomForest(as.factor(train_set$activity) ~ ., data = train_set, type="response")
  
  # predict 
  rf_pred <- predict(rf_model, newdata = valid_set)
  
  # fold별 모델 객체 생성
  assign(paste0("fold_",i),rf_model)
  # fold별 정확도 객체 생성
  assign(paste0("accuracy_",i),sum(rpart_p == valid_set$activity) / NROW(valid_set$activity))
  
}


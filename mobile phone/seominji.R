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
library(caret)
library(xgboost)
library(changepoint)

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

#### 통계치 관련 변수 ####

# 전체 데이터 생성(피크변수와 merge를 안전하게 하기 위해 d(파일 이름)을 추가)
HAR_total<-data.frame()
i<-0
for(f in fls){
  temp<-get(f)
  print(f)
  i<-i+1
  print(i)
  
  HAR_total <-rbind(HAR_total,
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

# rss 함수 정의
rss<-function(x) rms(x)*(length(x))*0.5

# range 함수 정의
range_ <- function(x){
  (diff(range(x)))
}

# mag 적용(gravity 추가)
HAR_total<- mag(HAR_total, "userAcceleration")
HAR_total<- mag(HAR_total, "rotationRate")
HAR_total<-mag(HAR_total, "gravity")

## 변수 추출 ##
# 통계 특징 구하기("maggravity","attitude.roll","attitude.pitch","attitude.yaw" 추가)
HAR_summary_extend<- HAR_total %>% group_by(id,exp_no,activity,d) %>% 
  summarise_at(.vars=c("gravity.x","gravity.y","gravity.z",
                       "userAcceleration.x","userAcceleration.y","userAcceleration.z",
                       "rotationRate.x","rotationRate.y","rotationRate.z",
                       "maguserAcceleration","magrotationRate", 
                       "maggravity","attitude.roll","attitude.pitch","attitude.yaw"), 
               .funs=c(mean, min, max ,sd ,skewness, rms, rss, IQR, e1071::kurtosis, range_))

# 널값 확인 
colSums(is.na(HAR_summary_extend))

## 통계 특징 변수만 학습 ##
# x,y 구분
x = HAR_summary_extend %>% ungroup %>% select(-d, -exp_no, -id, -activity) %>% data.matrix
y = HAR_summary_extend$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
Static_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                    nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                    objective = 'multi:softprob', eval_metric = 'mlogloss', 
                    verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = Static_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
Static_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
Static_conf

# 중요 변수 추출
Static_model = xgboost(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                     nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                     objective = 'multi:softprob', eval_metric = 'mlogloss', 
                     verbose = F)

Static_imp<- xgb.importance(model = Static_model)


#### ####

#### 피크 관련 변수 ####
Peak_rslt<-data.frame()

for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)}# d는 객체명 f는 객체가 들어간다

## 변수 추출 ##
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
  p_ro_moment_mean=ifelse(!is.null(p_ro),mean(p_ro[,1] - f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_moment_max=ifelse(!is.null(p_ro),max(p_ro[,1] - f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_moment_min=ifelse(!is.null(p_ro),min(p_ro[,1] - f$magrotationRate[p_ro[,2]+1]),0),
  p_ro_moment_std=ifelse(!is.null(p_ro),std(p_ro[,1] - f$magrotationRate[p_ro[,2]+1]),0),
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
  p_ac_af_std=ifelse(!is.null(p_ac),std(f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_moment_mean=ifelse(!is.null(p_ac),mean(p_ac[,1] - f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_moment_max=ifelse(!is.null(p_ac),max(p_ac[,1] - f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_moment_min=ifelse(!is.null(p_ac),min(p_ac[,1] - f$maguserAcceleration[p_ac[,2]+1]),0),
  p_ac_moment_std=ifelse(!is.null(p_ac),std(p_ac[,1] - f$maguserAcceleration[p_ac[,2]+1]),0)
                                         
  ))}

# 파고율 변수
temp<- data.frame()
for(d in fls){
  f<-get(d)
  f<-f %>% select(magrotationRate, maguserAcceleration)
  cfR<- crest(f$magrotationRate, 50, plot=TRUE)
  cfA<- crest(f$maguserAcceleration, 50, plot=TRUE)
  temp<- rbind(temp, data.frame(d, cfR=cfR$C, cfA=cfA$C))
}

Peak_final<- merge(Peak_rslt, temp, by="d")

# exp, id, activity 추출
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

## 피크 특징 변수만 학습 ##
# x,y 구분
x = Peak_final2 %>% ungroup %>% select(-d, -exp_no, -id, -activity) %>% data.matrix
y = Peak_final2$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)

Peak_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                    nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                    objective = 'multi:softprob', eval_metric = 'mlogloss', 
                    verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = Peak_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
Peak_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
Peak_conf

# 중요 변수 추출
Peak_model = xgboost(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                       nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                       objective = 'multi:softprob', eval_metric = 'mlogloss', 
                       verbose = F)

Peak_imp<- xgb.importance(model = Peak_model)


#### ####

####고속 푸리에 변환 (Fast Fourier transform) 적용 변수 ####
fft_data <- data.frame()
for(f in fls){
  temp<-get(f)
  
  temp<- as.data.frame(lapply(temp, fft)) # fft 적용
  temp<- as.data.frame(lapply(temp, Re)) # 앞에 실수부분만 가져옴
  
  fft_data <-rbind(fft_data,
                   temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                 id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2],
                                 activity=unlist(str_split(f,"\\_"))[1],d=f))
}

fft_data<- mag(fft_data, "userAcceleration")
fft_data<- mag(fft_data, "rotationRate")
fft_data<-mag(fft_data, "gravity")
colnames(fft_data)

## 변수 추출 ##
fft_data_summary<- fft_data %>% group_by(id,exp_no,activity,d) %>% 
  summarise_at(.vars=c("gravity.x","gravity.y","gravity.z",
                       "userAcceleration.x","userAcceleration.y","userAcceleration.z",
                       "rotationRate.x","rotationRate.y","rotationRate.z",
                       "maguserAcceleration","magrotationRate", 
                       "maggravity","attitude.roll","attitude.pitch","attitude.yaw"), 
               .funs=c(mean, min, max ,sd ,skewness, rms, rss, IQR, e1071::kurtosis, range_))

# 변수명 변경
names(fft_data_summary)<- str_replace_all(colnames(fft_data_summary),'fn',"fft")

## FFT 변환 변수만 학습 ##
# x,y 구분
x = fft_data_summary %>% ungroup %>% select(-d, -exp_no, -id, -activity) %>% data.matrix
y = fft_data_summary$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
fft_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                   nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                   objective = 'multi:softprob', eval_metric = 'mlogloss', 
                   verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = fft_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
Fourier_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
Fourier_conf

# 중요 변수 추출
Fourier_model = xgboost(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                     nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                     objective = 'multi:softprob', eval_metric = 'mlogloss', 
                     verbose = F)

Fourier_imp<- xgb.importance(model = Fourier_model)

#### ####

#### 변화 분석 변수 ####
## 변수 추출
ch_pt<-data.frame()

for(d in fls){
  f<-get(d)
  f<-mag(f, "rotationRate")
  f<-mag(f, "userAcceleration")
  f<-mag(f, "gravity")# method 값을 PELT 방식으로 바꿈(기본값은 AMOC)
  rslt<-sapply(f %>% select(magrotationRate, maguserAcceleration, maggravity), cpt.mean, method = "PELT")
  rslt_cpts1<-cpts(rslt$magrotationRate)
  # 변화시점에 해당하는 값을 가져와 통계 특징을 추출
  cp1_mean<- ifelse(length(rslt_cpts1) != 0, mean(f$magrotationRate[rslt_cpts1]),0)
  cp1_max<- ifelse(length(rslt_cpts1) != 0, max(f$magrotationRate[rslt_cpts1]),0)
  cp1_min<- ifelse(length(rslt_cpts1) != 0, min(f$magrotationRate[rslt_cpts1]),0)
  cp1_std<- ifelse(length(rslt_cpts1) != 0, std(f$magrotationRate[rslt_cpts1]),0)
  rslt_cpts2<-cpts(rslt$maguserAcceleration)
  cp2_mean<- ifelse(length(rslt_cpts2) != 0, mean(f$maguserAcceleration[rslt_cpts2]),0)
  cp2_max<- ifelse(length(rslt_cpts2) != 0, max(f$maguserAcceleration[rslt_cpts2]),0)
  cp2_min<- ifelse(length(rslt_cpts2) != 0, min(f$maguserAcceleration[rslt_cpts2]),0)
  cp2_std<- ifelse(length(rslt_cpts2) != 0, std(f$maguserAcceleration[rslt_cpts2]),0)
  rslt2<-sapply(f %>% select(magrotationRate, maguserAcceleration), cpt.var, method = "PELT")
  rslt2_cpts1<-cpts(rslt2$magrotationRate)
  cp3_mean<- ifelse(length(rslt2_cpts1) != 0, mean(f$magrotationRate[rslt2_cpts1]),0)
  cp3_max<- ifelse(length(rslt2_cpts1) != 0, max(f$magrotationRate[rslt2_cpts1]),0)
  cp3_min<- ifelse(length(rslt2_cpts1) != 0, min(f$magrotationRate[rslt2_cpts1]),0)
  cp3_std<- ifelse(length(rslt2_cpts1) != 0, std(f$magrotationRate[rslt2_cpts1]),0)
  rslt2_cpts2<-cpts(rslt2$maguserAcceleration)
  cp4_mean<- ifelse(length(rslt2_cpts2) != 0, mean(f$maguserAcceleration[rslt2_cpts2]),0)
  cp4_max<- ifelse(length(rslt2_cpts2) != 0, max(f$maguserAcceleration[rslt2_cpts2]),0)
  cp4_min<- ifelse(length(rslt2_cpts2) != 0, min(f$maguserAcceleration[rslt2_cpts2]),0)
  cp4_std<- ifelse(length(rslt2_cpts2) != 0, std(f$maguserAcceleration[rslt2_cpts2]),0)
  rslt3<-sapply(f %>% select(magrotationRate, maguserAcceleration), cpt.meanvar, method = "PELT")
  rslt3_cpts1<-cpts(rslt3$magrotationRate)
  cp5_mean<- ifelse(length(rslt3_cpts1) != 0, mean(f$magrotationRate[rslt3_cpts1]),0)
  cp5_max<- ifelse(length(rslt3_cpts1) != 0, max(f$magrotationRate[rslt3_cpts1]),0)
  cp5_min<- ifelse(length(rslt3_cpts1) != 0, min(f$magrotationRate[rslt3_cpts1]),0)
  cp5_std<- ifelse(length(rslt3_cpts1) != 0, std(f$magrotationRate[rslt3_cpts1]),0)
  rslt3_cpts2<-cpts(rslt3$maguserAcceleration)
  cp6_mean<- ifelse(length(rslt3_cpts2) != 0, mean(f$maguserAcceleration[rslt3_cpts2]),0)
  cp6_max<- ifelse(length(rslt3_cpts2) != 0, max(f$maguserAcceleration[rslt3_cpts2]),0)
  cp6_min<- ifelse(length(rslt3_cpts2) != 0, min(f$maguserAcceleration[rslt3_cpts2]),0)
  cp6_std<- ifelse(length(rslt3_cpts2) != 0, std(f$maguserAcceleration[rslt3_cpts2]),0)
  
  ch_pt<-rbind(ch_pt, data.frame(d, cp1=length(rslt_cpts1),cp2=length(rslt_cpts2),
                                 cp3=length(rslt2_cpts1), cp4=length(rslt2_cpts2),
                                 cp5=length(rslt3_cpts1), cp6=length(rslt3_cpts2),
                                 cp1_mean, cp1_max, cp1_min, cp1_std,
                                 cp2_mean, cp2_max, cp2_min, cp2_std,
                                 cp3_mean, cp3_max, cp3_min, cp3_std,
                                 cp4_mean, cp4_max, cp4_min, cp4_std,
                                 cp5_mean, cp5_max, cp5_min, cp5_std,
                                 cp6_mean, cp6_max, cp6_min, cp6_std))
}


for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)}# d는 객체명 f는 객체야!!!

# exp, id, activity 추출
id_f<-function(x){
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x, "\\_"))[1]
  return(cbind(exp_no, id, activity))
}

temp<-data.frame()
for(i in 1:nrow(ch_pt)){
  temp<-rbind(temp, id_f(ch_pt$d[i]))
}

ch_pt2<-cbind(ch_pt, temp)

# 널값 확인 후 0으로 변경
colSums(is.na(ch_pt2))
ch_pt2[is.na(ch_pt2)] <- 0

## 변화 변수만 학습 ##
# x,y 구분
x = ch_pt2 %>% ungroup %>% select(-d, -exp_no, -id, -activity) %>% data.matrix
y = ch_pt2$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
ch_pt_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                     nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                     objective = 'multi:softprob', eval_metric = 'mlogloss', 
                     verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = ch_pt_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
Chpoint_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
Chpoint_conf

# 중요 변수 추출
Chpoint_model = xgboost(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                        nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                        objective = 'multi:softprob', eval_metric = 'mlogloss', 
                        verbose = F)

Chpoint_imp<- xgb.importance(model = Chpoint_model)

### 모든 변수를 합하여 학습 진행 ####
Static<- HAR_summary_extend %>% ungroup() %>% select(-exp_no,-id,-activity)
Fourier<- fft_data_summary %>% ungroup() %>% select(-exp_no,-id,-activity)
Peak<-Peak_final2 %>% ungroup() %>% select(-exp_no,-id,-activity) 
Chpoint<- ch_pt2 %>% ungroup() %>% select(-exp_no,-id) 
final<- merge(Static, Fourier, by="d")
final<- merge(final, Peak, by="d")
final<- merge(final, Chpoint, by="d")
final2<- final %>% ungroup() %>% select(-d)

# 널값 확인 후 0으로 변경
colSums(is.na(final2))
final2[is.na(final2)] <- 0

# x,y 구분
x = final2 %>% ungroup %>% select(-activity) %>% data.matrix
y = final2$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
all_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                     nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                     objective = 'multi:softprob', eval_metric = 'mlogloss', 
                     verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = all_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
all_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
all_conf

# accuracy 확인
cat("통계 특징 accuracy는", round(Static_conf$overall[[1]],2),"입니다.")
cat("피크 특징 accuracy는", round(Peak_conf$overall[[1]],2),"입니다.")
cat("푸리에 변환 특징 accuracy는", round(Fourier_conf$overall[[1]],2),"입니다.")
cat("변화 특징 accuracy는", round(Chpoint_conf$overall[[1]],2),"입니다.")
cat("총 accuracy는", round(all_conf$overall[[1]],2),"입니다.")

## 가장 높은 정확도를 나타낸 통계특징과 푸리에 변환 특징 변수들만 사용하여 예측 ##
Static<- HAR_summary_extend %>% ungroup() %>% select(-exp_no,-id,-activity)
Fourier<- fft_data_summary %>% ungroup() %>% select(-exp_no,-id)
Sta_Fo<- merge(Static, Fourier, by = 'd')
Sta_Fo<- Sta_Fo %>% ungroup() %>% select(-d)

# 널값 확인 후 0으로 변경
colSums(is.na(Sta_Fo))
Sta_Fo[is.na(Sta_Fo)] <- 0

# x,y 구분
x = Sta_Fo %>% ungroup %>% select(-activity) %>% data.matrix
y = Sta_Fo$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
Sta_Fo_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                   nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                   objective = 'multi:softprob', eval_metric = 'mlogloss', 
                   verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = Sta_Fo_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
Sta_Fo_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
Sta_Fo_conf

## 각 특징별로 상위 변수들만 추출하여 예측 ##
xgb.ggplot.importance(Static_imp[1:30])
xgb.ggplot.importance(Peak_imp[1:30])
xgb.ggplot.importance(Fourier_imp[1:30])
xgb.ggplot.importance(Chpoint_imp[1:29])

Staitc_imp_df<- cbind(d = HAR_summary_extend$d,HAR_summary_extend[,Static_imp[1:30]$Feature])
Peak_imp_df<- cbind(d = Peak_final2$d, Peak_final2[,Peak_imp[1:30]$Feature])
Fourier_imp_df<- cbind(d = fft_data_summary$d,fft_data_summary[,Fourier_imp[1:30]$Feature])
Chpoint_imp_df<- cbind(d = ch_pt2$d, activity = ch_pt2$activity, ch_pt2[,Chpoint_imp[1:29]$Feature])

SP_df<- merge(Staitc_imp_df, Peak_imp_df, by = 'd')
SPF_df<- merge(SP_df, Fourier_imp_df, by = 'd')
import_df<-  merge(SPF_df, Chpoint_imp_df, by = 'd')


# x,y 구분
x = import_df %>% ungroup %>% select(-d, -activity) %>% data.matrix
y = import_df$activity

# xgboost 모델 학습(10-fold)
set.seed(1004)
import_model = xgb.cv(data = x,label = as.integer(as.factor(y))-1, num_class = levels(as.factor(y)) %>% length,
                   nfold = 10, nrounds = 500, early_stopping_rounds = 8, booster = 'gbtree',
                   objective = 'multi:softprob', eval_metric = 'mlogloss', 
                   verbose = F, prediction = T)

# 예측 데이터 생성(max.col 사용해서 가장 큰값 추출)
pred_df = import_model$pred %>% as.data.frame %>% 
  mutate(pred = levels(as.factor(y))[max.col(.)] %>% as.factor, actual = as.factor(y))

pred_df %>% select(pred,actual) %>% table

# confusionMatrix 생성
import_conf<- caret::confusionMatrix(pred_df$pred, pred_df$actual)
import_conf

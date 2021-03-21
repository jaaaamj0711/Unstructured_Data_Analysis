## library load

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(RWeka)
library(pracma)
library(signal)
library(seewave)
library(e1071)
library(e1071)
install.packages("e1071")
libraryy()

#### 핸드폰 센서데이터 분석
setwd("/Users/seominji/Desktop/Unstruct_DA")
d<-getwd()
d

fls <-dir(d,recursive = TRUE)
fls

library(stringr)

# 폴더에 있는 파일을 불러오는데 객체로 만들거댜
for(f in fls){
  a<- file.path(str_c(d,"/",f))
  temp<- read.csv(a)
  assign(f,temp)
}

summary(`wlk_15/sub_1.csv`)

# 가속도 크기를 구해보자

mag<- function(df, column){
  df[,str_c("mag", column)]<- with(df, sqrt(get(str_c(column, ".x"))^2+get(str_c(column,".y"))^2+get(str_c(column, ".z"))^2))
  return(df)
}

mag(`wlk_15/sub_1.csv`, "userAcceleration")


## sample data select
fls
# user1에서 walking 데이터만 추출
user1<- fls[str_detect(fls,"sub_1.csv")]
user1_walking<-user1[str_detect(user1,"wlk")]

# user1_walking 데이터 추출 후 합치기
user1_walking_total<- data.frame()

for(f in user1_walking){
  temp<-get(f) # 이름말고 내용을 가져와라
  
  user1_walking_total <-rbind(user1_walking_total,
                              temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                            id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2]))
}

# mag 적용
user1_walking_total<-mag(user1_walking_total, "userAcceleration")

# 샘플 데이터 시각화
user1_walking_total<- user1_walking_total%>% group_by(exp_no) %>% mutate(time=row_number()) %>% ungroup()



ggplot(user1_walking_total,aes(x=time,y=maguserAcceleration))+geom_line()+facet_wrap(.~exp_no,nrow=3)

save.image("day1.RData")
load("day1.RData")

## 전체 데이터 병합
HAR_total<- data.frame()
for(f in fls){
  temp<-get(f) # 이름말고 내용을 가져와라
  
  HAR_total <-rbind(HAR_total,
          temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
            id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2],
              activity=unlist(str_split(f, "\\_"))[1]))
}

head(HAR_total,  n=100)

# mag 적용
HAR_total<- mag(HAR_total, "userAcceleration")
HAR_total<- mag(HAR_total, "rotationRate")

# skewness 함수
skewness <- function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}


HAR_summary_extend<- HAR_total %>% group_by(id, exp_no, activity) %>% summarise_at(.vars=c("maguserAcceleration","magrotationRate"),
                                                                                   .funs=c(mean, min, max, sd, skewness, rms,IQR, e1071::kurtosis))
install.packages("fBasics")
library(fBasics)                                                      
                                                                                   
HAR_summary_extend2<- HAR_summary_extend %>% ungroup() %>% select(-c("id","exp_no"))

RF<- make_Weka_classifier("weka/classifiers/trees/RandomForest")
Bayes_net<-make_Weka_classifier("wake/classifiers/bayes/BayesNet")

m_extend<- J48(as.factor(activity)~., data=HAR_summary_extend2)
e_extend<- evaluate_Weka_classifier(m_extend, numFolds=10, complexity = TRUE, class = TRUE)
e_extend


## pca 적용
mtcars.pca<- prcomp(HAR_summary_extend2 %>% ungroup() %>% select(-activity), center=TRUE, scale. =TRUE)
mtcars.pca

m_pca<- RF(as.factor(activity)~., data=HAR_summary_extend2 %>% select(1,2,11,12,15,16))
e_pca<- evaluate_Weka_classifier(m_pca, numFolds = 10, complexity = TRUE, class = TRUE)
e_pca

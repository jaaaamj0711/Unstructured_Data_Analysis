# MotionSense Data Analysis

### 데이터 설명 
- 다음 데이터는 가속도계 및 자이로 스코프 센서(attitude, gravity, userAcceleration 및 rotationRate)에서 생성 된 시계열 데이터로 구성되어 있습니다.
- 성별, 나이, 몸무게, 키 등 총 24 명의 참가자가 동일한 환경과 조건에서 아래층, 위층, 걷기, 조깅, 앉기, 서기 등의 활동을 하였습니다.

### 분석 목표
- 원본 신호 데이터를 바탕으로 다양한 특징을 추출하여 유의미한 변수를 생성합니다.
- 참가자의 행동을 잘 분리하는 모델을 생성합니다.

### 분석 방법

#### 변수 추출
- 통계 특징 변수 추출: 원본 데이터에서 통계적 특징을 활용하여 변수를 추출합니다.
- 피크 특징 변수 추출: 원본 데이터셋에서 피크 특징을 활용하여 변수를 추출합니다.
- 변화 분석 변수 추출: 원본 데이터에서 변화 시점에 관한 특징을 활용하여 변수를 추출합니다.
- 푸리에 변환 적용 변수 추출: 원본 데이터에서 변화 시점에 관한 특징을 활용하여 변수를 추출합니다.

#### 모델링
- XGB boost 사용
- 10-fold cross validation을 사용합니다.


### 샤이니 앱 제작

- 분석된 결과를 가지고 R에서 제공하는 shiny 패키지를 활용하여 웹사이트를 구축합니다.

<img width="1427" alt="스크린샷 2021-06-04 오후 9 31 06" src="https://user-images.githubusercontent.com/55734436/120801507-37570b00-c57c-11eb-8f8c-ec8510e04827.png">

다음은 화면 예시입니다.

[사이트 링크](https://jaaaamj0711.shinyapps.io/shiny/)


### 분석 참고 파일

[- 변수 설명](https://github.com/jaaaamj0711/Unstructured_Data_Analysis/blob/main/%EB%B3%80%EC%88%98%20%EC%84%A4%EB%AA%85.pdf)  

[- 분석 코드](https://github.com/jaaaamj0711/Unstructured_Data_Analysis/blob/main/code_result.R)  

[- 샤이니 앱 코드](https://github.com/jaaaamj0711/Unstructured_Data_Analysis/blob/main/app.R)


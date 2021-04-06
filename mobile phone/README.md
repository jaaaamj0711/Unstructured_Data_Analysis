
## 데이터 소개 & 분석 목적

휴대폰 센서 데이터입니다. 실험자들을 해당 기기를 장착하고 일상생활을 평소처럼 하며 데이터를 축적하였습니다. 그렇게 쌓인 데이터로 아래와 같이 5개의 행동을 분류하는 것이 목표입니다.

- 아래층 오르기
- 위층 오르기
- 앉기
- 서기
- 조깅
 
#### 데이터 링크: https://github.com/mmalekzadeh/motion-sense

## 분석 진행

### 변수 추출

#### 1.통계 특징과 관련된 변수 추출

:기존에는 roationRate와 userAcceleraion 2개의 변수만 사용해서 통계 특징을 추출하였습니다. 

1. “gravity” 와 “attitude” 관련 변수들에 대한 통계 특징도 추출하였습니다.
* (“gravity” 변수는 mag 함수를 적용하여 통계 특징을 추출하였고, “attitude” 변수는 기존 값에서 통계 특징을 추출하였습니다.)

2. (mean, min, max, sd, skewness, rms, rss, IQR, kurtosis) + range 통계 특징을 추가 하였습다.
* (range는 최대값과 최솟값의 차이로 dispersion 관련 통계 특징입니다.)

![image](https://user-images.githubusercontent.com/55734436/113665331-41799c00-96e8-11eb-9329-ec0e57761e34.png)

#### 2. 피크와 관련된 변수 추출

1. 기존에는 피크 크기에서 (min, max, min, std) 통계 특징을 사용하였습니다. 여기에 range 통계 특징을 추가하였습니다.
2. 기존에는 magrotationRate 변수의 피크만 가지고 변수를 도출하였습니다. 이 부분을 똑같이 maguserAcceleration 에도 적용을 하였습니다.

![image](https://user-images.githubusercontent.com/55734436/113665398-60782e00-96e8-11eb-8018-dcb292d918ad.png)

maguserAcceleration의 그래프를 그려본 후 피크 기준값을 1로 사용하기로 결정 하였습니다.

![image](https://user-images.githubusercontent.com/55734436/113665424-6968ff80-96e8-11eb-8a6b-0c0778573b35.png)

다음과 같이 예제로 피크를 추출하고, 피크, 피크 발생 전, 피크 발생 후에 관한 그래프를 그려보았습니다.

![image](https://user-images.githubusercontent.com/55734436/113665826-20657b00-96e9-11eb-8954-bdb5c7b4f58b.png)

3. 피크가 발생하기 전과 후의 값도 의미가 있을 것이라고 생각하여 피크가 발생하기 바로 이전 값과 이후 값을 구하여 (min, max, min, std) 통계 특징을 구한 변수를 추가하였습니다.
4. 순간적인 변화를 알기 위해 피크가 발생하는 순간과 그 다음 순간의 차이를 구하여 (min, max, min, std) 통계 특징을 구한 변수를 추가하였습니다.

![image](https://user-images.githubusercontent.com/55734436/113665869-3410e180-96e9-11eb-97c4-bbf504f9bca4.png)


#### 3.고속 푸리 변환 적용 후 통계 특징 변수 추출

fft 함수로 푸리에 변환을 적용해 데이터를 변환하였습니다. 변환 후 실수 부분만 가져와서 통계 특징을 추출하였습니다.

![image](https://user-images.githubusercontent.com/55734436/113665545-9a493480-96e8-11eb-97ba-615548ed1261.png)


#### 4.변화 분석과 관련된 변수 추출

:기존에는 cpt.mean, cpt.var, cpt.meanvar에서 변화 분석을 진행할 때 method 파라미터를 기본값인 AMOC(single changepoint)를 사용하였습니다. 

(1)	method 방식 중 PELT(multiple changepoints) 방법이 좀 더 특징을 잘 추출하는 것으로 나타나 method를 변경하여 변화 분석을 진행하였습니다. 

:기존에는 변화가 몇 번 일어났는지에 관한 변수만 사용하였습니다.

(2)	변화가 일어났던 위치의 값을 가져와서 (mean, max, min, std) 통계 특징을 추출한 변수를 추가하였습니다. 

![image](https://user-images.githubusercontent.com/55734436/113666519-58b98900-96ea-11eb-9996-53c5ef58c6dd.png)



## 데이터 전처리


## 데이터 소개 & 분석 목적

휴대폰 센서 데이터입니다. 실험자들을 해당 기기를 장착하고 일상생활을 평소처럼 하며 데이터를 축적하였습니다. 그렇게 쌓인 데이터로 아래와 같이 5개의 행동을 분류하는 것이 목표입니다.

- 아래층 오르기
- 위층 오르기
- 앉기
- 서기
- 조깅
 
#### 데이터 링크: https://github.com/mmalekzadeh/motion-sense

## 변수 추출

### [통계 특징과 관련된 변수 추출]

기존에는 roationRate와 userAcceleraion 2개의 변수만 사용해서 통계 특징을 추출하였다. 

-	“gravity” 와 “attitude” 관련 변수들에 대한 통계 특징도 추출하였다.
* “gravity” 변수는 mag 함수를 적용하여 통계 특징을 추출하였고, “attitude” 변수는 원래값에서 통계 특징을 추출하였다.
-	(mean, min, max ,sd ,skewness, rms, rss, IQR, kurtosis) + range 통계 특징을 추가 하였다.
* range는 최대값과 최솟값의 차이를 구해 범위를 알려주는 함수이다.



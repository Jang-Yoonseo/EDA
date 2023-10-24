library(tidyverse)

#과제 1 
#만들어 놓은 smoothing_3RSSH_twiceit() 함수를 이용하면 편리할 것
library(tidyverse)
library(readxl)
elec_data <- read_excel('전력사용데이터.xlsx')
elec_data <- elec_data[c(-1,-2),] %>% mutate(index = 1:nrow(elec_data[c(-1,-2),]),
                                             시점 = rep(c(2016,2017,2018,2019,2020),each = 12))
colnames(elec_data) <- c('year','month','elec_usage','index')
par(mfrow= c(1,1))
attach(elec_data) #호출의 편리함을 위해 attach 사용
elec_data
plot(elec_usage ~ index , type = 'l') #raw data 
#우리의 목표는 최대한 순수한 신호를 보려는 것이다
#즉 평활을 실시한다.
#4253H,twiceit 평활
elec_4253H <- smoothing_4253H_twiceit(elec_usage)
#3RSSH,twiceit 평활
elec_3rssh <- smoothing_3RSSH_twiceit(elec_usage)
elec_3rssh

elec_data <- elec_data %>% mutate(elec_4253H,elec_3rssh)
elec_data

#시각화를 통해 비교하여보자
ggplot(data= elec_data , mapping = aes(x= index)) +
  geom_point(aes(y= elec_usage)) +
  geom_line(aes(y = elec_usage,color = 'raw_elec_usage'),size = 1,alpha = 0.7) +
  geom_line(aes(y= elec_4253H, color = '4253H twiceit '),size =2,alpha = 0.7) +
  geom_line(aes(y = elec_3rssh,color = '3RSSH twiceit'),size = 3,alpha = 0.7)

#두가지 기법으로 Smoothing 하여본 결과 들쭉 날쭉 한 부분이 약간의 주기성을 갖고 있는 것 처럼 보인다
#그리고 상식적으로 생각건데 특정 계절이 전력 사용량이 많을 것이며
#매 해마다 반복적으로 양상이 보일것으로 전망된다.
month[order(elec_4253H , decreasing = TRUE)] #높은 달들을 확인해보자
#결과로 보아 겨울에 사용량이 많으며 봄에 전력 사용량이 적음을 알 수 있다.

#분해
#시계열을 분해하자.
series <- ts(elec_usage,start=c(2016,1),frequency = 12) #시계열 자료 생성
par(mfrow=c(1,2)) 
plot(series, main = 'Rawdata의 시계열 도표') #시각화
decom.series <- decompose(series) #계절성 추출
plot(decom.series$trend,ylim = c(3900000,4500000),main = '월별 전력 소모 추세선') #추세변화
#전력 사용량 자체의 추세는 갈수록 줄어드는 추세라고 해석할 수 있다.
adj <- series - decom.series$seasonal #계절성 조절 계열에 대한 시도표
adj.series <- ts(adj , start=c(2016,1), frequency = 12)
adj.series %>% plot(ylim = c(3900000,4500000),main = '계절 조정 계열') #다른 월끼리 비교하기 위해 계절성을 제거한 시계열

# ACF 그리고 탐색하여보라 
par(mfrow = c(1,1))
plot(elec_usage, type='l') #자기 상관 그래프를 그리기 전에 원데이터 관찰
acf(elec_usage)
#ACF의 결과상으론 증가하는 추세인데 증가폭이 점점 감소하고, 그러다가 음의 자기 상관을 갖는 양상을 보인다.

library(tidyverse)
library(readxl)

#rice_data <- 
rice_data <- read_excel('대한민국연도별쌀생산량.xlsx')
colnames(rice_data) <- c('year','output')
# 데이터를 살펴보면 관측치가 첫행에 존재하는 것을 알 수 있다.
# 결측치를 제거하여보자
rice_data %>% filter(is.na(output)) #확인 결과 첫 행만 결측치
rice_data <- rice_data %>% filter(!is.na(output)) #결측치가 아닌 데이터들 정제

attach(rice_data) #변수를 호출하기 편하게 attach
ggplot(rice_data,mapping = aes(x= factor(year) , y= output)) +
  geom_point(color = 'black',size = 1.5) +
  geom_line(color = 'red',group = 1) +
  ggtitle("대한민국 연도별 쌍 생산량") + xlab("연도") + ylab("쌀 생산량") +
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.x = element_text(size =11),
        axis.title.y = element_text(size= 11))

#그래프만 보았을 땐 1980년도에 급격하게 쌀 생산량이 증가한 후
#그 이후로는 감소하는 추세지만
#중간에 증감이 반복되는 경향을 보인다

#4253H, twiceit 과 3RSSH,twiceit 으로 평활하여 보자
rice_4253H2 <- smoothing_4253H_twiceit(output)
rice_3RSSH2 <- smoothing_3RSSH_twiceit(output)

#원 자료에 행을 추가하여 보자
rice_data <- rice_data %>% mutate(index= 1:nrow(rice_data),
                     rice_4253H2 = rice_4253H2,
                     rice_3RSSH2 = rice_3RSSH2)
#ggplot을 이용하여 한번에 나타내어 보자
ggplot(data= rice_data , mapping = aes(x= index)) +
  geom_point(aes(y= output)) +
  geom_line(aes(y = output,color = 'raw_elec_usage'),size = 1,alpha = 0.7) +
  geom_line(aes(y= rice_4253H2, color = '4253H twiceit '),size =2,alpha = 0.7) +
  geom_line(aes(y = rice_3RSSH2,color = '3RSSH twiceit'),size = 3,alpha = 0.7)

#평활을 하는 이유는 RawData에서 잡음을 제거한 최대한 순수한 신호를 보기 위함이다
#각각의 기법을 이용하여 신호를 추출한 것이 그래프로 나타난 결과이다.
elec_usage

#분해는 보류하도록 한다.
###############################################3
#Decompose 실행


#시계열을 분해하자.
par(mfrow=c(1,2)) 
series2 <- ts(output,start=c(1980)) #쌀 생산량시계열 자료 생성
plot(series2,main =  '쌀 생산량의 시계열 도표') #시각화
decom.series2 <- decompose(output) #계절성 추출
plot(decom.series2$trend ,main = '연도별 쌀 생산량 추세선') #추세변화
#전력 사용량 자체의 추세는 갈수록 줄어드는 추세라고 해석할 수 있다.
adj2 <- series - decom.series$seasonal #계절성 조절 계열에 대한 시도표
adj.series2 <- ts(adj , start=c(2016,1), frequency = 12)
adj.series2 %>% plot(ylim = c(3900000,4500000),main = '계절 조정 계열') #다른 월끼리 비교하기 위해 계절성을 제거한 시계열
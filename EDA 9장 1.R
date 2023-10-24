# 탐자 과제 
library(tidyverse)

data <-c(569,416,422,565,484,520,573,518,501,505,468,382,310,334,359,372,439,446,349,395,461,511,583,590,620,578,534,631,600,438,516,534,467,457,392,467,500,493,410,412,416,403,422,459,467,512,534,522,545)

#내가 만든 함수를 이용하여 바로 평활을 하여보자
data_4253H <- smoothing_4253H_twiceit(data)
data_3RSSH <- smoothing_3RSSH_twiceit(data)

#겹쳐 그려보자
total_data <- tibble(idx = 1:length(data), data , data_4253H, data_3RSSH)
total_data

ggplot( data = total_data,mapping= aes(x = idx)) + 
  geom_point(mapping = aes(y=data)) +
  geom_line(mapping = aes(y = rawdata,color= 'raw data')) +
  geom_line(mapping = aes(y = data_4253H,color = '4253H_twiceit'),size= 2) +
  geom_line(mapping = aes(y= data_3RSSH, color = '3RSSH_twiceit'),size = 3,alpha= 0.6)

#평활을 하는 이유를 생각해보자면
#raw 데이터는 신호와 잡음을 담고 있는데
#4253H,twiceit 과 3RSSH,twice 라는 두 방법을 통해
#참 신호에 가까운 부분만을 뽑아낸 것이다.

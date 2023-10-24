library(tidyverse)

#함수로 만들자
smoothing_4253H <- function(data){
  N <- length(data)
  
  smthng_4 <- c() #4평활에 해당하는 벡터
  smthng_4[1] = median(c(data[1],data[2]))
  
  
  for ( i in seq(2,N-1)){
    smthng_4[i] = median(c(data[i-1],data[i],data[i+1],data[i+2]))
    if (i == N-1){
      smthng_4[N-1] = median(c(data[N-1],data[N]))
    }
    
  }
  
  smthng_42 <- c()
  smthng_42[1] = data[1]; smthng_42[N] = data[N]
  for ( i in seq(2,N-1)){
    smthng_42[i] = median(c(smthng_4[i-1],smthng_4[i]))
  }
  
  smthng_425 <- c()
  smthng_425[1] <- smthng_42[1]; smthng_425[N] <- smthng_42[N]
  for ( i in 2:(N-1)){
    if(i ==2 | i==(N-1)){
      smthng_425[i] = median(c(smthng_42[i-1],smthng_42[i],smthng_42[i+1]))
    }
    else smthng_425[i] = median(c(smthng_42[i-2],smthng_42[i-1],smthng_42[i],smthng_42[i+1],smthng_42[i+2]))
  }
  
  smthng_4253 <- c()
  smthng_4253[1] <- smthng_425[1] ; smthng_4253[N] <- smthng_425[N]
  for (i in 2:(N-1)){
    smthng_4253[i] = median(c(smthng_425[i-1],smthng_425[i],smthng_425[i+1]))
  }
  
  smthng_4253H <- c()
  smthng_4253H[1] <- smthng_4253[1] ; smthng_4253H[N] <- smthng_4253[N]
  
  for ( i in 2:(N-1)){
    smthng_4253H[i] = 0.25*(smthng_4253[i-1] + smthng_4253[i+1]) + 0.5*(smthng_4253[i])
  }
  
  return(smthng_4253H)
  
}

#위를 바탕으로 함수를 하나 더 생성

smoothing_4253H_twiceit <- function(data){
  sm_4253h <- smoothing_4253H(data)
  r1 <- data - sm_4253h
  twice <- smoothing_4253H(r1)
  
  return( sm_4253h + twice)
}

smoothing_4253H_twiceit(data)

##########################

#3RSSH,twice it 함수
#뒤의 과제들에도 편리하게 적용하기 위해 3RSSH,twiceit을 적용해주는 함수를 만들자
smoothing_3RSSH_twiceit <- function(data){
  smthng_3RSS <- smooth(data, kind = '3RSS')
  smthng_3RSS #Hanning 기능이 없으므로 따로 해주도록 하자
  
  N <- length(smthng_3RSS)
  
  smthng_3RSS <- smthng_3RSS %>% as.vector() #벡터로 만들어주고 
  smthng_3RSS
  
  hnng <- c()
  hnng[1] <- smthng_3RSS[1] ; hnng[N] <- smthng_3RSS[N]
  for (i in 2:(N-1)){
    hnng[i] <- 0.25*(smthng_3RSS[i-1] + smthng_3RSS[i+1]) + 0.5*smthng_3RSS[i]
  }
  hnng # 3RSSH의 결과
  rough_1 <- data - hnng
  
  #이제 Rough 에 대한 3RSSH 기법 적용(twiceit과 같다)
  r_3RSS_2 <- smooth(rough_1, kind= '3RSS')
  #hanning 적용
  hnng_2 <- c()
  hnng_2[1] <- r_3RSS_2[1] ; hnng_2[N] <- r_3RSS_2[N]
  
  for (i in 2:(N-1)){
    hnng_2[i] <- 0.25*(r_3RSS_2[i-1] + r_3RSS_2[i+1]) + 0.5*r_3RSS_2[i]
  }
  hnng_2 #3RSSH 거칠음
  
  rslt <- hnng + hnng_2
  return(rslt)
  
}
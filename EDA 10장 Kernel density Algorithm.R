#Kernel을 이용한 확률밀도함수 추정 Algorithm
data <- c(-2.1, -1.3, -0.4 , 1.9, 5.1 , 6.1)
#표본 점 1.9 지정
smpl <- 1.9

b = 0.9 * min(sd(data),IQR(data)/1.35)*(length(data)^(-1/5))
new_x <- c() # Kernel 기법에 따른 함숫값을 집어 넣어야하므로 새 벡터

#입력값( 새 벡터) 를 만드는 방법은 고정되어 있다.
for(i in 1: length(data)){
  new_x[i] = (smpl - data[i])/b
}

new_x # 새 벡터
new_x <- new_x[abs(new_x) <= 1] #indicator를 존중해야 하므로 추림

den <- (3/4)*(1 - new_x^2)/b # Epanechnikov 커널 적용 
sum(den)/length(data) # 만족하는 값들을 모두 더하고 데이터 개수만큼 나눔

x <- 5 + rnorm(50)
y <- rnorm(50)*0.6 + (x-2)*0.8 + 5

x3 <- jitter(x , amount = 0.5)
y3 <- jitter(y, amount = 0.5)
plot(y3 ~ x3 , xlim = c(0,9), ylim = c(0,9))

library(aplpack)
attach(mtcars)

bagplot(wt , mtcars$mpg, cex =1, xlab = 'Car Weight',
        ylab = 'Miles per Gallon')

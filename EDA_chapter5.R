#1번 과제
distance <- 12:22
size <- c(12,9.4,7.2,6.2,5.2,4.5,4.0,3.6,3.2,3.0,2.7)
df <- data.frame(distance,size)

attach(df)
fit_1 <- lm(size ~ distance)

par(mfrow= c(1,1))
par(mfrow = c(1,2))
plot(x= distance , y= size)
abline(fit_1,col = 'red')
summary(fit_1)


plot(x=log(distance),y= log(size) )
fit_2 <- lm(log(size) ~ log(distance))
abline(fit_2, col = 'blue')
summary(fit_2)

plot(x=distance,y= log(size) )
fit_3 <- lm(log(size) ~ distance)
abline(fit_3, col = 'blue')
summary(fit_3)



#2번 과제
M <- c(12,16.5,22,22,28,29.5,33.5,34,34,36,36.5,36.5,38.5,44,45,64,74.5,92,144.5)
HL <- c(11,16,21,20,22,24,32,33,30,30,34,34,37,43,42,54,64,86,132)
HU <- c(14,18,23,22,48,32,35,36,38,41,38,44,41,48,46,67,98,95,154)



data <- data.frame(M,HL,HU)
data
med.tran <- log10(M)
spr.tran <- log10(HU - HL)

fit_3 <- lm(spr.tran ~ med.tran)
fit_3

par(mfrow= c(1,2))
barplot(HU-HL, main = "균일화 전", ylab= "IQR(양 끝값을 모르므로)")
barplot(log(HU - HL),ylim = c(0,30), main = "균일화 후", y="log 변환 후 산포")

p = 1 - 0.9850
p

#p 가 거의 0에 수렴하니 대수 변환을 하는 것이 적절해 보인다.


data
#3번 과제
data <- c(114,1577,1788,2412,3480,4944,6443,7284,10874)
(md <- data[5]) #중위수
(h_l <- data[4]) #아래 경첩
(h_u <- data[6]) #위 경첩

int_u <- h_u - md #위 경첩부터 중위수 까지의 간격
int_l <- md - h_l #중위수 부터 아래 경첩 까지의 간격

p = 1 - (((h_l + h_u)/2)  - md)/((int_u^2 + int_l^2)/(4*md))
p
#p 가 거의 1/6 이므로 x의 1/6승으로 변환하는 것이 적절하다.


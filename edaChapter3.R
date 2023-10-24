before <-c(19, 20, 24, 25, 25, 26, 30, 31, 32, 36, 38, 39, 40, 42, 42, 45,
           45, 47, 48, 48, 52, 54, 55, 55, 56, 56, 58, 61, 62, 62, 63, 68, 71)
after <- c(11, 18, 22, 23, 25, 28, 29, 30, 30, 32, 32, 32, 35, 39, 40, 43,
           44, 45, 47, 47, 50, 51, 51, 53, 56, 62, 64, 65, 67, 73)

install.packages("aplpack")
library(aplpack)

#before의 줄기-잎 그림
stem(before)
#before의 scale argument 변경
stem(before, scale = 2)
stem(before, scale = 1/2)



#after의 줄기- 잎 그림
stem(after)
#after의 scale argument 변경
stem(after, scale= 2)
stem(after, scale = 1/2)


#양쪽 줄기 잎 그림
stem.leaf.backback(before,after)

####跳跃序列的实现过程

#设定序列七点和终点以及
start = 0
end = 10
lambd <- 2

#分割区间数、漂移项、方差项
n = 1000
miu = 0.75
variance = 0.7

#窗宽
h = 1000 ^ (-0.2)

#生成的正太随机数的均值与方差
meanofrn = 0
sdofrn = 1


#为了避免出现序列为负的情况，将序列起始值调整为10
startvalue = 10

#序列在起始点的值，即股票的初始值，还有参数lamdha的值
lambdaofN <- 20

#步长delta
delt <- (end - start) / n

r <- rnorm(n, mean = meanofrn, sd = sdofrn)
x <- rep(NaN, n)
xspot <- seq(start, end, length.out = n + 1)


#生成在区间内发生跳跃的次数
N <- as.numeric(rpois(1, lambda = lambdaofN))

#生成发生跳跃的点在区间内的坐标
jumpspot <- sort(runif(N, start, end))
plot(jumpspot)

#用cut函数生成区间按照总的区间数分隔后的区间序列interval
interval <- as.data.frame(cut(xspot, xspot)[-1])
colnames(interval) <- "interval"

#用cut函数找出发生跳跃点所在的区间，它的长度等于发生跳跃的次数N
jumpinterval <- as.data.frame(cut(jumpspot, xspot))
#colnames(jumpinterval) <- "jumpinterval"

#将发生跳跃的区间和对应的跳跃点合并
jumpintervalandspot <- cbind(jumpinterval, jumpspot)
colnames(jumpintervalandspot) <- c("interval", "jumpspot")

#用merge函数将数据框interval和jumpintervalandspot按照他们的共同变量interval合并，参数all=T是为了保留所有的数据
#intervalandjump <-merge(interval,jumpintervalandspot,by.x = "interval",by.y = "interval",all = T)
intervalandjump <-merge(interval,jumpintervalandspot,by= "interval",all = T)
#将intervalandjump第二列的缺失值用零代替
intervalandjump[, 2][is.na(intervalandjump[, 2])] <- 0

#生成的序列J表示在1的时候发生跳跃，在0时不跳跃
J <- ifelse(!intervalandjump[, 2] == 0, 1, 0)

#生成画板的默认参数配置opar,opar <- par(no.readonly = TRUE)用来更改当前变量环境
opar <- par(no.readonly = T)
# par(mfrow=c(2,1))
# par(mfrow=c(1,1))
plot(jumpspot)
plot(J)
plot(intervalandjump[, 2])

#初始化序列的起始值
x[1] <- startvalue

#以下是产生跳跃序列的代码
#jump为跳跃幅度，它设置为一个服从正太分布的随机变量
#分布的标准差为前一个股票价格的百分之十的绝对值的三分之一
#这样设定可以让99.74%的跳跃值都控制在振幅的百分之十以内，
#如果jump超过10%的振幅，就以百分之十代替

#变量jumps用来记录发生跳跃的区间以及跳跃值
jumps<-c()

for (i in 1:n)
{
  jump<-rnorm(1,mean = 0,sd=(abs(x[i]*0.1))/3)
  jump<-ifelse(abs(jump)>abs(x[i]*0.1),sign(jump)*abs(x[i]*0.1),jump)
  x[i + 1] <- x[i] + miu * delt + sqrt(variance) * 0.1 * r[i] + J[i] * jump
  
  jumps[i]<-J[i] * jump
}
plot(x)
plot(x, type = 'l',ylim = c(0,30))
plot(x, type = 'l')

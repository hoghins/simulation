start=0
end=10
split_interval_num=1000
step_length_delt=(end-start)/split_interval_num

#实现公式二
#n为区间数
nojump_sim<-function(n,
                     kappa3,
                     theta,
                     eta,
                     delt){
        #设置随机种子
        set.seed(2)
        b_t <- rep(NaN, n)
        b_t[1]<-0
        for (i in 1:n){
                        b_t[i+1]=b_t[i]+
                        kappa3*(theta-b_t[i])*delt+
                        eta*sqrt(b_t[i])*sqrt(delt)*rnorm(1,0,1)
        }
        b_t
}

b_t<-nojump_sim(n=split_interval_num,
                kappa3=0.3319,
                theta=0.0624,
                eta=0.056,
                delt=step_length_delt)
plot(b_t,type="l",main="no jump simulation",xlab="n",ylab="b_t") 

#实现公式一
#step1:确定Z_t,kappa_bar
# Z_t<-function(N,mu_j,theta_j){rnorm(N,mu_j,theta_j)}
# z_t<-Z_t(N=split_interval_num,mu_j=0,theta_j=0.0196)
# kappa_bar<-function(mu_j,theta_j){exp(mu_j+0.5*theta_j^2)-1}
# kappa_bar(0,0.0196)

#step2:含有jump
#n为区间数
# jump_sim<-function(n,
#                    kappa1,
#                    lambda,
#                    delt,
#                    gamma_jump,
#                    mean_jump,
#                    sd_jump){
        #无跳跃部分
        (b_t<-nojump_sim(n=split_interval_num,
                        kappa3=0.3319,
                        theta=0.0624,
                        eta=0.056,
                        delt=step_length_delt))
        #Z_t
        set.seed(2)
        Z_t<-function(N,mu_j,theta_j){rnorm(N,mu_j,theta_j)}
        (z_t<-Z_t(N=split_interval_num,mu_j=0,theta_j=0.0196))
        #kappa_bar
        Kappa_bar<-function(mu_j,theta_j){exp(mu_j+0.5*theta_j^2)-1}
        (kappa_bar<-Kappa_bar(0,0.0196))
        
        #构建跳跃J[i]
        xspot <- seq(start, end, length.out = split_interval_num + 1)
        #生成在区间内发生跳跃的次数
        set.seed(2)
        N_jump<- as.numeric(rpois(1, lambd = 10))
        #生成发生跳跃的点在区间内的x轴坐标
        (jumpspot <- sort(runif(N_jump, start, end)))
        #用cut函数生成区间按照总的区间数分隔后的区间序列interval
        interval <- as.data.frame(cut(xspot, xspot)[-1])
        colnames(interval) <- "interval"
        #用cut函数找出发生跳跃点所在的区间，它的长度等于发生跳跃的次数N
        jump_interval <- as.data.frame(cut(jumpspot, xspot))
        #colnames(jumpinterval) <- "jumpinterval"
        #将发生跳跃的区间和对应的跳跃点合并
        jump_intervalandspot <- cbind(jump_interval, jumpspot)
        colnames(jump_intervalandspot) <- c("interval", "jumpspot")
        #用merge函数将数据框interval和jumpintervalandspot按照他们的共同变量interval合并，参数all=T是为了保留所有的数据
        #intervalandjump <-merge(interval,jumpintervalandspot,by.x = "interval",by.y = "interval",all = T)
        intervalandjump <-merge(interval,jump_intervalandspot,by= "interval",all = T)
        #将intervalandjump第二列的缺失值用零代替
        intervalandjump[, 2][is.na(intervalandjump[, 2])] <- 0
        #生成的序列J表示在1的时候发生跳跃，在0时不跳跃
        (J <- ifelse(!intervalandjump[, 2] == 0, 1, 0))
        
        #跳跃公式的实现
        #变量jumps用来记录发生跳跃的区间以及跳跃值
        jumps<-c()
        x <- rep(NaN, split_interval_num)
        x[1] <- 0.1
        x
        
         n=split_interval_num
         kappa1=0.68
         lambda=10
         delt=step_length_delt
         gamma_jump=0.03
         mean_jump= 0
         sd_jump=1
        
        
        for (i in 1:split_interval_num)
        {
                ##跳跃
                #jump_Amplitude<-rnorm(1,0,1) 
                x[i+1] <-x[i]+
                        kappa1*(b_t[i]-kappa_bar*lambda-x[i])*delt+ 
                        gamma_jump*sqrt(x[i])*sqrt(delt)*rnorm(1,0,1)+
                        (exp(z_t[i])-1)*x[i]*J[i]
                #存在的跳跃
                #jumps[i]<-J[i]*jump_Amplitude 
        }
#}

# r_t<-jump_sim(n=split_interval_num,
#          kappa1=0.68,
#          lambda=10,
#          delt=step_length_delt,
#          gamma_jump=0.03,
#          mean_jump= 0,
#          sd_jump=1)
plot(x,type="l",main="jump simulation",xlab="n",ylab="b_t") 


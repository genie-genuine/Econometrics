

rm(list=ls())
setwd("C:\\Users\\user\\Dropbox\\data")
cpi.data<-read.csv("CPI.csv",header=TRUE)
head(cpi.data)

## Make data
#Inf = (cpi(t)-cpi(t-1))/(cpi(t-1))*100*4, t = 2,...,T
cpi_t = cpi.data$CPI[2:nrow(cpi.data)]
cpi_t_1 = cpi.data$CPI[1:(nrow(cpi.data)-1)]
Inf.value = 4*100*(cpi_t-cpi_t_1)/cpi_t_1

#y.t : change in inflation (delta Inf(t) = Inf(t) - Inf(t-1), t = 2,...,T)
Inf_t = Inf.value[2:length(Inf.value)]
Inf_t_1 = Inf.value[1:(length(Inf.value)-1)]
y.t = Inf_t - Inf_t_1
x.t = cpi.data$Unemp[3:nrow(cpi.data)]

y_t = y.t[5:length(y.t)]
y_t_1 = y.t[4:(length(y.t)-1)]
y_t_2 = y.t[3:(length(y.t)-2)]
y_t_3 = y.t[2:(length(y.t)-3)]
y_t_4 = y.t[1:(length(y.t)-4)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4)

Un_t_1 = x.t[4:(length(x.t)-1)]
Un_t_2 = x.t[3:(length(x.t)-2)]
Un_t_3 = x.t[2:(length(x.t)-3)]
Un_t_4 = x.t[1:(length(x.t)-4)]

x.data = cbind(Un_t_1 = Un_t_1, Un_t_2= Un_t_2, Un_t_3 = Un_t_3, Un_t_4 = Un_t_4)

#============================================#
#   BIC = ln[SSR(p)/T] + (p+1)ln T/T         #
#   AIC = ln[SSR(p)/T] + (p+1)2/T            # 
#============================================#

## AR(p) : y(t) = phi0 + phi1*y(t-1) + ... + phip*y(t-p)
AR.aic = c(); AR.bic = c()
for( p in 1:10){
  AR.fit = Arima(y.t, order = c(p,0,0))
  AR.aic[p] = AR.fit$aic
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.aic, type = "b", pch =19, main = "AIC", ylab = "", xlab = "p")
abline(v = which.min(AR.aic), col=2, lty = 2)

plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)
#AIC보다 BIC가 더 작게 나옴
# p에 대한 penalty가 더 강해서

# y order q x order
## ADL(p,q)
ADL.aic = matrix(0, ncol = 4, nrow = 4); ADL.bic = matrix(0, ncol = 4, nrow = 4)
for( p in 1:4){
  for(q in 1:4){ # p=1, q=1
    ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q]) # 
    ADL.aic[p,q] = AIC(ADL.fit)
    ADL.bic[p,q] = AIC(ADL.fit,k = log(length(y.t)))
  }
}
# p가 1부터 4로 바뀔때

# BIC
colnames(ADL.aic) = c("q=1", "q=2", "q=3", "q=4")
row.names(ADL.aic) = c("p=1", "p=2", "p=3", "p=4")
ADL.aic

# AIC
colnames(ADL.bic) = c("q=1", "q=2", "q=3", "q=4")
row.names(ADL.bic) = c("p=1", "p=2", "p=3", "p=4")
ADL.bic

all.case = expand.grid(1:4,1:4)

# for 구문 하나로 model
# 교수님이 넘어가라고....
ADL.aic0 = matrix(0, ncol = 4, nrow = 4); ADL.bic0 = matrix(0, ncol = 4, nrow = 4)
for(k in 1:nrow(all.case)){
  p = all.case[k,1]; q = all.case[k,2]
  ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])
  ADL.aic0[p,q] = AIC(ADL.fit)
  ADL.bic0[p,q] = AIC(ADL.fit,k = log(length(y.t)))
}

colnames(ADL.aic0) = c("q=1", "q=2", "q=3", "q=4")
row.names(ADL.aic0) = c("q=1", "q=2", "q=3", "q=4")
ADL.aic0

colnames(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4")
row.names(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4")
ADL.bic0


#===================================================================#
# Random walk 실제모형: y(t) = y(t-1) + u(t)                        #
#             추정모형: y(t) = b0 + b1*y(t-1) + u(t)                #
#             E[beta1] = 1-5.3/T                                    #
#===================================================================#
# b1 추정값이 1보다 조금 작게 

# 함수 생성
# random walk로부터 데이터 생성
Data.ftn = function(N){
  y.t = c()          # yt에 어떤 것을 넣겠다 지정
  a.t = rnorm(N)
  y.t[1] = 0         # 첫번째 yt =0
  for(t in 2:N){
    y.t[t] = y.t[t-1] + a.t[t]
  }
  return(y.t)
}
plot(y.t, type="l")

beta1=c()
beta1.se = c()
for(i in 1:1000){
  Data = Data.ftn(N=100)
  y_t = Data[2:length(Data)]         # 한 시점 밀림
  y_t_1 = Data[1:(length(Data)-1)]
  AR1.fit = lm(y_t ~ y_t_1)          # 추정
  beta1[i] = AR1.fit$coef[2]          
  beta1.se[i] = summary(AR1.fit)$coef[2,2]
}
# 
# AR1.fit
# b1 참값 1 1번 추정했을때 0.94
# 1000번 반복했을 때 값들의 평균 0.947
# 실제값 -1

mean(beta1)
t.stat = (beta1-1)/beta1.se

par(mfrow=c(1,1))
hist(t.stat, breaks = 100, probability = TRUE, xlim = c(-4,4))
lines(density(t.stat), col=4)


x = seq(-4,4, length.out = 1000)
lines(x, dnorm(x), col=2)        # normal 분포에서 벗어난다
t통계량 표준화시키면 보통 normal z=1 



어느 것이 더 작은 aic와 bic를 만들어주는지
arima 
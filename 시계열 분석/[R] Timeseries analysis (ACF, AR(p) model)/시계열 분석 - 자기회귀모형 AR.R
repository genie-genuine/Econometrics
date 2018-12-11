###2018.11.20 Timeseries analysis (ACF, AR(p) model)
# getwd()
# setwd("C:\\Users\\user\\Dropbox\\data")

cpi.data <-read.csv("CPI.csv",header=TRUE)
head(cpi.data); CPI.value  = cpi.data$CPI
 
# Make data
# Inf = (cpi(t)-cpi(t-1))/(cpi(t-1))*100*4, t = 2,...,T
cpi_t = cpi.data$CPI[2:nrow(cpi.data)]       # t-1부터니까 2부터
cpi_t_1 = cpi.data$CPI[1:(nrow(cpi.data)-1)]
Inf.value = 4*100*(cpi_t-cpi_t_1)/cpi_t_1
head(Inf.value)

# 인플레이션 구하는 두번째 방법: 로그 변화율 
# Inf = (log(cpi(t))-log(cpi(t-1)))*4*100, t = 2,...,T
# log cpi(t)-log cpi(t-1)*100*4
Inf.value1 = 4*100*(log(cpi_t)-log(cpi_t_1))    
head(Inf.value1)  # 위의 방법 결과와 거의 동일

# 그래프로 두 값 비교 그래프
plot(Inf.value, type = "l")
lines(Inf.value1, col=2, lty =2)

head(Inf.value)
head(Inf.value1)

# y.t 
# change in inflation 
# delta Inf(t) = Inf(t) - Inf(t-1), t = 2,...,T
Inf_t = Inf.value[2:length(Inf.value)]       # 159
Inf_t_1 = Inf.value[1:(length(Inf.value)-1)] # 158 (전체갯수-1)

y.t = Inf_t - Inf_t_1   # 직접 차분
y.t0 = diff(Inf.value)  # delta inflation (인플레이션 차분 값): diff()
head(y.t)
head(y.t0) #완전히 일치

# Time series plot
par(mfrow = c(1,3)) # par(): 그래프 한 화면에
plot(CPI.value, type = "l", xlab = " ", ylab = " ", 
     main = "Customer Price Index (CPI)")
plot(Inf.value, type = "l", xlab = " ", ylab = " ", 
     main = "Inflation" )
plot(y.t, type = "l", xlab = " ", ylab = " ", 
     main = expression(paste(Delta, "Inflation", "(", y[t],")"))) 

# Sample autocorrelation function SACF 
# SACF = corr(y(t), y(t-k)), k=Lag
# k만큼 떨어져있는 시점에 대해서 correlation
# 자기상관 함수 acf()
CPI.acf = acf(CPI.value)     # 값 저장 
Inf.acf = acf(Inf.value)
y.acf = acf(y.t,lag.max =20) # 얼마만큼 떨어져 있나
# 떨어져있음
# 점선(band) - 2*sd(1/sqrt(T))
# band 밖으로 넘어가면 유의한 것 : 자기상관이 존재한다.
# white noise 자기상관 없는 것 - band 안으로 

gamma.j = c(); rho.j=c()
gamma.0 = var(y.t)을
y.bar = mean(y.t)
T = length(y.t)

for(j in 1:20){
  y_t = y.t[1:(T-j)]
  y_t_j = y.t[(j+1):T]
  gamma.j[j] = 1/T*sum((y_t-y.bar)*(y_t_j-y.bar))
  rho.j[j] = gamma.j[j]/gamma.0
}
rho.j = c(1, rho.j)
plot(rho.j, type = "h", ylim = c(-0.3, 1.0), xlab = "Lag", ylab = "ACF")
abline(h = 2/sqrt(T), col=4, lty = 2)
abline(h = -2/sqrt(T), col=4, lty = 2)
abline(h = 0, col=1)


## AR(p) model
# 자기회귀모형 : 차수p
# AR(1) : (y(t)-mu) = phi*(y(t-1)-mu) + a(t)
#         y(t) = phi0 + phi1*y(t-1) + a(t)

library(forecast) #ar(1) 모형 세가지 
ar.fit1   = arima(y.t, order = c(1,0,0)) #phi0 출력 X
ar.fit1.m = Arima(y.t, order= c(1,0,0)) #phi0 출력 X, forecast pckg
ar.fit1.c = ar.ols(y.t, order=1, demean=F, intercept=T)

ar.fit1       # intercept=mu 0.0061
ar.fit1.m

ar.fit1.c     # phi0 0.01816 , 단순회귀와 같음
lm(y.t[2:length(y.t)]~y.t[1:(length(y.t)-1)])

acf(ar.fit1$residuals)

# 3시점 떨어진 자료와 자기상관 분석 
# AR(3) : (y(t)-mu) = phi1*(y(t-1)-mu)+ phi2*(y(t-2)-mu)+ phi3*(y(t-3)-mu) + a(t)
#         y(t) = phi0 + phi1*y(t-1) + phi2*y(t-2) + phi3*y(t-3) + a(t)
ar.fit3 = Arima(y.t, order = c(3,0,0)) #mu뺀 모형
ar.fit3

acf(ar.fit3$residuals) # whitenoise 가정 

# AR(1) 잔차 자기상관 없어야 올바른 모형- 잔차가 whitenoise가 아니다
# 차수 높여야한다
# 2sd band안으들 들어와야

# 멀어진 시점과의 자기상관
# overfitting


#####예측 패키지, 직ㅈ
## AR(1) forecast : y(t+1) = mu + phi*(y(t)-mu)
ar.fit1.m = Arima(y.t, order = c(1,0,0))
forecast(ar.fit1.m, h=1)

mu = ar.fit1.m$coef[2]
phi = ar.fit1.m$coef[1]
mu + phi*(y.t[length(y.t)]-mu)

library(lmtest)
library(forecast)
rm(list=ls())
setwd("C:\\Users\\USER\\Documents\\Github\\Econometrics\\시계열 분석\\HW3")
rv.data<-read.csv("RV_IV_data.csv", header=TRUE)
head(rv.data)
#============================#
#     VIX = Y,   SNP.RV = X  #
#============================#
# MAKE DATA
y.t = rv.data$VIX[1:nrow(rv.data)]      # VIX
head(y.t); length(y.t) 
x.t = rv.data$SNP.RV[1:(nrow(rv.data))]   # SNP.RV
head(x.t); length(x.t)

# AR 모형 BIC : p=5
AR.bic = c()
for( p in 1:10){
  AR.fit = arima(y.t, order = c(p,0,0))
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2) # AR(5)

y_t = y.t[6:length(y.t)]
y_t_1 = y.t[5:(length(y.t)-1)]
y_t_2 = y.t[4:(length(y.t)-2)]
y_t_3 = y.t[3:(length(y.t)-3)]
y_t_4 = y.t[2:(length(y.t)-4)]
y_t_5 = y.t[1:(length(y.t)-5)]

x_t = x.t[6:length(x.t)]
x_t_1 = x.t[5:(length(x.t)-1)]
x_t_2 = x.t[4:(length(x.t)-2)]
x_t_3 = x.t[3:(length(x.t)-3)]
x_t_4 = x.t[2:(length(x.t)-4)]
x_t_5 = x.t[1:(length(x.t)-5)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, 
               y_t_4 = y_t_4, y_t_5 = y_t_5)
x.data = cbind(x_t_1 = x_t_1, x_t_2= x_t_2, x_t_3 = x_t_3, 
               x_t_4 = x_t_4, x_t_5 = x_t_5)

# ADL 모형 BIC : p=5, q=2
ADL.bic = matrix(0, ncol = 5, nrow = 5)
for( p in 1:5){
  for(q in 1:5){ 
    ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])  
    ADL.bic[p,q] = AIC(ADL.fit,k = log(length(y.t)))
  }
}
colnames(ADL.bic) = c("q=1", "q=2", "q=3", "q=4", "q=5")
row.names(ADL.bic) = c("p=1", "p=2", "p=3", "p=4", "p=5")
ADL.bic; min(ADL.bic)    # ADL(5,2)

## 1-step ahead forecasting
AR5.fore = c(); ADL52.fore = c()
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], x.t = x.t[1:i])
  
  y_t = train.data[6:nrow(train.data),1]
  y_t_1 = train.data[5:(nrow(train.data)-1),1]
  y_t_2 = train.data[4:(nrow(train.data)-2),1]
  y_t_3 = train.data[3:(nrow(train.data)-3),1]
  y_t_4 = train.data[2:(nrow(train.data)-4),1]
  y_t_5 = train.data[1:(nrow(train.data)-5),1]

  x_t = train.data[6:nrow(train.data),2]
  x_t_1 = train.data[5:(nrow(train.data)-1),2]
  x_t_2 = train.data[4:(nrow(train.data)-2),2]
  x_t_3 = train.data[3:(nrow(train.data)-3),2]
  x_t_4 = train.data[2:(nrow(train.data)-4),2]
  x_t_5 = train.data[1:(nrow(train.data)-5),2]
  
  # AR(5)
  AR5.fit = lm(y_t ~ y_t_1 + y_t_2 + y_t_3 + y_t_4 + y_t_5 )
  AR5.fore[i-3105] = sum(AR5.fit$coef*c(1, y_t[length(y_t)], 
                                        y_t_1[length(y_t)], 
                                        y_t_2[length(y_t)], 
                                        y_t_3[length(y_t)],
                                        y_t_4[length(y_t)] ))  
  # ADL(5,2)
  ADL.fit1 = lm(y_t ~                            
                  y_t_1 + y_t_2 +y_t_3 + y_t_4 + y_t_5 +    
                  x_t + x_t_1 );
  ADL52.fore[i-3105] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], 
                                           y_t_1[length(y_t)], 
                                           y_t_2[length(y_t)], 
                                           y_t_3[length(y_t)],
                                           y_t_4[length(y_t)],
                                           x_t[length(y_t)], 
                                           x_t_1[length(y_t)] ))
}

# 1. VKOSPI가 KOSPI 5분 실현변동성을 GRANGER CAUSE하는가
grangertest(y.t~x.t)

# 2. 예측력 비교
# AR(5)     VS   ADL(5,2)  
# AR5.fore  VS   ADL52.fore
# MAE
AR5.MAE = mean(abs(AR5.fore - y.t[3107:length(y.t)]))
ADL52.MAE = mean(abs(ADL52.fore - y.t[3107:length(y.t)]))
AR5.MAE < ADL52.MAE      # AR(5) 모형이 MAE 값이 더 작음 
# MSE
AR5.MSE = mean((AR5.fore - y.t[3107:length(y.t)])^2)
ADL52.MSE = mean((ADL52.fore - y.t[3107:length(y.t)])^2)
AR5.MSE < ADL52.MSE      # AR(5) 모형이 MSE 값이 더 작음  
result = matrix(c(AR5.MAE, ADL52.MAE, AR5.MSE, ADL52.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR5", "ADL(5,2)")
result # AR(5) 모형이 더 잘 예측

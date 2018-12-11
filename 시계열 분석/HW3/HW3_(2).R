library(lmtest)
library(forecast)
rm(list=ls())
setwd("C:\\Users\\USER\\Documents\\Github\\Econometrics\\시계열 분석\\HW3")
rv.data<-read.csv("RV_IV_data.csv", header=TRUE)
#============================#
#  VKOSPI = Y, KOSPI.RV = X  #
#============================#
# MAKE DATA
y.t = rv.data$VKOSPI[1:nrow(rv.data)]      # VKOSPI
head(y.t); length(y.t) 
x.t = rv.data$KOSPI[1:(nrow(rv.data))]   # KOSPI.RV
head(x.t); length(x.t)

# AR 모형 BIC : p=9
AR.bic = c()
for( p in 1:10){
  AR.fit = arima(y.t, order = c(p,0,0))
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)  # AR(9)

y_t = y.t[10:length(y.t)]
y_t_1 = y.t[9:(length(y.t)-1)]
y_t_2 = y.t[8:(length(y.t)-2)]
y_t_3 = y.t[7:(length(y.t)-3)]
y_t_4 = y.t[6:(length(y.t)-4)]
y_t_5 = y.t[5:(length(y.t)-5)]
y_t_6 = y.t[4:(length(y.t)-6)]
y_t_7 = y.t[3:(length(y.t)-7)]
y_t_8 = y.t[2:(length(y.t)-8)]
y_t_9 = x.t[1:(length(x.t)-9)]

  x_t = x.t[10:length(x.t)]
x_t_1 = x.t[9:(length(x.t)-1)]
x_t_2 = x.t[8:(length(x.t)-2)]
x_t_3 = x.t[7:(length(x.t)-3)]
x_t_4 = x.t[6:(length(x.t)-4)]
x_t_5 = x.t[5:(length(x.t)-5)]
x_t_6 = x.t[4:(length(x.t)-6)]
x_t_7 = x.t[3:(length(x.t)-7)]
x_t_8 = x.t[2:(length(x.t)-8)]
x_t_9 = x.t[1:(length(x.t)-9)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4,
               y_t_5 = y_t_5, y_t_6= y_t_6, y_t_7 = y_t_7, y_t_8 = y_t_8,
               y_t_9 = y_t_9)
x.data = cbind(x_t_1 = x_t_1, x_t_2= x_t_2, x_t_3 = x_t_3, x_t_4 = x_t_4,
               x_t_5 = x_t_5, x_t_6= x_t_6, x_t_7 = x_t_7, x_t_8 = x_t_8,
               x_t_9 = x_t_9)

# ADL 모형 BIC : p=5, q=1
ADL.bic = matrix(0, ncol = 9, nrow = 9)
for( p in 1:9){
  for(q in 1:9){ 
    ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])  
    ADL.bic[p,q] = AIC(ADL.fit,k = log(length(y.t)))
  }
}
colnames(ADL.bic) = c("q=1", "q=2", "q=3", "q=4", "q=5", "q=6", "q=7", "q=8", "q=9")
row.names(ADL.bic) = c("p=1", "p=2", "p=3", "p=4", "p=5", "p=6", "p=7", "p=8", "p=9")
ADL.bic; min(ADL.bic) # ADL(5,1)

## 1-step ahead forecasting
AR9.fore = c(); ADL51.fore = c()
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], x.t = x.t[1:i])
  
  y_t = train.data[10:nrow(train.data),1]
  y_t_1 = train.data[9:(nrow(train.data)-1),1]
  y_t_2 = train.data[8:(nrow(train.data)-2),1]
  y_t_3 = train.data[7:(nrow(train.data)-3),1]
  y_t_4 = train.data[6:(nrow(train.data)-4),1]
  y_t_5 = train.data[5:(nrow(train.data)-5),1]
  y_t_6 = train.data[4:(nrow(train.data)-6),1]
  y_t_7 = train.data[3:(nrow(train.data)-7),1]
  y_t_8 = train.data[2:(nrow(train.data)-8),1]
  y_t_9 = train.data[1:(nrow(train.data)-9),1]
  
  x_t = train.data[10:nrow(train.data),2]
  x_t_1 = train.data[9:(nrow(train.data)-1),2]
  x_t_2 = train.data[8:(nrow(train.data)-2),2]
  x_t_3 = train.data[7:(nrow(train.data)-3),2]
  x_t_4 = train.data[6:(nrow(train.data)-4),2]
  x_t_5 = train.data[5:(nrow(train.data)-5),2]
  x_t_6 = train.data[4:(nrow(train.data)-6),2]
  x_t_7 = train.data[3:(nrow(train.data)-7),2]
  x_t_8 = train.data[2:(nrow(train.data)-8),2]
  x_t_9 = train.data[1:(nrow(train.data)-9),2]
  
  # AR(9)
  AR9.fit = lm(y_t ~ y_t_1 + y_t_2 + y_t_3 + y_t_4 + 
                 y_t_5 + y_t_6 + y_t_7 + y_t_8 + y_t_9)
  AR9.fore[i-3105] = sum(AR9.fit$coef*c(1, y_t[length(y_t)], 
                                        y_t_1[length(y_t)], 
                                        y_t_2[length(y_t)], 
                                        y_t_3[length(y_t)],
                                        y_t_4[length(y_t)],
                                        y_t_5[length(y_t)],
                                        y_t_6[length(y_t)],
                                        y_t_7[length(y_t)],
                                        y_t_8[length(y_t)]))
  # ADL(5,1)
  ADL.fit1 = lm(y_t ~                            
                  y_t_1 + y_t_2 +y_t_3 + y_t_4 + y_t_5 +     
                  x_t );
  ADL51.fore[i-3105] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], 
                                           y_t_1[length(y_t)], 
                                           y_t_2[length(y_t)], 
                                           y_t_3[length(y_t)],
                                           y_t_4[length(y_t)],
                                           x_t[length(y_t)] ))
}

# 1. VKOSPI가 KOSPI 5분 실현변동성을 GRANGER CAUSE하는가
grangertest(y.t~x.t)

# 2. 예측력 비교
# AR(8)     VS   ADL(5,1)  
# AR8.fore  VS   ADL51.fore
# MAE
AR9.MAE = mean(abs(AR9.fore - y.t[3107:length(y.t)]))
ADL51.MAE = mean(abs(ADL51.fore - y.t[3107:length(y.t)]))
AR9.MAE < ADL51.MAE      # AR(9) 모형이 MAE 값이 더 작으므로 
# MSE
AR9.MSE = mean((AR9.fore - y.t[3107:length(y.t)])^2)
ADL51.MSE = mean((ADL51.fore - y.t[3107:length(y.t)])^2)
AR9.MSE < ADL51.MSE      # AR(9) 모형이 MSE 값이 더 작으므로 
result = matrix(c(AR9.MAE, ADL51.MAE, AR9.MSE, ADL51.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR9", "ADL(5,1)")
result    # AR(1) 모형이 더 잘 예측

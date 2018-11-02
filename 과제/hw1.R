getwd()
caschool = read.csv("~/caschool.csv",header=TRUE)               
attach(caschool)
head(caschool)

str(caschool)

plot(str,read_scr)
lm.fit0 <- lm(read_scr~str)   
summary(lm.fit0)
abline(lm.fit0,col="red")

lm.fit1 <- lm(read_scr~str+meal_pct+el_pct) 
summary(lm.fit1) 

lm.fit2 <- lm(read_scr~str+meal_pct+el_pct+log(avginc)) 
summary(lm.fit2) 
 
HiEL <- ifelse(el_pct>10, 1, 0)
lm.fit3 <- lm(read_scr~str+HiEL+str*HiEL)
summary(lm.fit3) 

lm.fit4 <- lm(read_scr~str+HiEL+str*HiEL+meal_pct+log(avginc))
summary(lm.fit4)


str2<-(caschool$str)^2
str3<-(caschool$str)^3
lm.fit5 <- lm(read_scr~str+str2+str3+HiEL+meal_pct+log(avginc))
summary(lm.fit5) 

lm.fit6 = lm(read_scr~str+str2+str3+HiEL+HiEL*str+HiEL*str2+HiEL*str3+meal_pct+log(avginc))
summary(lm.fit6)

lm.fit7 = lm(read_scr~str+str2+str3+el_pct+meal_pct+log(avginc))
summary(lm.fit7) 











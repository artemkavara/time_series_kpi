setwd("~/Time Series/Lab_3") #set to correct dir
source("ma.R")
source("create_model_df.R")
source("display_res.R")

ts_1 <- read.csv("Data/2001rts1.txt", header = F)
ts_2 <- read.csv("Data/RTSin.txt", header = F)

#transform data
library(zoo)
ts_1 <- ts(ts_1)
ts_2 <- ts(ts_2)

#Durbin-Watson

#demo
ts_demo <- as.ts(read.csv("example_for_DW.txt", header = F))
model_demo <- arima(ts_demo, order = c(1, 0, 0))
vec_eps <- model_demo$residuals
durbin_watson(ts_demo)

library(car)
durbinWatsonTest(as.vector(ts_demo))

#get summary
summary(ts_1)
summary(ts_2)

library(psych)
describe(ts_1)
describe(ts_2)

#get plots
plot(ts_1, main = "2001rts1.txt")
plot(ts_2, main = "RTSin.txt")

hist(ts_1, main = "2001rts1.txt")
hist(ts_2, main = "RTSin.txt")

library(forecast)
auto.arima(ts_1)
auto.arima(ts_2)

par(mfrow=c(1,2))
ts.plot(ts_1, moving_average(ts_1, 5), 
        gpars = list(col = c("black", "red")))
ts.plot(ts_1, moving_average(ts_1, 5, "exp"), 
        gpars = list(col = c("black", "red")))

ts.plot(window(ts_1, end = 50), 
        window(moving_average(ts_1, 5), end = 50), 
        gpars = list(col = c("black", "red")))
ts.plot(window(ts_1, end = 50),
        window(moving_average(ts_1, 5, "exp"), end=50), 
        gpars = list(col = c("black", "red")))

#pacf
pacf.ts_1 <- pacf(ts_1, lag = 12)
pacf.ts_2 <- pacf(ts_2, lag = 12)

pacf.ts_1
pacf.ts_2

plot(pacf.ts_1)
plot(pacf.ts_2)

par(mfrow=c(1, 1))
#1st approach, 1st ts
model_1.ts_1 <- lm(ts_1[2:length(ts_1)] ~ ts_1[1:length(ts_1)-1])
res_1.ts_1 <- model_1.ts_1$residuals
ma_res.ts_1 <- ma(res_1.ts_1, order = 5)
temp.pacf <- pacf(ma_res.ts_1, lag = 12, na.action = na.pass)
temp.pacf


display.res.arima(fitted(model_1.ts_1), ts_1[2:length(ts_1)], c(1, 6))
#display.res(model_1.ts_1)

#Best model - ARMA(1, 6)
model_1_r.ts_1 <- arima(ts_1, order = c(1, 0, 6))
display.res.arima(fitted(model_1_r.ts_1), ts_1, c(1, 6))

ma_5.ts_1 <- moving_average(ts_1, 5)
ma_5_exp.ts_1 <- moving_average(ts_1, 5, "exp")
ma_10.ts_1 <- moving_average(ts_1, 10)
ma_10_exp.ts_1 <- moving_average(ts_1, 10, "exp")

model_1_ma_5.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_5.ts_1, 5, 1, 6))
#7 = max(p,q)+1
pred_1_ma_5.ts_1 <- fitted(model_1_ma_5.ts_1) + ma_5.ts_1[7:length(ma_5.ts_1)]
#11 = max(p,q)+N
display.res.arima(pred_1_ma_5.ts_1, ts_1[11:length(ts_1)], order = c(1, 6))

model_1_ma_5_exp.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_5_exp.ts_1, 5, 1, 6))
pred_1_ma_5_exp.ts_1 <- fitted(model_1_ma_5_exp.ts_1) + ma_5_exp.ts_1[7:length(ma_5_exp.ts_1)]
display.res.arima(pred_1_ma_5_exp.ts_1, 
                  window(ts_1, start = 11,end = length(ts_1)), c(1, 6))

model_1_ma_10.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_10.ts_1, 10, 1, 6))
pred_1_ma_10.ts_1 <- fitted(model_1_ma_10.ts_1) + ma_10.ts_1[7:length(ma_10.ts_1)]
display.res.arima(pred_1_ma_10.ts_1, 
                  window(ts_1, start = 16, end = length(ts_1)), c(1, 6))

model_1_ma_10_exp.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_10_exp.ts_1, 10, 1, 6))
pred_1_ma_10_exp.ts_1 <- fitted(model_1_ma_10_exp.ts_1) + ma_10_exp.ts_1[7:(length(ma_10_exp.ts_1))]
display.res.arima(pred_1_ma_10_exp.ts_1,
                  window(ts_1, start = 16, end = length(ts_1)), c(1, 6))

#1st approach, 2nd ts
model_1.ts_2 <- lm(ts_2[2:length(ts_2)]~ ts_2[1:length(ts_2)-1])
res_1.ts_2 <- model_1.ts_2$residuals
temp.pacf <- pacf(ma(res_1.ts_2, 5), lag = 12, na.action = na.pass)
temp.pacf

display.res.arima(fitted(model_1.ts_2), ts_2[2:length(ts_2)], c(1, 0))

#Best model - ARMA(1, 6)
ma_5.ts_2 <- moving_average(ts_2, 5)
ma_5_exp.ts_2 <- moving_average(ts_2, 5, "exp")
ma_10.ts_2 <- moving_average(ts_2, 10)
ma_10_exp.ts_2 <- moving_average(ts_2, 10, "exp")


model_1_r.ts_2 <- arima(ts_2, order = c(1, 0, 6), method = "CSS")
display.res.arima(pred = fitted(model_1_r.ts_2), true = ts_2, order = c(1, 6))

model_1_ma_5.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_5.ts_2, 5, 1, 6))
pred_1_ma_5.ts_2 <- fitted(model_1_ma_5.ts_2) + ma_5.ts_2[7:(length(ma_5.ts_2))]
display.res.arima(pred_1_ma_5.ts_2, 
                  window(ts_2, start = 11, end = length(ts_2)), c(1, 6))

model_1_ma_5_exp.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_5_exp.ts_2, 5, 1, 6))
pred_1_ma_5_exp.ts_2 <- fitted(model_1_ma_5_exp.ts_2) + ma_5_exp.ts_2[7:length(ma_5_exp.ts_2)]
display.res.arima(pred_1_ma_5_exp.ts_2, 
                  window(ts_2, start = 11,end = length(ts_2)), c(1, 6))

model_1_ma_10.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_10.ts_2, 10, 1, 6))
pred_1_ma_10.ts_2 <- fitted(model_1_ma_10.ts_2) + ma_10.ts_2[7:(length(ma_10.ts_2))]
display.res.arima(pred_1_ma_10.ts_2, 
                  window(ts_2, start = 16, end = length(ts_2)), c(1, 6))

model_1_ma_10_exp.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_10_exp.ts_2, 10, 1, 6))
pred_1_ma_10_exp.ts_2 <- fitted(model_1_ma_10_exp.ts_2) + ma_10_exp.ts_2[7:(length(ma_10_exp.ts_2))]
display.res.arima(pred_1_ma_10_exp.ts_2, 
                  window(ts_2, start = 16, end = length(ts_2)), c(1, 6))


#2nd approach, 1st ts

pacf(ma(ts_1, order = 5), lag = 12, na.action = na.pass)

#Best model - ARMA(1, 1)
model_2_r.ts_1 <- arima(ts_1, order = c(1, 0, 1))
display.res.arima(fitted(model_2_r.ts_1), ts_1, c(1, 1))

#2.1, 1st ts
model_2_ma_5_own.ts_1 <- lm(y_k~., data=create_model_df_own(ts_1, ma_5.ts_1, 5, 1, 1))
pred_2_ma_5.ts_1 <- pred_own(fitted(model_2_ma_5_own.ts_1), ma_5.ts_1, 1, 1)
display.res.arima(pred_2_ma_5.ts_1, 
                  window(ts_1, start = 7, end = length(ts_1)), c(1, 1))

model_2_ma_10_own.ts_1 <- lm(y_k~., data=create_model_df_own(ts_1, ma_10.ts_1, 10, 1, 1))
pred_2_ma_10.ts_1 <- pred_own(fitted(model_2_ma_10_own.ts_1), ma_10.ts_1, 1, 1)
display.res.arima(pred_2_ma_10.ts_1, 
                  window(ts_1, start = 7+5, end = length(ts_1)), c(1, 1))

model_2_ma_5_exp_own.ts_1 <- lm(y_k~., data=create_model_df_own(ts_1, ma_5_exp.ts_1, 5, 1, 1))
pred_2_ma_5_exp.ts_1 <- pred_own(fitted(model_2_ma_5_exp_own.ts_1), ma_5_exp.ts_1, 1, 1)
display.res.arima(pred_2_ma_5_exp.ts_1, 
                  window(ts_1, start = 7, end = length(ts_1)), c(1, 1))

model_2_ma_10_exp_own.ts_1 <- lm(y_k~., data=create_model_df_own(ts_1, ma_10_exp.ts_1, 10, 1, 1))
pred_2_ma_10_exp.ts_1 <- pred_own(fitted(model_2_ma_10_exp_own.ts_1), ma_10_exp.ts_1, 1, 1)
display.res.arima(pred_2_ma_10_exp.ts_1, 
                  window(ts_1, start = 7+5, end = length(ts_1)), c(1, 1))



#2.2, 1st ts
model_2_ma_5.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_5.ts_1, 5, 1, 1))
pred_3_ma_5.ts_1 <- fitted(model_2_ma_5.ts_1) + ma_5.ts_1[2:length(ma_5.ts_1)]
display.res.arima(pred_3_ma_5.ts_1, ts_1[6:length(ts_1)], c(1, 1))

model_2_ma_10.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_10.ts_1, 10, 1, 1))
pred_3_ma_10.ts_1 <- fitted(model_2_ma_10.ts_1) + ma_10.ts_1[2:length(ma_10.ts_1)]
display.res.arima(pred_3_ma_10.ts_1, ts_1[11:length(ts_1)], c(1, 1))

model_2_ma_5_exp.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_5_exp.ts_1, 5, 1, 1))
pred_3_ma_5_exp.ts_1 <- fitted(model_2_ma_5_exp.ts_1) + ma_5_exp.ts_1[2:(length(ma_5_exp.ts_1))]
display.res.arima(pred_3_ma_5_exp.ts_1, ts_1[6:length(ts_1)], c(1, 1))

model_2_ma_10_exp.ts_1 <- lm(y_k~., data=create_model_df(ts_1, ma_10_exp.ts_1, 10, 1, 1))
pred_3_ma_10_exp.ts_1 <- fitted(model_2_ma_10_exp.ts_1) + ma_10_exp.ts_1[2:(length(ma_10_exp.ts_1))]
display.res.arima(pred_3_ma_10_exp.ts_1, ts_1[11:length(ts_1)], c(1, 1))

#2nd approach, 2nd ts
pacf(ma(ts_1, order = 5), lag = 12, na.action = na.pass)

#Best model - ARMA(1, 1)
model_2_r.ts_2 <- arima(ts_2, order = c(1, 0, 1), method = "CSS")
display.res.arima(fitted(model_2_r.ts_2), ts_2, c(1, 1))

#2.1, 2nd ts
model_2_ma_5_own.ts_2 <- lm(y_k~., data=create_model_df_own(ts_2, ma_5.ts_2, 5, 1, 1))
pred_2_ma_5.ts_2 <- pred_own(fitted(model_2_ma_5_own.ts_2), ma_5.ts_2, 1, 1)
display.res.arima(pred_2_ma_5.ts_2, 
                  window(ts_2, start = 7, end = length(ts_2)), c(1, 1))

model_2_ma_10_own.ts_2 <- lm(y_k~., data=create_model_df_own(ts_2, ma_10.ts_2, 10, 1, 1))
pred_2_ma_10.ts_2 <- pred_own(fitted(model_2_ma_10_own.ts_2), ma_10.ts_2, 1, 1)
display.res.arima(pred_2_ma_10.ts_2, 
                  window(ts_2, start = 7+5, end = length(ts_2)), c(1, 1))

model_2_ma_5_exp_own.ts_2 <- lm(y_k~., data=create_model_df_own(ts_2, ma_5_exp.ts_2, 5, 1, 1))
pred_2_ma_5_exp.ts_2 <- pred_own(fitted(model_2_ma_5_exp_own.ts_2), ma_5_exp.ts_2, 1, 1)
display.res.arima(pred_2_ma_5_exp.ts_2, 
                  window(ts_2, start = 7, end = length(ts_2)), c(1, 1))

model_2_ma_10_exp_own.ts_2 <- lm(y_k~., data=create_model_df_own(ts_2, ma_10_exp.ts_2, 10, 1, 1))
pred_2_ma_10_exp.ts_2 <- pred_own(fitted(model_2_ma_10_exp_own.ts_2), ma_10_exp.ts_2, 1, 1)
display.res.arima(pred_2_ma_10_exp.ts_2, 
                  window(ts_2, start = 7+5, end = length(ts_2)), c(1, 1))



#2.2, 2nd ts
model_2_ma_5.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_5.ts_2, 5, 1, 1))
pred_3_ma_5.ts_2 <- fitted(model_2_ma_5.ts_2) + ma_5.ts_2[2:(length(ma_5.ts_2))]
display.res.arima(pred_3_ma_5.ts_2, ts_2[6:length(ts_2)], c(1, 1))

model_2_ma_10.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_10.ts_2, 10, 1, 1))
pred_3_ma_10.ts_2 <- fitted(model_2_ma_10.ts_2) + ma_10.ts_2[2:(length(ma_10.ts_2))]
display.res.arima(pred_3_ma_10.ts_2, ts_2[11:length(ts_2)], c(1, 1))

model_2_ma_5_exp.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_5_exp.ts_2, 5, 1, 1))
pred_3_ma_5_exp.ts_2 <- fitted(model_2_ma_5_exp.ts_2) + ma_5_exp.ts_2[2:(length(ma_5_exp.ts_2))]
display.res.arima(pred_3_ma_5_exp.ts_2, ts_2[6:length(ts_2)], c(1, 1))

model_2_ma_10_exp.ts_2 <- lm(y_k~., data=create_model_df(ts_2, ma_10_exp.ts_2, 10, 1, 1))
pred_3_ma_10_exp.ts_2 <- fitted(model_2_ma_10_exp.ts_2) + ma_10_exp.ts_2[2:(length(ma_10_exp.ts_2))]
display.res.arima(pred_3_ma_10_exp.ts_2, ts_2[11:length(ts_2)], c(1, 1))

#Best models plot
ts.plot(pred_1_ma_5.ts_1[1:length(pred_1_ma_5.ts_1)], 
        window(ts_1, start = 11,end = length(ts_1)), 
        gpars = list(col = c("red", "black")))

ts.plot(pred_1_ma_5.ts_2, 
        window(ts_2, start = 11,end = length(ts_2)), 
        gpars = list(col = c("red", "black")))

#Multiple regression
rts1 <- read.csv("Data/rts1.txt", sep = " ", header = F)
RTScr <- read.csv("Data/RTScr.txt", sep = " ", header = F)
RTSeu <- read.csv("Data/RTSeu.txt", sep = " ", header = F)
RTSfn <- read.csv("Data/RTSfn.txt", sep = " ", header = F)
RTSin <- read.csv("Data/RTSin.txt", sep = " ", header = F)
RTSmm <- read.csv("Data/RTSmm.txt", sep = " ", header = F)
RTSog <- read.csv("Data/RTSog.txt", sep = " ", header = F)
RTStl <- read.csv("Data/RTStl.txt", sep = " ", header = F)

rts_df <- cbind(rts1, RTScr, RTSeu, RTSfn, RTSin, RTSmm, RTSog, RTStl)
colnames(rts_df) <- c("rts1", "RTScr", "RTSeu", "RTSfn", 
                      "RTSin", "RTSmm", "RTSog", "RTStl")
cor(rts_df)

model_rts <- lm(rts1 ~., data = rts_df)
model_rts$coefficients
summary(model_rts)

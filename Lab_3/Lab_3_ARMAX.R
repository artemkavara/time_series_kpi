#rts1 ts
t(describe(rts1))
ts.plot(rts1)


#Define describing features
pacf_rts <- sapply(tail(colnames(rts_df), -1),
                   function(col) part_cor_fun(rts_df[,"rts1"], rts_df[,col], 12))
row.names(pacf_rts) <- 0:11
#Plotting PACF functions

par(mfrow = c(2, 4))
for(col in colnames(pacf_rts)){
  barplot(height = pacf_rts[,col], main = col, xlab = "Lag",
          )
  lines(0:15, rep(0.3, 16), col = "red")
  lines(0:15, rep(-0.3, 16), col = "red")
}

#RTScr: lag 0-1
#RTSeu: lag 0-1
#RTSfn: lag 0-1
#RTSin: lag 0-2, 10, 11
#RTSmm: lag 0-2
#RTSog: lag 0-1
#RTStl: lag 0-1

par(mfrow = c(1, 1))
pacf(rts1, 12)$acf

#p = 1, 2
rts1 <- as.ts(rts1)
n<-length(rts1)
rts1_model_1 <- lm(rts1[3:n]~rts1[2:(n-1)]+rts1[1:(n-2)])
display.res(rts1_model_1)

res_rts1 <- rts1_model_1$residuals
ma_res.rts1 <- ma(res_rts1, order = 5)
pacf(ma_res.rts1, 12, na.action = na.pass)$acf

#q = 1, 4, 6, 11

#model ARMA(2, 11)
model_arma_rts1 <- arima(as.ts(rts1), c(2, 0, 11))
library(forecast)
display.res.arima(fitted(model_arma_rts1), rts1, c(2, 11))

#model ARMAX
rt1_armax <- df_armax(time_ser = as.ts(rts1), p = c(1, 2), q = c(1, 4, 6, 11), 
                         regr = rts_df[, -1], d = list(1, 1, 1, c(1,2, 10, 11),  c(1,2), 1, 1))
df_rt1_armax <- rt1_armax[[1]]
ts_new <- rt1_armax[[2]]
ma_ts_new <- rt1_armax[[3]]
model_armax <- lm(y_k ~., data = df_rt1_armax)
fitted_val_armax <- model_armax$fitted.values + ma_ts_new
display.res.arima(fitted_val_armax, ts_new, order = c(2, 22))

par(mfrow = c(1, 1))
ts.plot(as.ts(ts_new), as.ts(fitted_val_armax), 
        gpars = list(col = c("blue", "red")))

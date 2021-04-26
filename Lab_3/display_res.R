source("metrics.R")

display.res <- function(model){
  r.sq <- summary(model)$r.squared
  r.sq.adj <- summary(model)$adj.r.squared
  RSS <- sum(model$residuals**2)
  AIC <- AIC(model)
  DW <- durbinWatsonTest(as.vector(model$residuals))
  lst <- list("R_Squared" = r.sq,
              "Adjusted_R_Squared" = r.sq.adj,
              "RSS" = RSS,
              "AIC" = AIC,
              "Durbin_Watson" = DW)
  warning("formula: -2*log-likelihood + k*npar")
  return(lst)
}

display.res.arima <- function(pred, true, order){
  
  r.sq <- rsq(pred, true)
  RSS <- RSS(pred, true)
  MSE <- MSE(pred, true)
  RMSE <- RMSE(pred, true)
  AIC <- IKA(pred, true, ar = order[1], ma = order[2])
  DW <- durbin_watson(true-pred)
  lst <- list("R_Squared" = r.sq,
              "RSS" = RSS,
              "MSE" = MSE,
              "RMSE" = RMSE,
              "AIC" = AIC,
              "Durbin_Watson" = DW)
  return(lst)
}

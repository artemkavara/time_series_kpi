RSS <- function(pred, true){
  return (sum((pred-true)**2))
}

rsq <- function (pred, true){
  TSS <- var(true)
  ESS <- var(pred)
  return(ESS/TSS)
}

MSE <- function(pred, true){
  return(1/length(true)*RSS(pred, true))
}

RMSE <- function(pred, true){
  return(sqrt(MSE(pred, true)))
}

IKA <- function(pred, true, ar, ma){
  return(length(true)*log(RSS(pred, true)) + 2*(ar+ma+1))
}

durbin_watson <- function(vec_eps){
  num <- sum((vec_eps[2:length(vec_eps)]-vec_eps[1:length(vec_eps)-1])**2)
  denum <- sum(vec_eps**2)
  return(num/denum)
}

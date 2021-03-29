moving_average <- function(time_series, N, method = "ord"){
  
  if(method == "ord"){
    wghts <- rep(1, N)
  }
  else if(method == "exp"){
    wghts <- rev(sapply(X=seq(1, N), FUN = function(n) (1-2/(n+1))**n))
  }

  
  vec_ts <- as.vector(time_series)
  ma <- c()
  for(i in 1:(length(time_series)-N)){
    ma[i] <- ((time_series[as.integer(i):as.integer(i+N-1)]%*%wghts)/sum(wghts))[1, 1]
  }
  
  return(ts(ma))
}



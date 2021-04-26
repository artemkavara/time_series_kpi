part_cor_fun <- function(time_series, regres, n_lag = 12){
    #idea for implementation from 
    #https://stats.stackexchange.com/questions/129052/acf-and-pacf-formula
    
    #length of time series 
    n <- length(time_series)
    #get correlations for all needed lags
    full_corr <- sapply(1:n_lag, function(k) 
      cor(time_series[k:n], regres[1:(n-k+1)]))
    #append 1
    full_corr <- append(full_corr, 1, 0)
    #create result vector
    pacf_y_r <- rep(0, n_lag)
    #calculating of pacfs
    for (i in 1:n_lag){
        cor_matr <- toeplitz(full_corr[1:i])
        pacf_y_r[i] <- solve(cor_matr, full_corr[2:(i+1)])[i]
    }
    return(pacf_y_r)
}



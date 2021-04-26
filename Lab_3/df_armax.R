df_armax <- function(time_ser, p, q, regr, d, ma.order = 5){
    orig_ts <- time_ser
    max_param <- max(p, q)+1
    
    ma_ts <- ma(time_ser, order = ma.order)
    time_ser<- time_ser[!is.na(ma_ts)]
    ma_ts <- ma_ts[!is.na(ma_ts)]
    n <- length(time_ser)

    armax_df <- data.frame(time_ser[max_param:n] - ma_ts[max_param:n])
    colnames(armax_df) <- c("y_k")
    for (i in 1:length(p)){
            
            armax_df[i+1] <- time_ser[(max_param-p[i]):(n-p[i])]
            colnames(armax_df)[i+1] <- paste("y_k_",as.character(p[i]), sep="")
            
    }
    for(i in 1:length(q)){
            armax_df[i+length(p)+1] <- ma_ts[(max_param-q[i]):(n-q[i])]
            colnames(armax_df)[i+length(p)+1] <- paste("ma_k_",as.character(q[i]), sep="")
            
    }
    num_regr <- length(d)
    for (i in 1:num_regr){
        col_name <- colnames(regr)[i]
        regr_lag <- append(d[[i]], 0, 0)
        for (l in regr_lag){
            armax_df <- cbind(armax_df, regr[(max_param-l):(n-l), i])
            names(armax_df)[length(names(armax_df))]<- paste(col_name, "_k_", as.character(l), sep = "")
        }
    }
    return(list(armax_df, time_ser[max_param:n], ma_ts[max_param:n]))
}

create_model_df_own <- function(time_s, ma, N, p, q){
  
  temp_ts <- time_s[N:length(time_s)]
  
  k <- length(temp_ts)
  s <- max(p, q)
  temp_ts <- temp_ts[(s+1):k]- ma[(s+1):k]
  sum_a <- ifelse(q == 1, 1, sum(sapply(2:(q+1), function(k) (1 - 2/(q+1))**(k-1))))


  for(j in 1:q){
    a <- ifelse(j==1 || q == 1, 1, (1 - 2/(q+1))**(j-1))
    temp_ts <- temp_ts - (a/sum_a)*ma[(q-j+1+(s-q)):(k-j)]
  }

  model_df <- data.frame(temp_ts[(s+1):k])
  colnames(model_df)[1] <- "y_k"

  for (i in 1:p){
    
    model_df[i+1] <- temp_ts[(s+1-i):(k-i)]
    colnames(model_df)[i+1] <- paste("y_k_",as.character(i), sep="")
    
  }
  return(na.omit(model_df))
}

create_model_df <- function(time_s, ma, N, p, q){
  
  temp_ts <- time_s[N:length(time_s)]
  k <- length(temp_ts)
  s <- max(p, q)

  model_df <- data.frame(temp_ts[(s+1):k] - ma[(s+1):k])
  colnames(model_df)[1] <- "y_k"
  if (p) {
  for (i in 1:p){

    model_df[i+1] <- temp_ts[(s+1-i):(k-i)]
    colnames(model_df)[i+1] <- paste("y_k_",as.character(i), sep="")
  
  }
  }
  if(q){
  for(i in 1:q){
    
    model_df[i+p+1] <- ma[(s+1-i):(k-i)]
    colnames(model_df)[i+p+1] <- paste("ma_k_",as.character(i), sep="")
  
  }
  }
  return(model_df)
}

pred_own <- function(fit, ma, p, q){
  s <- max(p, q)
  fit <- fit+ma[(s+2):length(ma)]
  sum_a <- ifelse(q == 1, 1, sum(sapply(2:(q+1), function(k) (1 - 2/(q+1))**(k-1))))
  for(j in 1:q){
    a <- ifelse(j==1 || q == 1, 1, (1 - 2/(q+1))**(j-1))
    fit <- fit + (a/sum_a)*ma[(q-j+1):(length(fit)-j+1)]
  }
  return(fit)
}

create_model_df_own <- function(time_s, ma, N, p, q){
  model_df <- data.frame(time_s[1:(length(time_s)-N-p-q+1)])
  colnames(model_df)[1] <- "y_k"
  for(i in 2:(p+1)){
    model_df[i] <- time_s[i:(length(time_s)-N-p-q+i)]
    colnames(model_df)[i] <- paste("y_k_",as.character(i-1), sep="")
  }
  
  for(i in 1:(p+1)){
    for(j in 1:(q+1)){
      a <- ifelse(j==1, 1, (2/j)**j)
      sum_a <- ifelse(j==1, 1, sum(sapply(2:(q+1), function(k) (2/k)**k)))
      model_df[i] <- model_df[i] - (a/sum_a)*ma[j:(length(time_s)-N-p-q+j)]
    }
  }
  return(model_df)
}

create_model_df <- function(time_s, ma, N, p, q){
model_df <- data.frame(time_s[1:(length(time_s)-N-p-q+1)] - ma[1:(length(time_s) - N- p- q+1)])
colnames(model_df)[1] <- "y_k"
for (i in 2:(p+1)){
    model_df[i] <- time_s[i:(length(time_s)-N-p-q+i)]-ma[i:(length(time_s)-N-p-q+i)]
    colnames(model_df)[i] <- paste("y_k_",as.character(i-1), sep="")
  }

 for(i in 2:(q+1)){
    model_df[i+p] <- ma[i:(length(time_s)-N-p-q+i)]
    colnames(model_df)[i+p] <- paste("ma_k_",as.character(i-1), sep="")
  }
  return(model_df)
}
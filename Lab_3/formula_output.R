formula_1 <- function(model){
  coef_gen <- "$y(k)="
  coefs <- model$coefficients
  for (i in 1:length(coefs)){
    if (i == 1) {
      coef_gen <- paste(coef_gen, as.character(round(coefs[i], 4)))}
    else if (i==2) {
      coef_gen <- paste(coef_gen, ifelse(coefs[i]>0, "+", ""),
      as.character(round(coefs[i], 4)), "y(k-", as.character(i-1), ")")}
    else {
      if(i%%3 == 0){
        if(i == 3)
          coef_gen <- paste(coef_gen,"+ma(k)$\n$", ifelse(coefs[i]>0, "+", ""), as.character(round(coefs[i], 4)),
                            "ma(k-", as.character(i-2),
                            ")")
        else coef_gen <- paste(coef_gen,"$\n$",ifelse(coefs[i]>0, "+", ""), as.character(round(coefs[i], 4)),
                               "ma(k-", as.character(i-2),
                               ")")
      }
      
      else coef_gen <- paste(coef_gen,ifelse(coefs[i]>0, "+", ""), as.character(round(coefs[i], 4)),
                             "ma(k-", as.character(i-2),
                             ")")}
  }
  coef_gen <- paste(coef_gen, "$")
  cat(coef_gen)
}

formula_2 <- function(model){
  coef_gen <- "$y(k)="
  coefs <- model$coefficients
  for (i in 1:length(coefs)){
    if (i == 1) {
      coef_gen <- paste(coef_gen, as.character(round(coefs[i], 4)))}
    else if (i==2) {
      coef_gen <- paste(coef_gen,ifelse(coefs[i]>0, "+", ""),
                        as.character(round(coefs[i], 4)), "y(k-", as.character(i-1),
                        ")")}
  }
  coef_gen <- paste(coef_gen, "+ma(k)+ma(k-1)$")
  cat(coef_gen)
  
}

formula_1(model_2_ma_10_exp.ts_2)
formula_2(model_2_ma_10_exp_own.ts_2)

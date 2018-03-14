################
## REGRESSION ##
################

create_preperiod <- function(reg_data) {
  reg_data[,preperiod:=T]#sample(.N)>.N*.2]
  return(reg_data)
}

fit_regregression <- function(reg_data) {
  reg_vars <- paste(REGRESSION_FEATURES, collapse="+")
  reg_formula <- paste("kilowatt_hours ~", reg_vars)
  fit <- lm(data=reg_data[preperiod==T], reg_formula) ### What about incomplete lead lags??
  return(fit)
}

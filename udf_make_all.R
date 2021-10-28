udf_make_all <- function(df1, df2, yrs_train = 47, yrs_test = 2, ln = F, s = 12){
  
  data <- udf_make_data(df1, df2, yrs_train = yrs_train, yrs_test = yrs_test, ln = ln)
  
  models <- udf_make_models(data[[2]], data[[3]], data[[4]], data[[5]], s = s, ln = ln, plotAR = F)
  
  return(models)
  
  
}
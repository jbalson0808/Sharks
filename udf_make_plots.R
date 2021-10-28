udf_make_plots <- function(df, forecast, yrs_train, title){
  
  data <- udf_make_data(df, yrs_train = yrs_train)
  data <- data[[1]]
  
  #only show 5 year training window plus 2 year forecast regardless of years trained for modeling
  data <- data[(length(data) - 84 + 1): length(data)]
  forecast <- forecast[(length(forecast) - 84 + 1): length(forecast)]

  plot(data,main=title,ylim=c(0,25), ylab='Shark Attacks',type='o', xlab='Months')
  points(forecast,type='o',lty=1,lwd=3,col=2,cex=.6)
  
}
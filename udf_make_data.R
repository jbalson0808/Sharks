udf_make_data <- function(df1, df2 = NULL, yrs_train = 47, yrs_test = 2, ln = F){
  
  # input 2 data frames with 3 columns: year, month, volume. 2nd data set optional
  # returns list of 6 ts objects: full data set, training, and test
  # ln = True will transform volume column using ln(x+1)
  
  months_train = yrs_train * 12
  months_test = yrs_test * 12
  months_total = months_train + months_test
  row_start = nrow(df1) - months_total + 1
  row_count = nrow(df1) * 1
  
  # df1
  df1 <- df1[row_start:row_count,1:length(df1)]
  
  if (ln == T){df1[,3] <- log1p(df1[,3])}  
  
  df1_train <- df1[1:months_train,1:length(df1)]
  df1_test <- df1[(months_train+1):nrow(df1),1:length(df1)]
  
  minyr_train <- min(df1_train$year)
  minyr_test <- min(df1_test$year)
  
  ts1 <- ts(df1[,3], frequency = 12, start = minyr_train)
  ts1_train <- ts(df1_train[,3], frequency = 12, start = minyr_train)
  ts1_test <- ts(df1_test[,3], frequency = 12, start = minyr_test)
  
  if (is.null(df2) == T){
    
    return(list(ts1,ts1_train,ts1_test))
    
  }
  
  else{
    
    # df2
    df2 <- df2[row_start:row_count,1:length(df2)]
    
    if (ln == T){df2[,3] <- log1p(df2[,3])}  
    
    df2_train <- df2[1:months_train,1:length(df2)]
    df2_test <- df2[(months_train+1):nrow(df1),1:length(df2)]
    
    ts2 <- ts(df2[,3], frequency = 12, start = minyr_train)
    ts2_train <- ts(df2_train[,3], frequency = 12, start = minyr_train)
    ts2_test <- ts(df2_test[,3], frequency = 12, start = minyr_test)
    
    df_list <- list( ts1, ts1_train, ts1_test  
                    ,ts2, ts2_train, ts2_test
                    )
    
    return(df_list)
  }
}
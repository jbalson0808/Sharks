    
 udf_get_all_ase <- function(){   
    forecast_type <- c('Seasonal','Stationary','SPN','VAR','MLP')
    
    #non-Log models
    i = 1
    ASE24_1 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_1 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_1 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_1,ASE12_1)  
    names(ASE_1)[3:4] <- c('ASE24','ASE12')
    
    i = 2
    ASE24_2 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_2 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_2 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_2,ASE12_2)  
    names(ASE_2)[3:4] <- c('ASE24','ASE12')
    
    i = 3
    ASE24_3 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_3 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_3 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_3,ASE12_3) 
    names(ASE_3)[3:4] <- c('ASE24','ASE12')
    
    i = 4
    ASE24_4 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_4 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_4 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_4,ASE12_4) 
    names(ASE_4)[3:4] <- c('ASE24','ASE12')
    
    i = 5
    ASE24_5 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_5 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_5 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_5,ASE12_5)  
    names(ASE_5)[3:4] <- c('ASE24','ASE12')
    
    i = 6
    ASE24_6 <- c(modelsAll[[i]][[2]],modelsAll[[i]][[6]],modelsAll[[i]][[13]],modelsAll[[i]][[17]],modelsAll[[i]][[21]])
    ASE12_6 <- c(modelsAll[[i]][[4]],modelsAll[[i]][[8]],modelsAll[[i]][[15]],modelsAll[[i]][[19]],modelsAll[[i]][[23]])
    ASE_6 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_6,ASE12_6) 
    names(ASE_6)[3:4] <- c('ASE24','ASE12')
    
    df_ASE <- rbind(ASE_1,ASE_2,ASE_3,ASE_4,ASE_5,ASE_6)
    df_ASE$model_type <- 'non-log'
    # df_ASE
    
    # log models
    i = 1
    ASE24_1 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_1 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_1 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_1,ASE12_1)  
    names(ASE_1)[3:4] <- c('ASE24','ASE12')
    
    i = 2
    ASE24_2 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_2 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_2 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_2,ASE12_2)  
    names(ASE_2)[3:4] <- c('ASE24','ASE12')
    
    i = 3
    ASE24_3 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_3 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_3 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_3,ASE12_3) 
    names(ASE_3)[3:4] <- c('ASE24','ASE12')
    
    i = 4
    ASE24_4 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_4 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_4 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_4,ASE12_4) 
    names(ASE_4)[3:4] <- c('ASE24','ASE12')
    
    i = 5
    ASE24_5 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_5 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_5 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_5,ASE12_5)  
    names(ASE_5)[3:4] <- c('ASE24','ASE12')
    
    i = 6
    ASE24_6 <- c(modelsAllLog[[i]][[2]],modelsAllLog[[i]][[6]],modelsAllLog[[i]][[13]],modelsAllLog[[i]][[17]],modelsAllLog[[i]][[21]])
    ASE12_6 <- c(modelsAllLog[[i]][[4]],modelsAllLog[[i]][[8]],modelsAllLog[[i]][[15]],modelsAllLog[[i]][[19]],modelsAllLog[[i]][[23]])
    ASE_6 <- data.frame(forecast_type, train_yrs = forecast_years[[i]], ASE24_6,ASE12_6) 
    names(ASE_6)[3:4] <- c('ASE24','ASE12')
    
    df_ASELog <- rbind(ASE_1,ASE_2,ASE_3,ASE_4,ASE_5,ASE_6)
    df_ASELog$model_type <- 'log'
    # df_ASELog
    
    df_ASEAll <- rbind(df_ASE,df_ASELog)
    df_ASEAll
    
    # write.csv(df_ASEAll,paste(path,'ts_model_ase.csv', sep=''), row.names=F)
    
}
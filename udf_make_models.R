udf_make_models <- function(ts1.train, ts1.test, ts2, ts2.train = NULL, s = 12, ln = F){
  
  ltrain <- length(ts1.train)
  ltrain1 <- ltrain + 1
  ltest <- length(ts1.test)
  lts <- ltrain + ltest
  
  ## non-stationary, seasonal
  seas_zeros <- s - 1

  seas_trans <- artrans.wge(ts1.train,phi.tr = c(rep(0,seas_zeros),1), plottr = TRUE)

  
  seas_aic <- aic.wge(seas_trans,p=0:16,q=0:4,type = "bic")
  # seas_aic <- aic.wge(seas_trans) #default values for speed

  f_seas <- fore.aruma.wge(ts1.train, s = 12, phi = seas_aic$phi, theta=seas_aic$theta, n.ahead = ltest, limits = F, plot=F)

  if (ln==T){
    f_seas$f <- expm1(f_seas$f)
    ts1.test <- expm1(ts1.test)
  }

  f_seas$f[f_seas$f<0] <- 0

  ASE_seas <- mean((ts1.test - f_seas$f)^2)
  ASE_seas12 <- mean((ts1.test[1:12] - f_seas$f[1:12])^2)

  ts.sharks.fc.seas <- ts(data.frame(forecast = c(rep(NA,ltrain),f_seas$f)), frequency = 12, start = time(ts1.train)[1])


  ## stationary
  stat_aic <- aic.wge(ts1.train,p=0:16,q=0:4,type = "bic")
  # stat_aic <- aic.wge(ts1.train) #default values for speed

  f_stat <- fore.arma.wge(ts1.train, phi = stat_aic$phi, theta=stat_aic$theta, n.ahead = ltest, limits = F, plot=F)

  if (ln==T){
    f_stat$f <- expm1(f_stat$f)
  }

  f_stat$f[f_stat$f<0] <- 0

  ASE_stat <- mean((ts1.test - f_stat$f)^2)
  ASE_stat12 <- mean((ts1.test[1:12] - f_stat$f[1:12])^2)

  ts.sharks.fc.stat <- ts(data.frame(forecast = c(rep(NA,ltrain),f_stat$f)), frequency = 12, start = time(ts1.train)[1])

  ## sig plus noise (SPN)
  t <- seq(1,length(ts1.train),1)

  df_tmp <- data.frame(x=ts1.train, t=t)

  spn_fit <- lm(x~t, data=df_tmp)

  spn_cfit <- cochrane.orcutt(spn_fit)

  t.all <- seq(1,lts,1)

  spn_fit_y1 <- spn_fit$coefficients[2] * t.all + spn_fit$coefficients[1]

  spn_aic <- aic.wge(spn_fit$residuals,p=0:16,q=0:4,type = "bic")
  # spn_aic <- aic.wge(spn_fit$residuals) #default values for speed
  
  f_noise <- fore.arma.wge(spn_fit$residuals,phi = spn_aic$phi, theta = spn_aic$theta, n.ahead = length(ts1.test), limits = F, plot=F)
  
  f_noise_y1 <- f_noise$f
  
  if (ln==T){
    spn_fit_y1 <- expm1(spn_fit_y1)
    f_noise_y1 <- expm1(f_noise_y1)
  }

  spn_fit_y2 <- spn_fit_y1 + c(rep(0,ltrain), f_noise_y1)
  spn_fit_y2[spn_fit_y2<0] <- 0
  spn_fit_test <- spn_fit_y2[ltrain1:lts]

  ASE_spn <- mean((ts1.test - spn_fit_test)^2)
  ASE_spn12 <- mean((ts1.test[1:12] - spn_fit_test[1:12])^2)

  ts.sharks.fc.spn <- ts(data.frame(forecast = c(rep(NA,ltrain), spn_fit_y2[(length(ts1.train)+1):length(t.all)]))
                         , frequency = 12, start = time(ts1.train)[1])
  
  if (is.null(ts2) == T){

    f_list = list(
      # seasonal
      seas_aic, ASE_seas, ts.sharks.fc.seas, ASE_seas12
      # stationary
      ,stat_aic, ASE_stat, ts.sharks.fc.stat, ASE_stat12
      # spn
      ,spn_fit, spn_cfit, spn_aic
      ,spn_fit_y1, ASE_spn, ts.sharks.fc.spn, ASE_spn12
    )

    return(f_list)
    
  }

  else{
    
    ## VAR
    VAR <- VAR(cbind(ts1.train,ts2.train),lag.max = ltest, type = "both")
    
    pred <- predict(VAR,n.ahead = ltest)
    
    f_var <- pred$fcst$ts1.train[,1]
    
    if (ln==T){
      f_var <- expm1(f_var)
    }
    
    f_var[f_var<0] <- 0
    
    ASE_VAR <- mean((ts1.test - f_var)^2)
    ASE_VAR12 <- mean((ts1.test[1:12] - f_var[1:12])^2)
    
    ts.sharks.fc.var <- ts(data.frame(forecast = c(rep(NA,ltrain),f_var)), frequency = 12, start = time(ts1.train)[1])
    
    # ## MLP
    set.seed(8)

    mlp_fit <- mlp(ts1.train, comb = "mean", xreg = data.frame(ice.cream = ts2.train)) #higher reps make long run times

    f_mlp <- forecast(mlp_fit, h = 24, xreg = data.frame(ice.cream = ts2))

    if (ln==T){
      f_mlp$mean <- expm1(f_mlp$mean)
    }
    
    f_mlp$mean[f_mlp$mean<0] <- 0

    ASE_MLP <- mean((ts1.test - f_mlp$mean)^2)
    ASE_MLP12 <- mean((ts1.test[1:12] - f_mlp$mean[1:12])^2)

    ts.sharks.fc.mlp <- ts(data.frame(forecast = c(rep(NA,ltrain),f_mlp$mean)), frequency = 12, start = time(ts1.train)[1])

    f_list = list(
      # seasonal
      seas_aic, ASE_seas, ts.sharks.fc.seas, ASE_seas12
      # stationary
      ,stat_aic, ASE_stat, ts.sharks.fc.stat, ASE_stat12
      # spn
      ,spn_fit, spn_cfit, spn_aic
      ,spn_fit_y1, ASE_spn, ts.sharks.fc.spn, ASE_spn12
      # VAR
      ,pred, ASE_VAR, ts.sharks.fc.var, ASE_VAR12
      # # MLP
      ,mlp_fit, ASE_MLP, ts.sharks.fc.mlp, ASE_MLP12
    )

    return(f_list)

  }
  
  on.exit(options(warn = oldw))
  
}
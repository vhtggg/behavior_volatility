
# library -----------------------------------------------------------------
library(RODBC)
library(dplyr)
library(svDialogs)

setwd(choose.dir())


# import ------------------------------------------------------------------
casa <- read.csv(choose.files(), stringsAsFactors = F)

names(casa) <- c("date", "balance")
casa <- casa[order(casa$date),]

casa$date <- as.Date(casa$date)



# volatility --------------------------------------------------------------
splitz <- svDialogs::dlg_input(
  "Split date (first day of backtest data, mm/dd/yyyy): ",
  if (exists("splitz")) strftime(splitz, "%m/%d/%Y") else strftime(max(casa$date) - 364, "%m/%d/%Y")
)$res %>% as.Date("%m/%d/%Y")

casa_study <- casa[casa$date < splitz,]
casa_test <- casa[casa$date >= splitz,]


bucket <- c(1, 7, 15, 30, 61, 92, 184, 272, 365, 549, 731, 1095)
names(bucket) <- c("Daily", "2-7 days", "8-15 days", "16-30 days",
                   "M2", "M3", "M4-6", "M7-9", "M10-12", "M13-18", "M18-24", "Y2-3")

conf_lvl <- strsplit(
  svDialogs::dlg_input("Confidence level: ", "0.99")$res, 
  split = ","
)[[1]] %>% as.numeric()

all_result <- lapply(conf_lvl, function(conf_lvl){
  
  ##### study data #####
  runoff <- lapply(bucket, function(x){
    casa_study$balance[(1+x):nrow(casa_study)]/
      casa_study$balance[1:(nrow(casa_study)-x)]-1
  })
  
  all_runoff <- as.data.frame(names(bucket), stringsAsFactors = F)
  names(all_runoff) <- paste("confidence lvl = ", conf_lvl, sep = "")
  
  all_runoff$runoff <- sapply(runoff, function(x){
    -quantile(x, probs = 1-conf_lvl)
  })
  all_runoff$runoff[which(all_runoff$runoff < 0)] <- 0
  all_runoff$runoff[which(all_runoff$runoff > 1)] <- 1
  
  
  ##### test data #####
  runoff_test <- lapply(bucket, function(x){
    if (x > nrow(casa_test)) {
      NULL
    } else {
      if (x == nrow(casa_test)) {
        casa_test$balance[(0+x):nrow(casa_test)]/
          casa_test$balance[1:(nrow(casa_test)-x)]-1
      }else{
        casa_test$balance[(1+x):nrow(casa_test)]/
          casa_test$balance[1:(nrow(casa_test)-x)]-1
      }
    }
  })
  
  
  all_runoff$runoff_test <- sapply(runoff_test, function(x){
    -quantile(x, probs = 1-conf_lvl)
  })
  all_runoff$runoff_test[which(all_runoff$runoff_test < 0)] <- 0
  all_runoff$runoff_test[which(all_runoff$runoff_test > 1)] <- 1
  
  ##### backtest #####
  all_runoff$threshold <- sapply(1:length(bucket), function(x){
    round(qnorm(conf_lvl)*
            sqrt(length(runoff_test[[x]])*conf_lvl*(1-conf_lvl))+
            (1-conf_lvl)*length(runoff_test[[x]]), digit=0)
  }) 
  all_runoff$count <- sapply(1:length(bucket), function(x){
    sum(runoff_test[[x]] < -all_runoff$runoff[[x]])
  })
  
  all_runoff$backtest <- all_runoff$threshold >= all_runoff$count
  all_runoff$backtest[which(all_runoff$backtest==T)] <- "PASS"
  all_runoff$backtest[which(all_runoff$backtest==F)] <- "FAIL"
  
  
  ###### calibration #######
  all_runoff$calibrated_runoff <- sapply(1:length(bucket), function(x){
    if (all_runoff$backtest[[x]] == "PASS" ) {
      all_runoff$runoff[[x]]
    } else{
      all_runoff$runoff_test[[x]]
    }
  })
  
  ##### result #####
  a <- all_runoff$calibrated_runoff
  b <- all_runoff$calibrated_runoff
  for (i in (2:length(bucket))) {
    if (b[i]>max(na.omit(a[1:i-1]))){
      b[i] <- a[i] - max(na.omit(a[1:(i-1)]))
    } else {
      b[i] = 0
    }
  }
  
  all_runoff$uncumulative_runoff <- b
  
  all_runoff$core <- ""
  all_runoff$core[[1]] <- 1- sum(all_runoff$uncumulative_runoff)
  all_runoff
})

for (i in 1:length(conf_lvl)) {

  write.csv(
    all_result[[i]],
    choose.files(
      default = paste(
        "volatility",
        crc,clt,own,atd,
        conf_lvl[[i]],".csv"
      )
    ), row.names = F
  )

}



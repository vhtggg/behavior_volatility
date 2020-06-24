# library -----------------------------------------------------------------
library(RODBC)
library(dplyr)
library(svDialogs)


setwd(choose.dir())
# myconn <-odbcConnect(dlg_input("DSN: ")$res,
#                      uid = dlg_input("UID: ")$res, 
#                      pwd = dlg_input("PASSWORD: ")$res, 
#                      believeNRows=FALSE)
# 


# import ------------------------------------------------------------------
macro <- read.csv(choose.files(), stringsAsFactors = F)
macro$date <- as.Date(macro$date, "%Y-%m-%d")



casa <- read.csv(choose.files(), stringsAsFactors = F)


names(casa) <- c("date", "balance")

casa$date <- as.Date(casa$date)
casa <- casa[order(casa$date),]

casa <- cbind.data.frame(casa$date[2:nrow(casa)], 
                         diff(casa$balance)/casa$balance[1:(nrow(casa)-1)])
names(casa) <- c("date", "change")

casa$date <- as.Date(casa$date)+1
casa <- casa %>% filter(
  date >= as.Date(
    dlg_input(
      message = "CASA start date (yyyy/mm/dd): ",
      default = "2014-01-01"
    )$res
  )  && date <= as.Date(
    dlg_input(
      message = "CASA end date (yyyy/mm/dd): ",
      default = "2018-12-31"
    )$res
  )
)


# correlation -------------------------------------------------------------
lag <- dlg_input(message = "correlation lag: ", default = "90") %>% as.numeric()
pb <- txtProgressBar(min = 1, max = (lag+1)*(length(macro)-1), style = 3)

correlation <- sapply(2:length(macro), function(y){
  sapply(0:lag, function(x){
    a <- merge(casa, macro[c(1,y)], by = 1, all.x = T)
    a <- a[complete.cases(a),]
    n <- nrow(a)
    
    setTxtProgressBar(pb, (y-2)*(lag+1)+(x+1))
    
    if (n > x) cor(a[[2]][(1+x):n], a[[3]][1:(n-x)]) 
    else NULL
  })
}) %>% as.data.frame()

row.names(correlation) <- 0:lag
names(correlation) <- names(macro)[2:length(macro)]



# result + export ---------------------------------------------------------

result <- 0

for (column in 1:ncol(correlation)){
  for (row in 1:nrow(correlation)){
    if (correlation[row,column] > 0.4 || correlation[row,column] < -0.4) result <- result + 1
  }
}

svDialogs::dlg_message(paste(
  if (result == 0) "no correlation" else paste(result, "variable(s) with lag correlated")
))

write.csv(correlation, choose.files(), row.names = F)








library(dplyr)
library(svDialogs)
library(readxl)
library(zoo)


# import + format ---------------------------------------------------------
macro <- read.csv(choose.files(), stringsAsFactors = F)

names(macro)[[1]] <- "date"
macro$date <- as.Date(macro$date)



macro <- merge.data.frame(
  data.frame(
    date = seq(min(macro$date), max(macro$date), by = "day")
  ),
  macro,
  all = T, by = 1
)

name_macro <- names(macro)



# interpolation -----------------------------------------------------------
macro <- cbind.data.frame(
  macro$date,
  as.data.frame(
    sapply(2:length(macro), function(x){
      a <- macro[c(1,x)]
      a[complete.cases(a),]
      approx(x = a[[1]], y = a[[2]], method = "linear", xout = all[[1]])$y
    })
  )
) 
names(macro) <- name_macro



# forwardfill -------------------------------------------------------------
macro <- sapply(macro, function(x){
  na.locf(x,na.rm = F)
}) %>% as.data.frame()

names(macro) <- name_macro

# forwardfill -------------------------------------------------------------
macro <- sapply(macro, function(x){
  na.locf(x,na.rm = F, fromLast = T)
}) %>% as.data.frame()

names(macro) <- name_macro


# export ------------------------------------------------------------------
write.csv(
  macro, 
  choose.files(
    default = "macro.csv",
    caption = "SAVE MACRO FILE AS CSV"
  )
)















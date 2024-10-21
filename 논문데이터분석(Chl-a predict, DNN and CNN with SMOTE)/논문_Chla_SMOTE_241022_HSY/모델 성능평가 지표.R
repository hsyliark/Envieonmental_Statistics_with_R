res_SC <- read.csv("C:/Users/User/Desktop/df_SC_res_ver2.csv",
                   sep=",",header=T)
res_JS <- read.csv("C:/Users/User/Desktop/df_JS_res_ver2.csv",
                   sep=",",header=T)

RMSE <- function(y, yhat) {sqrt(sum((y-yhat)^2)/length(y))}
MAE <- function(y, yhat) {sum(abs(y-yhat))/length(y)}
MAPE <- function(y, yhat) {sum(abs((y-yhat)/y))*100/length(y)}
NMB <- function(y, yhat) {100*sum((y-yhat)/y)}
IOA <- function(y, yhat) {1-(sum((y-yhat)^2))/(sum((abs(yhat-mean(y))+abs(y-mean(y)))^2))} 

IOA(res_SC$Chla, res_SC$LSTM)
IOA(res_JS$Chla, res_JS$LSTM)

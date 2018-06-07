
#pre-requisito
#####
if (!require("pacman")) install.packages("pacman")
pacman::p_load('Quandl', 'devtools', 'xts','zoo','quantmod','readr','dplyr',"quadprog") #load/install packages
# Get your API key from quandl.com
quandl_api = "hKvQo_m3Xtemy_Hug787"
# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)
quandl_get <-
  function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8"),  #  Adj. Open
      paste0("WIKI/", sym, ".9"),  # Adj. High
      paste0("WIKI/", sym, ".10"), # Adj. Low
      paste0("WIKI/", sym, ".11"), # Adj. Close
      paste0("WIKI/", sym, ".12")), # Adj. Volume
      start_date = start_date,
      type = "zoo"
    ))
  } #funcao download stock especifica
#https://www.kaggle.com/camnugent/sandp500 url para download SP500
options(scipen=999) #opcional
#####





stocks <- read_csv("all_stocks_5yr.csv")  


head(stocks)

stocks$Name <- as.factor(stocks$Name)
levels(stocks$Name)[2:7]

getStock <- function(db,name){
  df <- filter(db,Name == name)
  df$return <- Delt(df$close)
  return(df)
}


aal <- getStock(stocks,'AAL')
aapl <- getStock(stocks,'AAPL')
ppg <- getStock(stocks,'PPG')
utx <- getStock(stocks,'UTX')
zts <- getStock(stocks,'ZTS')
tap <- getStock(stocks,'TAP')
tgt <- getStock(stocks,'TGT')
oxy <- getStock(stocks,'OXY')


stock <- data.frame(aal$return,aapl$return,ppg$return,utx$return,zts$return,tap$return,tgt$return,oxy$return)[-1,]
colnames(stock) = c('AAL','AAPL','PPG','UTX','ZTS','TAP','TGT','OXY')
means <- sapply(stock,mean)
matrix_cov <- as.matrix(cov(stock[-1,]))


Dmat <- 2*matrix_cov
dvec <- rep(0,8)
Amat <- matrix(c(means,-means,rep(1,8),rep(-1,8),diag(length(means))),8,12)

# compute efficient frontier for eight stocks
varP=vector()
sigmaP=vector()
w1=vector()
w2=vector()
w3=vector()
w4=vector()
w5=vector()
w6=vector()
w7=vector()
w8=vector()

#Expected Returns 20 values
Rvals=seq(min(means)+0.1^10,max(means)-0.1^10,length.out=20);


for (i in 1:length(Rvals)) {
  R=Rvals[i]
  bvec <- c(R,-R,1,-1,0,0,0,0,0,0,0,0)
  qpSol=solve.QP(Dmat,dvec,Amat,bvec)
  varP[i]=qpSol$value
  sigmaP[i]=sqrt(varP[i])
  w1[i]=qpSol$solution[1];
  w2[i]=qpSol$solution[2];
  w3[i]=qpSol$solution[3];
  w4[i]=qpSol$solution[4];
  w5[i]=qpSol$solution[5];
  w6[i]=qpSol$solution[6];
  w7[i]=qpSol$solution[7];
  w8[i]=qpSol$solution[8];
}

#Portfolio weights
weightsoutput<-data.frame(w1,w2,w3,w4,w5,w6,w7,w8)
#Checking whether summation is equal to one
weightsum<-apply(weightsoutput,1,sum)

#Efficient frontier Plot 
plot(sigmaP,Rvals,type = 'l',lty = 1,lwd=3, xlab = 'Risk',ylab = 'Returns', main = 'Simple mean variance method',col = 'blue')
  
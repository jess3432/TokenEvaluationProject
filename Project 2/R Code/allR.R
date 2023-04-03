library(ggplot2)
library(dplyr)
library(tidyr)
library(grf)
library(reshape)
library(pwr)
library(effectsize)
library(readxl)

#-------------------------------Metcalfe Model----------------------------------

BTC<-read.csv("C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/peoject2/Bitcoin.csv")

head(BTC$active_addresses_count,10)
head(BTC$Price..Open.,10)
colnames(BTC)
# BTC <- BTC %>% drop_na(all_of(BTC))
# BTC <- as.numeric(BTC)
BTC$active_addresses_count1.5<-BTC$active_addresses_count^1.5
BTC$active_addresses_count_log<-BTC$active_addresses_count*log(BTC$active_addresses_count)
BTC$active_addresses_count2<-BTC$active_addresses_count^2


# model1:NV = C*n^1.5
BTCModelMetcalfe_1<-lm(BTC$Price~BTC$active_addresses_count1.5)
summary(BTCModelMetcalfe_1)

# model2:NV = C*n^log(n)
BTCModelMetcalfe_2<-lm(BTC$Price~BTC$active_addresses_count_log)
summary(BTCModelMetcalfe_2)

# model3:NV = C*n^2
BTCModelMetcalfe_3<-lm(BTC$Price~BTC$active_addresses_count2)
summary(BTCModelMetcalfe_3)


plot(BTC$Price..Open.~BTC$Active.Addresses.Count, main="Scatterplot for BTC",
     xlab="Number of Active Addresses", ylab="Price of BTC")
lines(BTC$Active.Addresses.Count,predict(BTCModelMetcalfe,data.frame(x=seq(0,365))),col="limegreen")

# ETH 
ETH<-read.csv("C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/peoject2/Ethereum.csv")
head(ETH$Active.Addresses.Count,10)
head(ETH$Price,10)
colnames(ETH)
ETH$Active.Addresses.Count1.5<-ETH$Active.Addresses.Count^1.5
ETH$Active.Addresses.Count_log<-ETH$Active.Addresses.Count*log(ETH$Active.Addresses.Count)
ETH$Active.Addresses.Count2<-ETH$Active.Addresses.Count^2

# model1:NV = C*n^1.5
ETHModelMetcalfe<-lm(ETH$Price~ETH$Active.Addresses.Count1.5)
summary(ETHModelMetcalfe)

# model2:NV = C*n^log(n)
ETHModelMetcalfe_2<-lm(ETH$Price~ETH$Active.Addresses.Count_log)
summary(ETHModelMetcalfe_2)

# model3:NV = C*n^2
ETHModelMetcalfe_3<-lm(ETH$Price~ETH$Active.Addresses.Count2)
summary(ETHModelMetcalfe_3)

plot(ETH$Price~ETH$Active.Addresses.Count, main="Scatterplot for ETH(formular2)",
     xlab="Number of Active Addresses", ylab="Price of ETH")
lines(ETH$Active.Addresses.Count,predict(ETHModelMetcalfe_2,data.frame(x=seq(0,365))),col="steelblue")

# BNB 
BNB<-read.csv("C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/peoject2/Binance.csv")
head(BNB$Active.Addresses.Count,10)
head(BNB$Price..Open.,10)
colnames(BNB)
BNB$Active.Addresses.Count1.5<-BNB$Active.Addresses.Count^1.5
BNB$Active.Addresses.Count_log<-BNB$Active.Addresses.Count*log(BNB$Active.Addresses.Count)
BNB$Active.Addresses.Count2<-BNB$Active.Addresses.Count^2

# model1:NV = C*n^1.5
BNBModelMetcalfe_1<-lm(BNB$Price..Open.~BNB$Active.Addresses.Count1.5)
summary(BNBModelMetcalfe_1)

# Call:
#   lm(formula = BNB$Price..Open. ~ BNB$Active.Addresses.Count1.5)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -579.25  -91.91   -9.45   53.95  420.57 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   1.326e+02  5.905e+00   22.46   <2e-16 ***
#   BNB$Active.Addresses.Count1.5 2.127e-07  5.657e-09   37.61   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 104.2 on 791 degrees of freedom
# (1032 observations deleted due to missingness)
# Multiple R-squared:  0.6413,	Adjusted R-squared:  0.6408 
# F-statistic:  1414 on 1 and 791 DF,  p-value: < 2.2e-16

# model2:NV = C*n^log(n)
BNBModelMetcalfe_2<-lm(BNB$Price..Open.~BNB$Active.Addresses.Count_log)
summary(BNBModelMetcalfe_2)
# Call:
#   lm(formula = BNB$Price..Open. ~ BNB$Active.Addresses.Count_log)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -472.29  -64.86  -19.79   48.12  399.74 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    9.141e+01  5.904e+00   15.48   <2e-16 ***
#   BNB$Active.Addresses.Count_log 1.982e-05  4.509e-07   43.95   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 93.77 on 791 degrees of freedom
# (1032 observations deleted due to missingness)
# Multiple R-squared:  0.7095,	Adjusted R-squared:  0.7091 
# F-statistic:  1932 on 1 and 791 DF,  p-value: < 2.2e-16


# model3:NV = C*n^2
BNBModelMetcalfe_3<-lm(BNB$Price..Open.~BNB$Active.Addresses.Count2)
summary(BNBModelMetcalfe_3)
# Call:
#   lm(formula = BNB$Price..Open. ~ BNB$Active.Addresses.Count2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -677.01  -88.41    6.43   62.98  430.97 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 1.713e+02  5.947e+00   28.80   <2e-16 ***
#   BNB$Active.Addresses.Count2 1.535e-10  4.893e-12   31.37   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 116.1 on 791 degrees of freedom
# (1032 observations deleted due to missingness)
# Multiple R-squared:  0.5543,	Adjusted R-squared:  0.5538 
# F-statistic: 983.9 on 1 and 791 DF,  p-value: < 2.2e-16


plot(BNB$Price..Open.~BNB$Active.Addresses.Count, main="Scatterplot for BNB(formular2)",
     xlab="Number of Active Addresses", ylab="Price of BNB")
lines(BNB$Active.Addresses.Count,predict(BNBModelMetcalfe_2,data.frame(x=seq(0,365))),col="gold")







#-------------------------Relative Evaluation Model-----------------------------


#Model 4 Relative Evaluation 
library(lubridate)
library(ggplot2)
library(sentimentr)
library(timetk)
library(xts)
library(dplyr)
library(tidymv)
library(readxl)
library(reshape2)



#----------------------------------Loading Trading Data-------------------------

BTC<-read_excel("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/CryptoRegressionDataVersion2.xlsx", 
                sheet = "Bitcoin")
BTC$Time <- as.Date(BTC$Time , format = "%m/%d/%y") 
BTC[order(BTC$Time ),]

ETH<-read_excel("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/CryptoRegressionDataVersion2.xlsx", 
                sheet = "Ethereum")
ETH$Time <- as.Date(ETH$Time , format = "%m/%d/%y") 
ETH[order(ETH$Time ),]

BNB<-read_excel("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/CryptoRegressionDataVersion2.xlsx",
                sheet = "Binance")
colnames(BNB)
BNB$Time <- as.Date(BNB$Time , format = "%m/%d/%y") 
BNB[order(BNB$Time ),]


#----------------------------------Analyzing Reddit Posts-----------------------


#Analyzing Reddit Post Data Sentiment - BTC
BTC_comments <- read.csv("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/BTCRedditPosts.csv")
BTCcomments_sentiment <- sentiment(BTC_comments$title)
BTC_comments$created_utc <- as.Date(as.POSIXct(BTC_comments$created_utc, origin="1970-01-01"))
names(BTC_comments)[1] <- 'Date'
BTCcomments_sentiment = subset(BTCcomments_sentiment, select = c(element_id,sentiment))
BTCcomments_sentiment  <- aggregate(sentiment ~ element_id, data=BTCcomments_sentiment, FUN=mean)
BTCcommentsandsent <- cbind(Date=BTC_comments$Date, BTC=BTCcomments_sentiment)
BTCcommentsandsent<-BTCcommentsandsent[ order(BTCcommentsandsent$Date , decreasing = FALSE ),]
BTCReddit<-aggregate( BTC.sentiment ~ Date , BTCcommentsandsent , mean )
head(BTCReddit)
write.table(BTCcommentsandsent,file="BTCreddit_sentimentr.csv", na="", sep=",", row.names = FALSE)
BTC<-left_join(BTC, BTCReddit, by = c("Date" = "Date"))
head(BTC,500)
write.table(BTC,file="BTCData.csv", na="", sep=",", row.names = FALSE)
#BTCData<- read.csv("C:/Users/jessi/Documents/BTCData.csv")

#Analyzing Reddit Post Data Sentiment - ETH
ETH_comments <- read.csv("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/ETHRedditPosts.csv")
ETHcomments_sentiment <- sentiment(ETH_comments$title)
ETH_comments$created_utc <- as.Date(as.POSIXct(ETH_comments$created_utc, origin="1970-01-01"))
names(ETH_comments)[1] <- 'Date'
ETHcomments_sentiment = subset(ETHcomments_sentiment, select = c(element_id,sentiment))
ETHcomments_sentiment  <- aggregate(sentiment ~ element_id, data=ETHcomments_sentiment, FUN=mean)
ETHcommentsandsent <- cbind(Date=ETH_comments$Date, ETH=ETHcomments_sentiment)
ETHcommentsandsent<-ETHcommentsandsent[ order(ETHcommentsandsent$Date , decreasing = FALSE ),]
ETHReddit<-aggregate( ETH.sentiment ~ Date , ETHcommentsandsent , mean )
head(ETHReddit)
write.table(ETHcommentsandsent,file="ETHreddit_sentimentr.csv", na="", sep=",", row.names = FALSE)
ETH<-left_join(ETH, ETHReddit, by = c("Date" = "Date"))
head(ETH$ETH.sentiment,1000)
write.table(ETH,file="ETHData.csv", na="", sep=",", row.names = FALSE)
ETHData<- read.csv("C:/Users/jessi/Documents/ETHData.csv")

#Analyzing Reddit Post Data Sentiment - BNB
BNB_comments <- read.csv("C:/Users/jessi/Documents/MSBUAN/FINTECH/Project2Evaluation/BNBRedditPosts.csv")
BNBcomments_sentiment <- sentiment(BNB_comments$title)
BNB_comments$created_utc <- as.Date(as.POSIXct(BNB_comments$created_utc, origin="1970-01-01"))
names(BNB_comments)[1] <- 'Date'
BNBcomments_sentiment = subset(BNBcomments_sentiment, select = c(element_id,sentiment))
BNBcomments_sentiment  <- aggregate(sentiment ~ element_id, data=BNBcomments_sentiment, FUN=mean)
BNBcommentsandsent <- cbind(Date=BNB_comments$Date, BNB=BNBcomments_sentiment)
BNBcommentsandsent<-BNBcommentsandsent[ order(BNBcommentsandsent$Date , decreasing = FALSE ),]
BNBReddit<-aggregate( BNB.sentiment ~ Date , BNBcommentsandsent , mean )
write.table(BNBcommentsandsent,file="BNBreddit_sentimentr.csv", na="", sep=",", row.names = FALSE)
BNB<-left_join(BNB, BNBReddit, by = c("Date" = "Date"))
head(BNB,100)
tail(BNB,100)
write.table(BNB,file="BNBData.csv", na="", sep=",", row.names = FALSE)
#BNBData<- read.csv("C:/Users/jessi/Documents/BNBData.csv")



#----------------------------------Creating Models------------------------------
colnames(BTC)
BTCModel<-lm(`Return+1`~Volume+Market.Cap+NVT+Volatility+Supply.on.Exchanges+Adjusted.Transaction.Volume+
               Transactions.Count+Sharpe.Ratio+Active.Addresses.Count+Reddit.Subscribers+Reddit.Active.Users+
               BTC.sentiment+Total.Fees+Bitcoin.Avg.TPS+Bitcoin.Unique.Addresses.Used+Bitcoin.Avg.Block.Size.inMB+
               Bitcoin.Total.Hashrate.inTHperSecond+Bitcoin.Miner.Revenue.inUSD+Bitcoin.Avg.Transaction.Fee.inUSD+
               Difficulty.inTrillions,data=BTC)
summary(BTCModel)
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.087226 -0.015757 -0.001036  0.015183  0.093632 
# 
# Coefficients:
#                                       Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)                          -1.125e-01  2.443e-02  -4.604 4.97e-06 ***
# Volume                               -2.592e-12  3.982e-13  -6.510 1.49e-10 ***
# Market.Cap                           -7.140e-13  3.140e-14 -22.739  < 2e-16 ***
# NVT                                   3.445e-05  5.530e-05   0.623 0.533555    
# Volatility                            1.303e-02  6.861e-03   1.898 0.058071 .  
# Supply.on.Exchanges                   8.594e-12  3.649e-13  23.548  < 2e-16 ***
# Adjusted.Transaction.Volume           1.376e-12  3.603e-13   3.819 0.000147 ***
# Transactions.Count                    3.757e-08  6.935e-08   0.542 0.588193    
# Sharpe.Ratio                          3.159e-03  3.265e-04   9.675  < 2e-16 ***
# Active.Addresses.Count                5.334e-09  2.652e-08   0.201 0.840692    
# Reddit.Subscribers                   -6.614e-09  4.150e-09  -1.594 0.111447    
# Reddit.Active.Users                   1.459e-06  4.053e-07   3.600 0.000342 ***
# BTC.sentiment                         1.490e-02  1.053e-02   1.415 0.157608    
# Total.Fees                            1.731e-08  3.325e-09   5.207 2.57e-07 ***
# Bitcoin.Avg.TPS                       3.308e-03  3.419e-03   0.968 0.333622    
# Bitcoin.Unique.Addresses.Used         9.387e-08  1.581e-08   5.937 4.70e-09 ***
# Bitcoin.Avg.Block.Size.inMB          -2.322e-02  1.414e-02  -1.642 0.101143    
# Bitcoin.Total.Hashrate.inTHperSecond  3.451e-10  1.319e-10   2.616 0.009106 ** 
# Bitcoin.Miner.Revenue.inUSD          -3.550e-10  4.325e-10  -0.821 0.412054    
# Bitcoin.Avg.Transaction.Fee.inUSD    -3.531e-03  8.727e-04  -4.047 5.81e-05 ***
# Difficulty.inTrillions                8.649e-04  1.004e-03   0.862 0.389161    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02589 on 658 degrees of freedom
# (416 observations deleted due to missingness)
# Multiple R-squared:  0.535,	Adjusted R-squared:  0.5208 
# F-statistic: 37.85 on 20 and 658 DF,  p-value: < 2.2e-16

ggplot( data = BTC, aes( Date, Price..Open. )) +
  geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) +
  labs(title = "Bitcoin Price Over Time") +
  ylab("BTC Price (in USD)")

BTC$Active.Addresses.Count1.5<-BTC$Active.Addresses.Count^1.5
BTC_predictions<-predict(BTCModelMetcalfe1, BTC)

ggplot(data=BTC,aes(x=Date, y=BTC_predictions)) +
  geom_area(fill = rgb(0.2, 0.8, 0.75, alpha = 0.5)) +
  labs(title = "Bitcoin Predictions from Metcalfe Model Over Time") +
  ylab("BTC Price (in USD)")


#--------------------------------------ETH Model--------------------------------

colnames(ETH)
ETHModel<-lm(`Return+1`~Volume+Market.Cap+NVT+Volatility+Supply.on.Exchanges+Adjusted.Transaction.Volume+
               Transactions.Count+Sharpe.Ratio+Active.Addresses.Count+Reddit.Subscribers+Reddit.Active.Users+
               ETH.sentiment+Total.Fees+Ethereum.Avg.TPS+Ethereum.Unique.Addresses.Used+Ethereum.Avg.Block.Size.inMB+
               Ethereum.Total.Hashrate.inTHperSecond+Ethereum.Miner.Revenue.inUSD+Ethereum.Avg.Transaction.Fee.inUSD+
               Difficulty,data=ETH)
summary(ETHModel)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.16536 -0.02660 -0.00275  0.02256  0.22889 
# 
# Coefficients:
#                                        Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)                            2.644e-02  3.968e-02   0.666   0.5055    
# Volume                                -1.523e-12  9.199e-13  -1.656   0.0984 .  
# Market.Cap                            -9.045e-13  8.851e-14 -10.219  < 2e-16 ***
# NVT                                    5.216e-05  1.562e-04   0.334   0.7386    
# Volatility                            -1.183e-02  9.436e-03  -1.254   0.2106    
# Supply.on.Exchanges                    1.022e-11  1.042e-12   9.805  < 2e-16 ***
# Adjusted.Transaction.Volume            6.357e-13  4.947e-13   1.285   0.1994    
# Transactions.Count                    -5.609e-06  2.475e-06  -2.266   0.0239 *  
# Sharpe.Ratio                           2.237e-03  9.020e-04   2.481   0.0135 *  
# Active.Addresses.Count                -1.849e-08  4.641e-08  -0.398   0.6905    
# Reddit.Subscribers                     1.272e-07  4.423e-08   2.876   0.0042 ** 
# Reddit.Active.Users                   -3.941e-06  3.726e-06  -1.058   0.2907    
# ETH.sentiment                         -1.249e-02  1.372e-02  -0.910   0.3630    
# Total.Fees                            -1.881e-09  1.319e-09  -1.426   0.1546    
# Ethereum.Avg.TPS                       4.875e-01  2.138e-01   2.280   0.0230 *  
# Ethereum.Unique.Addresses.Used        -8.294e-10  3.868e-10  -2.144   0.0325 *  
# Ethereum.Avg.Block.Size.inMB          -9.889e-01  4.285e-01  -2.308   0.0214 *  
# Ethereum.Total.Hashrate.inTHperSecond -2.242e-06  4.253e-07  -5.271 2.04e-07 ***
# Ethereum.Miner.Revenue.inUSD          -1.164e-09  4.551e-10  -2.558   0.0108 *  
# Ethereum.Avg.Transaction.Fee.inUSD     4.122e-03  1.715e-03   2.404   0.0166 *  
# Difficulty                             1.758e-04  3.363e-05   5.228 2.55e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04581 on 487 degrees of freedom
# (587 observations deleted due to missingness)
# Multiple R-squared:  0.2592,	Adjusted R-squared:  0.2288 
# F-statistic: 8.519 on 20 and 487 DF,  p-value: < 2.2e-16

ggplot( data = ETH, aes( Date, Price..Open. )) +
  geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) +
  labs(title = "Ethereum Price Over Time")

ETH$Active.Addresses.Count1.5<-ETH$Active.Addresses.Count^1.5
ETH_predictions<-predict(ETHModelMetcalfe1, ETH)

ggplot(data=ETH,aes(x=Date, y=ETH_predictions)) +
  geom_area(fill = rgb(0.2, 0.8, 0.75, alpha = 0.5)) +
  labs(title = "Ethereum Predictions from Metcalfe Model Over Time") +
  ylab("ETH Price (in USD)")



#--------------------------------------BNB Model--------------------------------


colnames(BNB)
BNBModel<-lm(`Return+1`~Volume+Market.Cap+NVT+Volatility+Circulating.Supply+
               Sharpe.Ratio+Active.Addresses.Count+BNB.sentiment+Binance.Avg.TPS+
               Reddit.Subscribers+Reddit.Active.Users+Binance.Unique.Addresses.Used+
               Binance.Avg.Block.Size.inBytes,data=BNB)
summary(BNBModel)
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.226068 -0.034496  0.004214  0.029822  0.136898 
# 
# Coefficients:
#                                 Estimate   Std. Error t value Pr(>|t|)
# (Intercept)                    -3.932e-02  6.008e-01  -0.065    0.948
# Volume                          2.151e-12  1.451e-11   0.148    0.883
# Market.Cap                     -1.154e-12  1.867e-12  -0.618    0.540
# NVT                            -8.934e-04  6.764e-04  -1.321    0.194
# Volatility                     -2.511e-02  1.578e-02  -1.591    0.119
# Circulating.Supply              1.459e-09  4.002e-09   0.365    0.717
# Sharpe.Ratio                    1.269e-03  3.519e-03   0.361    0.720
# Active.Addresses.Count         -7.001e-08  5.904e-08  -1.186    0.243
# BNB.sentiment                   4.194e-02  5.657e-02   0.741    0.463
# Binance.Avg.TPS                 2.890e-04  9.654e-04   0.299    0.766
# Reddit.Subscribers              4.898e-08  1.440e-07   0.340    0.736
# Reddit.Active.Users            -5.663e-06  7.705e-06  -0.735    0.467
# Binance.Unique.Addresses.Used   4.185e-10  8.904e-10   0.470    0.641
# Binance.Avg.Block.Size.inBytes -8.768e-08  6.603e-07  -0.133    0.895
# 
# Residual standard error: 0.06572 on 41 degrees of freedom
# (738 observations deleted due to missingness)
# Multiple R-squared:  0.2439,	Adjusted R-squared:  0.004216 
# F-statistic: 1.018 on 13 and 41 DF,  p-value: 0.4536


ggplot( data = BNB, aes( Date, Price..Open. )) +
  geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) +
  labs(title = "Binance Coin Price Over Time") 

BNB$Active.Addresses.Count1.5<-BNB$Active.Addresses.Count^1.5
BNB_predictions<-predict(BNBModelMetcalfe1, BNB)

ggplot(data=BNB,aes(x=Date, y=BNB_predictions)) +
  geom_area(fill = rgb(0.2, 0.8, 0.75, alpha = 0.5)) +
  labs(title = "BNB Predictions from Metcalfe Model Over Time") +
  ylab("BNB Price (in USD)")

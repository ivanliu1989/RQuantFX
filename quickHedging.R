rm(list=ls());gc()
loadQuantPackages()
library(Quandl)
source("./R/notifications.R")
source("./R/machineLearning.R")
source("./R/utils.R")
source("./R/pricing.R")
Sys.setenv(TZ='Australia/Melbourne')

####################################################
### OANDA MEAN REVERSION STRATEGY -- Daily BASIS ###
####################################################
# Model Configurations ----------------------------------------------------
configs = list(
    ACCESS_TOKEN_PRICE = '' # Broker Token
    ,ACCOUNT_TYPE_PRICE = 'real' # Broker account type
    ,ACCOUNT_ID = '' # Broker account number
    ,pair1 = c('AUD', 'USD') # Currency Pair
    ,freq = 'S5' # Trading interval
)      


POS.AUDUSD = 1
aud.take.profit = 30 # pips
aud.stop.loss = 10 # pips
current.price = getLatestPrices(configs)

# Execution ---------------------------------------------------------------
# Long
aud.order.msg = createOandaOrder(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, 
                                 INSTRUMENTS = paste0(configs$pair1[1], '_', configs$pair1[2]), UNITS = POS.AUDUSD,
                                 ORDERTYPE = 'MARKET',
                                 TAKEPROFIT = current.price$prices.new + aud.take.profit * 0.0001, 
                                 STOPLOSS = current.price$prices.new - aud.stop.loss * 0.0001,
                                 TIMEINFORCE = "IOC")

if(aud.order.msg$status_code > 201) OandaSendAlert(type = "Go Long", pos = POS.AUDUSD)

# Short
aud.order.msg = createOandaOrder(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, 
                                 INSTRUMENTS = paste0(configs$pair1[1], '_', configs$pair1[2]), UNITS = -POS.AUDUSD,
                                 ORDERTYPE = 'MARKET',
                                 TAKEPROFIT = current.price$prices.new - aud.take.profit * 0.0001, 
                                 STOPLOSS = current.price$prices.new + aud.stop.loss * 0.0001,
                                 TIMEINFORCE = "IOC")

if(aud.order.msg$status_code > 201) OandaSendAlert(type = "Go Short", pos = POS.AUDUSD)

# Rolling Moving Average
MaRatio <- function(x, N){
    Mavg <- rollapply(x, N , mean)
    colnames(Mavg) <- 'Price.Ratio.MA'
    Mavg
}
# Rolling Standard Deviation
Sd <- function(x, N){
    Stand.dev <- rollapply(x, N, sd)
    colnames(Stand.dev) <- "Price.Ratio.SD"
    Stand.dev
}
# Z Score
ZScore <- function(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD){
    z <- (Price.Ratio-Price.Ratio.MA)/Price.Ratio.SD
    colnames(z)<- 'Z.Score'
    z
}
# ConvertToXts
convertToXts <- function(cm, tzone = ""){
    dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", cm[, 1], perl = TRUE))),
                     class = c("POSIXct","POSIXt"), tzone = tzone)
    if(ncol(cm) > 2){
        x <- xts(as.matrix(apply(cm[, -1], 2, as.numeric)), order.by = dts, tzone = tzone)
    }else{
        x <- xts(as.numeric(cm[, -1]), order.by = dts, tzone = tzone)
    }
    return(x)
}

# Momentum Calculation
momentumIndicators = function(spreads){
    mom.crossover = momentum.Crossover(tail(spreads, 200)) # Hamming 1-10 | Spearman -1 to 1 (> 0 bearish | < 0 bullish)
    mom.hamming.dist = as.numeric(tail(mom.crossover$hamming.Dist, 2)) # 1 - 10 weak to strong momentum
    mom.spearman = as.numeric(tail(mom.crossover$spearman, 2))  # high means short and low means long (-1 to 1)
    mom.thickness = as.numeric(tail(mom.crossover$thickness, 2)) / mean(mom.crossover$thickness) # higher thickness means higher volatilities or stronger momentum
    
    macd.indicator = MACD(spreads, maType = 'EMA')
    macd = tail(as.numeric(macd.indicator$macd),2) # macd > signal bearish | macd < signal bullish
    macd.signal.all = na.omit(as.numeric(macd.indicator$macd - macd.indicator$signal)) # neg bearish | pos bullish
    macd.signal = tail(macd.signal.all, 2)
    
    ema.signal.all = na.omit(as.numeric(spreads - EMA(spreads, n = look.back), 2)) # pos bullish | neg bearish
    ema.signal = tail(ema.signal.all, 2) # pos bullish | neg bearish
    
    mom.rsi = as.numeric(tail(momentum.RSI(spreads), 2)) # >70 over buy | <30 over sell
    
    # Final effectiveness
    mom.effect = mean(c(mom.hamming.dist[2]/5, 
                        abs(mom.spearman[2])/0.5, 
                        mom.thickness[2], 
                        abs(macd.signal[2])/mean(abs(macd.signal.all)),
                        abs(ema.signal[2])/mean(abs(ema.signal.all)),
                        ifelse(mom.rsi[2]> 50, mom.rsi[2]/70, (100-mom.rsi[2])/70)
    ))
    mom.direction = mean(c(ifelse(mom.spearman[2]>0,-1,1),
                           ifelse(macd.signal[2]>0,1,-1),
                           ifelse(ema.signal[2]>0,1,-1),
                           ifelse(ema.signal[2]>0,1,-1),
                           ifelse(mom.rsi[2] >= 60, 1, ifelse(mom.rsi[2]<= 40, -1, 0))
    ))
    
    mom.effect.lst = mean(c(mom.hamming.dist[1]/5, 
                        abs(mom.spearman[1])/0.5, 
                        mom.thickness[1], 
                        abs(macd.signal[1])/mean(abs(macd.signal.all)),
                        abs(ema.signal[1])/mean(abs(ema.signal.all)),
                        ifelse(mom.rsi[1]> 50, mom.rsi[1]/70, (100-mom.rsi[1])/70)
    ))
    mom.direction.lst = mean(c(ifelse(mom.spearman[1]>0,-1,1),
                           ifelse(macd.signal[1]>0,1,-1),
                           ifelse(ema.signal[1]>0,1,-1),
                           ifelse(ema.signal[1]>0,1,-1),
                           ifelse(mom.rsi[1] >= 60, 1, ifelse(mom.rsi[1]<= 40, -1, 0))
    ))
    
    return(list(
        mom.hamming.dist = mom.hamming.dist[2],
        mom.spearman = mom.spearman[2],
        mom.thickness = mom.thickness[2],
        macd.signal = macd.signal[2],
        ema.signal = ema.signal[2],
        mom.rsi = mom.rsi[2],
        mom.effect = mom.effect,
        mom.direction = mom.direction,
        mom.effect.lst = mom.effect.lst,
        mom.direction.lst = mom.direction.lst
    ))
}


# Record Activities
recordActivity = function(configs, tradeActivity){
    setDT(tradeActivity)
    
    if(file.exists(configs$activity.book)){
        activity.book = fread(configs$activity.book)
        activity.book[, names(tradeActivity)[!names(tradeActivity)%in%names(activity.book)] := 0]
        activity.book = activity.book[, names(tradeActivity), with = F]
        activity.book = rbind(activity.book, tradeActivity)
        write.csv(activity.book, file = configs$activity.book, row.names = FALSE)
    }else{
        write.csv(tradeActivity, file = configs$activity.book, row.names = FALSE)
    }
}

# Get last day transactions
getLstDayTxns = function(configs, sinceID = 0){
    transactions = RQuantAPI::getOandaTxnsID(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, sinceID)
    setDT(transactions)
    transactions.lst = transactions[type %in% c('MARKET_ORDER',"ORDER_FILL","TAKE_PROFIT_ORDER","TRAILING_STOP_LOSS_ORDER", "ORDER_CANCEL") & as.Date(time) == as.Date(max(time))]
    transactions.lst = transactions.lst[, .(batchID, id, accountID, type, time, accountBalance, instrument, units, reason, price, pl, financing, commission, 
                                            tradeID, closedTradeID, tradeCloseTransactionID, distance, takeProfitOnFill.price, trailingStopLossOnFill.distance, 
                                            tradeOpened.tradeID, tradeOpened.units, tradeClose.tradeID, tradeClose.units)]    
    return(transactions.lst)
}


# Get AutoChartistSignal
getAutoChartistSignal = function(configs){
    signals = Autochartist.Oanda(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', PERIOD = '4 hours')$signals.keylevel
    signals = signals[signals$instrument %in% c('AUD_CAD', 'USD_CAD', 'AUD_USD'), c("instrument","data.prediction.timefrom", "data.prediction.timeto","data.prediction.pricelow", "data.prediction.pricehigh", "meta.probability", "meta.interval", "meta.direction")]
    signals$data.prediction.timefrom = as.POSIXct(as.numeric(signals$data.prediction.timefrom), origin="1970-01-01",tz = "Australia/Melbourne")
    signals$data.prediction.timeto = as.POSIXct(as.numeric(signals$data.prediction.timeto), origin="1970-01-01",tz = "Australia/Melbourne")
    
    if(is.null(signals)){
        return(NULL)
    }else{
        return(list(signals = signals,    
                    prob = mean(signals$meta.probability),
                    direction = sum(signals$meta.direction)))  
    }
}
# Pricing Functions ------------------------------------------------------
# Get Latest Prices
getLatestPrices = function(configs){
    AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, oanda.count = 2, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2], oanda.granularity = configs$freq)
    audcad = Cl(AUDCAD$OA.MID);
    audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
    audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)
    AUDCAD.bid = AUDCAD$OA.BID
    AUDCAD.ask = AUDCAD$OA.ASK
    
    cols = c('AUDCAD')
    portf.new = na.omit(audcad.ret); names(portf.new) = cols
    prices.new = na.omit(audcad); names(prices.new) = cols
    cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols
    
    return(list(AUDCAD = tail(AUDCAD$OA.MID, 1),
                portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = index(tail(prices.new,1)),
                AUDCAD.bid = tail(AUDCAD.bid,1),
                AUDCAD.ask = tail(AUDCAD.ask,1)))
}
# Get Latest Trade IDs
getLatestTradeID = function(configs){
    return(list(
        audcad.tradeid = getOandaTrades(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades$id[1]
    ))
}
getLatestTrades = function(configs){
    return(list(
        audcad.trade = getOandaTrades(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades[1,]
    ))
}
getLatestVIX = function(latest = TRUE, n = 2500){
    if(latest){
        vix.raw = tail(Quandl("CBOE/VIX", api_key="H83fxGxUfi-GBx1JyDx8", start_date=Sys.Date()-5, order = 'asc'))
    }else{
        vix.raw = Quandl("CBOE/VIX", api_key="H83fxGxUfi-GBx1JyDx8", order = 'asc')
    }
    vix.raw$vix.spreads = Hi(vix.raw) - Lo(vix.raw)
    vix.raw$vix.change = Cl(vix.raw) - Op(vix.raw)
    vix = OHLC(vix.raw)
    vix.spreads = tail(Hi(vix) - Lo(vix),n)
    vix.change = tail(Cl(vix) - Op(vix), n)
    return(list(vix.raw = vix.raw, vix = tail(Cl(vix), n),vix.spreads=vix.spreads,vix.change=vix.change))
}
# Get the low freq corr
getPairCor = function(configs){
    AUDUSD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                     oanda.count = 480, Cur1 = 'AUD', Cur2 = 'USD', oanda.granularity = configs$secondary.freq)
    USDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                     oanda.count = 480, Cur1 = 'USD', Cur2 = 'CAD', oanda.granularity = configs$secondary.freq)
    
    cor.long = cor(as.numeric(Cl(AUDUSD$OA.MID)), as.numeric(Cl(USDCAD$OA.MID)))
    cor.short = as.numeric(runCor(x = as.numeric(Cl(AUDUSD$OA.MID)), y = as.numeric(Cl(USDCAD$OA.MID)), n=96))
    cor.diff = cor.short / cor.long
    return(tail(cor.diff, 1))
}


getSecondaryMomentum = function(configs){
    AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                     oanda.count = 480, Cur1 = 'AUD', Cur2 = 'CAD', oanda.granularity = configs$secondary.freq)
    spread.low = Op(AUDCAD$OA.MID)
    momentums.low = momentumIndicators(spread.low)
    return(momentums.low)
}
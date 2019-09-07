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
    order.book.file = '~/Common/AUDCAD_OANDA_MRMOM_H1/orderbook/AUDCAD_OANDA_H1.csv' # Order book location
    ,activity.book = '~/Common/AUDCAD_OANDA_MRMOM_H1/orderbook/AUDCAD_OANDA_H1_ACTIVITY.csv'
    ,log.file = file(paste0('~/Common/AUDCAD_OANDA_MRMOM_H1/log/AUDCAD_OANDA_H1_', as.Date(Sys.time()), '.Rout'), open = 'wt') # Log file location
    ,plotPath = "~/Common/AUDCAD_OANDA_MRMOM_H1/plots/audcad_intraday_H1_" # ZScore plots location
    ,modelPath = "~/Common/AUDCAD_OANDA_MRMOM_H1/models/audcad_intraday_H1_"
    ,strategyPath = '~/Common/AUDCAD_OANDA_MRMOM_H1/' # Strategy root folder
    ,pair1 = c('AUD', 'CAD') # Currency Pair
    ,initAssets = 10000 # net cap
    ,leverage = 20 # leverage
    ,reinvest = TRUE # reinvest or not
    ,posiRate = 0.001 # position ratio
    ,threshold = 0.8 # mean reversion threshold
    ,mean.threshold = 0.5 # mean reversion p value
    ,moment.threshold = 1.25 # trending threshold - effect * direction
    ,ACCESS_TOKEN_PRICE = '' # Broker Token
    ,ACCOUNT_TYPE_PRICE = 'real' # Broker account type
    ,ACCOUNT_ID = '' # Broker account number
    ,train.length = 200 # Mean reversion lookback
    ,ml.length = 1800 # Machine learning lookback
    ,backtest.n = 1800 # Backtest periods
    ,min.lookback = 48 # Min half life
    ,max.lookback = 120 # Max half life
    ,sd.n = 3 # Standard deviation for SL & TP
    ,freq = 'H1' # Trading interval
    ,secondary.freq = "M5" # Secondary frequency to measure short term movements
    ,pred.n = 2 # Machine learning look ahead
    ,exe.pip = 0.0006 # threshold of execution pip difference
    ,volatility.threshold = 0.05 # Volatility multiplier, NOT USING for now
    ,VIX.threshold = 20
    ,corr.threshold = 0.7
)
inLong = FALSE
inShort = FALSE
tradeType = 'None'

# Sunday approximately 5 p.m. to Friday 5 p.m
# Get prices univers (maximum last 2500 points)
AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                 oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD', oanda.granularity = configs$freq)
# Caculate some basic price series
audcad = Cl(AUDCAD$OA.MID);
audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)
audcad.vol = Vo(AUDCAD$OA.MID)
cols = c('AUDCAD')
portf = na.omit(audcad.ret); names(portf) = cols
prices = na.omit(audcad); names(prices) = cols
cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols
AUDCAD = AUDCAD$OA.MID
# VIX
vix = getLatestVIX(F, nrow(AUDCAD))

sink(configs$log.file, append = TRUE)
sink(configs$log.file, type="message", append = TRUE)
while(TRUE){
    
    tryCatch({
        latestPrices = getLatestPrices(configs)
        lastDate = index(tail(prices, 1))
        
        if(latestPrices$update.date > lastDate){
            
            latestVIX = getLatestVIX(T, 1)
            cat(paste0('\n\nNew Event Found! Start trading with ', lastDate, '............\n'))
            portf = rbind(portf, latestPrices$portf.new)
            AUDCAD = rbind(AUDCAD, latestPrices$AUDCAD)
            prices = rbind(prices, latestPrices$prices.new)
            cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)
            if(!tail(vix$vix,1) == tail(latestVIX$vix,1)){
                vix$vix = c(vix$vix, latestVIX$vix)
                vix$vix.spreads = c(vix$vix.spreads, latestVIX$vix.spreads)
                vix$vix.change = c(vix$vix.change, latestVIX$vix.change)
            }
            r = nrow(prices)
            goLong = FALSE
            goShort = FALSE
            closePos = FALSE
            
            # Model data --------------------------------------------------------------
            tmp.prices = tail(prices, configs$train.length)
            vix.dat = as.xts(vix$vix.raw[, c('vix.spreads', 'vix.change')], as.Date(vix$vix.raw$Date))
            ml.prices = merge.xts(AUDCAD, vix.dat, join = "left")
            ml.prices <- na.omit(na.locf(ml.prices))
            ml.prices <- ml.prices[!duplicated(index(ml.prices))]
            spreads = tmp.prices
            
            # Half Life ---------------------------------------------------------------
            partial.y = fit.par(spreads)
            pvmr.par = partial.y$pvmr # 0 to 1 indicating mean reversion
            if(partial.y$rho<0){
                half.life.par = 0
                look.back = HalfLifeMeanReversion(spreads)$half.life.round
            }else{
                half.life.par = log(0.5)/log(partial.y$rho)
                look.back = round(half.life.par)
            }
            look.back = min(max(c(look.back, configs$min.lookback)), configs$max.lookback)
            
            # 3. Statistics Test ------------------------------------------------------
            # Stationary Tests
            # ADF
            adf.res = adf.test(spreads, k = 0)
            adf.res.lag1 = adf.test(spreads, k = 1) #price change
            adf.signal=adf.res$p.value
            adf.signal.lag1=adf.res.lag1$p.value
            pp.signal = pp.test(spreads)$p.value # Phillips-Perron Unit Root test
            # Hurst
            hurst.signal = tryCatch({
                hurst.res = hurstexp(spreads, d = 2, display = F)
                hurst.res$Hal
            }, error = function(e){
                hurst.res = HurstExponentTest(spreads, lookback = look.back)
                return(as.numeric(tail(hurst.res$hurstKY, 1)))
            })
            # Variance ratio test
            vr.ratio = Auto.VR(spreads)
            var.ratio = vr.ratio$stat/vr.ratio$sum
            
            
            # 4. Mean Reversion -------------------------------------------------------
            # hedge ratio NOT REQUIRED any more
            
            # ZScore Calculation
            spreads.MA <- MaRatio(spreads, look.back)
            spreads.SD <- Sd(spreads, look.back)
            Z.Score <- ZScore(spreads,spreads.MA,spreads.SD)
            # Add ZScore / Position Multipliers to reduce momentum risks
            Z.Score.Diff = as.numeric(tail(Z.Score, 1)) - as.numeric(tail(Z.Score, 2)[1])
            Z.Score.Multiplier = 1#max(abs(Z.Score.Diff)/mean(abs(diff(Z.Score)), na.rm = T), 1)
            
            # Position Sizing
            zScore=as.numeric(tail(Z.Score,1))
            numUnits=-tail(zScore,1)
            # Dollar neutrality
            sizing = abs(numUnits)/1
            posUnits = configs$initAssets * configs$leverage * configs$posiRate
            dollars = round(as.numeric(posUnits * sizing))
            # positions
            last.price = tail(tmp.prices,1)
            positions = as.data.frame(round(dollars / tail(last.price,1)))
            names(positions) = paste0('POS.AUDCAD')
            
            
            # 5. Momentum -------------------------------------------------------------
            momentums = momentumIndicators(spreads)
            mom.hamming.dist = momentums$mom.hamming.dist
            mom.spearman = momentums$mom.spearman
            mom.thickness = momentums$mom.thickness
            macd.signal = momentums$macd.signal
            ema.signal = momentums$ema.signal
            mom.rsi = momentums$mom.rsi
            mom.effect = momentums$mom.effect
            mom.direction = momentums$mom.direction
            
            momentums.secondary = getSecondaryMomentum(configs)
            
            # 6. Machine learning -----------------------------------------------------
            if(!exists("xgbFitClass")){
                oa.ret = lag(ROC(Cl(ml.prices), n = configs$pred.n, type = 'discrete'), -configs$pred.n); names(oa.ret) = 'target'
                oa.ret = ifelse(oa.ret >= 0, 1,0)
                price.feat =  prepareMachinelearningFeatures(ml.prices)
                xgbFitClass1 = xgbBackTest(na.omit(merge(price.feat[1:800,], oa.ret)))
                xgbFitClass2 = xgbBackTest(na.omit(merge(price.feat[801:nrow(price.feat),], oa.ret)))
                price.feat$lastPred = c(predict(xgbFitClass1$xgbFit, data.matrix(price.feat[801:nrow(price.feat),])),
                                        predict(xgbFitClass2$xgbFit, data.matrix(price.feat[1:800,])))
                xgbFitClass = xgbBackTest(na.omit(merge(price.feat, oa.ret)))
            }
            price.feat =  tail(prepareMachinelearningFeatures(tail(ml.prices, 201)),1)
            price.feat$lastPred = (predict(xgbFitClass1$xgbFit, data.matrix(price.feat)) + predict(xgbFitClass2$xgbFit, data.matrix(price.feat)))/2
            aud.ml.ret = predict(xgbFitClass$xgbFit, data.matrix(price.feat))
            aud.ret.multiplier = sign(aud.ml.ret - xgbFitClass$pred.quantile['50%']) *
                (abs(aud.ml.ret - xgbFitClass$pred.quantile['50%']) + xgbFitClass$pred.quantile['50%'])/xgbFitClass$pred.quantile['50%']
            
            
            # 7. Risk Management ------------------------------------------------------
            AUD.sd = configs$sd.n * sd(tail(cur.spreads, look.back)) # )mean(sd(tail(cur.spreads, 12)), 
            
            
            # 8. Mean-reversion & Momentum signals
            mean.reversion.signal = mean(c(adf.signal, pp.signal, var.ratio))
            momentum.signal = as.numeric(sign(mom.direction) * mom.effect)
            ml.direction = as.numeric(ifelse(aud.ml.ret > xgbFitClass$pred.quantile['50%'], 1, ifelse(aud.ml.ret < xgbFitClass$pred.quantile['50%'], -1, 0)))
            zscore.direction = ifelse(zScore >= configs$threshold, -1, ifelse(zScore <= -configs$threshold, 1, 0))
            
            
            # 9. Get the short term correlationship
            cor.Ratio = getPairCor(configs)
            
            # 8. Execution ------------------------------------------------------------
            # 8.1 Mean-reverting signals
            if(ml.direction == zscore.direction &
               cor.Ratio >= configs$corr.threshold &
               abs(momentum.signal) < configs$moment.threshold & 
               mean.reversion.signal <= configs$mean.threshold & 
               (abs(zScore) >= configs$threshold | aud.ml.ret <= xgbFitClass$pred.quantile['20%'] | aud.ml.ret >= xgbFitClass$pred.quantile['80%'])){
                if(((zScore >= configs$threshold & aud.ml.ret < xgbFitClass$pred.quantile['45%']) | aud.ml.ret <= xgbFitClass$pred.quantile['20%']) & 
                   abs(tail(vix$vix,1)) <= configs$VIX.threshold & mom.hamming.dist <= 6 & abs(momentum.signal) <= 0.8){ # Added VIX  & momentum.signal < 0.5
                    goLong = FALSE
                    goShort = TRUE
                    closePos = FALSE
                }else if(((zScore <= -configs$threshold & aud.ml.ret > xgbFitClass$pred.quantile['55%']) | aud.ml.ret >= xgbFitClass$pred.quantile['80%']) & 
                         abs(tail(vix$vix,1)) <= configs$VIX.threshold & mom.hamming.dist <= 6 & abs(momentum.signal) <= 0.8){ # Added VIX  & momentum.signal > -0.5
                    goLong = TRUE
                    goShort = FALSE
                    closePos = FALSE
                }else if((inLong & zScore>configs$threshold) | (inShort & zScore < -configs$threshold)){
                    goLong = FALSE
                    goShort = FALSE
                    closePos = TRUE # sell OR buy
                }
                tradeType = 'MeanReversion'
            }else if(((abs(momentum.signal) >= configs$moment.threshold &
                       abs(momentums.secondary$mom.effect) >= configs$moment.threshold) |
                      ((abs(momentum.signal) + abs(momentums.secondary$mom.effect))/2) >= configs$moment.threshold) &
                     sign(mom.direction) == sign(momentums.secondary$mom.direction) & 
                     sign(mom.direction) == ml.direction){
                if(momentum.signal < 0 & aud.ml.ret < xgbFitClass$pred.quantile['40%']){
                    goLong = FALSE
                    goShort = TRUE # buy
                    closePos = FALSE
                }else if(momentum.signal > 0 & aud.ml.ret > xgbFitClass$pred.quantile['60%']){
                    goLong = TRUE # sell
                    goShort = FALSE
                    closePos = FALSE
                }else{
                    goLong = FALSE
                    goShort = FALSE
                    # closePos = TRUE # sell OR buy
                }
                tradeType = 'Momentum'
            }
            # else if(){
            #  # Change Points   
            # }    
            else if(abs(momentum.signal) < configs$moment.threshold & tradeType == 'Momentum'){
                goLong = FALSE
                goShort = FALSE
                closePos = TRUE # sell OR buy
                tradeType = 'CloseMomentum'
            }else{
                tradeType = 'None'
            }
            
            
            # 9. Positions & Risk Management --------------------------------------------------
            POS.AUDUSD = 0
            aud.take.profit = 0
            aud.stop.loss = 0
            AUDUSD.exe = as.numeric(last.price)
            current.price = getLatestPrices(configs)
            bid.price = as.numeric(Cl(current.price$AUDCAD.bid))
            ask.price = as.numeric(Cl(current.price$AUDCAD.ask))
            
            if(goLong & (bid.price - AUDUSD.exe <= configs$exe.pip)){
                POS.AUDUSD = min(round(as.numeric(positions) * (aud.ret.multiplier^6)), round(as.numeric(positions)*2))
                
                aud.take.profit = round(as.numeric(bid.price + sign(POS.AUDUSD) * (AUD.sd/Z.Score.Multiplier)), 5)# / Z.Score.Multiplier
                aud.stop.loss = round(as.numeric(bid.price - sign(POS.AUDUSD) * AUD.sd), 5)
                
                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', 
                                                 UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', #PRICE = bid.price,
                                                 TAKEPROFIT = aud.take.profit,
                                                 TRAILINGSTOP = AUD.sd, # STOPLOSS = aud.stop.loss,
                                                 TIMEINFORCE = "IOC")
                
                cat(paste0("\nGo Long | AUD Limit order at ", bid.price, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))
                
                if(aud.order.msg$status_code > 201) OandaSendAlert(type = "Go Long", pos = POS.AUDUSD)
                
                inLong = TRUE
                inShort = FALSE
                
                
            }else if(goShort & (AUDUSD.exe - ask.price <= configs$exe.pip)){
                POS.AUDUSD = -min(round(as.numeric(positions) * (aud.ret.multiplier^6)), round(as.numeric(positions)*2))
                
                aud.take.profit = round(as.numeric(ask.price + sign(POS.AUDUSD) *( AUD.sd/ Z.Score.Multiplier)), 5)# 
                aud.stop.loss = round(as.numeric(ask.price - sign(POS.AUDUSD) * AUD.sd), 5)
                
                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', #PRICE = ask.price,
                                                 TAKEPROFIT = aud.take.profit,
                                                 TRAILINGSTOP = AUD.sd, # STOPLOSS = aud.stop.loss,
                                                 TIMEINFORCE = "IOC")
                
                cat(paste0("\nGo Short | AUD Limit order at ", ask.price, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))
                
                if(aud.order.msg$status_code > 201) OandaSendAlert(type = "Go Short", pos = POS.AUDUSD)
                
                inLong = FALSE
                inShort = TRUE
                
                
            }else if(closePos & (inLong | inShort)){
                
                CurrentPositions = setDT(getOandaPositions(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
                
                POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_CAD', .(long.units,short.units)]))
                
                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD, TIMEINFORCE = "IOC")
                
                if(aud.order.msg$status_code > 201) OandaSendAlert(type = "Close Position", pos = POS.AUDUSD)
                
                inLong = FALSE
                inShort = FALSE
            }
            # Partial Close Positions -------------------------------------------------
            openPos = getOandaTrades(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, "AUD_CAD")$trades
            
            if(length(openPos)>0){
                for(i in 1:nrow(openPos)){
                    t = openPos[i,]
                    clsPrice = ifelse(as.numeric(t$initialUnits) >0, ask.price, bid.price)
                    opTime = as.POSIXct(strptime(t$openTime, "%Y-%m-%dT%H:%M:%OS"), origin="1970-01-01",tz = "AEST")
                    hrDiff = round(difftime(as.timeDate(as.character(latestPrices$update.date)), as.timeDate(as.character(opTime)), units = "hours"))
                    if(hrDiff >= configs$pred.n & as.numeric(t$unrealizedPL) > 0){
                        # Close positions
                        clsRes = cloaseOandaTrade(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, t$id)
                        # tradeActivity = data.frame(as.character(Sys.time()), OB.PRICES, -as.numeric(t$currentUnits),
                        #                            OB.PRICES,as.character(Sys.time()), 0, 0, 'Close', 0)
                        # setDT(tradeActivity)
                        # names(tradeActivity) = c("DateTime", 'Expected.Price', 'POS.AUDUSD', 'AUD.Exec.Price','AUD.Order.Time',
                        #                          'AUD.TakeProfit', 'AUD.StopLoss', 'Activity', 'Pips.Variance')
                        # intAct = recordActivity(configs, tradeActivity)
                        
                    }else if((clsPrice-as.numeric(t$price))/(ifelse(is.na(as.numeric(t$takeProfitOrder.price)), 0, as.numeric(t$takeProfitOrder.price))-as.numeric(t$price))>=0.7){
                        clsRes = cloaseOandaTrade(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, t$id)
                        # tradeActivity = data.frame(as.character(Sys.time()), OB.PRICES, -as.numeric(t$currentUnits),
                        #                            OB.PRICES,as.character(Sys.time()), 0, 0, 'Close', 0)
                        # setDT(tradeActivity)
                        # names(tradeActivity) = c("DateTime", 'Expected.Price', 'POS.AUDUSD', 'AUD.Exec.Price','AUD.Order.Time',
                        #                          'AUD.TakeProfit', 'AUD.StopLoss', 'Activity', 'Pips.Variance')
                        # intAct = recordActivity(configs, tradeActivity)
                    }
                }
            }
            
            EXE.TIME = as.character(Sys.time())
            cat(paste0('\nZScore: ', round(zScore, 5),
                       ' | Machine learning: ', round(aud.ml.ret/xgbFitClass$pred.quantile['50%'], 5),
                       ' | Correlation: ', round(cor.Ratio, 5),
                       ' | Mean-reversion: ', round(mean.reversion.signal,5),
                       ' | Short-term Momentum: ', round(momentums.secondary$mom.effect,5), ", ", round(momentums.secondary$mom.direction,5),
                       ' | Momentum: ', round(momentum.signal,5), '\n'))
            
            # Re-check positions
            CurrentPositions = setDT(getOandaPositions(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
            POS.AUDCAD = sum(as.numeric(CurrentPositions[instrument == 'AUD_CAD', .(long.units,short.units)]))
            
            OB.PRICES = ifelse(goLong, bid.price, ifelse(goShort, ask.price, last.price));
            latest.aud.price = NA
            latest.aud.openTime = NA
            if(goShort | goLong){
                # latestTradeID = getLatestTradeID(configs)
                latestTrade = getLatestTrades(configs)
                latest.aud.price = as.numeric(latestTrade$audcad.trade$price)
                latest.aud.openTime = latestTrade$audcad.trade$openTime
                latest.act = ifelse(goShort, 'Short', 'Long')
                
                # tradeActivity = data.frame(EXE.TIME, OB.PRICES, POS.AUDUSD,
                #                            latest.aud.price,latest.aud.openTime,
                #                            aud.take.profit, aud.stop.loss, latest.act)
                # setDT(tradeActivity)
                # tradeActivity[, Exe.pips := abs(latest.aud.price - OB.PRICES)*10000]
                # names(tradeActivity) = c("DateTime", 'Expected.Price', 'POS.AUDUSD',
                #                          'AUD.Exec.Price','AUD.Order.Time',
                #                          'AUD.TakeProfit', 'AUD.StopLoss', 'Activity', 'Pips.Variance')
                # intAct = recordActivity(configs, tradeActivity)
            }
            
            
            # 10. Order book --------------------------------------------------------------
            # Update Portf Information
            unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
            PnL = sum(as.numeric(CurrentPositions$pl))
            # Create latest order record
            
            tot.pos = setDT(getOandaPositions(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
            long.pos = sum(as.numeric(tot.pos[instrument == 'AUD_CAD', .(long.units)]))
            short.pos = sum(as.numeric(tot.pos[instrument == 'AUD_CAD', .(short.units)]))
            long.exposure = long.pos * OB.PRICES
            short.exposure = short.pos * OB.PRICES
            
            orderBook = data.frame(EXE.TIME, OB.PRICES, adf.signal, adf.signal.lag1, pp.signal,
                                   var.ratio, hurst.signal, pvmr.par, half.life.par, mean.reversion.signal,
                                   macd.signal, ema.signal, mom.hamming.dist, mom.spearman, mom.thickness, mom.rsi, momentum.signal,
                                   zScore,
                                   aud.ml.ret, aud.ret.multiplier,
                                   POS.AUDUSD,
                                   latest.aud.price,latest.aud.openTime,
                                   aud.take.profit, aud.stop.loss,
                                   inLong, inShort, goLong, goShort, closePos, tradeType, unrealizedPnL, PnL)
            names(orderBook) = c('DateTime', 'Expected.Price', 'ADF', 'ADF.lag1', 'Phillips-Perron',
                                 'Variance.Ratio', 'Hurst', 'Partial.Auto.pvmr','Partial.Auto.HL', 'Mean-Reversion',
                                 'MACD.Signal', 'EMA.Signal', 'Hamming.Dist', 'Mom.Spearman', 'Mom.Thickness', 'RSI', 'Momentum',
                                 'ZScore',
                                 'AUD.ML.Returns', 'AUD.ML.Ret.Multiplier',
                                 'POS.AUDUSD',
                                 'AUD.Exec.Price','AUD.Order.Time',
                                 'AUD.TakeProfit', 'AUD.StopLoss',
                                 'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 'tradeType', 'unrealizedPnL', 'PnL')
            setDT(orderBook)
            orderBook[, Exe.pips := abs(AUD.Exec.Price - Expected.Price)*10000]
            
            if(file.exists(configs$order.book.file)){
                Order.Book = fread(configs$order.book.file)
                Order.Book[, names(orderBook)[!names(orderBook)%in%names(Order.Book)] := 0]
                Order.Book = Order.Book[, names(orderBook), with = F]
                Order.Book = rbind(Order.Book, orderBook)
                write.csv(Order.Book, file = configs$order.book.file, row.names = FALSE)
            }else{
                write.csv(orderBook, file = configs$order.book.file, row.names = FALSE)
            }
            
            
            # Save ZScore Plots -------------------------------------------------------
            ZScore.plot = tail(Z.Score,configs$train.length-look.back)
            index(ZScore.plot) = index(ZScore.plot) + 3600 * 10
            lob <- shb <- clb <- xts(!as.logical(ZScore.plot[,1]),index(ZScore.plot))
            lob[as.character(round(index(lob), 'hours')) %in% as.character(round(as.POSIXct(Order.Book[goLong==T,DateTime]), 'hours'))] <- TRUE
            shb[as.character(round(index(shb), 'hours')) %in% as.character(round(as.POSIXct(Order.Book[goShort==T,DateTime]), 'hours'))] <- TRUE
            clb[as.character(index(clb)) %in% as.character(Order.Book[closePos==T,DateTime])] <- TRUE
            
            plotpath = paste0(configs$plotPath,format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".png")
            png(plotpath)
            print({
                chart_Series(ZScore.plot, theme = chart_theme(), name = "ZScore Chart - long(green)/short(red)/close(grey)")
                add_TA(lob, on = 1, col = "green", lty = 3)
                add_TA(shb, on = 1, col = "red", lty = 3)
                add_TA(clb, on = 1, col = "grey", lty = 3)
            })
            dev.off()
            
            
            # Daily Email -------------------------------------------------------------
            if(hour(Sys.time()) == 7){
                my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
                dir.create(my.dir)
                
                txns.dt = getLstDayTxns(configs)
                txns.dt$time = format(txns.dt$time, tz="Australia/Melbourne")
                
                if(tail(as.Date(txns.dt$time),1) >= as.Date(Sys.time()) - 1){
                    txns.ts = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                                      oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD', 
                                                      oanda.granularity = configs$secondary.freq)$OA.MID
                    # tzone(txns.ts) <- "Australia/Melbourne"
                    # index(txns.ts) = as.POSIXct(format(index(txns.ts), tz="Australia/Melbourne"))
                    index(txns.ts) = index(txns.ts) + 3600 * 10
                    txns.ts = txns.ts[paste0(min(as.Date(txns.dt$time)), "/", max(as.Date(txns.dt$time)))]
                    trades = txns.dt[reason == 'CLIENT_ORDER', batchID]
                    
                    # pdf(paste(my.dir, "trades.pdf",sep='/') ,width=12,height=6,paper='special')
                    
                    for(t in trades){
                        trade = txns.dt[batchID == t]
                        t.ID = trade[type == 'ORDER_FILL', id]
                        t.time = trade[type == 'ORDER_FILL', time]
                        t.units = trade[type == 'ORDER_FILL', units]
                        t.price = as.numeric(trade[type == "ORDER_FILL", price])
                        t.tp = as.numeric(trade[type == 'TAKE_PROFIT_ORDER', price])
                        t.ts = t.price - as.numeric(trade[type == 'TRAILING_STOP_LOSS_ORDER', distance])
                        rel.trade = txns.dt[tradeClose.tradeID == t.ID]
                        cls.time = rel.trade$time
                        action = paste0(ifelse(as.numeric(t.units) < 0, "Short", "Long"), ": ", t.units, " units")
                        
                        t.periods = txns.ts[paste0(round(as.POSIXlt(t.time),"mins"), "/", round(as.POSIXlt(cls.time),"mins"))]
                        plot.periods = txns.ts[paste0(round(as.POSIXlt(t.time)-7200,"mins"), "/", round(as.POSIXlt(cls.time)+7200,"mins"))]
                        
                        png(filename=paste(my.dir, paste0(t.ID,".png"),sep='/'), width=24, height=12,units = "cm",res = 300)
                        print(chartSeries(OHLC(plot.periods), theme="white", name = action, TA="addBBands();
                                addLines(h=t.tp, col='green');
                                addLines(h=t.price, col='black');
                                addLines(h=t.ts, col='red');
                                addLines(v=which(index(plot.periods) == as.POSIXlt(head(index(t.periods), 1))));
                                addLines(v=which(index(plot.periods) == as.POSIXlt(tail(index(t.periods), 1))))")
                        )
                        dev.off()
                    }
                    
                    OandaOrderBookDailyReport("AUDCAD_OANDA_MRMOM_H1", plotpath = plotpath, 
                                              order.book.file = configs$order.book.file, my.dir = my.dir, 
                                              txns.dt = txns.dt, configs)
                }
                
                unlink(my.dir, recursive = T)
            }
            
            
            # Train the machine learning model ----------------------------------------
            oa.ret = lag(ROC(Cl(ml.prices), n = configs$pred.n, type = 'discrete'), -configs$pred.n); names(oa.ret) = 'target'
            oa.ret = ifelse(oa.ret >= 0, 1,0)
            price.feat =  prepareMachinelearningFeatures(ml.prices)
            xgbFitClass1 = xgbBackTest(na.omit(merge(price.feat[1:800,], oa.ret)))
            xgbFitClass2 = xgbBackTest(na.omit(merge(price.feat[801:nrow(price.feat),], oa.ret)))
            price.feat$lastPred = c(predict(xgbFitClass1$xgbFit, data.matrix(price.feat[801:nrow(price.feat),])),
                                    predict(xgbFitClass2$xgbFit, data.matrix(price.feat[1:800,])))
            xgbFitClass = xgbBackTest(na.omit(merge(price.feat, oa.ret)))
            
            
            updateDirFilePermissions(configs$strategyPath)
        }
        
        else if(as.numeric(Sys.time())%%1500 <= 60){
            # Partial Close Positions -------------------------------------------------
            openPos = getOandaTrades(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, "AUD_CAD")$trades
            
            if(length(openPos)>0){
                for(i in 1:nrow(openPos)){
                    t = openPos[i,]
                    clsPrice = ifelse(as.numeric(t$initialUnits) >0, ask.price, bid.price)
                    opTime = as.POSIXct(strptime(t$openTime, "%Y-%m-%dT%H:%M:%OS"), origin="1970-01-01",tz = "AEST")
                    hrDiff = round(difftime(as.timeDate(as.character(latestPrices$update.date)), as.timeDate(as.character(opTime)), units = "hours"))
                    if(hrDiff >= configs$pred.n & as.numeric(t$unrealizedPL) > 0){
                        # Close positions
                        clsRes = cloaseOandaTrade(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, t$id)
                        tradeActivity = data.frame(as.character(Sys.time()), OB.PRICES, -as.numeric(t$currentUnits),
                                                   OB.PRICES,as.character(Sys.time()), 0, 0, 'Close', 0)
                        setDT(tradeActivity)
                        names(tradeActivity) = c("DateTime", 'Expected.Price', 'POS.AUDUSD', 'AUD.Exec.Price','AUD.Order.Time',
                                                 'AUD.TakeProfit', 'AUD.StopLoss', 'Activity', 'Pips.Variance')
                        intAct = recordActivity(configs, tradeActivity)
                        
                    }else if((clsPrice-as.numeric(t$price))/(as.numeric(ifelse(is.na(t$takeProfitOrder.price), 0, t$takeProfitOrder.price))-as.numeric(t$price))>=0.7){
                        clsRes = cloaseOandaTrade(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, t$id)
                        tradeActivity = data.frame(as.character(Sys.time()), OB.PRICES, -as.numeric(t$currentUnits),
                                                   OB.PRICES,as.character(Sys.time()), 0, 0, 'Close', 0)
                        setDT(tradeActivity)
                        names(tradeActivity) = c("DateTime", 'Expected.Price', 'POS.AUDUSD', 'AUD.Exec.Price','AUD.Order.Time',
                                                 'AUD.TakeProfit', 'AUD.StopLoss', 'Activity', 'Pips.Variance')
                        intAct = recordActivity(configs, tradeActivity)
                    }
                }
            }
        }
    },
    error = function(e){print(e)}
    )
    
    Sys.sleep(1)
}
sink(type="message")
sink()
# EconomicCalendarOanda(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD')

# Daily Email -------------------------------------------------------------
# if(hour(Sys.time()) == 7){
#     my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
#     dir.create(my.dir)
# 
#     txns.dt = getLstDayTxns(configs)
#     txns.dt$time = format(txns.dt$time, tz="Australia/Melbourne")
# 
#     if(tail(as.Date(txns.dt$time),1) >= as.Date(Sys.time()) - 1){
#         txns.ts = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
#                                           oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD',
#                                           oanda.granularity = configs$secondary.freq)$OA.MID
#         # tzone(txns.ts) <- "Australia/Melbourne"
#         # index(txns.ts) = as.POSIXct(format(index(txns.ts), tz="Australia/Melbourne"))
#         index(txns.ts) = index(txns.ts) + 3600 * 10
#         txns.ts = txns.ts[paste0(min(as.Date(txns.dt$time)), "/", max(as.Date(txns.dt$time)))]
#         trades = txns.dt[reason == 'CLIENT_ORDER', batchID]
# 
#         # pdf(paste(my.dir, "trades.pdf",sep='/') ,width=12,height=6,paper='special')
# 
#         for(t in trades){
#             trade = txns.dt[batchID == t]
#             t.ID = trade[type == 'ORDER_FILL', id]
#             t.time = trade[type == 'ORDER_FILL', time]
#             t.units = trade[type == 'ORDER_FILL', units]
#             t.price = as.numeric(trade[type == "ORDER_FILL", price])
#             t.tp = as.numeric(trade[type == 'TAKE_PROFIT_ORDER', price])
#             t.ts = t.price - as.numeric(trade[type == 'TRAILING_STOP_LOSS_ORDER', distance])
#             rel.trade = txns.dt[tradeClose.tradeID == t.ID]
#             cls.time = rel.trade$time
#             action = paste0(ifelse(as.numeric(t.units) < 0, "Short", "Long"), ": ", t.units, " units")
# 
#             t.periods = txns.ts[paste0(round(as.POSIXlt(t.time),"mins"), "/", round(as.POSIXlt(cls.time),"mins"))]
#             plot.periods = txns.ts[paste0(round(as.POSIXlt(t.time)-7200,"mins"), "/", round(as.POSIXlt(cls.time)+7200,"mins"))]
# 
#             png(filename=paste(my.dir, paste0(t.ID,".png"),sep='/'), width=24, height=12,units = "cm",res = 300)
#             print(chartSeries(OHLC(plot.periods), theme="white", name = action, TA="addBBands();
#                     addLines(h=t.tp, col='green');
#                     addLines(h=t.price, col='black');
#                     addLines(h=t.ts, col='red');
#                     addLines(v=which(index(plot.periods) == as.POSIXlt(head(index(t.periods), 1))));
#                     addLines(v=which(index(plot.periods) == as.POSIXlt(tail(index(t.periods), 1))))")
#             )
#             dev.off()
#         }
# 
#         OandaOrderBookDailyReport("AUDCAD Paper Trading - Hedging Test", plotpath = plotpath,
#                                   order.book.file = configs$order.book.file, my.dir = my.dir,
#                                   txns.dt = txns.dt, configs)#, mailList = 'ivan.liuyanfeng@gmail.com')
#     }
#     unlink(my.dir, recursive = T)
# }







transactions.lst



transactions = RQuantAPI::getOandaTxnsID(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID, 880)
setDT(transactions)
transactions.lst = transactions[type %in% c('MARKET_ORDER',"ORDER_FILL","TAKE_PROFIT_ORDER","TRAILING_STOP_LOSS_ORDER", "ORDER_CANCEL")]
transactions.lst = transactions.lst[, .(batchID, id, accountID, type, time, accountBalance, instrument, units, reason, price, pl, financing, commission, 
                                        tradeID, closedTradeID, tradeCloseTransactionID, takeProfitOnFill.price, 
                                        tradeOpened.tradeID, tradeOpened.units, tradeClose.tradeID, tradeClose.units)]    
txns.dt = copy(transactions.lst)
txns.dt$time = format(txns.dt$time, tz="Australia/Melbourne")


txns.ts = AUDCAD
index(txns.ts) = index(txns.ts) + 3600 * 10
txns.ts = txns.ts[paste0(min(as.Date(transactions.lst$time)), "/", max(as.Date(transactions.lst$time)))]

pl = txns.dt[!is.na(pl) & abs(as.numeric(pl)) > 0]
trades = txns.dt[type == 'ORDER_FILL']

txns.dt[pl<-150]

# During 2017-06-29 to 07-08 ----------------------------------------------
sum(as.numeric(pl$pl))
hist(as.numeric(pl$pl), breaks = 115)
table(as.numeric(pl$pl)>0)
quantile(as.numeric(pl$pl), )

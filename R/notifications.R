# Required Functions ------------------------------------------------------
OandaOrderBookDailyReport = function(title, plotpath,order.book.file,txns.dt, configs, my.dir, 
                                     mailList = c("ivan.liuyanfeng@gmail.com")){
    my.png.files<-c(list.files(my.dir, full.names = TRUE))
    # my.txt.file<-order.book.file
    write.csv(txns.dt, file = configs$activity.book)
    my.txns.file <- configs$activity.book
    my.body <- c(
        toHTML(paste0("Daily Trading Summary - ", title, " ", Sys.Date()), "h2")
        ,toHTML("Good morning guys! Please see attached for last day Order Book and ZScore Plot.", "h3")
    )
    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = c(my.png.files, 
                                 # my.txt.file, 
                                 my.txns.file)
    )
    tryCatch({
        result<-RQuantSendMail(
            to = mailList
            ,subject= paste0("Daily Trading Summary - ", title, " ",Sys.Date())
            ,msg=my.msg$html
            ,attach.files = my.msg$attach.files)
    })  
}

OandaSendAlert = function(mailList = c("ivan.liuyanfeng@gmail.com"), type, pos){
    my.body <- c(
        toHTML(paste0("[ALERT] Execution Failed ", Sys.time()), "h2")
        ,toHTML(paste0("ALERT! Trading Execution at ", Sys.time(), " was failed!"), "h3")
        ,toHTML('Execution Details:', "h3")
        ,toHTML(paste0('Type: ', type), "h4")
        ,toHTML(paste0('Positions: ', pos), "h4")
    )
    my.msg <- buildhtmlmsg(
        my.body
    )
    tryCatch({
        result<-RQuantSendMail(
            to = mailList
            ,subject= paste0("[ALERT] Execution Failed - ", Sys.time())
            ,msg=my.msg$html)
    })  
}

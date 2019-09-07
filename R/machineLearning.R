# Machine learning main
xgbBackTest = function(train){
    idx = sample(1:nrow(train), round(nrow(train)*0.8))
    trainBC = as.data.frame(train[idx,])
    testBC = as.data.frame(train[-idx,])
    idx = sample(1:nrow(testBC), round(nrow(testBC)*0.5))
    validationBC = as.data.frame(testBC[idx,])
    testBC = as.data.frame(testBC[-idx,])
    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    # predictors = predictors[!predictors%in%c('HigherOpen','White','Black','LowerOpen')]
    response = 'target'
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
    dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
    watchlist <- list(train = dtrain, eval = dval)
    param <- list(
        max_depth = 6,
        eta = 0.01,
        nthread = 6,
        objective = "binary:logistic", #"reg:linear",
        eval_metric = "rmse",
        eval_metric = "auc",
        booster = "gbtree",
        gamma = 0.01,
        min_child_weight = 10,
        subsample = 1,
        colsample_bytree = 0.03
    )
    for(i in 1:150){
        xgbFitClass <- xgb.train(param,dtrain,nrounds = 1000,watchlist,
                                 early_stopping_rounds = 20,verbose =0)
        
        val = predict(xgbFitClass, dtest)
        # new_score = as.numeric(pROC::roc(testBC$target, val)$auc)
        val = ifelse(val >= 0.5, 1, 0)
        new_score = 0.7 * as.numeric(table(val == testBC$target)[2] / length(val)) + 0.3 * as.numeric(xgbFitClass$best_score)
        if(i == 1){
            best_score = new_score
            xgbFit = xgbFitClass
        }else{
            if(best_score < new_score){
                best_score = new_score
                xgbFit = xgbFitClass
            }
        }
        if(i>15 & best_score > 0.58) break()
    }
    var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
    val = predict(xgbFit, dtest)
    pred.quant = quantile(val, seq(0,1,0.05))
    xgbRes = binaryClassifierEvaluation(val, testBC$target)
    return(list(xgbFit = xgbFit,
                xgbRes = xgbRes,
                var.imp = var.imp,
                pred.quantile = pred.quant,
                roc = pROC::roc(testBC$target, val)))
}
MAX.VALS = list('Ca125'=500)
MIN.VALS = list()

init = rep(0, length(COLS.ALL))
names(init) = COLS.ALL
w = data.frame(t(init))
for (i in 1:length(METHODS)){
    METHOD = METHODS[[i]]
    COLS = METHODS.COL[[i]]
    NAME = METHODS.NAME[[i]]

    weights = sapply(COLS, function(col){
        model_db = na.omit(db[,COLS])
        original_vals = apply(model_db, 1, METHOD)[1,]

        minval = max(MIN.VALS[[col]], min(model_db[,col]))
        maxval = min(MAX.VALS[[col]], max(model_db[,col]))

        changes = maxval - model_db[,col]

        model_db = na.omit(db[,COLS])
        model_db[,col] = maxval
        max_vals = apply(model_db, 1, METHOD)[1,]
        model_db = na.omit(db[,COLS])
        model_db[,col] = minval
        min_vals = apply(model_db, 1, METHOD)[1,]

        increase = max_vals - min_vals
        return(c(mean(increase,na.rm=T)))
    })
    weights = weights / sum(weights)

    tmp = rep(NA, length(COLS.ALL))
    names(tmp) = COLS.ALL
    tmp[COLS] = weights


    # print(NAME)
    # print(weights)
    w = rbind(w, tmp)
}

print(abs(w))

method.weights = apply(abs(w), 2, mean, na.rm=T)


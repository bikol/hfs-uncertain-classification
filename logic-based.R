source('similarities-helpers.R')

CO.IMPL.LK = function(x, y) {
    return(1-abs(x-y))
}

CO.IMPL.GD = function(x, y) {
    if (x == y)
        return(1)
    else
        return(min(x, y))
}

CO.IMPL.RC = function(x, y) {
    return(1-max(x, y) + x*y)
}

CO.IMPL.KD = function(x, y) {
    if (1-x <= y)
        return(min(x, y))
    else
        return(min(1-x, 1-y))
}

CO.IMPL.GG = function(x, y) {
    if ( x == 0 && y == 0)
        return(1)
    else
        return(min(x, y)/max(x, y))
}

CO.IMPL.RS = function(x, y) {
    if (x == y)
        return(1)
    else
        return(0)
}

CO.IMPL.YG = function(x, y) {
    if ( x == 0 && y == 0)
        return(1)
    else
        return(min(x^y, y^x))
}

CO.IMPL.WB = function(x, y){
    if (x<1 && y<1)
        return(1)
    else
        return(min(x, y))
}

CO.IMPL.FD = function(x, y) {
    if (y<=min(x, 1-x))
        return(1-x)
    else if (1-x < y && y < x)
        return(y)
    else if (x<y && y<1-x)
        return(1-y)
    else
        return(x)
}

CO.IMPLS= list(CO.IMPL.LK, CO.IMPL.GD, CO.IMPL.RC, CO.IMPL.KD, CO.IMPL.GG, CO.IMPL.RS, CO.IMPL.YG, CO.IMPL.WB, CO.IMPL.FD)
CO.IMPLS.NAME = list('LK', 'GD', 'RC', 'KD', 'GG', 'RS', 'YG', 'WB', 'FD')

# little bit of testing
stopifnot(all(sapply(CO.IMPLS, function(func){
    xs = seq(0,1,by=0.1)
    ys = seq(0,1,by=0.1)
    test1 = all(apply(expand.grid(xs, ys), 1, function(row){
            func(row[[1]], row[[2]]) == func(row[[2]], row[[1]])
    }))
    test2 = all(apply(expand.grid(xs, ys), 1, function(row1){
        return(all(apply(expand.grid(xs, ys), 1, function(row2){
            if(row1[[1]] <= row2[[1]] && row2[[1]] <= row2[[2]] && row2[[2]]<=row1[[2]])
                return(func(row1[[1]], row1[[2]]) <= func(row2[[1]], row2[[2]]))
            else
                return(TRUE)
        })))
    }))
    test3 = all(func(0,1) == 0, func(1, 1) == 1, func(0, 0) == 1)
    return(all(test1, test2, test2))
})))

GEN.SIM.LB = function(co.impl, aggr){
    force(co.impl); force(aggr)
    return(function(iA, iB){
        result = apply(cbind(iA$lower, iA$upper, iB$lower, iB$upper), 1, function(value){
            force(value)
            la = value[[1]]
            ua = value[[2]]
            lb = value[[3]]
            ub = value[[4]]

            result = 0.0
            if( ub < la )
                result = co.impl(la, ub)
            else if ( ua < lb )
                result = co.impl(ua, lb)
            else
                result = max( co.impl(max(la, lb), max(la, lb)), co.impl(min(ua, ub), min(ua, ub)) )
            return(c(min(co.impl(la, ub), co.impl(ua, lb)), result))
        })

        return(list(
            ly = aggr(result[1,]),
            uy = aggr(result[2,])))
    })
}

GEN.KNN.SIM.LB = function(co.impl, k, nbs.selector, vote.strategy, aggr) {
    force(co.impl);force(k);force(nbs.selector);force(vote.strategy);force(aggr)
    SIM = GEN.SIM.LB(co.impl, aggr)
    return(function(training.set){
        force(training.set)
        return(function(m){
            force(m)
            sims = lapply(training.set, function(nb){
                return(SIM(list(lower=nb$m[1,], upper=nb$m[2,]), list(lower=m[1,], upper=m[2,])))
            })
            sel = nbs.selector(sims, k)
            nbsTypes = sapply(training.set[sel], "[[", 'type')
            return(vote.strategy(nbsTypes, sims[sel]))
        })
    })
}

GEN.KNN.SIM.LB.RENN = function(co.impl, k, nbs.selector, vote.strategy, aggr) {
    force(co.impl);force(k);force(nbs.selector);force(vote.strategy);force(aggr)
    SIM = GEN.SIM.LB(co.impl, aggr)
    return(function(training.set){
        force(training.set)
        classes = unique(sapply(training.set, "[[", 'type'))
        while(length(training.set)>1) {
            correct = sapply(1:length(training.set), function(inst_no){
                instance = training.set[[inst_no]]
                training.set.without = training.set[-inst_no]
                sims = lapply(training.set.without, function(nb){
                    return(SIM(list(lower=nb$m[1,], upper=nb$m[2,]), list(lower=instance$m[1,], upper=instance$m[2,])))
                })
                sel = nbs.selector(sims, k)
                nbsTypes = sapply(training.set.without[sel], "[[", 'type')
                result = vote.strategy(nbsTypes, sims[sel])
                return(!is.na(result) & result==instance$type[[1]])
            })
            # printDebug(paste("before", length(training.set), sum(correct)))
            if(!any(correct) | all(correct)) {
                # printDebug(paste("all correct", length(correct)))
                break
            } else {
                # printDebug(paste("not all", sum(correct)))
                new_classes = unique(sapply(training.set[correct], "[[", 'type'))
                # guarantee that number of classes will not decrease too much
                if(length(new_classes) <= length(classes)/2 ) {
                    # printDebug(paste("break missing classes", length(classes) ,length(new_classes)))
                    break
                } else {
                    training.set = training.set[correct]
                }
            }
        }
        # printDebug(paste("after", length(training.set)))
        return(function(m){
            force(m)
            sims = lapply(training.set, function(nb){
                return(SIM(list(lower=nb$m[1,], upper=nb$m[2,]), list(lower=m[1,], upper=m[2,])))
            })
            sel = nbs.selector(sims, k)
            nbsTypes = sapply(training.set[sel], "[[", 'type')
            return(vote.strategy(nbsTypes, sims[sel]))
        })
    })
}


GEN.KNN.SIM.LB.CNN = function(co.impl, k, nbs.selector, vote.strategy, aggr) {
    force(co.impl);force(k);force(nbs.selector);force(vote.strategy);force(aggr)
    SIM = GEN.SIM.LB(co.impl, aggr)
    return(function(training.set){
        force(training.set)
        store = training.set[1]
        grabbag = training.set[-1]
        modified = TRUE
        while(length(grabbag)>0 & modified){
            modified = FALSE
            # printDebug(paste(length(store), length(grabbag)))
            keep = c()
            for(i in 1:length(grabbag)){
                instance = grabbag[[i]]
                sims = lapply(store, function(nb){
                    return(SIM(list(lower=nb$m[1,], upper=nb$m[2,]), list(lower=instance$m[1,], upper=instance$m[2,])))
                })
                sel = nbs.selector(sims, k)
                nbsTypes = sapply(store[sel], "[[", 'type')
                result = vote.strategy(nbsTypes, sims[sel])
                if (is.na(result) | result!=instance$type[[1]]){
                    store = append(store, list(instance))
                    modified = TRUE
                } else {
                    keep = c(keep, i)
                }
            }
            grabbag = grabbag[keep]
        }
        training.set = store
        # printDebug(paste("after", length(training.set)))

        return(function(m){
            force(m)
            sims = lapply(training.set, function(nb){
                return(SIM(list(lower=nb$m[1,], upper=nb$m[2,]), list(lower=m[1,], upper=m[2,])))
            })
            sel = nbs.selector(sims, k)
            nbsTypes = sapply(training.set[sel], "[[", 'type')
            return(vote.strategy(nbsTypes, sims[sel]))
        })
    })
}


KNN.LB.MED = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                          expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
                    1, function(row){
                        list(GEN.KNN.SIM.LB(row[[1]],row[[2]], row[[3]], row[[4]], median),
                             paste('med_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                             'median', 'All', row[[5]])
                    })

KNN.LB.MED.RENN = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                     expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
               1, function(row){
                   list(GEN.KNN.SIM.LB.RENN(row[[1]],row[[2]], row[[3]], row[[4]], median),
                        paste('med_renn_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                        'median', 'RENN', row[[5]])
               })

KNN.LB.MED.CNN = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                                 expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
                           1, function(row){
                               list(GEN.KNN.SIM.LB.CNN(row[[1]],row[[2]], row[[3]], row[[4]], median),
                                    paste('med_cnn_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                                    'median', 'CNN', row[[5]])
                           })

KNN.LB.MEAN = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                         expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
                   1, function(row){
                       list(GEN.KNN.SIM.LB(row[[1]],row[[2]], row[[3]], row[[4]], mean),
                            paste('mean_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                            'mean', 'All', row[[5]])
                   })

KNN.LB.MEAN.RENN = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                                 expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
                           1, function(row){
                               list(GEN.KNN.SIM.LB.RENN(row[[1]],row[[2]], row[[3]], row[[4]], mean),
                                    paste('mean_renn_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                                    'mean', 'RENN', row[[5]])
                           })

KNN.LB.MEAN.CNN = apply(cbind(expand.grid(CO.IMPLS, KS, NBS.SELECTORS, VOTE.STRATEGIES),
                               expand.grid(CO.IMPLS.NAME, KS, NBS.SELECTORS.NAME, VOTE.STRATEGIES.NAME)),
                         1, function(row){
                             list(GEN.KNN.SIM.LB.CNN(row[[1]],row[[2]], row[[3]], row[[4]], mean),
                                  paste('mean_cnn_', row[[6]],'_(', row[[5]], ')_(', row[[7]], ')_', row[[8]], sep=''),
                                  'mean', 'CNN', row[[5]])
                         })

RANDOM = list(
    list(function(training.set){
        force(training.set)
        values = unique(sapply(training.set, '[[', 'type'))
        values = sample(values)
        return(function(m){
            force(m)
            return(sample(values, 1))
        })
    },
    'rand1',
    'mean',
    'All',
    'R'
    ),
    list(function(training.set){
        force(training.set)
        values = unique(sapply(training.set, '[[', 'type'))
        values = sample(values)
        return(function(m){
            force(m)
            return(sample(values, 1))
        })
    },
    'rand2',
    'median',
    'All',
    'R'
    )
)

# at least 2 classifiers must be defined
# name and class must not contain '-' and '=' signs (must be valid data.frame column name)
KNN.LIST = c(
    RANDOM
    ,KNN.LB.MED.CNN
    ,KNN.LB.MEAN.CNN
    ,KNN.LB.MED.RENN
    ,KNN.LB.MEAN.RENN
    ,KNN.LB.MED
    ,KNN.LB.MEAN
)


if(exists("CLASSIFIER.NUMBER.LIMIT") && is.finite(CLASSIFIER.NUMBER.LIMIT)){
    # KNN.LIST = KNN.LIST[sample(length(KNN.LIST), CLASSIFIER.NUMBER.LIMIT)]
    KNN.LIST = KNN.LIST[1:(min(length(KNN.LIST), CLASSIFIER.NUMBER.LIMIT))]
}

# KNN.LIST = sample(KNN.LIST, length(KNN.LIST))

KNN = sapply(KNN.LIST,'[[',1)
KNN.NAME = sapply(KNN.LIST,'[[',2)
KNN.CLASS = sapply(KNN.LIST,'[[',3)
KNN.SUBCLASS = sapply(KNN.LIST,'[[',4)
KNN.SUBSUBCLASS = sapply(KNN.LIST,'[[',5)

KNN.BINDED.DESCRIPTION = data.frame(Method=KNN.NAME,
                                    Class=KNN.CLASS,
                                    Subclass=KNN.SUBCLASS,
                                    Subsubclass=KNN.SUBSUBCLASS)


getOptimizedClassifiers = function(inputData, performanceMeasure, select_classifiers=3) {
    result = apply(expand.grid(unique(KNN.CLASS), unique(KNN.SUBCLASS), unique(KNN.SUBSUBCLASS)), 1, function(row){
        df = subset(inputData, Measure==performanceMeasure & Method %in% KNN.NAME &
                        Class==row[[1]] & Subclass==row[[2]] & Subsubclass==row[[3]])
        opts = c()
        if(nrow(df)>0){
            if(PERFORMANCE.MEASURE.DESC){
                opts = arrange(df, desc(Value))[1:min(nrow(df), select_classifiers), 1]
            } else {
                opts = arrange(df, Value)[1:min(nrow(df), select_classifiers), 1]
            }
        } else {
            opts = c()
        }
        return(opts)
    })

    return(unlist(result))
}
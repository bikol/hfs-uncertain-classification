# ---- init ----

rm(list=ls())

source("config.R")
source("stats.R")
# source("similarities.R")
source("logic-based.R")
source("mcn-test.R")
source("utils.R")

library(parallel)
library(reshape2)
library(dplyr)
library(Matrix)
library(igraph)
library(RCurl)


EVALUATION.OUTPUT.FILE = 'dyslexic-eval-output.RData'
EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

# ---- read-datasets ----

printDebug("read datasets")

DATA.COLS.NUM = 12

dataset.dyslexic = lapply(1:10, function(fold) {
    train.data = read.keel(paste0(KEEL.DATABASE.LOCATION, '/dyslexic_12_4-10-',fold,'tra.dat'))
    test.data = read.keel(paste0(KEEL.DATABASE.LOCATION, '/dyslexic_12_4-10-',fold,'tst.dat'))

    # normalise into [0,1] interval
    train.data[, 1:(DATA.COLS.NUM*2)] = train.data[, 1:(DATA.COLS.NUM*2)]/10
    test.data[, 1:(DATA.COLS.NUM*2)] = test.data[, 1:(DATA.COLS.NUM*2)]/10

    # multiply observations with multiple classes
    train.data = bind_rows(train.data,
                           filter(train.data, !is.na(Class2)) %>% mutate(Class1=Class2),
                           filter(train.data, !is.na(Class3)) %>% mutate(Class1=Class3))

    return(list(train=train.data, test=test.data))
})


# ---- parallel-init ----

printDebug("paralell init")

if (PARALLEL)
{
    CL = makePSOCKcluster(
        names=PARALLEL.HOSTS,
        master=PARALLEL.MASTER,
        user=PARALLEL.HOST.USERNAME,
        outfile="",
        homogeneous=FALSE,
        useXDR=FALSE,
        timeout=24*3600,
        methods = FALSE
    )

    clusterExport(CL, c("SEED", "OBSUCRE.REPEAT", "PARALLEL.LIBS.PATH"))
    clusterCall(CL, function(){
        if (length(PARALLEL.LIBS.PATH) > 0 ){
            .libPaths( c(PARALLEL.LIBS.PATH , .libPaths() ) )
        }
        set.seed(SEED)
    })

    clusterExport(cl=CL, list('topo_sort', 'graph_from_adjacency_matrix', 'DescIterBinSearch', 'AscIterBinSearch',
                              'KNN.MAX.CASE.BASE.SIZE', 'invPerm', 'PROBE.SIZE',
                              'KNN', 'KNN.NAME',
                              'DATA.COLS.NUM', 'read.keel', 'KEEL.DATABASE.LOCATION',
                              'multiclassToOutcome', 'filter', 'bind_rows','mutate', '%>%',
                              'printDebug', 'DEBUG', 'dataset.dyslexic'))
    usedLapply = function(...){ clusterApplyLB(CL, ...) }
    print(CL)
} else {
    usedLapply = lapply
}

# ---- training-statistics-knn ----
if(!SKIP.KNN) {
    printDebug("training statistics knn")

    outcomes.knns = usedLapply(1:length(KNN), function(j){
        printDebug(paste0("Using ", KNN.NAME[[j]]," classifier"))
        startTime = as.integer(Sys.time())
        diags = c()

        fold.size = PROBE.SIZE/10

        diags = sapply(1:10, function(fold) {
            # reading training and test set for 10 fold CV
            train.data = dataset.dyslexic[[fold]][['train']]
            test.data = dataset.dyslexic[[fold]][['test']]

            # convert training set into proper format accepted by classifier
            ts = apply(train.data, 1 , function(x){
                return(list(m=matrix(as.numeric(x[1:(DATA.COLS.NUM*2)]), nrow=2), type=x[DATA.COLS.NUM*2+1]))
            })

            if(length(ts) > KNN.MAX.CASE.BASE.SIZE) {
                classifier = KNN[[j]](ts[sample(length(ts), min(length(ts), KNN.MAX.CASE.BASE.SIZE))])
            } else {
                classifier = KNN[[j]](ts)
            }

            # selection of appropriate columns
            tmp = apply(test.data[ , 1:(DATA.COLS.NUM*2)], 1, function(row) {
                # matrix in format required by aggregation method is created and passed into classifier
                return(classifier(matrix(row, nrow=2)))
            })

            converted = apply(cbind(tmp, test.data[ ,(DATA.COLS.NUM*2+1):(DATA.COLS.NUM*2+4)]),
                              1, multiclassToOutcome)
            return(converted)
        })
        printDebug(paste0("Finished (",floor(100*j/length(KNN)),"%)\t", KNN.NAME[[j]],"\tin ", as.integer(Sys.time()) - startTime, ' secs'))
        return(unlist(diags))
    })

    printDebug("finished knn classification")

    outcomes.knns        = data.frame(Dummy1=NA, Dummy2=0, Dummy3=1, outcomes.knns)
    names(outcomes.knns) = c('Dummy1', 'ObscureLevel', 'ObscureRepeat', KNN.NAME)

    training.stats.knns = melt(calculate.stats(
        aggregate.outcomes(outcomes.knns)
    ),
    id.vars = "ObscureLevel" ,
    variable.name = "Method",
    value.name = "Value") %>%
        dplyr::rename(Measure=L1)

    training.stats.knns = suppressWarnings( # suppress different factor levels warning
        left_join(training.stats.knns,
                  KNN.BINDED.DESCRIPTION,
                  by="Method")
    )
}


# ---- training-statistics-bind ----
if(!SKIP.TRAINING) {
    printDebug("training statistics bind")
    training.stats.all = suppressWarnings( # suppress different factor levels warning
        bind_rows( if(!SKIP.KNN) training.stats.knns else data.frame(),
                   data.frame() # allow to bind multiple types
        )
    )
}

# ---- training-statistics-performance-calculation ----
if(!SKIP.TRAINING) {
    printDebug("training statistics performance calculation")

    training.stats.all.perf = aggregate(training.stats.all$Value,
                                        list(Method=training.stats.all$Method,
                                             Measure=training.stats.all$Measure),
                                        mean) %>%
        dplyr::rename(Value=x)

    training.stats.all.perf = suppressWarnings( # suppress different factor levels warning
        left_join(training.stats.all.perf,
                  distinct(select(training.stats.all,
                                  Method, Class, Subclass, Subsubclass)),
                  by="Method")
    )
}


# ---- convert-statistics-performance-to-wide-format ----

printDebug("convert statistics performance to wide format")

training.stats.all.perf.wide = dcast(training.stats.all.perf,
                                    Method + Class + Subclass + Subsubclass ~ Measure,
                                    value.var="Value")

# ---- parallel-shutdown ----

printDebug("parallel shutdown")

if (PARALLEL)
    stopCluster(CL)

# ---- save-evaluation ----

printDebug("save evaluation")

save.image(EVALUATION.OUTPUT.LOCATION)

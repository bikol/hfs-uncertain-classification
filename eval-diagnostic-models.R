# ---- init ----

rm(list=ls())

source("config.R")
source("stats.R")
source("logic-based.R")
source("utils.R")

library(parallel)
library(reshape2)
library(dplyr)
library(igraph)
library(Matrix)

EVALUATION.OUTPUT.FILE = 'models-eval-output.RData'
EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

# ---- read-datasets ----

printDebug("read datasets")

DATA.COLS.NUM = 6

colClass = c("factor", "numeric", "integer", "integer",
             rep("numeric", times=2*DATA.COLS.NUM))

ds.training = read.csv(TRAINING.LOCATION, colClasses=colClass)
ds.test     = read.csv(TEST.LOCATION,     colClasses=colClass)


# ---- main-evaluation-procedure
source('eval.R')

# ---- save-evaluation ----

printDebug("save evaluation")

save.image(EVALUATION.OUTPUT.LOCATION)

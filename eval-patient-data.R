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

EVALUATION.OUTPUT.FILE = 'patients-eval-output.RData'
EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

# ---- read-datasets ----

printDebug("read datasets")

DATA.COLS.NUM = 17

colClass = c("factor", "numeric", "integer", "integer",
             rep("numeric", times=DATA.COLS.NUM))

ds.training = read.csv(PATIENT.TRAINING.LOCATION, colClasses=colClass)
ds.test     = read.csv(PATIENT.TEST.LOCATION,     colClasses=colClass)

# ---- convert datasets into interval format

ds.training = bind_cols(ds.training[,1:4], convert.to.interval.format(ds.training[,5:ncol(ds.training)]))
ds.test = bind_cols(ds.test[,1:4], convert.to.interval.format(ds.test[,5:ncol(ds.test)]))

# ---- add weights to model

attr.weights = c(OvarianCancerInFamily = 0.0811846902074589, HormoneReplacementTherapy = 0.0527697295397988,
  Age = 0.156737736782637, ADimension = 0.0760681154580488, PainAtExamination = 0.0453391400018752,
  Ascites = 0.120576196790257, PapBloodFlow = 0.187496297991853,
  Solid = 0.0979902358579511, ASolidDimension = 0.254532021123308,
  InnerWall = 0.075542711873786, Shadow = 0.242857210966557, Color = 0.145871689872967,
  Septum = 0.0325681773592309, SmEchogenicity = 0.108436335099078,
  Location = 0.0178667189850239, SmInnerWallThickness = 0.0532077692760447,
  TumorVolume = 0.108750245242299, Pap = 0, APapDimension = 0.132154585890316,
  SeptumThickness = 0.013517755542476, AgeAfterMenopause = 0.0621290882774307,
  Ca125 = 0.325574909094303, Ri = 0.112595419847328, UterusRemoved = 0.00541518259389858,
  IotaQuality = 0)


# ---- main-evaluation-procedure
source('eval.R')

# ---- save-evaluation ----

printDebug("save evaluation")

save.image(EVALUATION.OUTPUT.LOCATION)

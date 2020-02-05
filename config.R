SEED                = 1337

THREADS             = 32
PARALLEL            = TRUE
PARALLEL.MASTER     = "localhost"
PARALLEL.HOSTS      = rep("localhost", THREADS)
PARALLEL.HOST.USERNAME = "ruser"
PARALLEL.LIBS.PATH  = c()

DEBUG               = TRUE

TRAINING.SIZE       = 200
PROBE.SIZE.NEGATIVE = 75
PROBE.SIZE.POSITIVE = 75
PROBE.SIZE          = PROBE.SIZE.NEGATIVE + PROBE.SIZE.POSITIVE

KNN.MAX.CASE.BASE.SIZE   = Inf
CLASSIFIER.NUMBER.LIMIT   = Inf

SKIP.TRAINING       = F
SKIP.KNN            = F

OBSCURE.MAX         = 0.5
OBSCURE.PERCENTAGES = c(seq(0.0, OBSCURE.MAX, 0.05))
OBSUCRE.REPEAT      = 100

PERFORMANCE.MEASURE      = "Cost matrix"
PERFORMANCE.MEASURE.DESC = FALSE

# should not be less then 2
# CLASSIFIER.OPTIMISATION.NUMBER = 50

DATASETS.DIR           = 'datasets'
DATABASE.FILE          = 'db-2015-04-30.csv'
TRAINING.FILE          = 'training.csv'
TEST.FILE              = 'test.csv'
PATIENT.TRAINING.FILE          = 'patient-training.csv'
PATIENT.TEST.FILE              = 'patient-test.csv'
EVALUATION.OUTPUT.FILE = 'evaluation-output.RData'

DATABASE.LOCATION          = paste(DATASETS.DIR, DATABASE.FILE, sep='/')
TRAINING.LOCATION          = paste(DATASETS.DIR, TRAINING.FILE, sep='/')
TEST.LOCATION              = paste(DATASETS.DIR, TEST.FILE,     sep='/')
PATIENT.TRAINING.LOCATION          = paste(DATASETS.DIR, PATIENT.TRAINING.FILE, sep='/')
PATIENT.TEST.LOCATION              = paste(DATASETS.DIR, PATIENT.TEST.FILE,     sep='/')
EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

GEN.DATA.URL = "https://min.wmi.amu.edu.pl/data/ovarian-tumor-aggregation"
KEEL.DATABASE.LOCATION = paste0(DATASETS.DIR,"/keel-dyslexic/")
KEEL.DATA.URL = 'https://sci2s.ugr.es/keel/dataset/data/lowQuality/dyslexic_12_4-10-fold.zip'

set.seed(SEED)

if (file.exists("config.R.user"))
    source("config.R.user")
library(class)    # provides knn function
library(gmodels)  # Various R Programming Tools for Model Fitting
#----------------------------------------------------------------------------------------------------
tbl <- read.table("wisc_bc_data.csv", sep=",", header=TRUE, nrow=-1)
dim(tbl)
str(tbl)
tbl <- tbl[, -1]
str(tbl)
stopifnot(is.factor(tbl$diagnosis))
table(tbl$diagnosis)
#   B   M
# 357 212
round(prop.table(table(tbl$diagnosis)) * 100, digits=1)
#    B      M
# 62.7   37.3

# choose min-max normalization.  but why not z-score standardization?
# books says, "the traditional method of rescaling features for k-NN
# is min-max normalization.

normalize <- function(v){
   (v - min(v))/(max(v) - min(v))
}

tbl.norm <- as.data.frame(lapply(tbl[, 2:31], normalize))
str(tbl)
str(tbl.norm)
fivenum(as.numeric(tbl.norm))
lapply(tbl.norm, class)
do.call(hist, tbl.norm)
boxplot(as.matrix(tbl.norm))   # plots each column
nrow(tbl.norm)

training.set <- 1:469
test.set <- 470:569
tbl.train <- tbl.norm[training.set,]
tbl.test  <- tbl.norm[test.set,]

labels.train <- tbl[training.set,1]
labels.test  <- tbl[test.set,1]

args(knn)
prediction <- knn(train=tbl.train, test=tbl.test, cl=labels.train, k=21)

stopifnot(length(prediction) == length(test.set))  # 100

xt <- CrossTable(x=labels.test, y=prediction, prop.chisq=FALSE)
tbl.assess <- xt$t
class(tbl.assess)
dim(tbl.assess)
true.negative <- tbl.assess[1,1]    # 61
true.positive <- tbl.assess[2,2]    # 37
false.positive <- tbl.assess[1,2]   # 0
false.negative <- tbl.assess[2,1]   # 2


tbl.z <- as.data.frame(scale(tbl[,-1]))
par(mfrow=c(1,2))
boxplot(as.matrix(tbl.z))
boxplot(as.matrix(tbl.norm))

fivenum(as.matrix(tbl.z))   # -3.1093489 -0.6651222 -0.2286439  0.4690336 12.0620671
summary(tbl.z$area_mean)    # -1.4532 -0.6666 -0.2949  0.0000  0.3632  5.2459

tbl.train <- tbl.z[training.set,]
tbl.test  <- tbl.z[test.set,]

prediction <- knn(train=tbl.train, test=tbl.test, cl=labels.train, k=21)
# prediction is a list of factors, 1 per tbl.test row, either Benign or Malignant
xt <- CrossTable(x=labels.test, y=prediction, prop.chisq=FALSE)
xt$t   # now 5 false negatives M tumors classified as B

## -----------------------------------------------------------------------------------
## Demo file for scoremodel; start with 'demo(scoremodel)'
## -----------------------------------------------------------------------------------

devAskNewPage(ask = FALSE)
library(scoremodel)
data(CreditData)
#convert character variables to factors

mysample <- convertType(CreditData, toType = "fac", vars = -1)
#homogeneity exclusion

mysample <- delNArate(mysample, narate_critical = 0.9)[[3]]
mysample <- delFewValues(mysample, minN = 5, minF = 2, exclude = "target")
mysample <- delSinvalPercent(mysample, percent = 0.9, exclude = "target")
#split dataset to train and test

set.seed(21)
splitresult <- splitData(mysample, size = 0.7, ifpercent = TRUE)
train <- splitresult[[1]]
test <- splitresult[[2]]
#optimal binning and generate woe configuration list

configlist <- dfBinningFun(train, binMethod = 1, p = 0.05, maxcat = 10, aliquots = 5)
#woe encoding

x_train_encoding <- woeEncodeFun_df(train[-length(train)], config_list = configlist)
x_test_encoding <- woeEncodeFun_df(test[-length(test)], config_list = configlist)
train_encoding <- cbind(x_train_encoding, train["target"])
#collinear elimination

train_encoding_red <- collElimination(train_encoding, cor_critical = 0.8)
#model training

fit <- LRfit(train_encoding_red, sig = 0.05)
summary(fit)
#prediction

p_train <- LRpredict(fit)
p_test <- LRpredict(fit, newdata = x_test_encoding)
#calculate KS of model

myks(train[,"target"], p_train)
myks(test[,"target"], p_test)
#calculate PSI of model

psi(p_train, p_test, bins = 10, binMethod = "EF")
#model performance visualization

myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = FALSE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = TRUE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
devAskNewPage(ask = TRUE)

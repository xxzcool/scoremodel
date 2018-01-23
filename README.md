# scoremodel
## Credit scoring modeling toolbox based on R

A set of flexible, efficient and easy to use functions to build a credit scoring model from beginning to end, including dataset reading and preprocessing, variable binning and woe-encoding, model performance and stability validation, as well as visualization.

### Installation
```R
install.packages("devtools")
devtools::install_version("smbinning", version = 0.4)
devtools::install_github("xxzcool/scoremodel")
```
#### Notes:
This package depends on 'smbinning' prior to version 0.4, so you have to install the specified version of 'smbinning' at first.

### Example
```R
library(scoremodel)
data(CreditData)
#convert character variables to factors
mysample <- convertToFactor(CreditData)
#homogeneity inspect
mysample <- delNArate(mysample, narate_critical = 0.9)[[3]]
mysample <- delFewValues(mysample, minN = 5, minF = 2, exclude = "target")
mysample <- delSinvalPercent(mysample, percent = 0.9, exclude = "target")
#split dataset to train and test
splitresult <- splitData(mysample, size = 0.7, ifpercent = TRUE)
train <- splitresult[[1]]
test <- splitresult[[2]]
#optimal binning and generate woe configuration list
configlist <- dfBinningFun(train, binMethod = 1, p = 0.05, maxcat = 10, aliquots = 5)
#woe_encoding
x_train_encoding <- woeEncodeFun_df(train[-length(train)], config_list = configlist)
x_test_encoding <- woeEncodeFun_df(test[-length(test)], config_list = configlist)
train_encoding <- cbind(x_train_encoding, train["target"])
#collinear elimination
train_encoding_red <- collElimination(train_encoding, cor_critical = 0.8)
#model training
fit <- LRfit(train_encoding_red, sig = 0.01)
#prediction
p_train <- LRpredict(fit)
p_test <- LRpredict(fit, newdata = x_test_encoding)
#calculate KS of model
myks(train[,"target"], p_train)
myks(test[,"target"], p_test)
#calculate PSI of model
psi(p_train, p_test)
#model performance visualization
myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = FALSE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = TRUE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
```

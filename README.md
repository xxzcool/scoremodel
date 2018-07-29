# scoremodel
[![Build Status](https://travis-ci.org/xxzcool/scoremodel.svg?branch=master)](https://travis-ci.org/xxzcool/scoremodel) [![GitHub version](https://badge.fury.io/gh/xxzcool%2Fscoremodel.svg)](https://badge.fury.io/for/gh/xxzcool/scoremodel) [![Open Source Love](https://badges.frapsoft.com/os/gpl/gpl.svg?v=102)](https://github.com/xxzcool/scoremodel/blob/master/LICENSE) 
## Credit scoring modeling toolbox based on R


## Overview
A set of flexible, efficient and easy to use functions to build a credit scoring model from beginning to end, including dataset reading and preprocessing, variable binning and woe-encoding, model performance and stability validation, as well as visualization.


## Installation
```R
install.packages("devtools")
devtools::install_version("smbinning", version = 0.4)
devtools::install_github("xxzcool/scoremodel")
```
#### Notes:
This package depends on 'smbinning' prior to version 0.4, so you have to install the specified version of 'smbinning' at first.


## Usage
The following is a demo for illustrating how to use this package to establish a credit scoring model from beginning to end.

```R
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
```


## Version Records
#### scoremodel 0.3.2 2018-07-29
- Fix bug of 'convertType': Beforehand check if there is at least one variable can be converted to avoid errors.
- Fix bug of 'LRfit': Adaptable update with dependency package 'broom' to avoid errors.

#### scoremodel 0.3.1 2018-02-22
- Speed up four functions, including 'woeEncodeFun_df', 'executeBinFun_df', 'rawPredictFun_df' and 'crossValidation'.
- No longer export four less-used auxiliary functions, including 'woeEncodeFun', 'executeBinFun', 'rawPredictFun', 'maxSinvalPercent_x'.

#### scoremodel 0.3.0 2018-02-11
- Greatly speed up three functions, including 'delSinvalPercent', 'dfIV' and 'collElimination'.

#### scoremodel 0.2.1 2018-02-05
- Fix bug of 'LRfit': add recursive judgment to filter out insignificant variables completely.

#### scoremodel 0.2.0 2018-01-31
- Add an auxiliary function of 'insertElement'.
- Fix bug of 'psi': add two parameters of bins number and binning method, and set its percentage to a minima if the bin is missing to avoid calculation failure.
- More flexible of 'convertType': rename function from 'convertToFactor' to 'convertType', and add two parameters of toType and vars to integrate three types of conversions and be able to specify converted variables.
- Rename some output files of 'preBinningFun'.
- Travis CI building pass for the first time.

#### scoremodel 0.1.0 2018-01-22
- First release.

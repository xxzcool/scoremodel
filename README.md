# scoremodel
## Credit scoring modeling toolbox based on R

A set of flexible, efficient and easy to use functions to build a credit scoring model from beginning to end, including dataset reading and preprocessing, variable binning and woe-encoding, model performance and stability validation, as well as visualization.

## Example:
```R
data(CreditData)
mysample <- convertToFactor(CreditData)
splitresult <- splitData(mysample, size = 0.7, ifpercent = TRUE)
train <- splitresult[[1]]
test <- splitresult[[2]]
configlist <- dfBinningFun(train, binMethod = 1, p = 0.05, maxcat = 10, aliquots = 5)
x_train_encoding <- woeEncodeFun_df(train[-length(train)], config_list = configlist)
x_test_encoding <- woeEncodeFun_df(test[-length(test)], config_list = configlist)
train_encoding <- cbind(x_train_encoding, train["target"])
train_encoding_red <- collElimination(train_encoding, cor_critical = 0.8)
fit <- LRfit(train_encoding_red, sig = 0.05)
p_train <- LRpredict(fit)
p_test <- LRpredict(fit, newdata = x_test_encoding)
print("ks of train:")
myks(train[,"target"], p_train)
print("ks of test:")
myks(test[,"target"], p_test)
print("psi of test:")
psi(p_train, p_test)
myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = FALSE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
myCurves(train[,"target"], p_train, test[,"target"], p_test, ontest = TRUE,
         lift_bins = 10, P0 = 600, PDO = 50, color_scheme = 1, ifsave = FALSE)
```

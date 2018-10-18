##******author:zhonghongfa******
##******create:2017-09-20*******
##******update:2018-10-18*******

#' @title Read Dataset Based on Data-dictionary
#'
#' @description
#' \code{fread_basedict} is a wrapper of \code{fread}, which allows you read dataset based on your data-dictionary(see details).
#'
#' @details
#' the format of data-dictionary is required:
#' 1.the first column must be names of variables;
#' 2.the second column must be labels of variables;
#' 3.the third column must be flags which indicate whether it is a variable;
#' 4.the fourth column must be types of variables, and the value in this column should be 'VARCHAR', 'NUMBER' or NULL.
#'
#' @param myfile File name character of data set, must be a csv file.
#' @param mydict File name character of variable dictionary, must be a csv file.
#' @param na A character vector which to be interpreted as NA values, default \code{c("",".","NA","N/A","NULL")}.
#' @param excludevar A vector consisting of the variable names want to be excluded in 'mydict' file, default \code{NULL}.
#'
#' @return A dataframe
#' @importFrom data.table fread
#' @export
fread_basedict <- function(myfile,mydict,na=c("",".","NA","N/A","NULL"),excludevar=NULL) {
  # library(data.table)
  vardict <- fread(mydict,colClasses=list(character=c(1:2,4),numeric=c(3)),data.table=FALSE,na.strings=na)
  if(!is.null(excludevar)) {
    vardict <- vardict[!vardict[,1] %in% excludevar,]
  }
  ifVar <- which(vardict[,3]==0)
  ifVarName <- vardict[,1][ifVar]
  vardict <- vardict[vardict[,3]==1,]           #IF_VAR==1
  charVar <- which(toupper(vardict[,4])=="VARCHAR")
  numerVar <- which(toupper(vardict[,4])=="NUMBER")
  charVarName <- vardict[,1][charVar]
  numerVarName <- vardict[,1][numerVar]
  mydata <- fread(myfile,colClasses=list(character=charVarName,numeric=numerVarName,NULL=ifVarName),data.table=FALSE,na.strings=na)
  return(mydata)
}


#' @title Convert Types of Variables
#'
#' @description
#' \code{convertType} will convert specified variables in dataset to other types.
#'
#' @details
#' when \code{vars=-1}, which variables to be converted depends on \code{toType}:
#' if \code{toType="fac"}, all the character variables will be converted;
#' if \code{toType="cha"}, all the factor variables will be converted;
#' if \code{toType="int"}, all the numeric variables will be converted.
#'
#' @param df A dataframe to be converted.
#' @param vars Vector of column names or numbers to convert, \code{-1} means to convert all matched variables based on \code{toType} automatically, see details.
#' @param toType The type converted to be, values must be one of \code{c("fac","cha","int")}. If \code{toType="int"}, the converted result is intercepting the integer part of specified variable, not rounding.
#'
#' @return A new dataframe with the same order of variables
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' data(CreditData)
#' sample_fac <- convertType(CreditData, toType="fac")
convertType <- function(df,toType,vars=-1) {
  # library(dplyr)
  ncl <- ncol(df)
  nm <- names(df)
  if(length(toType) != 1) stop("the length of parameter 'toType' must be equal to 1")
  if(!toType %in% c("fac","cha","int")) stop("parameter 'toType' must be one of c('fac','cha','int')")
  if(sum(is.na(vars)) > 0) stop("parameter 'vars' contains NA")
  if(is.character(vars)) {
    if(sum(!vars %in% nm) > 0) stop(paste(paste(vars[which(!vars %in% nm)],collapse=","),"is not in the variable names of 'df'"))
    df.spec <- df[vars]
    df.unspec <- df[!nm %in% vars]
    if(toType == "fac") {
      df.factor <- as.data.frame(sapply(df.spec,as.factor))
      res <- cbind(df.unspec,df.factor)
    } else if(toType == "cha") {
      df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
      res <- cbind(df.unspec,df.cha)
    } else {
      df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
      res <- cbind(df.unspec,df.int)
    }
  } else if(is.numeric(vars)) {
    if(!all(as.integer(vars) == vars)) stop("parameter 'vars' is float type, not allowed")
    if(length(vars) == 1) {
      if(vars < 1 & vars != -1) stop("parameter 'vars' is less than 1")
      if(vars > ncl) stop("parameter 'vars' is over the number of dataframe's columns")
      if(vars == -1) {
        if(toType == "fac") {
          if(sum(!sapply(df,is.numeric)) == 0) stop("no character variables can be converted to factors")
          df.num <- df[sapply(df,is.numeric)]
          df.str <- df[!sapply(df,is.numeric)]
          df.factor <- as.data.frame(sapply(df.str,as.factor))
          res <- cbind(df.num,df.factor)
        } else if(toType == "cha") {
          if(sum(!sapply(df,is.factor)) == 0) stop("no factors can be converted to character variables")
          df.factor <- df[sapply(df,is.factor)]
          df.unfac <- df[!sapply(df,is.factor)]
          df.cha <- as.data.frame(sapply(df.factor, as.character), stringsAsFactors = FALSE)
          res <- cbind(df.unfac,df.cha)
        } else {
          if(sum(sapply(df,is.numeric)) == 0) stop("no numeric variables can be converted to integer variables")
          df.num <- df[sapply(df,is.numeric)]
          df.str <- df[!sapply(df,is.numeric)]
          df.int <- as.data.frame(sapply(df.num, as.integer), stringsAsFactors = FALSE)
          res <- cbind(df.str,df.int)
        }
      } else {
        df.spec <- df[vars]
        df.unspec <- df[-vars]
        if(toType == "fac") {
          df.factor <- as.data.frame(sapply(df.spec,as.factor))
          res <- cbind(df.unspec,df.factor)
        } else if(toType == "cha") {
          df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
          res <- cbind(df.unspec,df.cha)
        } else {
          df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
          res <- cbind(df.unspec,df.int)
        }
      }
    } else if(length(vars) > 1) {
      if(min(vars,na.rm=TRUE) < 1) stop("the min element in 'vars' is less than 1")
      if(max(vars,na.rm=TRUE) > ncl) stop("the max element in 'vars' is over the number of dataframe's columns")
      df.spec <- df[vars]
      df.unspec <- df[-vars]
      if(toType == "fac") {
        df.factor <- as.data.frame(sapply(df.spec,as.factor))
        res <- cbind(df.unspec,df.factor)
      } else if(toType == "cha") {
        df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
        res <- cbind(df.unspec,df.cha)
      } else {
        df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
        res <- cbind(df.unspec,df.int)
      }
    } else {
      stop("the length of vector 'vars' must be more than or equal to 1")
    }
  } else {
    stop("parameter 'vars' must be an integer or character vector")
  }
  res <- select(res,nm)    #sort the variables
  return(res)
}


#' @title Splitting Dataset into Train and Test Set
#'
#' @description
#' \code{splitData} will split the dataset into train and test set by given sampling fraction or quantity, then return a list.
#'
#' @param df A dataframe to be splitted.
#' @param size A numeric. Specifies the sampling fraction or quantity of train set.
#' @param ifpercent Logical, indicating whether the sampling fraction or quantity to be specified.
#'
#' @return A list contains train and test set, and train set is the fisrt.
#' @export
#'
#' @examples
#' data(CreditData)
#' splitresult <- splitData(CreditData, size = 0.7, ifpercent = TRUE)
#' train <- splitresult[[1]]
#' test <- splitresult[[2]]
splitData <- function(df,size,ifpercent=TRUE) {
  nrw <- nrow(df)
  if(!is.numeric(size)) stop("the parameter 'size' must be a numeric")
  if(length(size) != 1) stop("the length of parameter 'size' must be 1")
  if(ifpercent) {
    if(size < 0 | size > 1) stop("the sampling fraction must between 0 and 1")
    size <- round(nrw * size)
  } else {
    if(size < 1 | size > nrw) stop(paste("the sampling quantity must between 1 and ",nrw,sep=""))
    size <- round(size)
  }
  id <- 1:nrw
  id.train <- sample(id, size, replace = FALSE)
  id.test <- id[!id %in% id.train]
  train <- df[id.train,]
  test <- df[id.test,]
  return(list(train,test))
}


#' @title Logistic Regression Model Training by Stepwise
#'
#' @description
#' \code{LRfit} is a simple wrapper of \code{\link{glm}} for logistic regression, you only need to specify the data and significance-level for training.
#'
#' @param df A dataframe only with Xs and Y variable, all the category X variables must have been encoded(such as WOE, One-Hot and Dumb-Variable encoding).
#' @param sig A numeric of significance-level.
#'
#' @return Same as \code{\link{glm}}
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats binomial
#' @importFrom stats step
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export
LRfit <- function(df,sig=0.05) {
  nm <- names(df)
  ncl <- ncol(df)
  if(ncl<2) stop("the ncol of 'df' must be least 2")
  fmla <- as.formula(paste(nm[ncl]," ~ .",sep=""))
  fit <- glm(fmla,data=df,family=binomial())
  step_fit <- step(fit,direction="both")
  step_df <- tidy(step_fit)
  step_df <- step_df[-1,]
  if(max(step_df$p.value) >= sig | min(step_df$estimate) <=0) {
    step_df <- filter(step_df, p.value < sig & estimate > 0)
    filvars <- step_df[,1]
    df <- select(df,c(filvars,nm[ncl]))
    return(LRfit(df, sig))
  } else {
    return(step_fit)
  }
}


#' @title Auxiliary Function: Extract Elements From Fit Object
#'
#' @description
#' Auxiliary function: \code{extfromFit} will extract elements from a \code{fit} object, then return a vector.
#'
#' @details
#' parameter \code{fit} can be generated by function \code{\link{LRfit}} or \code{\link{glm}}.
#' if element 'term' is extracted, the extracting result already contains '(Intercept)' at the first one, please remember to remove it if you just need variable names.
#'
#' @param fit A fitted object of class inheriting from \code{\link{LRfit}} or \code{\link{glm}}.
#' @param element Name character of element to be extracted, can be one of c("term","estimate","std.error","statistic","p.value").
#'
#' @return A vector
#' @importFrom broom tidy
#' @export
extfromFit <- function(fit,element) {
  # library(broom)
  ele_vec <- c("term","estimate","std.error","statistic","p.value")
  if(!element %in% ele_vec) stop("parameter 'element' must be one of c('term','estimate','std.error','statistic','p.value')")
  fit_df <- tidy(fit)
  result <- fit_df[,element]
  return(result)
}


#' @title Logistic Regression Model Predicting
#'
#' @description
#' \code{LRpredict} is a wrapper of \code{\link{predict}}, which can predict probabilities on train or test set. Different from \code{\link{rawPredictFun_df}}, it does not need to give model coefficient and woe configuration list, but the given \code{newdata} must be woe-encoded in advance when predicts on test.
#'
#' @details
#' parameter \code{fit} can be generated by function \code{\link{LRfit}} or \code{\link{glm}}.
#'
#' @param fit A fitted object of class inheriting from \code{\link{LRfit}} or \code{\link{glm}}.
#' @param newdata A new dataset to be predicted, which only contain x variables, and must have been woe-encoded. Default \code{NULL}, means predicting on train.
#'
#' @return A numeric vector
#' @family model prediction functions
#' @importFrom stats predict
#' @importFrom broom tidy
#' @export
LRpredict <- function(fit,newdata=NULL) {
  if(is.null(newdata)) {
    p <- predict(fit,type="response")
  } else {
    fit_df <- as.data.frame(tidy(fit), stringsAsFactors = FALSE)
    filvars <- fit_df[-1,1]
    newdata <- newdata[filvars]
    p <- predict(fit,newdata=newdata,type="response")
  }
  return(p)
}


# @title Auxiliary Function: Predict p Value of A Raw Observation
#
# @description
# Auxiliary function: \code{rawPredictFun} will predict the p value of an observation, based on woe configuration list(see details) and model coefficients. Different from \code{\link{LRpredict}}, this function can predict on one raw observation directly which have not been woe-encoded or binned.
#
# @details
# parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#
# @param x A data frame with only one row, which is the observation to be predicted, and it only contain X variables.
# @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a X variable.
# @param coef_vec A vector of model coefficients.
#
# @return A numeric
# @family model prediction functions
# @export
rawPredictFun <- function(x,config_list,coef_vec) {
  nclx <- ncol(x)
  len_conf <- length(config_list)
  if(nclx != len_conf) stop("the length of 'config_list' is not equal to the ncol of 'x'")
  woe_vec <- numeric(nclx + 1)
  woe_vec[1] <- 1
  for(i in 1:nclx) {
    woedf <- config_list[[i]]
    if(is.na(x[1,i])) {
      na_loc <- which(is.na(woedf[,1]))
      if(length(na_loc) == 1) {
        woe_vec[i+1] <- woedf[na_loc,2]
      } else if(length(na_loc) == 0) {
        stop(paste("there is a value '",x[1,i],"' of the ",i,"th variable not in bins of 'config_list'",sep=""))
      } else stop(paste("the ",i,"th element of 'config_list' is illegal",sep=""))
    } else if(is.numeric(x[,i])) {
      bin_loc <- which(woedf[,1] >= x[1,i])
      if(length(bin_loc) >= 1) {
        woe_vec[i+1] <- woedf[bin_loc[1],2]
      } else stop(paste("there is a value '",x[1,i],"' of the ",i,"th variable not in bins of 'config_list'",sep=""))
    } else {
      bin_loc <- which(woedf[,1] == x[1,i])
      if(length(bin_loc) == 1) {
        woe_vec[i+1] <- woedf[bin_loc,2]
      } else if(length(bin_loc) == 0) {
        stop(paste("there is a value '",x[1,i],"' of the ",i,"th variable not in bins of 'config_list'",sep=""))
      } else stop(paste("the ",i,"th element of 'config_list' is illegal",sep=""))
    }
  }
  p <- 1/(1 + exp(-sum(coef_vec*woe_vec)))
  return(p)
}


#' @title Predict p Value of Entire Raw Dataset
#'
#' @description
#' \code{rawPredictFun_df} will predict the p value of all observations, based on woe configuration list(see details) and model coefficients. Different from \code{\link{LRpredict}}, this function can predict on raw observations directly which have not been woe-encoded or binned.
#'
#' @details
#' parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#'
#' @param x_sample A dataframe with only X variables and contains all the observations to be predicted.
#' @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a x variable.
#' @param coef_vec A vector of model coefficients.
#'
#' @return A numeric vector
#' @family model prediction functions
#' @importFrom plyr alply
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel parSapply
#' @importFrom parallel stopCluster
#' @export
rawPredictFun_df <- function(x_sample,config_list,coef_vec) {
  # library(plyr)
  # library(parallel)
  x_sample_list <- alply(x_sample,1,.parallel=TRUE)
  clnum <- detectCores()
  cl <- makeCluster(getOption("cl.cores",clnum))
  p <- parSapply(cl,x_sample_list,rawPredictFun,config_list=config_list,coef_vec=coef_vec)
  stopCluster(cl)
  return(p)
}


#' @title Transform Prediction to Standard Score
#'
#' @description
#' \code{transformScore} will transform the predicted probability to standard score by giving base score and scale, then return a integer vector.
#'
#' @details
#' parameter \code{theta0} must be the bad:good of train at any time.
#'
#' @param p A numeric vector, the predicted probability.
#' @param theta0 A numeric, the base ratio of bad:good in train.
#' @param P0 An integer, the base score, default \code{600}.
#' @param PDO An integer, the scale, default \code{50}.
#'
#' @return A integer vector
#' @family model prediction functions
#' @export
transformScore <- function(p,theta0,P0=600,PDO=50) {
  B <- PDO/log(2)
  A <- P0 + B*log(theta0)
  score <- A - B*log(p/(1-p))
  return(round(score))
}


# @title Auxiliary Function: Encode One Observation of A Variable as WOE
#
# @description
# Auxiliary function: \code{woeEncodeFun_xval} will encode one observation of a variable as woe value based on \code{woedf}, then return a numeric.
#
# @details
# parameter \code{woedf} is a component of woe configuration list which generated by function \code{\link{dfBinningFun}}.
#
# @param xval An observation value of a variable.
# @param woedf A datafrme corresponding to current variable, which is a component of woe configuration list.
#
# @return A numeric
# @family dataset binning and woe-encoding functions
# @export
woeEncodeFun_xval <- function(xval,woedf) {
  if(is.na(xval)) {
    na_loc <- which(is.na(woedf[,1]))
    if(length(na_loc) == 1) {
      return(woedf[na_loc,2])
    } else if(length(na_loc) == 0) {
      stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
    } else stop("the 'woedf' is illegal")
  } else if(is.numeric(xval)) {
    bin_loc <- which(woedf[,1] >= xval)
    if(length(bin_loc) >= 1) {
      return(woedf[bin_loc[1],2])
    } else stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
  } else {
    bin_loc <- which(woedf[,1] == xval)
    if(length(bin_loc) == 1) {
      return(woedf[bin_loc,2])
    } else if(length(bin_loc) == 0) {
      stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
    } else stop("the 'woedf' is illegal")
  }
}


# @title Auxiliary Function: Encode A Variable as WOE
#
# @description
# Auxiliary function: \code{woeEncodeFun} will encode a variable as woe values, then return a numeric vector.
#
# @details
# parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#
# @param x_sample A dataframe contains the specified variable named \code{xnm}.
# @param xnm A name character of specified variable.
# @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a x variable.
#
# @return A numeric vector
# @family dataset binning and woe-encoding functions
# @export
woeEncodeFun <- function(x_sample,xnm,config_list) {
  x_vec <- x_sample[,xnm]
  woedf <- config_list[[xnm]]
  woe_vec <- sapply(x_vec,woeEncodeFun_xval,woedf=woedf)
  return(woe_vec)
}


#' @title Encode Dataset as WOE
#'
#' @description
#' \code{woeEncodeFun_df} will encode all observations as woe values, then return a new dataframe.
#'
#' @details
#' parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#'
#' @param x_sample A dataframe with only X variables and contains all the observations to be encoded.
#' @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a x variable.
#'
#' @return A dataframe after woe-encoded
#' @family dataset binning and woe-encoding functions
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel parSapply
#' @importFrom parallel stopCluster
#' @export
woeEncodeFun_df <- function(x_sample,config_list) {
  # library(parallel)
  xnms <- names(x_sample)
  clnum <- detectCores()
  cl <- makeCluster(getOption("cl.cores",clnum))
  ori_env <- environment(woeEncodeFun_xval)
  clusterExport(cl,"woeEncodeFun_xval",envir = ori_env)
  x_sample_encoding <- parSapply(cl,xnms,woeEncodeFun,x_sample=x_sample,config_list=config_list,USE.NAMES = TRUE)
  stopCluster(cl)
  return(as.data.frame(x_sample_encoding))
}


# @title Auxiliary Function: Execute Binning on One Observation of A Variable
#
# @description
# Auxiliary function: similar to \code{\link{woeEncodeFun_xval}}, based on \code{woedf} to execute binning on one observation of a variable, but will not proceed woe-encode.
#
# @details
# parameter \code{woedf} is a component of woe configuration list which generated by function \code{\link{dfBinningFun}}.
#
# @param xval An observation value of a variable.
# @param woedf A datafrme corresponding to current variable, which is a component of woe configuration list.
#
# @return A character
# @family dataset binning and woe-encoding functions
# @export
executeBinFun_xval <- function(xval,woedf) {
  if(is.na(xval)) {
    na_loc <- which(is.na(woedf[,1]))
    if(length(na_loc) == 1) {
      return(NA)
    } else if(length(na_loc) == 0) {
      stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
    } else stop("the 'woedf' is illegal")
  } else if(is.numeric(xval)) {
    bin_loc <- which(woedf[,1] >= xval)
    if(length(bin_loc) >= 1) {
      j <- bin_loc[1]
      if(j==1) {
        bin_val <- paste("(low,",woedf[j,1],"]",sep="")
      } else if(woedf[j,1]==Inf) {
        bin_val <- paste("(",woedf[j-1,1],",high)",sep="")
      } else {
        bin_val <- paste("(",woedf[j-1,1],",",woedf[j,1],"]",sep="")
      }
      return(bin_val)
    } else stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
  } else {
    bin_loc <- which(woedf[,1] == xval)
    if(length(bin_loc) == 1) {
      return(woedf[bin_loc,1])
    } else if(length(bin_loc) == 0) {
      stop(paste("the value '",xval,"' is not in bins of 'woedf'",sep=""))
    } else stop("the 'woedf' is illegal")
  }
}


# @title Auxiliary Function: Execute Binning on A Variable
#
# @description
# Auxiliary function: similar to \code{\link{woeEncodeFun}}, based on \code{config_list} to execute binning on a variable, but will not proceed woe-encode.
#
# @details
# parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#
# @param x_sample A dataframe contains the specified variable named \code{xnm}.
# @param xnm A name character of specified variable.
# @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a x variable.
#
# @return A character vector
# @family dataset binning and woe-encoding functions
# @export
executeBinFun <- function(x_sample,xnm,config_list) {
  x_vec <- x_sample[,xnm]
  woedf <- config_list[[xnm]]
  bin_vec <- sapply(x_vec,executeBinFun_xval,woedf=woedf)
  return(bin_vec)
}


#' @title Execute Binning on Dataset
#'
#' @description
#' \code{executeBinFun_df} is similar to \code{\link{woeEncodeFun_df}}, based on \code{config_list} to execute binning on all observations, but will not proceed woe-encode.
#'
#' @details
#' parameter \code{config_list} can be generated by function \code{\link{dfBinningFun}}.
#'
#' @param x_sample A dataframe with only X variables and contains all the observations to be encoded.
#' @param config_list A list of woe configuration, whose every component is a dataframe corresponding to a x variable.
#'
#' @return A dataframe after binned
#' @family dataset binning and woe-encoding functions
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel parSapply
#' @importFrom parallel stopCluster
#' @export
executeBinFun_df <- function(x_sample,config_list) {
  # library(parallel)
  xnms <- names(x_sample)
  clnum <- detectCores()
  cl <- makeCluster(getOption("cl.cores",clnum))
  ori_env <- environment(executeBinFun_xval)
  clusterExport(cl,"executeBinFun_xval",envir = ori_env)
  x_sample_binning <- parSapply(cl,xnms,executeBinFun,x_sample=x_sample,config_list=config_list,USE.NAMES = TRUE)
  stopCluster(cl)
  return(as.data.frame(x_sample_binning))
}


#' @title Auxiliary Function: Improved Equal-Frequency Binning
#'
#' @description
#' Auxiliary function: \code{getEqualFreqCuts} will get the improved Equal-Frequency binning cut points, return a vector.
#'
#' @param x A numeric vector you want to bin.
#' @param n The number of bins for Equal-Frequency binning.
#'
#' @return A vector
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' x <- c(1,2,3,3,3,3,3,4,6)
#' as.vector(quantile(x, probs=seq(0,1,1/3)))
#' getEqualFreqCuts(x,3)
getEqualFreqCuts <- function(x,n) {
  cuts <- as.vector(quantile(x, probs=seq(0,1,1/n), na.rm=TRUE))
  cuts_uniq <- unique(cuts)
  if(length(cuts) > length(cuts_uniq)) {
    cuts_rep <- as.numeric(names(which(table(cuts)>1)))
    suppressWarnings(      #suppress the Warning message
      cuts_add <- sapply(cuts_rep,function(elemnt) {return(max(x[which(x<elemnt)],na.rm=TRUE))})
    )
    cuts <- sort(unique(c(cuts_uniq,cuts_add)))
  }
  return(cuts)
}


#' @title Auxiliary Function: Optimally Binning of Given Variable
#'
#' @description
#' Auxiliary function: \code{smbinning2} is an enhanced and integrated optimal binning function for score model, which contains 4 different binning methods(see details) for numeric and factor variables.
#'
#' @details
#' the last variable in \code{df} must be binary response variable (0,1). Integer (int) is required. Name of y must not have a dot. Name "default" is not allowed.
#' \code{binMethod=c(1,2,3,4)}, meanings:
#'   1 means optimal binning, and equal-frequency binning is an alternative when optimal binning is not available.
#'   2 means optimal binning, and equal-interval binning is an alternative when optimal binning is not available.
#'   3 means equal-frequency binning.
#'   4 means equal-interval binning.
#' when \code{x} represents a continuous variable: At least 5 different values(excluding NA). Value Inf is not allowed.
#' when \code{x} represents a factor variable: At least 2 different values(excluding NA). Value Inf is not allowed.
#'
#' @param df A dataframe only with Xs and Y variables, and the last variable must be Y, see details.
#' @param x A name character of one x variable(if x variable is a character variable, it must be converted to factor in advance), \code{x} must not have a dot, see details.
#' @param binMethod An integer from 1 to 4, indicates 4 different binning methods(see details).
#' @param p A numeric, means percentage of records per bin, from 0 to 0.5.
#' @param maxcat An integer, specifies the maximum number of categories.
#' @param aliquots An integer, specifies the number of bins for equal-frequency or equal-interval binning method.
#'
#' @return A dataframe with cutpoints and corresponding woes
#' @family dataset binning and woe-encoding functions
#' @importFrom smbinning smbinning
#' @importFrom smbinning smbinning.custom
#' @importFrom smbinning smbinning.factor
#' @export
#'
#' @examples
#' data(CreditData)
#' mysample <- convertType(CreditData, toType="fac")
#' splitresult <- splitData(mysample, size = 0.7, ifpercent = TRUE)
#' train <- splitresult[[1]]
#' test <- splitresult[[2]]
#' smbinning2(train, x = "bscore", binMethod = 1, p = 0.05, maxcat = 10, aliquots = 5)
smbinning2 <- function(df,x,binMethod,p,maxcat,aliquots) {
  # library(smbinning)
  nm <- names(df)
  ncl <- ncol(df)
  mydf <- df[c(x,nm[ncl])]
  if(is.numeric(mydf[,1])) {     #if x is a numeric variable
    if(binMethod==1) {      #optimal binning, and equal-frequency binning is an alternative when optimal binning is not available
      res <- smbinning(mydf,y=nm[ncl],x=x,p=p)
      if(is.data.frame(res[[1]])) {     #if x can be Optimally Binned
        x_woe <- res$ivtable$WoE
        len_woe <- length(x_woe)
        x_vals <- res$cuts
        len_cuts <- length(x_vals)
        if(sum(is.na(mydf[,1]))==0) {
          x_woe <- x_woe[1:(len_woe-2)]
          x_vals <- c(x_vals,Inf)
        } else {
          x_woe <- x_woe[1:(len_woe-1)]
          x_vals <- c(x_vals,Inf,NA)
        }
      } else {
        cuts <- getEqualFreqCuts(mydf[,1],aliquots)      #improved equal-frequency binning
        cuts <- cuts[2:(length(cuts)-1)]
        res <- smbinning.custom(mydf,y=nm[ncl],x=x,cuts=cuts)
        x_woe <- res$ivtable$WoE
        len_woe <- length(x_woe)
        x_vals <- res$cuts
        len_cuts <- length(x_vals)
        if(sum(is.na(mydf[,1]))==0) {
          x_woe <- x_woe[1:(len_woe-2)]
          x_vals <- c(x_vals,Inf)
        } else {
          x_woe <- x_woe[1:(len_woe-1)]
          x_vals <- c(x_vals,Inf,NA)
        }
      }
    } else if(binMethod==2) {      #optimal binning, and equal-interval binning is an alternative when optimal binning is not available
      res <- smbinning(mydf,y=nm[ncl],x=x,p=p)
      if(is.data.frame(res[[1]])) {     #if x can be Optimally Binned
        x_woe <- res$ivtable$WoE
        len_woe <- length(x_woe)
        x_vals <- res$cuts
        len_cuts <- length(x_vals)
        if(sum(is.na(mydf[,1]))==0) {
          x_woe <- x_woe[1:(len_woe-2)]
          x_vals <- c(x_vals,Inf)
        } else {
          x_woe <- x_woe[1:(len_woe-1)]
          x_vals <- c(x_vals,Inf,NA)
        }
      } else {
        xmin <- min(mydf[,1],na.rm=TRUE)
        xmax <- max(mydf[,1],na.rm=TRUE)
        cuts <- seq(xmin,xmax,length.out=aliquots + 1)
        cuts <- cuts[2:(length(cuts)-1)]
        res <- smbinning.custom(mydf,y=nm[ncl],x=x,cuts=cuts)
        x_woe <- res$ivtable$WoE
        len_woe <- length(x_woe)
        x_vals <- res$cuts
        len_cuts <- length(x_vals)
        if(sum(is.na(mydf[,1]))==0) {
          x_woe <- x_woe[1:(len_woe-2)]
          x_vals <- c(x_vals,Inf)
        } else {
          x_woe <- x_woe[1:(len_woe-1)]
          x_vals <- c(x_vals,Inf,NA)
        }
      }
    } else if(binMethod==3) {      #equal-frequency binning
      cuts <- getEqualFreqCuts(mydf[,1],aliquots)
      cuts <- cuts[2:(length(cuts)-1)]
      res <- smbinning.custom(mydf,y=nm[ncl],x=x,cuts=cuts)
      x_woe <- res$ivtable$WoE
      len_woe <- length(x_woe)
      x_vals <- res$cuts
      len_cuts <- length(x_vals)
      if(sum(is.na(mydf[,1]))==0) {
        x_woe <- x_woe[1:(len_woe-2)]
        x_vals <- c(x_vals,Inf)
      } else {
        x_woe <- x_woe[1:(len_woe-1)]
        x_vals <- c(x_vals,Inf,NA)
      }
    } else if(binMethod==4) {      #equal-interval binning
      xmin <- min(mydf[,1],na.rm=TRUE)
      xmax <- max(mydf[,1],na.rm=TRUE)
      cuts <- seq(xmin,xmax,length.out=aliquots + 1)
      cuts <- cuts[2:(length(cuts)-1)]
      res <- smbinning.custom(mydf,y=nm[ncl],x=x,cuts=cuts)
      x_woe <- res$ivtable$WoE
      len_woe <- length(x_woe)
      x_vals <- res$cuts
      len_cuts <- length(x_vals)
      if(sum(is.na(mydf[,1]))==0) {
        x_woe <- x_woe[1:(len_woe-2)]
        x_vals <- c(x_vals,Inf)
      } else {
        x_woe <- x_woe[1:(len_woe-1)]
        x_vals <- c(x_vals,Inf,NA)
      }
    }
  } else {     #if x is a factor variable
    if(length(table(mydf[x])) < 2) stop(paste("error in 'smbinning2' Function: '",x,"' variable with less than 2 different values(excluding NA)",sep=""))
    res <- smbinning.factor(mydf,y=nm[ncl],x=x,maxcat=maxcat)
    if(is.data.frame(res[[1]])) {
      x_woe <- res$ivtable$WoE
      len_woe <- length(x_woe)
      x_vals <- res$cuts
      len_cuts <- length(x_vals)
      if(sum(is.na(mydf[,1]))==0) {
        x_woe <- x_woe[1:(len_woe-2)]
      } else {
        x_woe <- x_woe[1:(len_woe-1)]
        x_vals <- c(x_vals,NA)
      }
    } else {
      x_woe <- 0
      x_vals <- "NULL"
    }
  }
  res <- data.frame(vals=x_vals,woe=x_woe,stringsAsFactors=FALSE)
  #I'm not sure if it has any impact when set stringsAsFactors=FALSE above
  return(res)
}


#' @title Generate WOE Configuration List Automatically
#'
#' @description
#' \code{dfBinningFun} is a wrapper and parallelization of \code{\link{smbinning2}}, so it can generate the woe configuration list by using optimal binning automatically.
#'
#' @details
#' woe configuration list is a list object lengths as number of x variables in \code{mydf}, whose component is a dataframe consisting of two columns, named as 'vals' and 'woe'.
#'
#' @param mydf A dataframe only with Xs and Y variables, the last column must be Y. All character x variables must be converted to factors in advance.
#' @param binMethod An integer from 1 to 4, indicates 4 different binning methods(see details \code{\link{smbinning2}}), default \code{1}.
#' @param p A numeric, means percentage of records per bin, from 0 to 0.5, default \code{0.05}.
#' @param maxcat An integer, specifies the maximum number of categories, default \code{10}.
#' @param aliquots An integer, specifies the number of bins for equal-frequency or equal-interval binning method, default \code{5}.
#'
#' @return A list corresponding to X variables in \code{mydf}
#' @family dataset binning and woe-encoding functions
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @export
dfBinningFun <- function(mydf,binMethod=1,p=0.05,maxcat=10,aliquots=5) {
  if(!binMethod %in% c(1,2,3,4)) stop("error in 'dfBinningFun' Function: the parameter of 'binMethod' is not in c(1,2,3,4)")
  if(aliquots<2) stop("error in 'dfBinningFun' Function: the parameter of 'aliquots' is less than 2")
  aliquots <- floor(aliquots)

  # library(parallel)
  ncl <- ncol(mydf)
  if(ncl<2) stop("the ncol of 'mydf' must be least 2")
  nm <- names(mydf)
  x_vec <- nm[-length(nm)]
  clnum <- detectCores()
  cl <- makeCluster(getOption("cl.cores",clnum))
  gEFC_env <- environment(getEqualFreqCuts)
  clusterEvalQ(cl,library(smbinning))
  clusterExport(cl,"getEqualFreqCuts",envir = gEFC_env)
  myres <- parLapply(cl,x_vec,smbinning2,df=mydf,binMethod=binMethod,p=p,maxcat=maxcat,aliquots=aliquots)
  stopCluster(cl)
  names(myres) <- x_vec
  return(myres)
}


#' @title Generate WOE Configuration List Manually
#'
#' @description
#' \code{genConfigList} can be used to generate the woe configuration list by batch computing WOEs with given binnings.
#'
#' @details
#' \code{cut_list} must contains max and Inf value in every continuous-representing component. Component such as \code{c(10,20,30,Inf,NA)}.
#'
#' @param df A dataframe only with Xs and Y variables, and the last variable must be Y. Y must be 0 or 1, with 0 represents good and 1 represents bad.
#' @param cut_list A list of cut points vector corresponding X variables, but for factor variable, the element is fixed to constant 'factor'.
#'
#' @return A list corresponding to X variables in \code{df}
#' @family dataset binning and woe-encoding functions
#' @export
#'
#' @examples
#' data(CreditData)
#' mysample <- CreditData[c("gender","gscore","target")]
#' genConfigList(mysample, cut_list = list("factor",c(534,566,577,617,Inf,NA)))
genConfigList <- function(df,cut_list) {
  ncl <- ncol(df)
  nrw <- nrow(df)
  nm <- names(df)
  if(ncl < 2) stop("the ncol of 'df' must be least 2")
  if(nrw < 2) stop("the nrow of 'df' must be least 2")
  totalY <- as.numeric(table(df[ncl]))
  if(length(totalY) != 2) stop("the number of Y values must be equal to 2")
  totalgood <- totalY[1]
  totalbad <- totalY[2]
  config_list <- list()
  for(i in 1:(ncl-1)) {
    elemt <- cut_list[[i]]
    if(length(elemt) == 1 & elemt[1] == "factor") {
      uniqueX <- unique(df[,i])
      woe_vec <- sapply(uniqueX,getWOE,df=df[c(i,ncl)],totalgood=totalgood,totalbad=totalbad)
      config_list[[i]] <- data.frame(vals=uniqueX,woe=woe_vec)
    } else if(length(elemt) > 1) {
      cutbound_list <- convertCutPoints(elemt)
      woe_vec <- sapply(cutbound_list,getWOE,df=df[c(i,ncl)],totalgood=totalgood,totalbad=totalbad)
      config_list[[i]] <- data.frame(vals=elemt,woe=woe_vec)
    } else {
      stop(paste("the ",i,"th element of 'cut_list' is not 'factor' string",sep=""))
    }
  }
  names(config_list) <- nm[-ncl]
  return(config_list)
}


#' @title Compute KS Value of Score Model
#'
#' @description
#' \code{myks} will compute KS value by giving target and prediction, then return a numeric.
#'
#' @param y A vector of target, only containing 1 and 0.
#' @param predict_y A vector of p prediction.
#'
#' @return A numeric
#' @family model performance functions
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @export
myks <- function(y,predict_y) {
  # library(ROCR)
  pred <- prediction(predictions=predict_y,labels=y)
  perf <- performance(pred,"tpr","fpr")
  ks <- max(attr(perf,"y.values")[[1]]-attr(perf,"x.values")[[1]])
  return(ks)
}


#' @title Auxiliary Function: Convert Cut Points to Cutbound
#'
#' @description
#' Auxiliary function: convert vector of cut points to a list of cutbound with lbound and ubound.
#'
#' @param cutpoints A vector with serial cut points, such as \code{c(10,20,30,Inf,NA)}, in this example, Inf and NA is necessary.
#'
#' @return A list length as cutpoints vector
#' @family dataset binning and woe-encoding functions
#' @export
#'
#' @examples
#' convertCutPoints(c(10,20,30,Inf,NA))
convertCutPoints <- function(cutpoints) {
  len <- length(cutpoints)
  flag <- FALSE
  if(sum(is.na(cutpoints)) != 0) {
    cutpoints <- cutpoints[-len]     #remove NA
    len <- len - 1
    flag <- TRUE
  }
  cutbound_list <- list()
  for(i in 1:len) {
    if(i==1) {
      cutbound_list[[1]] <- c(-Inf,cutpoints[1])
    } else {
      cutbound_list[[i]] <- c(cutpoints[i-1],cutpoints[i])
    }
  }
  if(flag==TRUE) cutbound_list[[len+1]] <- NA
  return(cutbound_list)
}


#*******************Plot Model Performance Graphs*********************

#' @title Auxiliary Function: Create the Appropriate Data Structure for Model Performance Visualization
#'
#' @description
#' Auxiliary function: create the appropriate data structure for function \code{\link{myCurves}}, return a list.
#'
#' @details
#' the returned list contains two components: a \code{df} 'data.frame(Quant, PropBad, AccumBad, PropGood, AccumGood, BadRate)' and a numeric \code{ks} value.
#'
#' @param y A vector of target, only containing 1 and 0.
#' @param predict_y A vector of p prediction.
#' @param gap A numeric, the gap of two quantile points, default \code{0.05}.
#'
#' @return A list
#' @family model performance functions
#' @importFrom stats quantile
#' @export
Curve_Data <- function(y, predict_y, gap=0.05) {
  cnt <- length(y)
  cnt_bad <- sum(y)     #y==1 means bad
  cnt_good <- cnt - cnt_bad
  cutPoints <- as.vector(quantile(predict_y, probs=seq(1,0,-gap), na.rm=TRUE))
  len_cutPoints <- length(cutPoints)
  accumProp_bad <- numeric()
  accumProp_good <- numeric()
  Prop_bad <- numeric()
  Prop_good <- numeric()
  accumBad <- numeric()
  Bad <- numeric()
  for(i in 1:(length(cutPoints)-1)) {
    accumProp_bad[i] <- sum(predict_y >= cutPoints[i+1] & y==1)/cnt_bad
    accumProp_good[i] <- sum(predict_y >= cutPoints[i+1] & y==0)/cnt_good
    accumBad[i] <- sum(predict_y >= cutPoints[i+1] & y==1)
    if(i==1) {
      Prop_bad[i] <- accumProp_bad[i]
      Prop_good[i] <- accumProp_good[i]
      Bad[i] <- accumBad[i]
    } else {
      Prop_bad[i] <- accumProp_bad[i] - accumProp_bad[i-1]
      Prop_good[i] <- accumProp_good[i] - accumProp_good[i-1]
      Bad[i] <- accumBad[i] - accumBad[i-1]
    }
  }
  badrate <- Bad/(cnt/(len_cutPoints-1))
  accumProp_bad <- c(0, accumProp_bad)
  accumProp_good <- c(0, accumProp_good)
  Prop_bad <- c(0, Prop_bad)
  Prop_good <- c(0, Prop_good)
  badrate <- c(0,badrate)
  ks <- max(abs(accumProp_bad - accumProp_good))
  df <- data.frame(Quant=seq(0,1,gap), PropBad=Prop_bad, AccumBad=accumProp_bad,
                   PropGood=Prop_good, AccumGood=accumProp_good, BadRate=badrate)
  return(list(df=df,ks=ks))
}


#' @title Model Performance Visualization
#'
#' @description
#' \code{myCurves} is an integrated, flexible as well as easy to use function of visualization for model performance.
#'
#' @details
#' the result graph has four subgraphs which constitute a matrix, top left is ROC Curve, top right is Lift Figure, lower left is K-S Curve, and lower right is Distribution Figure of standard score.
#'
#' @param ytrain A vector corresponding the response variable encoded with 0 and 1 of train set.
#' @param predict_ytrain A vector of predicted probability values of train set.
#' @param ytest A vector corresponding the response variable encoded with 0 and 1 of test set.
#' @param predict_ytest A vector of predicted probability values of test set.
#' @param ontest Logical, decide plot on train or on test, default \code{TRUE}.
#' @param lift_bins An integer, set the number of binnings, default \code{10}.
#' @param P0 An integer, the base score, default \code{600}.
#' @param PDO An integer, the scale, default \code{50}.
#' @param color_scheme An integer, choice the color scheme 1, 2 or 3, default scheme \code{1}.
#' @param ifsave Logical, whether to save the chart as a file, default \code{TRUE}.
#'
#' @return NULL
#' @family model performance functions
#' @import ggplot2
#' @importFrom pROC roc
#' @importFrom plotROC geom_roc
#' @importFrom plotROC style_roc
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @export
myCurves <- function(ytrain, predict_ytrain, ytest, predict_ytest,
                     ontest=TRUE, lift_bins=10, P0=600, PDO=50, color_scheme=1, ifsave=TRUE) {
  # suppressPackageStartupMessages(library(ggplot2))
  # suppressPackageStartupMessages(library(pROC))
  # suppressPackageStartupMessages(library(plotROC))
  # suppressPackageStartupMessages(library(gridExtra))
  # suppressPackageStartupMessages(library(RColorBrewer))

  if(color_scheme!=1 & color_scheme!=2 & color_scheme!=3) stop("Parameter 'color_scheme' must be equal 1, 2 or 3")
  colsname <- c("Dark2", "Set1", "Set2")
  mycolor <- brewer.pal(8, colsname[color_scheme])

  if(ontest) {
    y <- ytest
    predict_y <- predict_ytest
    chartnm <- "Test"
  } else {
    y <- ytrain
    predict_y <- predict_ytrain
    chartnm <- "Train"
  }
  mydata <- data.frame(y, predict_y)

  #ROC Curve
  roc_curve <- roc(y,predict_y)
  Specificity <- roc_curve$specificities
  Sensitivity <- roc_curve$sensitivities
  p_roc <- ggplot(mydata, aes(d = y, m = predict_y)) +
    geom_roc(n.cuts = 0, color = mycolor[1]) +
    style_roc(minor.breaks = NULL,guide = TRUE, xlab = "1-Specificity",ylab = "Sensitivity", theme = theme()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate('text', x = 0.5, y = 0.5, label=paste('AUC=', round(roc_curve$auc,3), sep="")) +
    labs(title = paste('ROC Curve of',chartnm))

  #Lift Curve
  LiftData <- Curve_Data(y,predict_y,1/lift_bins)$df      #to cut (1/lift_bins) bins
  LiftData <- LiftData[-1,c("Quant","BadRate")]
  p_lift <- ggplot(aes(x = Quant, y = BadRate), data = LiftData) +
    geom_bar(stat = 'identity', fill = mycolor[2]) +
    scale_x_continuous(breaks = seq(0.1,1,0.2)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'Quantile', y = 'BadRate', title=paste('Lift Figure of',chartnm))

  #K-S Curve
  KSData <- Curve_Data(y,predict_y)
  p_ks <- ggplot() +
    geom_line(aes(x = Quant, y = AccumBad), data = KSData$df, color = mycolor[3],size = 1.02) +
    geom_line(aes(x = Quant, y = AccumGood), data = KSData$df, color = mycolor[4],size = 1.02) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate(geom = 'text', x = 0.5, y = 1, label = paste('KS=', round(myks(y, predict_y),3), sep="")) +
    # annotate(geom = 'text', x = 0.5, y = 1, label = paste('KS=', round(KSData$ks,3), sep="")) +
    labs(x = 'Quantile', y = 'Cumulative Proportion', title=paste('K-S Curve of',chartnm))

  #Histogram of prediction
  bad_cnt <- sum(ytrain)    #must be ytrain
  good_cnt <- length(ytrain) - bad_cnt
  mydata$score <- transformScore(mydata$predict_y,bad_cnt/good_cnt,P0=P0,PDO=PDO)
  x_dnorm <- seq(min(mydata$score),max(mydata$score),length.out = 100)
  dnormVect <- dnorm(x_dnorm, mean = mean(mydata$score, na.rm = TRUE), sd = sd(mydata$score, na.rm = TRUE))
  dnormData <- data.frame(dnormVect)
  p_hist <- ggplot() +
    geom_histogram(aes(x = score, y = ..density..), mydata, bins = 20, fill = mycolor[5], na.rm = TRUE) +
    geom_line(aes(x = x_dnorm, y = dnormVect), dnormData, color = mycolor[6]) +
    theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
    annotate(geom = 'text', x = max(mydata$score) - 0.8*sd(mydata$score), y = max(dnormVect)/2,
             label = paste('mean=', round(mean(mydata$score, na.rm = TRUE),0),'\n',
                           'sd=', round(sd(mydata$score, na.rm = TRUE),0), sep="")) +
    labs(x = 'Score', y = 'Density', title=paste('Distribution Figure of',chartnm))

  # #Preparing Gain data
  # Pi <- table(y)[2]/sum(table(y))
  # Ptp <- Pi*Sensitivity
  # Pfp <- (1-Pi)*(1-Specificity)
  # Depth <- Ptp + Pfp
  # PV_Plus <- Ptp/Depth
  # Lift <- PV_Plus/Pi
  #
  # #Gain Curve
  # p <- ggplot(data = NULL, mapping = aes(x= Depth, y = PV_Plus))
  # p_gain <- p + geom_line(na.rm = TRUE, size = 1.02, color = mycolor[5])  +
  #   labs(x = 'Depth',y = 'Precision', title = 'Gain Curve') +
  #   theme(plot.title = element_text(hjust = 0.5))

  dt <- as.character(format(Sys.time(), "%Y%m%d%H%M%S"))
  if(ifsave) {
    ml <- arrangeGrob(p_roc,p_lift,p_ks,p_hist,ncol=2)
    ggsave(paste("PerformanceGraphs", dt, ".pdf", sep = ""), ml, width = 8.27, height = 8.27, units = "in")
    ggsave(paste("PerformanceGraphs", dt, ".png", sep = ""), ml, width = 8.27, height = 8.27, units = "in")
  }
  grid.arrange(p_roc,p_lift,p_ks,p_hist,ncol=2)
}


#********************Model Validation*********************

#' @title Cross Validation for Score Model
#'
#' @description
#' \code{crossValidation} is a easy to use function of t times k folds cross validation for score model. The woe will be recomputed at every time and fold validation.
#'
#' @details
#' this function will cost much time if you set \code{t} and \code{k} too large, \code{t=1} and \code{k=10} is recommended.
#'
#' @param df A dataframe only with Xs and Y variables, and the last variable must be Y, and all the X variables have been binned, but have not been woe-encoded.
#' @param t An integer, means t times, default \code{1}.
#' @param k An integer, means k folds, default \code{10}.
#' @param ifprint Logical, whether to print the intermediate results, default \code{TRUE}.
#'
#' @return A vector contains average of train and test ks value
#' @family model performance functions
#' @importFrom caret createFolds
#' @importFrom dplyr select
#' @importFrom stats binomial
#' @importFrom stats glm
#' @importFrom stats predict
#' @export
crossValidation <- function(df,t=1,k=10,ifprint=TRUE) {
  # suppressPackageStartupMessages(library(lattice))
  # suppressPackageStartupMessages(library(caret))
  # suppressPackageStartupMessages(library(dplyr))
  ks_train_cv <- matrix(nrow = t,ncol = k)
  ks_test_cv <- matrix(nrow = t,ncol = k)
  ncl <- ncol(df)
  for(i in 1:t) {
    if(ifprint) {
      print("---times---")
      print(i)
      cat("\n")
    }
    folds <- createFolds(y=df[,ncl], k=k)
    for(j in 1:k) {
      if(ifprint) {
        print("***folds***")
        print(j)
      }
      fold_train <- df[-folds[[j]],]
      fold_test <- df[folds[[j]],]

      config_list_train <- dfBinningFun(fold_train,binMethod=1,p=0.05,maxcat=10,aliquots=5)
      x_fold_train <- select(fold_train,-ncl)
      x_fold_train_encoding <- woeEncodeFun_df(x_fold_train,config_list_train)
      fold_train_encoding <- cbind(x_fold_train_encoding,fold_train[ncl])
      x_fold_test <- select(fold_test,-ncl)
      x_fold_test_encoding <- woeEncodeFun_df(x_fold_test,config_list_train)
      fold_test_encoding <- cbind(x_fold_test_encoding,fold_test[ncl])

      fit <- glm(target ~ ., data=fold_train_encoding, family=binomial())
      fold_train_encoding$p <- predict(fit,type="response")
      ks_train <- myks(fold_train_encoding$target,fold_train_encoding$p)
      fold_test_encoding$p <- predict(fit,newdata=x_fold_test_encoding,type="response")
      ks_test <- myks(fold_test_encoding$target,fold_test_encoding$p)

      ks_train_cv[i,j] <- ks_train
      ks_test_cv[i,j] <- ks_test
      if(ifprint) {
        print(paste("the KS of train sample:",round(ks_train,4)))
        print(paste("the KS of test sample:",round(ks_test,4)))
        cat("\n")
      }
    }
  }
  ks_train_avg <- mean(ks_train_cv)
  ks_test_avg <- mean(ks_test_cv)
  print(paste("the mean KS of train sample in CV:",round(ks_train_avg,4)))
  print(paste("the mean KS of test sample in CV:",round(ks_test_avg,4)))
  return(c(ks_train_avg,ks_test_avg))
}


#' @title Auxiliary Function: Insert Elements into Vector
#'
#' @description
#' Auxiliary function: \code{insertElement} can insert specified elements into the specified location of vector, then return a new vector.
#'
#' @details
#' the elements in \code{indexs} must be sorted by ascending.
#'
#' @param vec A vector to be inserted into.
#' @param indexs A vector consisting of location indexs corresponding to the location of new elements after inserted.
#' @param values A vector to be inserted into parameter \code{vec}.
#'
#' @return A vector
#' @export
#'
#' @examples
#' insertElement(c(6,8,10),c(2,4),c(7,9))
insertElement <- function(vec,indexs,values) {
  #limit the mode of 'indexs'
  if(!is.numeric(indexs)) {
    stop("parameter 'indexs' must be numeric")
  } else {
    indexs2 <- as.integer(indexs)
    if(!all(indexs == indexs2)) stop("parameter 'indexs' must be integer, float is not allowed")
  }
  #check whether the modes of 'vec' and 'values' are consistent
  if(!is.numeric(vec) | !is.numeric(values)) {
    if(class(vec) != class(values)) stop("the mode of parameter 'vec' and 'values' must be consistent")
  }

  len1 <- length(indexs)
  len2 <- length(values)
  if(len1 != len2) stop("the lengths of parameter 'indexs' and 'values' must be equal")
  if(len1 > 0) {
    for(i in 1:len1) {
      len <- length(vec)
      ind <- indexs[i]
      val <- values[i]
      if(ind == 1) vec <- c(val,vec)
      else if(ind > 1 & ind <= len) vec <- c(vec[1:(ind-1)],val,vec[ind:len])
      else vec <- c(vec,val)
    }
  }
  return(vec)
}


#' @title Compute the PSI Index of Score Model
#'
#' @description
#' \code{psi} is an efficient and easy to use function computing model's PSI index(see details), return a numeric.
#'
#' @details
#' generally, when PSI < 0.1, model is highly stable; when 0.1-0.25, the stability of model is general; when > 0.25, the stability of model is bad, the model must be rebuilt.
#'
#' @param p1 A vector of prediction in train data.
#' @param p2 A vector of prediction in test data.
#' @param bins An integer, set the number of binnings, default \code{10}.
#' @param binMethod A character string, specify the binning method, must be one of \code{c("EF","EI")}, "EF" means equal-frequency(default), "EI" means equal-interval.
#'
#' @return A numeric
#' @family model stability functions
#' @export
psi <- function(p1, p2, bins=10, binMethod="EF") {
  if(!binMethod %in% c("EF","EI")) stop("binMethod must be 'EF' or 'EI'")
  virtual <- 0.000001    #use a minima to replace zero avoiding calculation of psi failure
  bin_index <- 1:bins
  n1 <- length(p1)
  n2 <- length(p2)
  p1 <- sort(p1)
  p2 <- sort(p2)
  min_p <- min(p1, p2, na.rm = TRUE)
  max_p <- max(p1, p2, na.rm = TRUE)
  if(binMethod=="EF") {
    quantpoints <- as.vector(quantile(p1, probs=seq(0, 1, length.out = bins + 1), na.rm=TRUE))
    cutpoints <- c(min_p, quantpoints[2:(length(quantpoints) - 1)], max_p)
  } else {
    cutpoints <- seq(min_p, max_p, length.out = bins + 1)    #if len(cutpoints)=11, number of bins=10
  }
  p1_bin <- cut(p1, cutpoints, labels = FALSE, include.lowest = TRUE)
  p2_bin <- cut(p2, cutpoints, labels = FALSE, include.lowest = TRUE)
  p1_missbin <- bin_index[!bin_index %in% p1_bin]    #important step
  p2_missbin <- bin_index[!bin_index %in% p2_bin]    #important step
  p1_group_rate <- as.vector(table(p1_bin))/n1
  p2_group_rate <- as.vector(table(p2_bin))/n2
  if(length(p1_missbin) > 0) p1_group_rate <- insertElement(p1_group_rate,p1_missbin,rep(virtual,length(p1_missbin)))
  if(length(p2_missbin) > 0) p2_group_rate <- insertElement(p2_group_rate,p2_missbin,rep(virtual,length(p2_missbin)))
  mypsi <- sum((p2_group_rate - p1_group_rate) * log(p2_group_rate/p1_group_rate))
  return(mypsi)
}


#********************Data Preprocessing*********************

#' @title Auxiliary Function: Exclude Columns in Data Frame
#'
#' @description
#' Auxiliary function: \code{excludeCol} will exclude the given columns, then return a new dataframe.
#'
#' @param df A dataframe.
#' @param exclude Vector of column names or numbers to exclude.
#'
#' @return A dataframe
#' @family data preprocessing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' x_sample <- excludeCol(CreditData, exclude = "target")
excludeCol <- function(df,exclude) {
  ncl <- ncol(df)
  nm <- names(df)
  if(sum(is.na(exclude)) > 0) stop("parameter 'exclude' contains NA")
  if(is.character(exclude)) {
    if(sum(!exclude %in% nm) > 0) stop(paste(paste(exclude[which(!exclude %in% nm)],collapse=","),"is not in the variable names of 'df'"))
    df <- df[!nm %in% exclude]
  } else if(is.numeric(exclude)) {
    if(!all(as.integer(exclude) == exclude)) stop("parameter 'exclude' is float type, not allowed")
    if(min(exclude,na.rm=TRUE) < 1) stop("the min element in 'exclude' is less than 1")
    if(max(exclude,na.rm=TRUE) > ncl) stop("the max element in 'exclude' is over the number of dataframe's columns")
    df <- df[-exclude]
  } else {
    stop("parameter 'exclude' must be an integer or character vector")
  }
  return(df)
}


#' @title Compute and Delete NA Rate of Variable
#'
#' @description
#' \code{delNArate} will compute NA rate of every variable in \code{df} firstly, then delete variables whose na rate is greater than or equal to threshold, return a list contains deleted dataframe.
#'
#' @details
#' the returned list contains three components, one is dataframe named 'naratedf', second is a dataframe named 'delVardf', last one is an after-deleted dataframe.
#'
#' @param df A dataframe.
#' @param narate_critical A numeric, Specifies NA rate threshold for selecting and returning variables, default \code{0.9}.
#' @param exclude Vector of column names or numbers to exclude, default \code{NULL}.
#'
#' @return A list
#' @family data preprocessing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' result1 <- delNArate(CreditData, narate_critical = 0.7)
#' result2 <- delNArate(CreditData, narate_critical = 0.7, exclude = "multiloantimes")
#' mysample <- result2[[3]]
delNArate <- function(df,narate_critical=0.9,exclude=NULL) {
  df2 <- df
  nm2 <- names(df2)
  if(!is.null(exclude)) df <- excludeCol(df,exclude)
  nrw <- nrow(df)
  nm <- names(df)
  # cntna <- apply(df,2,countNA)
  cntna <- apply(df,2,function(x) sum(is.na(x)))         #Computing the NA percentage of variables
  narate <- cntna / nrw
  #be carefull! character vectors be converted to factors in 'data.frame' function
  naratedf <- data.frame(VarName=nm, NARate=narate, row.names=NULL, stringsAsFactors=FALSE)
  delVardf <- naratedf[naratedf$NARate >= narate_critical,]
  delvars <- delVardf[,1]
  delflag <- nm2 %in% delvars
  keepdata <- df2[!delflag]
  return(list(naratedf,delVardf,keepdata))
}


# @title Auxiliary Function: Get the Max Percent of the Given Variable's Single-Value
#
# @description
# Auxiliary function: return the max percent of Single-Value in the given x variable.
#
# @param df A dataframe.
# @param x_nm Name of x variable.
#
# @return A numeric
# @family data preprocessing functions
# @export
#
# @examples
# data(CreditData)
# maxSinvalPercent_x(CreditData, "gender")
maxSinvalPercent_x <- function(df,x_nm) {
  x <- df[,x_nm]
  len <- length(x)
  if(len==0) stop(paste("error in 'maxSinvalPercent_x' Function: vector '",x_nm,"' contains zero element",sep=""))
  cnts <- as.vector(table(x,useNA="ifany"))
  percents <- cnts/len
  return(max(percents,na.rm=TRUE))
}


#' @title Get the Max Percents of All Variable's Single-Value
#'
#' @description
#' \code{maxSinvalPercent} will return the max percent vector of every variable's Single-Value in \code{df}.
#'
#' @param df A dataframe.
#'
#' @return A vector
#' @family data preprocessing functions
#' @export
maxSinvalPercent <- function(df) {
  nm <- names(df)
  myres <- sapply(nm,maxSinvalPercent_x,df=df)
  return(myres)
}


#' @title Delete Variables Based on Single-Value Percent
#'
#' @description
#' \code{delSinvalPercent} will delete variables whose single-value percent is more than or equal to the given threshold, then return a new dataframe.
#'
#' @details
#' generally, in score model, the threshold of single value percent is often set to 0.9.
#'
#' @param df A dataframe.
#' @param percent The given threshold, default \code{0.9}.
#' @param exclude Vector of column names or numbers to exclude, default \code{NULL}.
#'
#' @return A dataframe after deleted
#' @family data preprocessing functions
#' @export
delSinvalPercent <- function(df,percent=0.9,exclude=NULL) {
  df2 <- df
  nm2 <- names(df2)
  if(!is.null(exclude)) df <- excludeCol(df,exclude)
  nm <- names(df)
  sinvalper <- maxSinvalPercent(df)
  delvars <- nm[which(sinvalper >= percent)]
  delflag <- nm2 %in% delvars
  return(df2[!delflag])
}


#' @title Delete Variables Based on Number of Different Values
#'
#' @description
#' \code{delFewValues} will delete variables whose number of different values is too few(see details) to use binning function, and return a new dataframe.
#'
#' @details
#' for numeric variable, number of different values must be 5 at least, excluding missing values;
#' for factor variable, must be 2 at least, excluding missing values also.
#'
#' @param df A dataframe.
#' @param minN Min number for numeric variable, default \code{5}.
#' @param minF Min number for factor variable, default \code{2}.
#' @param exclude Vector of column names or numbers to exclude, default \code{NULL}.
#'
#' @return A dataframe after deleted
#' @family data preprocessing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' mysample <- delFewValues(CreditData, minN = 5, minF = 2, exclude = "target")
delFewValues <- function(df,minN=5,minF=2,exclude=NULL) {
  df2 <- df
  nm2 <- names(df2)
  if(!is.null(exclude)) df <- excludeCol(df,exclude)
  nm <- names(df)
  numVal <- sapply(df,FUN = function(x) {return(length(table(x)))})
  varType <- sapply(df,is.numeric)
  delvars <- nm[which((varType & numVal < minN) | (!varType & numVal < minF))]
  delflag <- nm2 %in% delvars
  return(df2[!delflag])
}


#' @title Compute the Given Binning WOE
#'
#' @description
#' \code{getWOE} will compute the given binning WOE of x variable, return a numeric.
#'
#' @param df A dataframe only with two variables, X and Y.
#' @param cutbound A single value for factor variable, or a vector with lbound and ubound for numeric variable.
#' @param totalgood Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#' @param totalbad Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#'
#' @return a numeric
#' @family WOE and IV computing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' getWOE(CreditData[c("gender","target")], cutbound = "M")
#' getWOE(CreditData[c("gscore","target")], cutbound = c(-Inf,534))
#' getWOE(CreditData[c("gscore","target")], cutbound = c(577,617))
#' getWOE(CreditData[c("gscore","target")], cutbound = c(617,Inf))
#' getWOE(CreditData[c("gscore","target")], cutbound = NA)
getWOE <- function(df,cutbound,totalgood=NULL,totalbad=NULL) {
  ncl <- ncol(df)
  nm <- names(df)
  if(ncl != 2) stop("error in 'getWOE' Function: the ncol of 'df' must be equal to 2")
  totalY <- as.numeric(table(df[2]))
  if(length(totalY) != 2) stop("error in 'getWOE' Function: the number of Y values must be equal to 2")
  if(is.null(totalgood)) totalg <- totalY[1] else totalg <- totalgood
  if(is.null(totalbad)) totalb <- totalY[2] else totalb <- totalbad
  X <- df[,1]
  Y <- df[,2]
  len_cut <- length(cutbound)
  if(len_cut==1) {
    if(is.na(cutbound)) {
      totalY_sub <- as.numeric(table(Y[is.na(X)]))
    } else {
      totalY_sub <- as.numeric(table(Y[X==cutbound]))
    }
    if(length(totalY_sub)==1) stop(paste("purely binning error in 'getWOE' Function. variable:'",nm[1],"', binning:'=",cutbound,"'",sep=""))
    Good <- totalY_sub[1]
    Bad <- totalY_sub[2]
  } else if(len_cut==2) {
    lb <- cutbound[1]
    ub <- cutbound[2]
    totalY_sub <- as.numeric(table(Y[X>lb & X<=ub]))
    if(length(totalY_sub)==1) stop(paste("purely binning error in 'getWOE' Function. variable:'",nm[1],"', binning:(",lb,",",ub,"]",sep=""))
    Good <- totalY_sub[1]
    Bad <- totalY_sub[2]
  } else {
    stop("error in 'getWOE' Function: the length of parameter 'cutbound' is not 1 or 2")
  }
  WOE <- log((Bad/Good)/(totalb/totalg))
  return(WOE)
}


#' @title Compute the Given Binning IV
#'
#' @description
#' \code{getIV} will compute the given binning IV of x variable, return a numeric.
#'
#' @param df A dataframe only with two variables, X and Y.
#' @param cutbound A single value for factor variable, or a vector with lbound and ubound for numeric variable.
#' @param totalgood Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#' @param totalbad Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#'
#' @return A numeric
#' @family WOE and IV computing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' getIV(CreditData[c("gender","target")], cutbound = "M")
#' getIV(CreditData[c("gscore","target")], cutbound = c(-Inf,534))
#' getIV(CreditData[c("gscore","target")], cutbound = c(577,617))
#' getIV(CreditData[c("gscore","target")], cutbound = c(617,Inf))
#' getIV(CreditData[c("gscore","target")], cutbound = NA)
getIV <- function(df,cutbound,totalgood=NULL,totalbad=NULL) {
  ncl <- ncol(df)
  nm <- names(df)
  if(ncl != 2) stop("error in 'getIV' Function: the ncol of 'df' must be equal to 2")
  totalY <- as.numeric(table(df[2]))
  if(length(totalY) != 2) stop("error in 'getIV' Function: the number of Y values must be equal to 2")
  if(is.null(totalgood)) totalg <- totalY[1] else totalg <- totalgood
  if(is.null(totalbad)) totalb <- totalY[2] else totalb <- totalbad
  X <- df[,1]
  Y <- df[,2]
  len_cut <- length(cutbound)
  if(len_cut==1) {
    if(is.na(cutbound)) {
      totalY_sub <- as.numeric(table(Y[is.na(X)]))
    } else {
      totalY_sub <- as.numeric(table(Y[X==cutbound]))
    }
    if(length(totalY_sub)==1) stop(paste("purely binning error in 'getIV' Function. variable:'",nm[1],"', binning:'=",cutbound,"'",sep=""))
    Good <- totalY_sub[1]
    Bad <- totalY_sub[2]
  } else if(len_cut==2) {
    lb <- cutbound[1]
    ub <- cutbound[2]
    totalY_sub <- as.numeric(table(Y[X>lb & X<=ub]))
    if(length(totalY_sub)==1) stop(paste("purely binning error in 'getIV' Function. variable:'",nm[1],"', binning:(",lb,",",ub,"]",sep=""))
    Good <- totalY_sub[1]
    Bad <- totalY_sub[2]
  } else {
    stop("error in 'getIV' Function: the length of parameter 'cutbound' is not 1 or 2")
  }
  IV <- ((Bad/totalb)-(Good/totalg))*log((Bad/totalb)/(Good/totalg))
  return(IV)
}


#' @title Auxiliary Function: Compute Sum IV of Given Variable
#'
#' @description
#' Auxiliary function: \code{sumIV} will compute the sum IV of a binned X variable, return a numeric.
#'
#' @details
#' notice: the given X variable must have be binned in advance.
#'
#' @param df A dataframe only with Xs and Y variable, and the last variable must be Y.
#' @param x A name character of X variable to be computed.
#' @param totalgood Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#' @param totalbad Default \code{NULL}. if not default, will use them directly, otherwise will recalculate based on \code{df}.
#'
#' @return A numeric
#' @family WOE and IV computing functions
#' @export
#'
#' @examples
#' data(CreditData)
#' sumIV(CreditData, x = "gender")
sumIV <- function(df,x,totalgood=NULL,totalbad=NULL) {
  ncl <- ncol(df)
  nm <- names(df)
  if(ncl < 2) stop("error in 'sumIV' Function: the ncol of 'df' Must be greater than or equal to 2")
  totalY <- as.numeric(table(df[ncl]))
  if(length(totalY) != 2) stop("error in 'sumIV' Function: the number of Y values must be equal to 2")
  if(is.null(totalgood)) totalg <- totalY[1] else totalg <- totalgood
  if(is.null(totalbad)) totalb <- totalY[2] else totalb <- totalbad
  X <- df[,x]
  Ynm <- nm[ncl]
  mydf <- df[c(x,Ynm)]
  Xvals <- unique(X)
  IVs <- sapply(Xvals,getIV,df=mydf,totalgood=totalgood,totalbad=totalbad)
  return(sum(IVs))
}


#' @title Compute Sum IV of All X Variables
#'
#' @description
#' \code{dfIV} is a wrapper of function \code{\link{sumIV}}, so it can compute the sum IV of every X variable in \code{df}.
#'
#' @details
#' notice: all the X variables must have been binned in advance, such as after woe-encoded.
#'
#' @param df A dataframe only with Xs and Y variable, all the X variables must have been binned in advance, such as after woe-encoded.
#'
#' @return A dataframe contains two columns, one is \code{varname}, another is \code{IV}
#' @family WOE and IV computing functions
#' @export
dfIV <- function(df) {
  ncl <- ncol(df)
  nm <- names(df)
  Xnm <- nm[-ncl]
  myIVs <- sapply(Xnm,sumIV,df=df,USE.NAMES = FALSE)
  myres <- data.frame(varname=Xnm,IV=myIVs,stringsAsFactors=FALSE)
  return(myres)
}


#' @title Collinearity Elimination Automatically
#'
#' @description
#' \code{collElimination} is a recursive function, which will recursivly eliminate the collinearity X variables with smaller IV automatically, then return a new dataframe.
#'
#' @details
#' for collinearity elimination, recursion is necessary to avoid mistakenly-deleted.
#'
#' @param df A dataframe only with Xs and Y variable, all the X variables must have been binned, such as after woe-encoded.
#' @param cor_critical Specifies the correlation coefficient threshold for extracting, default \code{0.8}.
#'
#' @return A dataframe after elimination
#' @family collinearity elimination functions
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr summarise
#' @importFrom stats cor
#' @export
collElimination <- function(df,cor_critical=0.8) {     #Recursive Function
  # library(dplyr)
  IVs <- dfIV(df)   #return a dataframe, computing every X variable's IV
  ncl <- ncol(df)
  cordf <- as.data.frame(cor(df[-ncl]))
  varnm <- names(cordf)
  cor_critical_abs <- abs(cor_critical)
  indx <- which(cordf >= cor_critical_abs | cordf <= -cor_critical_abs, arr.ind = TRUE)
  var_coll <- data.frame(varname_row=varnm[indx[,1]],varname_col=varnm[indx[,2]])
  var_summ <- as.data.frame(summarise(group_by(var_coll,varname_row),n=n()-1))
  var_summ <- var_summ[var_summ$n>0,]
  if(nrow(var_summ)>0) {
    var_IV <- merge(var_summ,IVs,by.x="varname_row",by.y="varname")
    currentvar <- var_IV[which.max(var_IV[,3]),1]
    delvar_vec <- var_coll[var_coll[,1]==currentvar & var_coll[,2]!=currentvar,2]
    delindx <- names(df) %in% delvar_vec
    df <- df[!delindx]
    return(collElimination(df,cor_critical))
  } else {
    return(df)
  }
}


#' @title Collinearity Elimination by Giving Variables' IV
#'
#' @description
#' \code{collElimination2} is a recursive function, which will recursivly eliminate the collinearity X variables with smaller IV. Different from \code{\link{collElimination}}, it must provide parameter \code{IVdf}, so need not recalculate every variable's IV for speed up.
#'
#' @details
#' for collinearity elimination, recursion is necessary to avoid mistakenly-deleted.
#'
#' @param df A dataframe only with Xs and Y variable, all the X variables must have been binned, such as after woe-encoded.
#' @param IVdf A dataframe of every X variable's IV.
#' @param cor_critical Specifies the correlation coefficient threshold for extracting, default \code{0.8}.
#'
#' @return A dataframe after elimination
#' @family collinearity elimination functions
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr summarise
#' @importFrom stats cor
#' @export
collElimination2 <- function(df,IVdf,cor_critical=0.8) {
  # library(dplyr)
  ncl <- ncol(df)
  cordf <- as.data.frame(cor(df[-ncl]))
  varnm <- names(cordf)
  cor_critical_abs <- abs(cor_critical)
  indx <- which(cordf >= cor_critical_abs | cordf <= -cor_critical_abs, arr.ind = TRUE)
  var_coll <- data.frame(varname_row=varnm[indx[,1]],varname_col=varnm[indx[,2]])
  var_summ <- as.data.frame(summarise(group_by(var_coll,varname_row),n=n()-1))
  var_summ <- var_summ[var_summ$n>0,]
  if(nrow(var_summ)>0) {
    var_IV <- merge(var_summ,IVdf,by.x="varname_row",by.y="varname")
    currentvar <- var_IV[which.max(var_IV[,3]),1]
    delvar_vec <- var_coll[var_coll[,1]==currentvar & var_coll[,2]!=currentvar,2]
    delindx <- names(df) %in% delvar_vec
    df <- df[!delindx]
    return(collElimination(df,cor_critical))
  } else {
    return(df)
  }
}


#*************************Data Prebinning**************************

# @title Auxiliary Function: Optimal Binning for Single Continuous Variable
#
# @description
# Auxiliary function: a wrapper and enhancing of function \code{smbinning}.
#
# @param myx A name character of one continuous variable, \code{myx} must not have a dot.
# @param df A data frame of dataset consisting of only Xs and Y variables, the last column must be Y.
# @param binMethod An integer from 1 to 5, indicates 5 different binning methods.
# @param per A numeric, means percentage of records per bin, from 0 to 0.5.
# @param aliquots An integer, specifies the number of bins for equal-frequency or equal-interval binning method.
#
# @return A list
# @family dataset binning and woe-encoding functions
# @importFrom smbinning smbinning
# @importFrom smbinning smbinning.custom
# @export
#
# @examples
#
mysmbinning <- function(myx,df,binMethod,per,aliquots) {
  ##Auxiliary function
  # library(smbinning)
  nm <- names(df)
  ncl <- ncol(df)
  mydf <- df[c(myx,nm[ncl])]            #Subset for speeding up and Reducing memory occupation
  if(binMethod==1) {          #optimal binning, and equal-frequency binning is an alternative when optimal binning is not available
    res <- smbinning(mydf,y=nm[ncl],x=myx,p=per)
    if(is.data.frame(res[[1]])) {     #if x can be Optimally Binned
      res[[3]] <- NULL     #remove the 'ctree' element
    } else {
      cuts <- getEqualFreqCuts(mydf[,1],aliquots)
      cuts <- cuts[2:(length(cuts)-1)]
      res <- smbinning.custom(mydf,y=nm[ncl],x=myx,cuts=cuts)
    }
  } else if(binMethod==2) {         #optimal binning, and equal-interval binning is an alternative when optimal binning is not available
    res <- smbinning(mydf,y=nm[ncl],x=myx,p=per)
    if(is.data.frame(res[[1]])) {
      res[[3]] <- NULL
    } else {
      xmin <- min(mydf[,1],na.rm=TRUE)
      xmax <- max(mydf[,1],na.rm=TRUE)
      cuts <- seq(xmin,xmax,length.out=aliquots + 1)
      cuts <- cuts[2:(length(cuts)-1)]
      res <- smbinning.custom(mydf,y=nm[ncl],x=myx,cuts=cuts)
    }
  } else if(binMethod==3) {         #equal-frequency binning
    cuts <- getEqualFreqCuts(mydf[,1],aliquots)
    cuts <- cuts[2:(length(cuts)-1)]
    res <- smbinning.custom(mydf,y=nm[ncl],x=myx,cuts=cuts)
  } else if(binMethod==4) {         #equal-interval binning
    xmin <- min(mydf[,1],na.rm=TRUE)
    xmax <- max(mydf[,1],na.rm=TRUE)
    cuts <- seq(xmin,xmax,length.out=aliquots + 1)
    cuts <- cuts[2:(length(cuts)-1)]
    res <- smbinning.custom(mydf,y=nm[ncl],x=myx,cuts=cuts)
  } else if(binMethod==5) {        #only optimal binning
    res <- smbinning(mydf,y=nm[ncl],x=myx,p=per)
    if(is.data.frame(res[[1]])) {     #if x can be Optimally Binned
      res[[3]] <- NULL     #remove the 'ctree' element
    }
  }
  return(res)
}


# @title Auxiliary Function: Optimal Binning for Single Factor Variable
#
# @description
# Auxiliary function: a wrapper and enhancing of function \code{smbinning.factor}.
#
# @param myx A name character of one factor variable(if not a factor, must be converted in advance), \code{myx} must not have a dot.
# @param df A data frame of dataset consisting of only Xs and Y variables, the last column must be Y.
#
# @return A list
# @family dataset binning and woe-encoding functions
# @importFrom smbinning smbinning.factor
# @export
#
# @examples
#
mysmbinning.factor <- function(myx,df) {
  ##Auxiliary function
  # library(smbinning)
  nm <- names(df)
  ncl <- ncol(df)
  mydf <- df[c(myx,nm[ncl])]            #Subset for speeding up and Reducing memory occupation
  res <- smbinning.factor(mydf,y=nm[ncl],x=myx,maxcat=20)
  return(res)
}


#' @title Variable Pre-Binning, then Computing WOE and IV
#'
#' @description
#' \code{preBinningFun} is often used to pre-binning with returning plots and a lots of details for variables filtering, and it has integrated 5 different binning methods(see details). Important notes: the calculated WOE value in this function is the opposite of the actual value.
#'
#' @details
#' \code{binMethod=c(1,2,3,4,5)}, means:
#'   1 means optimal binning, and equal-frequency binning is an alternative when optimal binning is not available.
#'   2 means optimal binning, and equal-interval binning is an alternative when optimal binning is not available.
#'   3 means equal-frequency binning.
#'   4 means equal-interval binning.
#'   5 means optimal binning only.
#'
#' this function will generate four files in current directory, including 'binGraph.pdf', 'varSummary.csv', 'summaryIV.csv' and 'insignificantVars.csv'(if it exists), as well as mass csv files in '~/binDetails/' subdirectory. If the subdirectory does not exist, it will be created automatically.
#'
#' @param mydata A data frame of dataset consisting of only Xs and Y variables, the last column must be Y. All character x variables must be converted to factors in advance.
#' @param binMethod An integer from 1 to 5, indicates 5 different binning methods(see details), default \code{1}.
#' @param p A numeric, means percentage of records per bin, from 0 to 0.5, default \code{0.05}.
#' @param aliquots An integer, specifies the number of bins for equal-frequency or equal-interval binning method, default \code{5}.
#' @param mydict Optional, default \code{NULL}. File name character of variable dictionary with csv file extension, or a dataframe representing the variable dictionary, see details of this parameter in \code{\link{fread_basedict}}.
#'
#' @return A list
#' @family dataset binning and woe-encoding functions
#' @importFrom data.table fread
#' @importFrom smbinning smbinning.plot
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom showtext showtext_auto
#' @importFrom vcd spine
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#' @importFrom graphics boxplot
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom utils write.csv
#' @importFrom sysfonts font_add
#' @export
preBinningFun <- function(mydata,binMethod=1,p=0.05,aliquots=5,mydict=NULL) {
  if(!binMethod %in% c(1,2,3,4,5)) stop("the parameter of 'binMethod' is not in c(1,2,3,4,5)")
  if(aliquots<2) stop("the parameter of 'aliquots' is less than 2")
  aliquots <- floor(aliquots)

  # library(smbinning)
  # library(parallel)
  # library(showtext)
  # library(vcd)
  if(is.character(mydict)) {
    if(length(mydict) != 1) stop("the length of parameter 'mydict' must be equal to 1 if it is a character string")
    if(substr(tolower(mydict),nchar(mydict)-3,nchar(mydict)) != ".csv") stop("the parameter of 'mydict' must contain CSV file extension if it is a character string")
    vardict <- fread(mydict,colClasses=list(character=c(1:2,4),numeric=c(3)),data.table=FALSE,na.strings="")
  } else if(is.data.frame(mydict)) {
    if(ncol(mydict) < 4) stop("the ncol of parameter 'mydict' must be more than or equal to 4 if it is a dataframe")
    vardict <- mydict
  } else if(is.null(mydict)) {
    vardict <- data.frame(character(0),character(0),numeric(0),character(0))
  } else {
    stop("the parameter of 'mydict' must be a character string or a dataframe or default NULL")
  }

  cat("Data preparation...Please wait","\n")
  summ <- summary(mydata)
  summ <- t(summ)
  write.csv(summ,"varSummary.csv",na = "")

  mydata[,ncol(mydata)][mydata[,ncol(mydata)] == 0] <- 2
  mydata[,ncol(mydata)][mydata[,ncol(mydata)] == 1] <- 0
  mydata[,ncol(mydata)][mydata[,ncol(mydata)] == 2] <- 1

  var_numeric <- sapply(mydata,is.numeric)
  mydata_num <- mydata[var_numeric]
  cat("dim of numeric variables:","\n")
  print(dim(mydata_num[-length(mydata_num)]))

  mydata[,ncol(mydata)] <- as.factor(mydata[,ncol(mydata)])
  var_factor <- sapply(mydata,is.factor)
  mydata_fact <- mydata[var_factor]
  #be careful! if x is a factor, as.numeric will not return the desired result
  mydata_fact[,ncol(mydata_fact)] <- as.numeric(as.character(mydata_fact[,ncol(mydata_fact)]))
  cat("dim of factor variables:","\n")
  print(dim(mydata_fact[-length(mydata_fact)]))

  rm(mydata,summ)
  gc()

  x_num <- names(mydata_num)[-ncol(mydata_num)]
  x_fact <- names(mydata_fact)[-ncol(mydata_fact)]
  clnum <- detectCores()
  cl <- makeCluster(getOption("cl.cores",clnum))
  gEFC_env <- environment(getEqualFreqCuts)
  clusterEvalQ(cl,library(smbinning))
  clusterExport(cl,"getEqualFreqCuts",envir = gEFC_env)
  cat("Parallel computing...This will take a while","\n")
  myres <- parLapply(cl,x_num,mysmbinning,df=mydata_num,binMethod=binMethod,per=p,aliquots=aliquots)     #Enable maximum number of core for parallel computing
  myres_fact <- parLapply(cl,x_fact,mysmbinning.factor,df=mydata_fact)
  stopCluster(cl)
  myres <- lapply(myres,"[",c(1:2,4:6))
  myres <- c(myres,myres_fact)             #combine the list of myres with myres_fact

  mydata_num[,ncol(mydata_num)][mydata_num[,ncol(mydata_num)] == 0] <- 2       #recover the transcoding of Y
  mydata_num[,ncol(mydata_num)][mydata_num[,ncol(mydata_num)] == 1] <- 0
  mydata_num[,ncol(mydata_num)][mydata_num[,ncol(mydata_num)] == 2] <- 1
  mydata_fact[,ncol(mydata_fact)][mydata_fact[,ncol(mydata_fact)] == 0] <- 2
  mydata_fact[,ncol(mydata_fact)][mydata_fact[,ncol(mydata_fact)] == 1] <- 0
  mydata_fact[,ncol(mydata_fact)][mydata_fact[,ncol(mydata_fact)] == 2] <- 1

  mydata_num_nm <- names(mydata_num[-ncol(mydata_num)])
  mydata_fact_nm <- names(mydata_fact)
  mydata_combine_nm <- c(mydata_num_nm,mydata_fact_nm)
  mydata_num_ncol_x <- length(mydata_num_nm)

  ivtable <- list()
  for(i in 1:length(myres)) {
    ivtable[[i]] <- myres[[i]][[1]]
    if(is.data.frame(myres[[i]][[1]]))
      names(ivtable)[i] <- myres[[i]][[3]]
    else
      names(ivtable)[i] <- mydata_combine_nm[i]
  }
  summiv <- data.frame(name=character(0),summaryIV=numeric(0),stringsAsFactors=FALSE)
  insignvar <- data.frame(name=character(0),stringsAsFactors=FALSE)

  opar <- par(no.readonly=TRUE)
  showtext_auto(enable=TRUE)
  tryCatch(font_add("msyh","msyh.ttf"),error=function(e) {font_add("msyh","msyh.ttc")})
  pdf("binGraph.pdf")
  par(family="msyh")
  mypath <- getwd()
  n1 <- 0
  n2 <- 0
  for(i in 1:length(myres)) {
    if(is.data.frame(myres[[i]][[1]])) {
      n1 <- n1 + 1
      varrow <- which(vardict[,1]==myres[[i]][[3]])
      if(length(varrow)!=0)
        varnm_ch <- vardict[,2][varrow[1]]
      else
        varnm_ch <- myres[[i]][[3]]

      par(mfrow=c(2,2))
      #the "," in mydata_num[,myres[[i]][[3]]] is necessary
      if(i<=mydata_num_ncol_x) {
        boxplot(mydata_num[,myres[[i]][[3]]] ~ mydata_num[,ncol(mydata_num)],data=mydata_num, horizontal=T, frame=F, col="lightgray",main="Distribution")
        mtext(paste("IV:",myres[[i]][[2]]),3)
      }
      # } else {
      #   counts <- table(mydata_fact[,myres[[i]][[3]]],mydata_fact[,ncol(mydata_fact)],useNA="ifany")
      #   if(nrow(counts)>10) {
      #     counts <- counts[1:10,]
      #     spine(counts,main="Distribution of top 10")
      #     mtext(paste("IV:",myres[[i]][[2]]),3)
      #   } else {
      #     spine(counts,main="Distribution")
      #     mtext(paste("IV:",myres[[i]][[2]]),3)
      #   }
      # }
      if(i<=mydata_num_ncol_x)
        smbinning.plot(myres[[i]],option="dist",sub=varnm_ch)
      else
        smbinning.plot(myres[[i]],option="dist",sub=paste("IV:",myres[[i]][[2]]))
      smbinning.plot(myres[[i]],option="badrate",sub=varnm_ch)
      if(sum(is.infinite(ivtable[[i]]$WoE)) == 0)
        smbinning.plot(myres[[i]],option="WoE",sub=varnm_ch)
      summiv[n1,1] <- varnm_ch
      summiv[n1,2] <- myres[[i]][[2]]
    } else {
      n2 <- n2 + 1
      varrow <- which(vardict[,1]==mydata_combine_nm[i])
      if(length(varrow)!=0)
        varnm_ch <- vardict[,2][varrow[1]]
      else
        varnm_ch <- mydata_combine_nm[i]
      par(mfrow=c(1,1))
      if(i<=mydata_num_ncol_x) {
        boxplot(mydata_num[,mydata_combine_nm[i]] ~ mydata_num[,ncol(mydata_num)],data=mydata_num, horizontal=T, frame=F, col="lightgray",main="Distribution")
        mtext(varnm_ch,3)
      } else {
        counts <- table(mydata_fact[,mydata_combine_nm[i]],mydata_fact[,ncol(mydata_fact)],useNA="ifany")
        if(nrow(counts)>7) {
          counts <- counts[1:7,]
          spine(counts,main="Distribution of top 7")
          mtext(varnm_ch,3)
        } else {
          spine(counts,main="Distribution")
          mtext(varnm_ch,3)
        }
      }
      insignvar[n2,1] <- varnm_ch
    }
    if(!dir.exists(paste(mypath,"/binDetails/",sep=""))) dir.create(paste(mypath,"/binDetails/",sep=""))
    file=paste(mypath,"/binDetails/",varnm_ch,".csv",sep="")
    write.csv(ivtable[[i]],file)
  }

  file1 <- paste(mypath,"/summaryIV.csv",sep="")
  write.csv(summiv,file1)
  if(length(insignvar[,1])>0) {
    file2 <- paste(mypath,"/insignificantVars.csv",sep="")
    write.csv(insignvar,file2)
  }
  par(opar)
  dev.off()
  results <- list(myres,ivtable)
  cat("completed","\n")
  return(results)
}

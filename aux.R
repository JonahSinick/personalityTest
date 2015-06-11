library(cvTools)
library(hash)
library(stats)
library(ggplot2)
library(lme4)
library(softImpute)
library(randomForest)
library(plyr)
library(Metrics)
library(LiblineaR)
library(missForest)
library(MASS)
library(glmnet)
library(DAAG)
library(cluster)
library(Metrics)
library(adabag)
library(pls)
library(pROC)
library(ROCR)
library(pls)

drop = function(df, names, type="cols"){
  if(type == "rows"){
    for(name in names){
      df = df[!is.na(df[name]),]   
    }
  }
  if(type == "cols"){
    df = df[!(names(df) %in% names)]   
  }
  return(df)
}
getPrcomps = function(df, names){
  p = prcomp(scale(df[names]))
  print(round(100*p$rotation))
  return(as.data.frame(scale(p$x)))
}
getRegs = function(df, str){
  n = names(df)
  return(n[grep(str,n)])
}
processCluster = function(df, blks){
  for(b in blks){
    df[[b]] = as.factor(df[[b]])
  }  
  mrf = missForest(df[blks])
  df[blks] =  mrf$ximp
  for(b in blks){ 
    df[[b]] = as.numeric(df[[b]])
  }
  df[blks] = scale(df[blks])
  return(df)
}
buildPrcomps = function(df, nameHash, i){
  ndf = data.frame()
  ndf[1:nrow(df),] = 0
  for(key in keys(nameHash)){
    p = prcomp(df[nameHash[[key]]])
    tempDF = as.data.frame(p$x)[1:i]
    pcs = gsub("$",key,names(tempDF))
    ndf[pcs] = tempDF
  }
  return(ndf)
}
cor2 = function(df, df2 = df){
  return(round(100*cor(df, df2, use="pairwise.complete.obs")))
}

naRemove = function(df){
  for(n  in names(df)){
    df[[n]] = ifelse(is.na(df[[n]]), median(df[[n]], na.rm = TRUE), df[[n]])
  }
  return(df)
}
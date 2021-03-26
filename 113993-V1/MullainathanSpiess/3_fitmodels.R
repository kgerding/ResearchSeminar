## Fit tuned models for JEP paper
# Jann Spiess, March/April 2017

# Based on tuning results (see separate file)


# Load data
rawdata <- readRDS(file="ahs2011forjep.rdata")
instruction <- list(df=rawdata$df,settings=list(LHS="LOGVALUE",RHS=rawdata$vars))

# Fit models
thisname <- "jepfittedmodels"

tunedrf <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                              holdoutvar="holdout", holdout=T,
                              predictors=list(rf=rf(nodesize=7,ntree=700,pmtry=0.2,numericfactors=F,bootstrap=T)),
                              nfold = 8)
saveRDS(tunedrf,file=paste0(thisname,"-rfout"))

olsbaseline <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                          holdoutvar="holdout", holdout=T,
                                          predictors=list(ols=ols()),
                                          nfold = 8)
saveRDS(olsbaseline,file=paste0(thisname,"-olsbl"))

treedepth <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                holdoutvar="holdout", holdout=T,
                                predictors=list(tree=regtree(minbuckets=c(1),maxdepths=c(4),numericfactors=F)),
                                nfold = 8)
saveRDS(treedepth,file=paste0(thisname,"-treedepth"))

tunedtree <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                holdoutvar="holdout", holdout=T,
                                predictors=list(tree=regtree(minbuckets=c(60),maxdepths=c(10),numericfactors=F)),
                                nfold = 8)
saveRDS(tunedtree,file=paste0(thisname,"-tunedtree"))

lassoplain <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                 holdoutvar="holdout", holdout=T,
                                 predictors=list(lasso=elnet(lambdas=0.00673794699908547, alpha=c(1), interactions="")),
                                 nfold = 8)
saveRDS(lassoplain,file=paste0(thisname,"-lassoplain"))

elnetfit <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                 holdoutvar="holdout", holdout=T,
                                 predictors=list(elnet=elnet(lambdas=0.0111089965382423, alpha=0.8, interactions="")),
                                 nfold = 8)
saveRDS(elnetfit,file=paste0(thisname,"-elnet"))

xgboostfit <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                            holdoutvar="holdout", holdout=T,
                            predictors=list(xgb=xgbcontinous(etas = c(.01), max_depths = c(3), nrounds = c(1000))),
                            nfold = 8)
saveRDS(xgboostfit,file=paste0(thisname,"-xgboost"))

# Ensemble based on individual tuning results (calculates weights)
ensemblefit <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                    holdoutvar="holdout", holdout=T,
                                    predictors=list(rf=rf(nodesize=7,ntree=700,pmtry=0.2,numericfactors=F,bootstrap=T),
                                                    lasso=elnet(lambdas=0.00673794699908547, alpha=c(1), interactions=""),
                                                    tree=regtree(minbuckets=c(1),maxdepths=c(4),numericfactors=F)
                                                    ),
                                    nfold = 8)
saveRDS(ensemblefit,file=paste0(thisname,"-ensemble"))
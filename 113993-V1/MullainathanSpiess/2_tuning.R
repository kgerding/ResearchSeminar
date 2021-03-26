## Tune and fit models for JEP paper
# Jann Spiess, March/April 2017


# Load environment

thisname <- "jeptuning"

# Load base data
localget <- readRDS(file="ahs2011forjep.rdata")
localahs <- localget$df
ahsvars <- localget$vars
getformula <- localget$getformula


# Random forest -- tune on server

# Prepare data locally
parallelprep(df=localahs, LHS="LOGVALUE", RHS=ahsvars,
             holdoutvar="holdout",
             predictor=rf(nodesizes=c(5,7,10,15,20,25,30,40,50),ntrees=c(400,500,700),pmtrys=c(.2,.3,.4),numericfactors=F,bootstrap=T),
             nfold = 8,
             savename=paste0(thisname,"-rfin"))
# Example execution of a single node
parallelsinglepredict(savename=paste0(thisname,"-rfin"),iteration=1)
  # For full tuning, run through as many iterations
  # as there are combinations of tuning parameters
# Analysis after all nodes have run
tunedrf <- parallelanalysis(savename=paste0(thisname,"-rfin"),fitit=T)
saveRDS(tunedrf,file=paste0(thisname,"-rfout"))


# Other methods -- tune locally (can be parallelized as above, but these are quicker than RF)

instruction <- readRDS(file=paste0(thisname,"-rfin"))

# OLS
olsbaseline <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                          holdoutvar="holdout", holdout=T,
                                          predictors=list(ols=ols()),
                                          nfold = 8)
saveRDS(olsbaseline,file=paste0(thisname,"-olsbl"))

# Tree by depth
treedepth <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                holdoutvar="holdout", holdout=T,
                                predictors=list(tree=regtree(minbuckets=c(1),maxdepths=c(2,3,4,5,6,7,8),numericfactors=F)),
                                nfold = 8)
saveRDS(treedepth,file=paste0(thisname,"-treedepth"))

# Tree well-tuned
tunedtree <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                holdoutvar="holdout", holdout=T,
                                predictors=list(tree=regtree(minbuckets=c(10,20,30,40,50,60,70,80,90,100,110,120),maxdepths=c(6,7,8,9,10,11,12,20,30),numericfactors=F)),
                                nfold = 8)
saveRDS(tunedtree,file=paste0(thisname,"-tunedtree"))

# LASSO
lassoplain <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                 holdoutvar="holdout", holdout=T,
                                 predictors=list(lasso=elnet(lambdas=exp(-seq(3,8,by=.5)), alpha=c(1), interactions="")),
                                 nfold = 8)
saveRDS(lassoplain,file=paste0(thisname,"-lassoplain"))

# Elastic net
elnet <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                                 holdoutvar="holdout", holdout=T,
                                 predictors=list(elnet=elnet(lambdas=exp(-seq(3,8,by=.5)), alpha=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), interactions="")),
                                 nfold = 8)
saveRDS(elnet,file=paste0(thisname,"-elnet"))

# Boosted tree
xgboostfit <- predictiveanalysis(df=instruction$df, LHS=instruction$settings$LHS, RHS=instruction$settings$RHS,
                            holdoutvar="holdout", holdout=T,
                            predictors=list(xgb=xgbcontinous(etas = c(.002,.005,.007,.01,.015), max_depths = c(1,2,3,4,5,6,7,8), nrounds = c(500,1000,1500,2000,2500))),
                            nfold = 8)
saveRDS(xgboostfit,file=paste0(thisname,"-xgboost"))
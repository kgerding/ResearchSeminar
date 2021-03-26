## Read in fitted values and produce Table 1 for JEP paper
# Jann Spiess, March/April 2017

library(ggplot2)
library(rpart)


# Load base data
basedata <- readRDS(file="ahs2011forjep.rdata")

# Fitted objects
olsbaseline <- readRDS(file="jepfittedmodels-olsbl")
tunedrf <- readRDS(file="jepfittedmodels-rfout")
lassoplain <- readRDS(file="jepfittedmodels-lassoplain")
elnetout <- readRDS(file="jepfittedmodels-elnet")
treedepth <- readRDS(file="jepfittedmodels-treedepth")
tunedtree <- readRDS(file="jepfittedmodels-tunedtree")
xgboostfit <- readRDS(file="jepfittedmodels-xgboost")
ensemble <- readRDS(file="jepfittedmodels-ensemble")

# Combine data into one
fulldata <- olsbaseline$returndf[,c("LOGVALUE","holdout")]
fulldata$OLS <- olsbaseline$returndf$prediction
fulldata$Tree <- treedepth$returndf$prediction
fulldata$Lasso <- lassoplain$returndf$prediction
fulldata$Forest <- tunedrf$returndf$prediction
fulldata$Ensemble <- ensemble$returndf$prediction
fulldata$TreeFull <- tunedtree$returndf$prediction
fulldata$TreeNaive <- predict(rpart(basedata$getformula(basedata$vars),basedata$df[!basedata$df$holdout,]),
                              basedata$df)
fulldata$BoostedTree <- xgboostfit$returndf$prediction
fulldata$ElNet <- elnetout$returndf$prediction

# Calculate losses
losses <- fulldata[,c("LOGVALUE","holdout")]
isvar <- mean((losses$LOGVALUE[!losses$holdout] - mean(losses$LOGVALUE[!losses$holdout]))^2)
oosvar <- mean((losses$LOGVALUE[losses$holdout] - mean(losses$LOGVALUE[losses$holdout]))^2)

# Put together into one table
table1 <- data.frame(setup=NULL,predictor=NULL,parameters=NULL,
                     ismse=NULL,isr2=NULL,
                     oosmse=NULL,oosse=NULL,oosr2=NULL,oosr2ci1=NULL,oosr2ci2=NULL)

for(method in setdiff(names(fulldata),c("LOGVALUE","holdout"))) {
  losses[,method] <- (fulldata[,method] - fulldata$LOGVALUE)^2
  
  table1 <- rbind(table1,data.frame(method=method,
                       ismse=mean(losses[!losses$holdout,method]),
                       isr2= 1 - mean(losses[!losses$holdout,method]) / isvar,
                       oosmse=mean(losses[losses$holdout,method]),
                       oosse=sd(losses[losses$holdout,method]) / sqrt(sum(losses$holdout)),oosr2=1 - mean(losses[losses$holdout,method]) / oosvar,
                        oosr2ci1=NA,oosr2ci2=NA))
  
}


# Holdout bootstrap for R2
holdoutsample <- losses[losses$holdout,setdiff(names(losses),c("holdout"))]
B <- 10000
methods <- setdiff(names(fulldata),c("LOGVALUE","holdout"))
bstab <- data.frame(matrix(NA,ncol=length(methods),nrow=0))
names(bstab) <- methods
for(b in 1:B) {
  thissample <- sample.int(nrow(holdoutsample),replace=T)
  for(method in methods) {
    bstab[b,method] <- 1 - mean(holdoutsample[thissample,method]) / mean((holdoutsample[thissample,"LOGVALUE"] - mean(holdoutsample[thissample,"LOGVALUE"]))^2)
  }
}
cis <- lapply(bstab, quantile, probs=c(.025,.975), name=FALSE)

for(methodnr in 1:length(methods)) {
  table1[methodnr,"oosr2ci1"] <- cis[[methods[methodnr]]][1]
  table1[methodnr,"oosr2ci2"] <- cis[[methods[methodnr]]][2]
}

# Add quintile MSEs
bins <- 5
holdoutsample <- losses[losses$holdout,setdiff(names(losses),c("holdout"))]
holdoutsample$decile <- ceiling(ecdf(holdoutsample$LOGVALUE)(holdoutsample$LOGVALUE)*bins)
msedeciles <- aggregate(holdoutsample, list(holdoutsample$decile), mean)[2:(ncol(holdoutsample)+1)]

reldeciles <- msedeciles
reldeciles[,2:(ncol(msedeciles) - 1)] <- (msedeciles$OLS - msedeciles[,2:(ncol(msedeciles) - 1)]) / msedeciles$OLS
 
diffdeciles <- msedeciles
diffdeciles[,2:(ncol(msedeciles) - 1)] <- msedeciles[,2:(ncol(msedeciles) - 1)] - msedeciles$OLS
 
table1[,8+ 1:(3*bins)] <- NA
names(table1) <- c(names(table1)[1:8],c(1:bins,1:bins,1:bins))

for(methodnr in 1:length(setdiff(names(fulldata),c("LOGVALUE","holdout")))) {
 table1[methodnr,8+(1:bins)] <- reldeciles[,setdiff(names(fulldata),c("LOGVALUE","holdout"))[[methodnr]]]
 table1[methodnr,8+bins+(1:bins)] <- diffdeciles[,setdiff(names(fulldata),c("LOGVALUE","holdout"))[[methodnr]]]
 table1[methodnr,8+2*bins+(1:bins)] <- msedeciles[,setdiff(names(fulldata),c("LOGVALUE","holdout"))[[methodnr]]]
}

table1[1,8+(1:bins)] <- msedeciles[,"OLS"]
table1[1,8+bins+(1:bins)] <- msedeciles[,"OLS"]

write.csv(table1,file="jepfittedmodels-table1.csv")

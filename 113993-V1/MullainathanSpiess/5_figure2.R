## Creates barcode plot for JEP paper
# Jann Spiess, March/April 2017


# Load and prepare data

basedata <- readRDS(file=paste0("ahs2011forjep.rdata"))

localahs <- basedata$df
Y <- localahs[,"LOGVALUE"]
thisrhs <- paste(basedata$vars,collapse=" + ")

X <- model.matrix(as.formula(paste("LOGVALUE", thisrhs, sep = " ~ ")),localahs)


# Select (and fix) tuning parameter

set.seed(123)
firstsubsample <- sample(nrow(localahs),nrow(localahs)/10)

library(glmnet)

firstlasso <- glmnet(X[firstsubsample,],Y[firstsubsample])
losses <- apply(predict(firstlasso,newx=X[-firstsubsample,]),2,function(Yhat) mean((Yhat - Y[-firstsubsample])^2))
plot(log(firstlasso$lambda),losses)
lambda <- firstlasso$lambda[which.min(losses)]


# Fit LASSO models

I <- length(unique(localahs$lassofolds))

barcodes <- matrix(0,nrow=I,ncol=firstlasso$dim[1])
lassonormcoeff <- matrix(0,nrow=I,ncol=firstlasso$dim[1])
lassolosses <- vector(mode='numeric',length=I)
lassose <- vector(mode='numeric',length=I)

for(i in 1:I) {
  thissubsample <- localahs$lassofolds == i
  
  thislasso <- glmnet(X[thissubsample,],Y[thissubsample])
  thislosses <- apply(predict(thislasso,newx=X[-thissubsample,]),2,function(Yhat) mean((Yhat - Y[-thissubsample])^2))
  thislambda <- firstlasso$lambda[which.min(thislosses)]
  
  barcodes[i,as.vector(!(thislasso$beta[,which.min(thislosses)] == 0))] <- 1
  pointlosses <- (predict(thislasso,newx=X[-thissubsample,],s=thislambda) - Y[-thissubsample])^2
  lassolosses[i] <- mean(pointlosses)
  lassose[i] <- sd(pointlosses) / sqrt(length(pointlosses))
}


# Barcode plot

library(reshape2)
library(ggplot2)

barcodeplotdata <- melt(barcodes)
names(barcodeplotdata) <- c("Iteration","Coefficient","Selected")
barcodeplotdata$Selected <- as.factor(barcodeplotdata$Selected)
barcodeplotdata$Iteration <- as.factor(barcodeplotdata$Iteration)

barcodeplot <- ggplot(data = barcodeplotdata, aes(x=Iteration, y=Coefficient, fill=Selected)) + 
  geom_tile() + scale_fill_manual(values=c("white","black"),labels=c("zero", "nonzero")) + theme_bw() +
  labs(list(x = "Fold of the sample", y = "Parameter in the linear model",fill="Estimate")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

print(barcodeplot)
ggsave(barcodeplot,file="barcode.png")


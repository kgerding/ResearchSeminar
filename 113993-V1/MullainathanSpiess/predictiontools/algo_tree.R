## Regression tree

library(rpart)

#' Regression tree
regtreealg <- function(df,LHS,RHS,from,to,
                       parameter=list(minbucket=10,maxdepth=30,mingain=0,numericfactors=F),quiet=T) {
  if(!quiet) {
    print(paste0("Regression tree with ",paste(parameter,collapse = "/")))
    start.time <- Sys.time()
  }
  if(parameter$numericfactors) {
    asNumeric <- function(x) as.numeric(as.character(x))
    factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                       asNumeric))
  } else {
    factorsNumeric <- function(d) d   
  }
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))
  thistree <- rpart::rpart(formula=modelformula,data=factorsNumeric(df[from,]),method="anova",
            control=rpart::rpart.control(minbucket=parameter$minbucket,
                                  maxdepth=parameter$maxdepth,cp=parameter$mingain))
  preds <- predict(thistree, newdata = factorsNumeric(df[to,RHS]))  
  
  if(!quiet) { print(paste0("Done in ",Sys.time() - start.time)) }
  return(preds)
}

#' Regression tree wrapper
#' @export
regtree <- function(minbuckets=c(1,5,10,20,30,40,50,70,100,150,200,500),maxdepths=30,mingains=0,numericfactors=F) {
  paramgrid <- expand.grid(minbucket=minbuckets,maxdepth=maxdepths,mingain=mingains,numericfactors=numericfactors)
  return(list(algorithm=regtreealg,
              parameters=split(paramgrid,seq(nrow(paramgrid)))))
}

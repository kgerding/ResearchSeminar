## Random forest

library(randomForest)

#' Random forest
rfalg <- function(df,LHS,RHS,from,to,parameter=list(pmtry=.2,ntree=100,nodesize=100,numericfactors=T,bootstrap=F),quiet=T) {
  if(!quiet) {
    print(paste0("Random forest with ",paste(parameter,collapse = "/")))
    start.time <- Sys.time()
  }
  if(parameter$numericfactors) {
    asNumeric <- function(x) as.numeric(as.character(x))
    factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                       asNumeric))
  } else {
    factorsNumeric <- function(d) d   
  }
  
  thisrf <- randomForest::randomForest(y = unlist(df[from,LHS]), x = factorsNumeric(df[from,RHS]),
                                   mtry = floor(parameter$pmtry * length(RHS)), ntree = parameter$ntree,
                                   replace = parameter$bootstrap,
                                   nodesize = parameter$nodesize)
  preds <- predict(thisrf, newdata = factorsNumeric(df[to,RHS]))  
  if(!quiet) { print(paste0("Done in ",Sys.time() - start.time)) }
  return(preds)
}

#' Random forest wrapper
#' @export
rf <- function(pmtrys=c(.1,.2,.3,.4),ntrees=100,nodesizes=c(5,10,20,50,100,200,400,1000),numericfactors=T,bootstrap=F) {
  paramgrid <- expand.grid(pmtry=pmtrys,ntree=ntrees,nodesize=nodesizes,numericfactors=numericfactors,bootstrap=bootstrap) 
  return(list(algorithm=rfalg,
              parameters=split(paramgrid,seq(nrow(paramgrid)))))
}
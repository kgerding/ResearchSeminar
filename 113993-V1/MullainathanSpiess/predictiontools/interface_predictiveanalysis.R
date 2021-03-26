## Main code to generate predictions and analyze them

#' Construct predictions and analyze them
#' @export
predictiveanalysis <- function(df,
                               LHS,RHS,clusters=NULL,
                               holdoutvar=NULL,holdoutprop=.3,
                               predictors=list(ols=ols()),
                               quiet=F,nfold = 5) {
  
  # Add clusters if none given
  if(is.null(clusters)) {
    df$cluster <- 1:nrow(df)
    clusters <- "cluster"
  }
  
  # Add holdout if none given
  if(is.null(holdoutvar)) {
    df <- gen_holdout(df=df,holdoutprop=holdoutprop,clusters=clusters)
    holdoutvar <- "holdout"
  }
  
  # Hard-code holdout column
  names(df)[names(df)==holdoutvar] <- "holdout"
  
  # Add inner folds
  df$fold <- NA
  df[!df$holdout,] <- gen_folds(df=df[!df$holdout,],nfold=nfold,clusters=clusters)
  
  # Obtain prediction return
  predictionreturn <- holdoutpredict(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                                     ids=NULL,algolist=predictors,
                                     innerfolds=nfold,quiet=quiet,insample=T)
  
  # Analyze and return results
  return(predictability(predictionreturn,LHS,clusters))
}
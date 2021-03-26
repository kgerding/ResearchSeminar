## Scripts for testing predictability

#' Produce prediction draw for holdout
holdoutpredict <- function(df,LHS,RHS,clusters,ids,algolist,innerfolds,losstype,quiet=T,insample=F) {
  
  # Get ensemble predictions
  ensemblereturn <- ensemblepredict(df=df[!df$holdout,],LHS=LHS,RHS=RHS,clusters=clusters,
                                    algolist=algolist,innerfolds=innerfolds,quiet=quiet,requirepredictions=F)
  if(!insample) {
    # Predict on holdout
    df$prediction <- NA
    df[df$holdout,"prediction"] <-
      oospredict(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                       from=!df$holdout,to=df$holdout,
                 ensembleweights=ensemblereturn$ensembleweights,
                 algolist=algolist,innerfolds=innerfolds,quiet=quiet)
    holdoutdf <- df[df$holdout,c(LHS,"prediction",RHS,clusters,ids)]
    outdf <- holdoutdf
  } else {
   # Predict everywhere
    df$prediction <-
      oospredict(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                 from=!df$holdout,to=(df$holdout | !df$holdout),
                 ensembleweights=ensemblereturn$ensembleweights,
                 algolist=algolist,innerfolds=innerfolds,quiet=quiet)
    outdf <- df[,c(LHS,"prediction","holdout",RHS,clusters,ids)]
    holdoutdf <- outdf[outdf$holdout,]
  }
  
  loss <- mean((holdoutdf$prediction - holdoutdf[,LHS])^2)

  drawreturn <- list(outdf=outdf,loss=loss,ensemble=ensemblereturn$ensembleweights,params=ensemblereturn$paramlist)
  
  return(drawreturn)
}


  
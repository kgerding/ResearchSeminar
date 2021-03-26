#' Analyze prediction outcome
predictability <- function(predictionreturn,LHS,clusters="cluster") {
  l2losses <- (predictionreturn$outdf$prediction - predictionreturn$outdf[,LHS])^2
  
  ismse <- mean(l2losses[!predictionreturn$outdf$holdout])
  isrefmse <- mean((mean(predictionreturn$outdf[!predictionreturn$outdf$holdout,LHS]) - predictionreturn$outdf[!predictionreturn$outdf$holdout,LHS])^2)
  isr2 <- 1 - ismse / isrefmse   
  
  print(isr2)
  
  oosmse <- mean(l2losses[predictionreturn$outdf$holdout])
  oosrefmse <- mean((mean(predictionreturn$outdf[predictionreturn$outdf$holdout,LHS]) - predictionreturn$outdf[predictionreturn$outdf$holdout,LHS])^2)
  oosr2 <- 1 - oosmse / oosrefmse
  
  print(oosr2)
  
  # Clustered standard errors
  clusterse <- function(x,cl,ho) {
    x <- x[ho]
    cl <- cl[ho]
    return(
      sqrt(sum((aggregate(x - mean(x), by=list(cl), FUN=sum)[,2])^2)) /
        length(x)
    )
  }

  oosse <- clusterse(l2losses,predictionreturn$outdf[,clusters],predictionreturn$outdf$holdout)
    
  # Obtain return dataframe
  returndf <- predictionreturn$outdf[,c(LHS,"prediction",clusters,"holdout")]
  
  # Return it all as list
  return(list(ismse=ismse,isr2=isr2,oosmse=oosmse,oosse=oosse,oosr2=oosr2,returndf=returndf))
}
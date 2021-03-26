## Manual parallelization tools to distribute tuning to server

library(ggplot2)


#' Prepare data for server-side analysis (single prediction algorithm)
#' @export
parallelprep <- function(df,LHS,RHS,clusters=NULL,
                               holdoutvar=NULL,holdoutprop=.3,
                               predictor=ols(),
                               quiet=F,
                               nfold = 5,
                               savename="servertest") {
  
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
  df[!df$holdout,] <- gen_folds(df=df[!df$holdout,setdiff(names(df),"fold")],nfold=nfold,clusters=clusters)
  
  # Save the data
  saveRDS(object=list(df=df,
               settings=list(LHS=LHS,RHS=RHS,clusters=clusters,quiet=quiet),
               predictor=predictor),
          file=savename)

}


#' Perform a single cross-validation prediction iteration
#' @export
parallelsinglepredict <- function(savename="servertest",iteration=1) {
  
  instruction <- readRDS(file=savename)
  
  df <- instruction$df[!instruction$df$holdout,]

  df$prediction <- NA

  folds <- unique(df$fold)

  for (i in 1:length(folds)) {
    if(!instruction$settings$quiet) { print(paste0("Fold ", i, " of ", length(folds))) }
    
    fold_out  <- which(df$fold == folds[i])
    fold_in   <- which(df$fold != folds[i])
    
    df$prediction[fold_out] <- instruction$predictor$algorithm(df=df,
                LHS=instruction$settings$LHS,RHS=instruction$settings$RHS,
                from=fold_in,to=fold_out,
                parameter=instruction$predictor$parameters[[iteration]],
                quiet=instruction$settings$quiet)
  }
  
  # Save output
  saveRDS(object=list(iteration=iteration,
      parameter=instruction$predictor$parameters[[iteration]],
      cvloss=mean((df$prediction - df[,instruction$settings$LHS])^2)),
      file=paste0(savename,iteration))
  
}


#' Combine the return from individual nodes, analyze, and return best tuning parameter
#' @export
parallelanalysis <- function(savename="servertest",quiet=F,fitit=F) {
  
  instruction <- readRDS(file=savename)
  
  returns <- lapply(
    intersect(list.files(),paste0(savename,1:length(instruction$predictor$parameters))),
    function(filename) readRDS(file=filename))
  
  parameters <- lapply(returns,function(ret) ret$parameter)
  mean_l2 <- unlist(lapply(returns,function(ret) ret$cvloss))
  
  pick_me <- which.min(mean_l2)
  pickparam <- parameters[[pick_me]]
  
  # Plot loss by parameters -- experimental
  if(!quiet) {
    df <- instruction$df[!instruction$df$holdout,]
    var <- mean((df[,instruction$settings$LHS] - mean(df[,instruction$settings$LHS]))^2)
    ggsave(paste0(savename,Sys.time(),".png"),plot=tuningplot(mean_l2,parameters,pick_me,var))
    print(paste0("In inner tuning, picked ",paste(pickparam,collapse = "/")))
  }
  
  if(fitit) {
    return(list(pickparam=pickparam,
      analysis=predictiveanalysis(df=instruction$df,
                                   LHS=instruction$settings$LHS,RHS=instruction$settings$RHS,
                                   clusters=instruction$settings$clusters,
                                   holdoutvar="holdout",holdout=T,
                                   predictors=list(list(algorithm=instruction$predictor$algorithm,parameters=list(pickparam))))))
  } else {
    return(pickparam)
  }
}


  
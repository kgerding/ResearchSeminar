## Tools for prediction
# Contains tools for k-fold tuning and ensemble construction
 
library(ggplot2)

#' K-fold tuning
#' Creates OOS predictions for every tuning parameter respecting the folds,
#' picks optimal parameters, and returns fitted values
tuning <- function(df,LHS,RHS,clusters,
                   fold_in,fold_out,
                   algorithm,parameters,innerfolds=5,quiet=T) {
  if(length(parameters) > 1) { 
    phats <- matrix(rep(NA, nrow(df[fold_in,]) * length(parameters)), ncol = length(parameters))
    thisdf <- gen_folds(df=df[fold_in,], nfold = innerfolds,clusters=clusters,foldname="foldinner")
    dv_vector <- thisdf[,LHS]
    ifolds <- unique(thisdf$foldinner)
    for (ii in 1:length(ifolds)) {
      ifold_out  <- which(thisdf$foldinner == ifolds[ii])
      ifold_in   <- which(thisdf$foldinner != ifolds[ii])        
      
      for(j in 1:length(parameters)) {
        thispreds <- algorithm(df=thisdf,LHS=LHS,RHS=RHS,
                               from=ifold_in,to=ifold_out,parameter=parameters[[j]],quiet=quiet)
        phats[ifold_out,j] <- thispreds
      }
    }
    
    l2      <- apply(phats, 2, function(x) (dv_vector - x)^2)
    mean_l2 <- apply(l2, 2, mean)
    pick_me <- which.min(mean_l2)
    pickparam <- parameters[[pick_me]]
    
    # Plot loss by parameters -- experimental
    if(!quiet) {
      var <- mean((dv_vector - mean(dv_vector))^2)
      ggsave(paste0(Sys.time(),".png"),plot=tuningplot(mean_l2,parameters,pick_me,var))
      print(paste0("In inner tuning, picked ",paste(pickparam,collapse = "/")))
    }
    
  } else {
    pickparam <- parameters[[1]]
  }
  preds <- algorithm(df=df,LHS=LHS,RHS=RHS,
                     from=fold_in,to=fold_out,parameter=pickparam,quiet=quiet)
  return(list(preds=preds,param=pickparam))
}

#' K-fold tuned prediction for use in ensemble
tunedpredict <- function(df,LHS,RHS,clusters,
                         algorithm,parameters,innerfolds=5,quiet=T) {
  # 'algorithm' is assumed to be function algorithm(df,LHS,RHS,
  #                                                 from,to,parameter)
  
  predictions <- rep(NA,nrow(df))
  
  folds <- unique(df$fold)
  
  # For each previously defined fold split df into observations in that fold (the hold 'out') and the rest 'in' the training set
  for (i in 1:length(folds)) {
    if(!quiet) { print(paste0("Fold ", i, " of ", length(folds))) }
    
    fold_out  <- which(df$fold == folds[i])
    fold_in   <- which(df$fold != folds[i])
    
    tuned <- tuning(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                    fold_in=fold_in,fold_out=fold_out,
                    algorithm=algorithm,parameters=parameters,innerfolds=innerfolds,quiet=quiet)
    
    # Save predictions fit on the rest of the data 
    predictions[fold_out] <- tuned$preds
    
  }
  
  # Returns predictions
  return(predictions)
}


#' Ensemble prediction
#' @export
#' @examples
ensemblepredict <- function(df,LHS,RHS,clusters,
                            algolist,innerfolds,quiet=T,requirepredictions=T,
                            ols=T) {
  # 'algolist' is assumed list with algorithm, parameters for every entry

  numalg <- length(algolist)
  
  if(requirepredictions | numalg > 1) {
    
    phats     <- matrix(rep(NA, nrow(df) * length(algolist)), ncol = length(algolist))
    dv_vector <- df[,LHS]
    
    if(!quiet) { print(paste0("Calculating ensemble weights for ", paste0(names(algolist), collapse=", "))) }
    
    for(a in 1:length(algolist)) {
      if(!quiet) { print(paste0("Fitting ", names(algolist)[a])) }
      algoreturn <- tunedpredict(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                                 algorithm=algolist[[a]]$algorithm,parameters=algolist[[a]]$parameters,
                                 innerfolds=innerfolds,quiet=quiet)
      phats[,a] <- algoreturn
    }
    
    saveRDS(cbind(dv_vector,phats),file="ensembleintermediate")
    
    if(!quiet) { 
      print(paste0("Individual CV MSE are ",
                   paste(colMeans((phats - dv_vector)^2),collapse="/")))
    }
    
    if(numalg > 1) {
      
      if(ols) {
        # Using OLS to fit ensemble weights
      
        ensembleweights <- as.vector(coef(lm(dv_vector ~ phats)))
        
      } else {
        # Use weights that add up to one without intercept
        
        realw <- function(w) c(w,1-sum(w))
        Amat <- diag(rep(1,numalg - 1),nrow=numalg,ncol=numalg - 1)
        Amat[numalg,] <- rep(-1,numalg - 1)
        lossfromw <- function(w) {
          # Function to minimize
          return(mean((phats %*% realw(w) - dv_vector)^2))
        }
        lossfromwgrad <- function(w) {
          # Gradient
          return(as.vector(2 * t(Amat) %*% t(phats) %*% (phats %*% realw(w) - dv_vector) / length(dv_vector)))
        }
        # initialize with equal weights
        startw <- rep(1/numalg,numalg - 1)
        cvec <- c(rep(0,numalg - 1),-1)
        optimret <- constrOptim(theta=startw,f=lossfromw,grad=lossfromwgrad,ui=Amat,ci=cvec)
        ensembleweights <- c(0,realw(optimret$par))
        var <- mean((dv_vector - mean(dv_vector))^2)
        
        if(!quiet) { 
          print(paste0("CV MSE at optimal weight is ",lossfromw(optimret$par)))
        }
        
        if(numalg == 2 && !quiet) {
          # Plot weight and R2 -- experimental
          w <- (0:100)/100
          print(ggplot(data.frame(W=w,R2=1 - sapply(w,lossfromw) / var),aes(x=W,y=R2)) + geom_line() + theme_bw())
          print(paste0("R2 gain over first predictor is ",(lossfromw(1) - lossfromw(optimret$par)) / var))
          print(paste0("Relative gain is ",(lossfromw(1) - lossfromw(optimret$par)) / (var - lossfromw(optimret$par))))
        }
        
      }
      }
    else {
      ensembleweights <- c(0,1)
    }
    
    predictions <- ensembleweights[1] + as.vector(phats %*% ensembleweights[2:length(ensembleweights)])
    
  }
  else {
   # Only one algorithm and no predictions required at this point, so return trivial weight
    ensembleweights <- c(0,1)
    predictions <- NULL
  }
  
  print(paste0("Chosen weights are ",paste(ensembleweights,collapse = "/")))
  
  # Returns ensemble weights, individual parameters, and predictions
  return(list(ensembleweights=ensembleweights,predictions=predictions))
}


#' OOS prediction from existing ensemble
#' Returns out-of-sample predictions for given ensemble
#' @export
oospredict <- function(df,LHS,RHS,clusters,
                               from,to,
                               ensembleweights,algolist,innerfolds,quiet=T) {
  phats <- matrix(rep(NA, nrow(df[to,]) * length(algolist)), ncol = length(algolist))
  
  # Get the predictions from fitting to the whole training sample
  for(a in 1:length(algolist)) {
    tuned <- tuning(df=df,LHS=LHS,RHS=RHS,clusters=clusters,
                    fold_in=from,fold_out=to,
                    algorithm=algolist[[a]]$algorithm,parameters=algolist[[a]]$parameters,
                    innerfolds=innerfolds,quiet=quiet)
    phats[,a] <- tuned$preds
  }
  
  # Predict linearly according to ensemble
  phat <- ensembleweights[1] + as.vector(phats %*% ensembleweights[2:length(ensembleweights)])

  # Returns predicted values
  return(phat)
}


#' Generate loss plot by tuning parameter -- experimental
tuningplot <- function(losses,parameters,pick,var) {
  # Obtain data frame and parameter names
  parameterlist <- do.call(rbind,parameters)
  parameternames <- names(sort(apply(parameterlist, 2, function(x)length(unique(x))),decreasing=T))

  # Cut down to at most three parameters
  if(length(parameternames) > 3) {
    newname <- paste0(parameternames[3:length(parameternames)],collapse="_")
    parameterlist[,newname] <- apply(parameterlist[,parameternames[3:length(parameternames)]], 1, function(x) paste(x,collapse="_"))
    parameternames <- c(parameternames[1:2],newname)
    parameterlist <- parameterlist[,parameternames]
  }
  
  # Add loss
  parameterlist$R2 <- 1 - losses / var
  
  # Highlight the chosen one
  parameterlist$Chosen <- 0
  parameterlist[pick,"Chosen"] <- 1
  parameterlist$Chosen <- as.factor(parameterlist$Chosen)
  
  # Prepare graph
  library(ggplot2)
  if(length(parameternames) == 1) {
    thisplot <- ggplot(parameterlist,aes_string(x=parameternames[1], y="R2"))
  } else if(length(parameternames) == 2) {
    parameterlist[,parameternames[2]] <- as.factor(parameterlist[,parameternames[2]])
    thisplot <- ggplot(parameterlist,aes_string(x=parameternames[1], y="R2",
                                  color=parameternames[2])) #group=parameternames[2])
  } else if(length(parameternames) == 3) {
    parameterlist[,parameternames[2]] <- as.factor(parameterlist[,parameternames[2]])
    parameterlist[,parameternames[3]] <- as.factor(parameterlist[,parameternames[3]])
    thisplot <- ggplot(parameterlist,aes_string(x=parameternames[1],y="R2",
                                  color=parameternames[2],shape=parameternames[3]))
    }
  
  # Return ggplot object with colorblind-friendly scale
  return(thisplot + geom_point(aes(size=Chosen)) + geom_line() + scale_size_discrete(guide = "none") + theme_bw() +
           scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")))
}

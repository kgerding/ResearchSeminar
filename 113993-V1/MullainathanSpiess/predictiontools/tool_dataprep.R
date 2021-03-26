## Tools for data preparation

library(dplyr)


#' Generate folds
gen_folds <- function(df, nfold = 5,clusters,foldname="fold") {

  cl <- unique(df[,clusters])
  cl <- cl[order(cl)]
  
  fold_size <- ceiling(length(cl) / nfold)
  
  folds <- rep(1:nfold, fold_size)
  folds <- folds[order(folds)]
  folds <- folds[1:length(cl)]
  folds <- sample(folds)  # Randomize fold assignment to clusters
  
  cl_df <- data.frame(CL=cl,FO=folds)
  names(cl_df) <- c(clusters,foldname)

  df2 <- dplyr::left_join(df[,setdiff(names(df),foldname)], cl_df, by = clusters)
    
  df[,foldname] <- df2[,foldname]
  
  return(df)
}


#' Generate holdout
#' @export
gen_holdout <- function(df, holdoutprop=.5,clusters) {

    cl <- unique(df[, clusters])
  cl <- cl[order(cl)]
  
  holdout_size <- floor(length(cl) * holdoutprop)
  
  holdout <- c(rep(T, holdout_size),rep(F,length(cl) - holdout_size))
  holdout <- sample(holdout)  # Randomize holdout assignment to clusters
  
  cl_df <- data.frame(holdout = holdout)
  cl_df[,clusters] <- cl
  
  df <- dplyr::left_join(df, cl_df, by=clusters)
  
  return(df)
}

## Elastic net prediction
# Ridge, lasso are implemented as special cases of elastic net

library(glmnet)

#' Elastic net
elnetalg <- function(df,LHS,RHS,from,to,
                       parameter=list(lambda=1, alpha=.5, interactions=""),quiet=T) {
  parameter$interactions <- as.character(parameter$interactions)
  if(!quiet) {
    print(paste0("Elastic net with ",paste(parameter,collapse = "/")))
    start.time <- Sys.time()
  }
  if(parameter$interactions == "") {
    modelformula <- as.formula(paste0(LHS, " ~ ", paste(setdiff(RHS, parameter$interactions), collapse=" + ")))
  } else if(parameter$interactions == "pairwise") {
    modelformula <- as.formula(paste0(LHS, " ~ (",paste(setdiff(RHS, parameter$interactions), collapse=" + "),
                                      ")^2"))
  } else if(length(parameter$interactions) == 1) {
    modelformula <- as.formula(paste0(LHS, " ~ (",paste(setdiff(RHS, parameter$interactions), collapse=" + "),
                                      ") * ",parameter$interactions))
  } else {
    modelformula <- as.formula(paste0(LHS, " ~ (",paste(setdiff(RHS, parameter$interactions), collapse=" + "),
                                      ") * (",paste(parameter$interactions, collapse=" + "),")"))
  }
  matrixdata <- model.matrix(modelformula,data=df)
  thisnet <-   glmnet::glmnet(matrixdata[from,-1],
                  as.matrix(df[from,LHS]),
                  family="gaussian",
                  lambda=parameter$lambda,
                  alpha=parameter$alpha)
  preds <- predict(thisnet, s=parameter$lambda, matrixdata[to,-1], type="response")
  # print(thisnet$beta)
  if(!quiet) { print(paste0("Done in ",Sys.time() - start.time)) }
  return(preds)
}

#' Elastic net wrapper
#' @export
elnet <- function(lambdas=exp(-seq(-8,10,by=.5)), alphas=c(0,.5,1), interactions="") {
  paramgrid <- expand.grid(lambda=lambdas,alpha=alphas,interactions=interactions)
  return(list(algorithm=elnetalg,
              parameters=split(paramgrid,seq(nrow(paramgrid)))))
}

## Boosted tree

library(xgboost)

#' XGBoost continuous
xgbcontinousalg <- function(df, LHS, RHS, from, to, parameter = list(eta = .3, max_depth = 6, nround = 1000, gamma=0), quiet = T) {
  if(!quiet) {
    print(paste0("XGBoost with ",paste(parameter,collapse = "/")))
    start.time <- Sys.time()
  }
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(setdiff(RHS, parameter$interactions), collapse=" + "), " - 1"))
  matrixdata <- model.matrix(modelformula,data=df)
  xgbcontinous <- xgboost::xgboost(data = matrixdata[from,], label = unlist(df[from, LHS]),
                                   eta = parameter$eta, max.depth = parameter$max_depth, nround = parameter$nround, 
                                   gamma = parameter$gamma,
                                   objective = "reg:linear",
                                   verbose=0)
  preds <-predict(xgbcontinous, newdata = matrixdata[to,])
  if(!quiet) { print(paste0("Done in ",Sys.time() - start.time)) }
  return(preds)
}

#' XGBoost continuous wrapper
#' @export
xgbcontinous <- function(etas = c(.01, .1, .3, .5), max_depths = c(1, 4, 6), nrounds = c(1000, 2000),gammas=c(0)) {
  paramgrid <- expand.grid(eta = etas, max_depth = max_depths, nround = nrounds, gamma=gammas)
  return(list(algorithm = xgbcontinousalg, 
              parameters = split(paramgrid, seq(nrow(paramgrid)))))
  
}
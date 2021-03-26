## Linear prediction

#' OLS
olsalg <- function(df,LHS,RHS,from,to,parameter=NULL,quiet=T) {
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))
  linmod <- lm(modelformula,df[from,])
  preds <- predict(linmod,df[to,])  
  return(preds)
}

#' Logit
logitalg <- function(df,LHS,RHS,from,to,parameter=NULL,quiet=T) {
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))
  linmod <- glm(modelformula,data=df[from,],family="binomial")
  preds <- predict(linmod,newdata=df[to,],type="response")  
  return(preds)
}

#' Average
avgalg <- function(df,LHS,RHS,from,to,parameter=NULL,quiet=T) {
  return(rep(mean(df[from,LHS]),nrow(df[to,])))
}

#' OLS wrapper
#' @export
ols <- function() {
  list(algorithm=olsalg,parameters=NULL)}

#' Logit wrapper
#' @export
logit <- function() {
  list(algorithm=logitalg,parameters=NULL)}

#' Mean wrapper
#' @export
avg <- function() {
  list(algorithm=avgalg,parameters=NULL)}

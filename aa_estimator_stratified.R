#' Stratified estimation of map accuracy and area
#'
#' @title aa_estimator_stratified
#' @description Estimator for estimating stratified area proportions
#' @param stratum vector of stratum identifiers
#' @param y_u indicator function (Boolean vector)
#' @param h vector of unique stratum identifiers that correspond to N_h
#' @param N_h number of samples (area) for each stratum h
#' @return estimate and its standard error
#' @references Stehman, S. V., 2014. Estimating area and map accuracy for stratified random sampling when the strata are different from the map classes. Int. J. Remote Sens. 35, 4923–4939. https://doi.org/10.1080/01431161.2014.930207
#' @examples
#' r <- c("A","A","A","A","A","C","B","A","B","C","A","B","B","B","B","B","A","A","B","B","C","C","C","C","C","D","D","B","B","A","D","D","D","D","D","D","D","C","C","B")
#' m <- c("A","A","A","A","A","A","A","B","B","B","A","B","B","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","B","B","D","D","D","D","D","D","D","D","D","D")
#' s <- c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4")
#' h <- c("1", "2", "3", "4")
#' N_h <- c(40000, 30000, 20000, 10000)
#' aa_stratified(s, r, m, h=h, N_h=N_h)
#' @author Dirk Pflugmacher
#' @export
#' 


aa_estimator_stratified <- function(stratum, y_u, h, N_h) {
  
  if (! (class(N_h) %in% c('numeric', 'integer'))) {
    return(invisible(NULL))
  }
  
  df.u <- data.frame(h=stratum, y_u=y_u)
  df.u <- df.u[order(df.u$h),]
  
  df.h <- aggregate(list(y_h=df.u$y_u, n_h=!is.na(df.u$h)), by=list(h=df.u$h), sum)
  df.h$ym_h <- df.h$y_h/df.h$n_h
  
  df.u <- merge(df.u, df.h)
  
  t2_yh <- (df.u$y_u - df.u$ym_h)^2 / (df.u$n_h-1)
  
  s2 <- aggregate(list(s2_yh=t2_yh, n_h=!is.na(df.u$h)), by=list(h=df.u$h), sum)
  
  df.h <- merge(df.h, data.frame(h=h, N_h=N_h))
  
  df.h <- df.h[order(df.h$h),]
  s2 <- s2[order(s2$h),]
  
  Y <- sum(df.h$N_h * df.h$ym_h)
  R <- Y / sum(N_h)
  
  R_var <- 1/(sum(N_h)^2) * sum(df.h$N_h^2 * s2$s2_yh /s2$n_h)
  
  return(list(R=R, R_SE=sqrt(R_var)))
  
}

#' Stratified estimation of map accuracy and area
#'
#' @title aa_stratified
#' @description Calculates Agreement and Area using stratified estimators
#' @param stratum vector of stratum identifiers
#' @param reference vector of reference class labels 
#' @param map vector of map class labels 
#' @param h vector of unique stratum identifiers that correspond to N_h
#' @param N_h number of samples (area) for each stratum h
#' @return list of accuracy and area proportion estimates and associated standard errors
#' @references Stehman, S. V., 2014. Estimating area and map accuracy for stratified random sampling when the strata are different from the map classes. Int. J. Remote Sens. 35, 4923–4939. https://doi.org/10.1080/01431161.2014.930207
#' @examples   
#' r <- c("A","A","A","A","A","C","B","A","B","C","A","B","B","B","B","B","A","A","B","B","C","C","C","C","C","D","D","B","B","A","D","D","D","D","D","D","D","C","C","B")
#' m <- c("A","A","A","A","A","A","A","B","B","B","A","B","B","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","B","B","D","D","D","D","D","D","D","D","D","D")
#' s <- c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4")
#' h <- ("1", "2", "3", "4")
#' N_h <- (40000, 30000, 20000, 10000)
#' aa_stratified(s, r, m, h=h, N_h=N_h)
#' @author Dirk Pflugmacher
#' @export
#' 



aa_stratified <- function(stratum, reference, map, h=NULL, N_h=NULL) {
  
  if (! (class(N_h) %in% c('numeric', 'integer'))) {
    return(invisible(NULL))
  }
  
  reference <- as.character(reference)
  map <- as.character(map)
  stratum <- as.character(stratum)
  if (!is.null(h)) h <- as.character(h)
  
  # overall accuracy
  oa <- aa_estimator_stratified(stratum, y_u=(map==reference), h=h, N_h=N_h)
  
  # confusion matrix
  cmp <- aa_confusion_matrix_stratified(stratum, reference, map, h=h, N_h=N_h)
  
  # stats
  classes <- sort(unique(unique(reference), unique(map)))
  stats <- data.frame(class=classes)
  area <- data.frame(class=classes)
  area$proportion <- 0
  area$proportion_se <- 0
  stats$ua <- 0
  stats$ua_se <- 0
  stats$pa <- 0
  stats$pa_se <- 0
  for (i in 1:nrow(stats)) {
    
    # area proportion
    tmp0 <- aa_estimator_stratified(stratum, y_u=(reference==stats$class[i]), h=h, N_h=N_h)
    area$proportion[i] <- tmp0$R
    area$proportion_se[i] <- tmp0$R_SE
    
    # user's accuracy
    tmp1 <- aa_estimator_stratified_ratio(stratum, h=h, N_h=N_h,
                                          x_u=(map==stats$class[i]), 
                                          y_u=((reference==stats$class[i]) & (map==stats$class[i])))
    stats$ua[i] <- tmp1$R
    stats$ua_se[i] <- tmp1$R_SE
    
    # producer's accuracy
    tmp2 <- aa_estimator_stratified_ratio(stratum, h=h, N_h=N_h,
                                          x_u=(reference==stats$class[i]), 
                                          y_u=((reference==stats$class[i]) & (map==stats$class[i])))
    stats$pa[i] <- tmp2$R
    stats$pa_se[i] <- tmp2$R_SE
    rm(tmp1, tmp2)
  }
  
  return(list(cm=cmp*length(stratum), cmp=cmp, stats=stats, accuracy=c(oa$R, oa$R_SE), area=area))
}

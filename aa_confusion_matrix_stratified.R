#' Stratified estimation of map accuracy and area
#'
#' @title aa_confusion_matrix_stratified
#' @description Constructs sampling-bias adjusted confusion matrix
#' @param stratum vector of stratum identifiers
#' @param reference vector of reference class labels 
#' @param map vector of map class labels 
#' @param h vector of unique stratum identifiers that correspond to N_h
#' @param N_h number of samples (area) for each stratum h
#' @return confusion matrix
#' @references Stehman, S. V., 2014. Estimating area and map accuracy for stratified random sampling when the strata are different from the map classes. Int. J. Remote Sens. 35, 4923–4939. https://doi.org/10.1080/01431161.2014.930207
#' @examples   
#' r <- c("A","A","A","A","A","C","B","A","B","C","A","B","B","B","B","B","A","A","B","B","C","C","C","C","C","D","D","B","B","A","D","D","D","D","D","D","D","C","C","B")
#' m <- c("A","A","A","A","A","A","A","B","B","B","A","B","B","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","B","B","D","D","D","D","D","D","D","D","D","D")
#' s <- c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4")
#' h <- ("1", "2", "3", "4")
#' N_h <- (40000, 30000, 20000, 10000)
#' aa_confusion_matrix_stratified(s, r, m, h=h, N_h=N_h)
#' @author Dirk Pflugmacher
#' @export
#' 


aa_confusion_matrix_stratified <- function(stratum, reference, map, h=NULL, N_h=NULL) {
  
  if (! (class(N_h) %in% c('numeric', 'integer'))) {
    return(invisible(NULL))
  }
  
  reference <- as.character(reference)
  map <- as.character(map)
  
  classes <- sort(unique(unique(reference), unique(map)))
  
  cm <- matrix(data=NA, nrow=length(classes), ncol=length(classes))
  colnames(cm) <- classes
  rownames(cm) <- classes
  for (i in 1:length(classes)) {
    for (j in 1:length(classes)) {
      tmp <- aa_estimator_stratified(stratum, h=h, N_h=N_h, y_u=(map==classes[i]) & (reference == classes[j]))
      cm[i, j] <- tmp$R
    }
  }
  return(cm)
}

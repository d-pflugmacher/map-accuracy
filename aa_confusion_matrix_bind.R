#' Bind confusion matrix
#'
#' @title aa_confusion_matrix_bind
#' @description Bind Producer's and User's accuracy and standard error to a confusion matrix
#' @param aalist output from aa_card
#' @param proportion express confusion matrix in area proportions instead of counts
#' @param firstUp convert first letter in classnames to upper case
#' @param accuracy.multiplier Detault 1, Set to 100 to obtain percent values
#' @param area.percent True (default) if area proportion estimates are desired in percent
#' @return confusion matrix
#' @author Dirk Pflugmacher
#' @export
#' 

aa_confusion_matrix_bind <- function(aalist, proportion=F, firstUp=T, area.percent=F, accuracy.multiplier=1) {
  if (proportion) {
    cm <- aalist$cmp
    if (area.percent) cm <- cm * 100
  } else {
    cm <- aalist$cm
  }
  
  cm <- cbind(cm, Total=c(apply(cm, 1, sum)))
  cm <- rbind(cm, Total=c(apply(cm, 2, sum)))

  cm <- cbind(cm, users_accuracy=aalist$stats$ua[1:nrow(cm)]*accuracy.multiplier, 
              standard_error=aalist$stats$ua_se[1:nrow(cm)]*accuracy.multiplier)
  cm <- rbind(cm, producers_accuracy=aalist$stats$pa[1:ncol(cm)]*accuracy.multiplier)
  cm <- rbind(cm, standard_error=aalist$stats$pa_se[1:ncol(cm)]*accuracy.multiplier)
  cm["producers_accuracy", "users_accuracy"] <- aalist$accuracy[1] * accuracy.multiplier
  cm["standard_error", "standard_error"] <- aalist$accuracy[2] * accuracy.multiplier
    
  if (proportion) {
    cm <- cbind(cm, n=(apply(aalist$cm, 1, sum))[1:nrow(cm)])
    cm <- rbind(cm, n=c(apply(aalist$cm, 2, sum))[1:ncol(cm)])
    cm['n', 'n'] <- sum(aalist$cm, na.rm=T)
  }
  
  cm <- data.frame(cm)
  
  if (firstUp) {
    row.names(cm) <- firstup(row.names(cm))
    colnames(cm) <- firstup(colnames(cm))
  }
  
  return(cm)
}
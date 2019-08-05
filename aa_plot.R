require(ggplot2)
require(tidyr)

aa_plot <- function(aaStats){
  tmp1 <- gather(aaStats[,c('class', 'ua', 'pa')], src, accuracy, -class)
  tmp2 <- gather(aaStats[,c('class', 'ua_se', 'pa_se')], src, se, -class)
  tmp2$src <- gsub('_se', '', tmp2$src)
  result <- merge(tmp1, tmp2)
  result$se_low <- result$accuracy - result$se
  result$se_upp <- result$accuracy + result$se
  result$src <- gsub('pa', "Producer's accuracy", result$src)
  result$src <- gsub('ua', "User's accuracy", result$src)
  
  p <- ggplot(result, aes(class, accuracy, fill=src)) + 
    geom_bar(stat="identity") +
    xlab('Class') + ylab('Accuracy') +
    # coord_flip() + # scale_y_continuous(limits=c(0, 0.8)) +
    scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
    geom_errorbar(aes(ymin=se_low, ymax=se_upp),
                  width=.5, size =0.3,                   # Width of the error bars
                  position=position_dodge(.9)) +
    #scale_fill_brewer(palette="Dark2") + # Paired Dark2
    #scale_fill_manual(values=cbPalette[c(3,2,4,6,1)]) +
    # scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3","#E69F00")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(), legend.position= "bottom") 
  
  print(p)
}
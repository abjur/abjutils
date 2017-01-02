#' Produce frequency and relative frequency Tables
#' 
#' Produces a contingency table of the elements of a vector calculating relative
#' frequencies as well.
#' 
#' @param x A vector.
#' @param label Quoted name of the column to create in output .
#' 
#' @return A data.frame containing frequency and relative frequencies for the
#' levels o x.
#' 
#' @export
tabela <- function(x, label='variavel') {  
  tab1 <- data.frame(table(x))
  tab2 <- data.frame(prop.table(table(x))*100)
  tab3 <- merge(tab1,tab2, by='x')
  tab3 <- tab3[order(tab3$Freq.y, decreasing=T),]
  tab3$Freq.y <- round(tab3$Freq.y,2)
  tab3 <- as.data.frame(rbind(as.matrix(tab3), c("Total", sum(tab1$Freq), "100.0")))
  names(tab3) <- c(label, 'Freq\032ncia', '   %')
  tab3
}
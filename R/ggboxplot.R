#' Funcao.
#' @export
#' @import ggplot2
#' @import grid
ggboxplot <- function(data, var.cont, 
                      var.categ = NULL, 
                      tit.cont = var.cont, 
                      tit.categ = var.categ){
  # ggboxplot - BOXPLOT PARA UMA VAR CONTÍNUA VS CONJUNTO DE CATEGÓRICAS
  # Função que desenha boxplot para uma var.cont para combinações de níveis das var.categ.
  # Parâmetros:
  # data - 
  # var.categ -
  # var.cont -
  # tit.categ -
  # tit.cont -
  var.categ <- as.character(var.categ)
  var.cont <- as.character(var.cont)
  Tot <- tapply(data[,var.cont],data[,var.categ],function(x)sum(!is.na(x)))
  Tot[1] <- paste("n =  ",Tot[1],"       ")
  mediana.geral <- median(data[,var.cont], na.rm=TRUE)
  df <- data[,c(var.categ,var.cont)]
  names(df) <- c("Var1","Var2")
  ggplot(df, aes(x=Var1, y=Var2, fill=Var1)) + 
    geom_boxplot(outlier.size=4) +
    geom_hline(yintercept=mediana.geral,colour="red") +
    labs(fill=tit.categ, y=tit.cont) +
    scale_x_discrete(labels=Tot) +
    theme_bw() +
    theme(axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=20),
          legend.title=element_text(size=18),
          legend.text=element_text(size=16),
          legend.key.size = unit(1.5, "cm"),
          aspect.ratio = 2/(1 + sqrt(5)))
}
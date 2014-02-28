#' teste
#'
#' @export
ggbarplot <- function(data, var.categ.principal, var.categ.secundarias=NULL, tit.categ.principal = var.categ.principal, tit.categ.secundarias = var.categ.secundarias, pos_legenda=NULL){
  # ggbarplot- GRÁFICO DE BARRAS PARA UMA VAR CATEG PRINCIPAL VS CONJUNTO DE VAR CATEGS EXPLICATIVAS
  # Função que desenha um grÁfico de barras para uma var.categ.principal para combinações de níveis das var.categ.secundarias.
  # Parâmetros:
  # data -
  # var.categ.principal -
  # var.categ.secundarias -
  # tit.categ.principal - 
  # tit.categ.secundarias -
  # pos_legenda - EM TESTE! GAMBI! Recebe um objeto theme com especificações para a legenda.
  # OBS: só aceita uma var secundária por enquanto.
  library(grid)
  library(ggplot2)
  library(reshape2)
  
  nVarsSec <- length(var.categ.secundarias)
  
  
  var.categ.principal <- as.character(var.categ.principal)
  var.categ.secundarias <- as.character(var.categ.secundarias)
  
  df <- table(data[,c(var.categ.principal,var.categ.secundarias)])
  df <- as.data.frame(df)
  
  names(df) <- c(paste("Var", 1:(nVarsSec+1), sep=""), "Freq")
  tab <- df
  
  if(nVarsSec > 0){
    varNum <- paste("Var", 2:(nVarsSec+1), sep="")
    df$Tot <- rep(tapply(df$Freq, df[,varNum], sum),each=length(levels(df$Var1)))
    
    linha_tab <- paste(varNum, collapse="+")
    
    
  } else {
    varNum <- NULL
    df$Tot <- rep(sum(df$Freq),each=length(levels(df$Var1)))
    
    linha_tab <- "1"
  }
  
  df$Prop <- with(df, Freq/Tot)
  df$Lab <- with(df, paste(round(100*Prop,0),"%",sep=""))
  df <- df[!is.nan(df$Prop),]
  
  df <- data.frame(df, 
                   x = as.factor(rep(ifelse(df[df$Var1==levels(df$Var1)[2],"Prop"]>0.75,levels(df$Var1)[1],levels(df$Var1)[2]), each=length(levels(df$Var1)))),
                   y = 1,
                   label = paste("n =",df$Tot))
  
  coluna_tab <- "Var1"
  formula_tab <- as.formula(paste(linha_tab,"~",coluna_tab))
  
  p <- ggplot(data=df, aes(x = Var1, fill = Var1)) +
    geom_bar(position = "dodge", aes(weights = Prop)) +
    geom_text(aes(y=Prop+.06, label=Lab), size = 5) +
    labs(title=paste(tit.categ.secundarias, collapse=", "), fill=tit.categ.principal, x="", y = "Proporção") +
    geom_text(aes(x=x,y=y,label=label)) +
    scale_y_continuous(labels=function(x) {paste(format(x*100, nsmall=0),"%",sep="")}, limits=c(0,1)) +
    pos_legenda +
    theme_bw() +
    theme(axis.title.x=element_text(size=18),
          axis.title.y=element_text(size=18),
          axis.text.x=element_blank(),
          axis.text.y=element_text(size=16),
          axis.ticks=element_blank(),
          legend.title=element_text(size=18),
          legend.text=element_text(size=16),
          legend.key.size = unit(1.5, "cm"),
          strip.text.x = element_text(size=16),
          strip.text.y = element_text(size=16, angle=0))
  
  
  if(nVarsSec > 0){
    linha_facet <- ifelse(nVarsSec < 2, ".", paste(varNum[1:(nVarsSec/3)],collapse="+"))
    coluna_facet <- paste(varNum[!varNum%in%linha_facet], collapse="+")
    formula_facet_grid <- as.formula(paste(linha_facet,"~",coluna_facet))
    
    p = p +
      facet_grid(formula_facet_grid , scales = "free_x")
  }
  
#   # tabela de contingencia
#   tab <- dcast(tab, formula_tab, fun.aggregate=function(x){sum(x)}, margins=c("grand_row"), value.var="Freq")
#   
#   if(nVarsSec > 0){
#     names(tab)[1:length(var.categ.secundarias)] <- var.categ.secundarias
#     tab <- cbind(tab, "Total" = apply(tab[ , (nVarsSec + 1):length(tab)],1,sum))  
#   } else {
#     tab <- tab[,-1]
#   }
#   print(xtable(tab), type="html", html.table.attributes='border="1" class="excel ggbarplot" width="50px"', include.rownames=FALSE)
  
  return(p)
}
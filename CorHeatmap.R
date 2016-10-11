library(ggplot2)
library(reshape2)

CorHeatmap <- function(corr.mat, digits = 2, add.text = TRUE) {
  
  corr.mat <- round(corr.mat, digits)
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

  lower_tri <- get_lower_tri(corr.mat)
  lower_tri.melt <- melt(lower_tri, na.rm = TRUE)
  lower_tri.melt$Var1 <- factor(lower_tri.melt$Var1)
  lower_tri.melt$Var2 <- factor(lower_tri.melt$Var2, levels = rev(levels(factor(lower_tri.melt$Var2))))

  g <- ggplot(data = lower_tri.melt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+
    coord_fixed() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.4, 0.2),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
    ifelse(add.text,
           return(g + geom_text(aes(label = value), color = "black", size = 4)),
           return(g))
}

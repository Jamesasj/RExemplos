library(tidyr)
library(corrplot)
setwd("C:/PROJETOS/RExemplos")
notas <- read.csv("notas.csv", sep=";")
matrix <- spread(notas, avaliado, valor)
matrix[is.na(matrix)] <- 0

metodos <- c("pearson", "kendall", "spearman")

for (metodo in metodos) {
  jpeg(paste(metodo,".jpg"))
  correlacionado <- cor(matrix, method = metodo)
  corrplot.mixed(correlacionado, lower.col = "black", number.cex = .6)
  corrplot(correlacionado)
  dev.off()
}
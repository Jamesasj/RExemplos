library(tidyr)
library(dplyr)
library(corrplot)
library(skmeans)
setwd("C:/PROJETOS/Avaliacao_pares/")
file_alunos <- "bases/logica-pnota-TT1-636_consolidado.csv"
file_professor <- "bases/logica-pnota-TT1-636_notastreinoShort.csv"
file_saida <- "pictures/"

notasA <- read.csv(file_alunos, sep=";")
notasP<- read.csv(file_professor, sep=";")

notasA$nota <- as.numeric(notasA$nota)
notasP$nota <- as.numeric(notasP$nota)

notasA <- notasA %>% filter(avaliado %in% notasP$avaliado) 
notasP <- notasP %>% filter(avaliado %in% notasA$avaliado) 

matrix <- spread(notasA, avaliador, nota)
matrix$avaliado <- NULL

prof <- notasP$nota
matrix2 <- cbind(prof, matrix)
matrix[is.na(matrix)] <- 0
matrix2[is.na(matrix2)] <- 0
tMatrix <- t(matrix)
#tmatrix2 <- t(matrix2)
corr <- cor(matrix2)
corr[is.na(corr)] <- 0
corAlunoProf <- corr[2:ncol(matrix)]
corr2 <- corr[order(-corr[,1]), order(-corr[,1])]
testes <- cbind(corAlunoProf,tMatrix)
tMatrix2 <- t(testes)
cluster <- skmeans(tMatrix2[,1], 4, method = "genetic")

#testes <- testes[order(-testes[,1]),]

par(mfcol=c(1,2), mar=c(3,3,3,3))
corrplot(corr2, method = "color")
plot(testes[,1], col=cluster$cluster, pch = 19, cex = 1.5)
text(testes[,1], row.names(testes), cex = .5, pos = 4)
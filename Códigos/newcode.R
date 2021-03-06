"mostra o diret�rio atual ========================================================================="
getwd()

"configura o diret�rio a ser trabalhado ==========================================================="
setwd("E:\\Users\\razor\\Sync\\Google Drive - pessoal\\Projeto R")

"mostra o diret�rio atual novamente, confirmando a mudan�a ========================================"
getwd()

"l� arquivo contendo o dataset, jogando os dados em uma vari�vel FILMES ==========================="
filmes = read.csv("filmes.csv",header=TRUE,sep=";",dec=".")

"mostra os nomes das colunas ======================================================================"
names(filmes)

"mostra quantidade de linhas e colunas da tabela =================================================="
dim(filmes)

"mostra as 10 primeiras linhas da tabela =========================================================="
head(filmes, n=10)

"mostra quais os tipos de dados de cada coluna ===================================================="
str(filmes)

"gera a listagem de paises ========================================================================"
paises <- unique(filmes$Pais_Producao)

"gera a listagem de generos ======================================================================="
generos <- unique(filmes$Genero)

lucro <- filmes$Receita - filmes$Orcamento
filmes <- cbind(filmes, lucro)

"quantidade de filmes lan�ados - genero x pais"
qtde_filme_pais <- table(filmes$Genero,filmes$Pais_Producao)
#as.data.frame.matrix(qtde_filme_pais)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 12))
barplot(qtde_filme_pais, xlab="Pa�ses", col=rainbow(13), legend = rownames(qtde_filme_pais),args.legend = list(x ='topright', bty='y', inset=c(-0.3,0)))


"media de orcamento, receita, lucro, or�amento m�ximo e m�nimo, receita m�xima e m�nima ==========="
teste <- c(mean(filmes$Orcamento), mean(filmes$Receita), (mean(filmes$Receita) - mean(filmes$Orcamento)))
bb <- barplot(teste, col=c("red","blue","green"), ylab="Valores em D�lar", names.arg = c("Or�amento","Receita","Lucro"))


"Filme com maior or�amento ========================================================================"
temp1 <- filmes[which.max(filmes$Orcamento),2]
temp2 <- filmes[which.max(filmes$Orcamento),3]
temp3 <- filmes[which.max(filmes$Orcamento),7]
info1 <- c(as.character(temp1), as.character(temp2), temp3)
info1

"Filme com menor or�amento ========================================================================"
temp1 <- filmes[which.min(filmes$Orcamento),2]
temp2 <- filmes[which.min(filmes$Orcamento),3]
temp3 <- filmes[which.min(filmes$Orcamento),7]
info2 <- c(as.character(temp1), as.character(temp2), temp3)
info2

"Filme com maior receita =========================================================================="
temp1 <- filmes[which.max(filmes$Receita),2]
temp2 <- filmes[which.max(filmes$Receita),3]
temp3 <- filmes[which.max(filmes$Orcamento),8]
info3 <- c(as.character(temp1), as.character(temp2), temp3)
info3

"Filme com menor receita =========================================================================="
temp1 <- filmes[which.min(filmes$Receita),2]
temp2 <- filmes[which.min(filmes$Receita),3]
temp3 <- filmes[which.min(filmes$Orcamento),8]
info4 <- c(as.character(temp1), as.character(temp2), temp3)
info4

"Filme com maior lucro ============================================================================"
temp1 <- filmes[which.max(filmes$lucro),2]
temp2 <- filmes[which.max(filmes$lucro),3]
temp3 <- filmes[which.max(filmes$lucro),13]
info5 <- c(as.character(temp1), as.character(temp2), temp3)
info5

"Filme com menor lucro ============================================================================"
temp1 <- filmes[which.min(filmes$lucro),2]
temp2 <- filmes[which.min(filmes$lucro),3]
temp3 <- filmes[which.min(filmes$lucro),13]
info6 <- c(as.character(temp1), as.character(temp2), temp3)
info6


"Propor��o de Filmes Lan�ados por G�nero =========================================================="
slices <- as.data.frame(table(filmes$Genero))
slices$percent <- slices$Freq
slices$texto <- paste(slices$Var1," (", slices$percent,"%)", sep = "")
pie(slices$percent,labels = slices$texto, col = hcl.colors(13, "Set 2"))

"G�nero de filme mais lan�ado ====================================================================="
freq <- table(filmes$Genero)
names(freq)[freq == max(freq)]

"G�nero de filme menos lan�ado ====================================================================="
freq <- table(filmes$Genero)
names(freq)[freq == min(freq)]

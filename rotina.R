remove(list=ls())
setwd("D:/BKP/Backup Pendrive/UFMG/Disciplinas/Estatistica experimental/Videos/_Analise multivariada/CP")

##################################################
############################
#Exemplo 1
D=read.table("DadosReg.txt",head=T)
rownames(D)=D$Doses
D=D[,-1]

DP=apply(D,2,function(x) (x-mean(x))/sd(x))
DP=DP[,-4]

cp=princomp(DP)
plot(cp)
VAR=cp$sdev^2
100*VAR/sum(VAR)

cp$scores

cor(DP,cp$scores)




##################################################
############################
#Exemplo 2
D2=read.table("dadosTese.txt",head=TRUE)
rownames(D2)=D2$Genotipos
D2=D2[,-1]

DP2=apply(D2,2,function(x) (x-mean(x))/sd(x))
cp2=princomp(DP2)
var=cp2$sdev^2
100*var/sum(var)
cumsum(100*var/sum(var))
biplot(cp2)

cor(DP2,cp2$scores)


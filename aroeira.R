dados<-read.csv2("aroeira.csv")

summary(dados)

names(dados)

#install.packages("dplyr")
library(dplyr)

#mostra uma amostra aleatoria dos seus dados
dplyr::sample_n(dados, 10)

#checa a sua estrutura de dados
str(dados)

#constroi o modelo de %g pra ver se a distribuicao dos residuos eh normal
modelo<-lm(germ~prog+rep+prog:rep, data=dados)
summary(modelo)

shapiro.test(modelo$residuals)
#quando p-value > 0.05, seus dados (ou seus residuos) tem distribuicao normal, 
#entao voce pode usar analise de variancia
#no seu caso, não tem distribuição normal, porque o valor foi menor que 0,05

#instala o pacote que faz o teste de kruskal-wallis, que é um teste não paramétrico, 
  #com efeito semelhante ao teste de análise de variância e o tukey

#install.packages("agricolae")
library(agricolae)

#esse teste de kw tem no pacote base, mas não mostra as letrinhas bonitinhas, só dá que 
  #existe diferença entre os tratamentos ou não 
  #no seu caso, tem diferença sim
kruskal.test(germ~prog, data=dados)
kruskal.test(germ~rep, data=dados)

#aqui já é o teste do pacote agricolae, que mostra com as letrinhas.
  #como você tem 45 progênies, vai quase o alfabeto inteiro...
kprog<-kruskal(dados$germ, dados$prog)
kprog

krep<-kruskal(dados$germ, dados$rep)
krep

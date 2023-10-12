#AUTOR: Deoclecio J. Amorim
#E-MAIL: deocleciojardim@hotmail.com
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
#options(scipen = 999)# Este valor alto (999) evita o retorno dos números em 
#notação científica.
rm(list=ls(all=T))#Limpando a memória


#Dados
dados <- read.csv("dados/dados_bb.csv",sep=",", header = 1)

#Alterando os nomes do conjunto de dados
colnames(dados)<-c("Id","Curso", "Ano ou Módulo","Q1", "Q2","Q3","Q4","Q5",
                   "Q6","Q7","Q8")

# Substituir strings por 0 ou 1
#Agro=1
#adm=0
dados[dados$Curso=="Técnico Integrado em Agropecuária", "Curso"] <- "Agro"
dados[dados$Curso=="Técnico Subsequente em Agropecuária", "Curso"] <- "AgroSub"
dados[dados$Curso=="Técnico Integrado em Administração", "Curso"] <- "Adm"
# dados[dados$Q1=="Sim", "Q1"] <- "Q1sim"
# dados[dados$Q1=="Não", "Q1"] <- "Q1nao"
# dados[dados$Q2=="Sim", "Q2"] <- "Q2sim"
# dados[dados$Q2=="Não", "Q2"] <- "Q2nao"
# dados[dados$Q3=="Sim", "Q3"] <- "Q3sim"
# dados[dados$Q3=="Não", "Q3"] <- "Q3nao"
# dados[dados$Q4=="Sim", "Q4"] <- "Q4sim"
# dados[dados$Q4=="Não", "Q4"] <- "Q4nao"
dados[dados$Q5=="A Estatística possui aplicação limitada", "Q5"] <- "Limitada"
dados[dados$Q5=="A Estatística está associada a diversas disciplinas e é frequentemente usada nas diversas áreas", "Q5"] <- "Não limitada"
# dados[dados$Q6=="Sim", "Q6"] <- "Q6sim"
# dados[dados$Q6=="Não", "Q6"] <- "Q6nao"
# dados[dados$Q7=="Sim", "Q7"] <- "Q7sim"
# dados[dados$Q7=="Não", "Q7"] <- "Q7nao"
# dados[dados$Q8=="Sim", "Q8"] <- "Q8sim"
# dados[dados$Q8=="Não", "Q8"] <- "Q8nao"




#subset
dados<-dados[,2:11]
dados<-dados[,-2]
dados<-dados[-66,]

library(tidyverse)
library(igraph)
dados_empilhados <- dados %>%
  gather(key = "Chave", value = "Resposta", Q1:Q8)

# Escolha a linha que você deseja eliminar (por exemplo, a segunda linha)

dados_empilhados <-dados_empilhados[,-2]



net_edgelist <- graph.data.frame(dados_empilhados, directed=T)
plot(net_edgelist)


plot(net_edgelist, 
     edge.arrow.size=0.1,
     edge.width=0.3,
     vertex.size=30,
     vertex.color="grey50",
     edge.color="blue",
     vertex.label.color="black",
     vertex.frame.color="darkgreen",
     layout=layout.circle)



cnet <- cluster_edge_betweenness(net_edgelist)
plot(cnet,
     net_edgelist,
     vertex.size = 10,
     vertex.label.cex = 0.8)




hs <- hub_score(net_edgelist)$vector
as <- authority.score(net_edgelist)$vector
set.seed(123)
plot(net_edgelist,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)






resultadosRede <- data.frame(densidade <- edge_density(net_edgelist),
                             diametro <- diameter(net_edgelist, directed = FALSE, weights = NA),
                             distancia_media <- mean_distance(net_edgelist, directed = FALSE),
                             transitividade <- transitivity(net_edgelist))

# Adicionando os nomes das colunas
colnames(resultadosRede) <- c("Densidade", "Diâmetro", "Distância Média", "Transitividade")

resultadosRede









str(dados)
library(MASS)
fit.LDA = lda(Curso~Q1, data = dados)
fit.LDA

dista=dist(dados, method="euclidean")

as.matrix(dista)



mod1<-glm(Curso~., family = binomial(), data = dados)%>% stepAIC(trace =F,direction = "backward")
summary(mod1)
hnp(mod1,print.on = T)



#Visualizamos os valores preditos na escala de probabilidades
library(visreg)
visreg(mod1, "Q1", gg=T, overlay=T, xlab="Q1", ylab="Q1", scale="response") +
  ggtitle("Valores Preditos na Escala de Probabilidades") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold"))



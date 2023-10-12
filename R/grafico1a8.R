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

# Substituir strings
dados[dados$Q5=="A Estatística possui aplicação limitada", "Q5"] <- "Limitada"
dados[dados$Q5=="A Estatística está associada a diversas disciplinas e é frequentemente usada nas diversas áreas", "Q5"] <- "Não limitada"



#Pacotes utilizados
library(tidyverse)
library(ggrepel)
library(shadowtext)
library(grid)



q1 <- dados%>%
  group_by(Q1)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q1


q2 <- dados%>%
  group_by(Q2)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q2


q3 <- dados%>%
  group_by(Q3)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q3

q4 <- dados%>%
  group_by(Q4)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q4


q5 <- dados%>%
  group_by(Q5)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q5


q6 <- dados%>%
  group_by(Q6)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q6



q7 <- dados%>%
  group_by(Q7)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/72,2))
q7


q8 <- dados%>%
  group_by(Q8)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/71,2))
q8

q8<-q8[2:3,]
q8

##---Comando para deixar todos os títulos dos gráficos ggplot centralizados
personal_title = theme(plot.title =
                         element_text(face="bold"))

#Visão geral da estatística

plt1 <- ggplot(q1, aes(x = Q1, y = Porcentagem, fill = Q1)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 1") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(a)")+personal_title

plt1



plt2 <- ggplot(q2, aes(x = Q2, y = Porcentagem, fill = Q2)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 2") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(b)")+personal_title

plt2


plt3 <- ggplot(q3, aes(x = Q3, y = Porcentagem, fill = Q3)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.7), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 3") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(c)")+personal_title

plt3



plt4 <- ggplot(q4, aes(x = Q4, y = Porcentagem, fill = Q4)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.7), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 4") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(d)")+personal_title

plt4



graf2<-cowplot::plot_grid(plt1,plt2, plt3,plt4,ncol=2)
graf2

ggsave("graf2.tiff",
       scale = 1, width = 16, height = 11, dpi = 300, 
       units =  "cm")







plt5 <- ggplot(q5, aes(x = Q5, y = Porcentagem, fill = Q5)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 2.9) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 5") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(a)")+personal_title

plt5


plt6 <- ggplot(q6, aes(x = Q6, y = Porcentagem, fill = Q6)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 6") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(b)")+personal_title

plt6


plt7 <- ggplot(q7, aes(x = Q7, y = Porcentagem, fill = Q7)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 7") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(c)")+personal_title

plt7


plt8 <- ggplot(q8, aes(x = Q8, y = Porcentagem, fill = Q8)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#A9A9A9", "#A9A9A9")) +
  ylab("") +
  xlab("Questão 8") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(d)")+personal_title

plt8


graf3<-cowplot::plot_grid(plt5,plt6, plt7,plt8,ncol=2)
graf3

ggsave("graf3.tiff",
       scale = 1, width = 17, height = 11, dpi = 300, 
       units =  "cm")




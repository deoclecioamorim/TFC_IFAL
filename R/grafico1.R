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

#Modificando strings
#dados[dados$Curso=="Técnico Integrado em Agropecuária", "Curso"] <- "Agro"
#dados[dados$Curso=="Técnico Integrado em Administração", "Curso"] <- "Adm"
#Pacotes utilizados
library(tidyverse)
library(ggrepel)
library(shadowtext)
library(grid)



curso <- dados%>%
  group_by(Curso)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/77,2))
curso


curso2 <- curso %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

curso2

##---Comando para deixar todos os títulos dos gráficos ggplot centralizados
personal_title = theme(plot.title =
                         element_text(face="bold"))

plt1 <- ggplot(curso, aes(x = Curso, y = Porcentagem, fill = Curso)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#808080", "#A9A9A9", "#C0C0C0")) +
  ylab("") +
  xlab("Curso") +
  theme_bw()+theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank())+
  theme(legend.position="top")+ggtitle("(a)")+personal_title

plt1





#Gráfico de barras referente ao ano ou módulo

ano <- dados%>%
  group_by(`Ano ou Módulo`)%>%
  summarise(n=n(),
            Porcentagem=round((100*n())/77,2))
ano



# Define a ordem desejada dos níveis na variável 'Ano ou Módulo'
ano <- ano %>%
  mutate(`Ano ou Módulo` = factor(`Ano ou Módulo`, 
                                  levels = c("1º ano", "2º ano",
                                            "3º ano", "Módulo I",
                                            "Módulo II", "Módulo III")))

# Cria o gráfico de barras com os nomes no eixo Y na ordem especificada
plt2 <- ggplot(ano, aes(x = `Ano ou Módulo`, y = Porcentagem)) +
  geom_col(fill = "grey50", width = 0.6) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, by = 5), 
    expand = c(0, 0)
  )+geom_text(aes(label = paste0(Porcentagem, "%")), position = position_stack(vjust = 1.01), size = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black",size = 8),
                   axis.text.y = element_text(color = "black"))+
ylab("Porcentagem (%)")+ xlab("Perfil dos alunos")+
  theme(legend.position="top")+ggtitle("(b)")+personal_title

plt2





graf1<-cowplot::plot_grid(plt1,plt2, ncol=1)
graf1


# graf1 <- graf1 + 
#   labs(title = NULL, subtitle = NULL) +
#   theme(
#     plot.margin = margin(0.1, 0, 0.1, 0.01, "npc")
#   )
# 
# graf1
# 
# grid.text(
#   "A", 
#   0, 
#   0.92,
#   just = c("left", "bottom"),
#   gp = gpar(
#     fontsize = 20,
#     fontface = "bold",
#     fontfamily = "Econ Sans Cnd"
#   )
# )
# 
# 
# grid.text(
#   "B", 
#   0, 
#   0.5,
#   just = c("left", "bottom"),
#   gp = gpar(
#     fontsize = 20,
#     fontface = "bold",
#     fontfamily = "Econ Sans Cnd"
#   )
# )

ggsave("graf1.tiff",
       scale = 1, width = 16, height = 11, dpi = 300, 
       units =  "cm")

# grid.text(
#   "Representatividade de cada curso do IFAL", 
#   0, 
#   0.89,
#   just = c("left", "bottom"),
#   gp = gpar(
#     fontsize = 12,
#     fontfamily = "Econ Sans Cnd"
#   )
# )
# 
# 
# grid.text(
#   "Porcentagem de alunos que responderam por cada ano ou módulo", 
#   0, 
#   0.45,
#   just = c("left", "bottom"),
#   gp = gpar(
#     fontsize = 12,
#     fontfamily = "Econ Sans Cnd"
#   )
# )
# 
# 
# grid.lines(
#   x = c(0, 1),
#   y = 1,
#   gp = gpar(col = GREY, lwd = 4)
# )
# 
# grid.rect(
#   x = 0,
#   y = 1,
#   width = 0.05,
#   height = 0.025,
#   just = c("left", "top"),
#   gp = gpar(fill = GREY, lwd = 0)
# )
# 
# grid.text(
#   "Fonte: Resultados originais da pesquisa", 
#   x = 0.005, 
#   y = 0.06, 
#   just = c("left", "bottom"),
#   gp = gpar(
#     col = GREY,
#     fontsize = 10,
#     fontfamily = "Econ Sans Cnd"
#   )
# )
# 









##Grafico de pizza

##---Comando para deixar todos os títulos dos gráficos ggplot centralizados
personal_title = theme(plot.title =
                         element_text(face="bold",hjust = 0.5))

cursograf <- ggplot(curso, aes(x = "" , y = n, fill = fct_inorder(Curso))) +
  geom_col(width = 1, color = 1) +theme_void()+
  coord_polar(theta = "y", start = 0) +
  geom_label_repel(data = curso2,
                   aes(y = pos, label = paste0(Porcentagem, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())+
  scale_fill_brewer(palette = "Pastel1", name = "Curso")+
  theme(legend.text=element_text(size=10),
        legend.title = element_text(hjust = 0.5, size=12),
        legend.key.size = unit(0.5,"cm"))


cursograf 



# ... Seu código anterior ...

# Reduza a margem entre o gráfico e a legenda
cursograf  <- cursograf +
  theme(legend.margin = margin(0, 0, 0, 0),  # Margem interna da legenda (top, right, bottom, left)
        plot.margin = margin(0, 0, 0, 0))    # Margem externa do gráfico (top, right, bottom, left)

# ... Seu código posterior ...




plt1 <- cursograf + 
  labs(title = NULL, subtitle = NULL) +
  theme(
    plot.margin = margin(0.15, 0, 0.1, 0.01, "npc")
  )

plt1 

grid.text(
  "A", 
  0, 
  0.925,
  just = c("left", "bottom"),
  gp = gpar(
    fontsize = 22,
    fontface = "bold",
    fontfamily = "Econ Sans Cnd"
  )
)

grid.text(
  "Número de alunos que responderam de acordo com o curso", 
  0, 
  0.875,
  just = c("left", "bottom"),
  gp = gpar(
    fontsize = 16,
    fontfamily = "Econ Sans Cnd"
  )
)

grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = GREY, lwd = 4)
)

grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = GREY, lwd = 0)
)

grid.text(
  "Fonte: Resultados originais da pesquisa", 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = GREY,
    fontsize = 10,
    fontfamily = "Econ Sans Cnd"
  )
)







Theme <- function(){theme(axis.text.x = element_text(angle = 85, 
                                                      vjust = .5,
                                                      face = "bold",
                                                      size = 12),
                           panel.grid.major = element_line(color = "#C0C0C0"),
                           panel.grid.minor = element_blank(),
                           panel.background = element_rect(fill = "#A52A2A"),
                           plot.title = element_text(face = "bold",
                                                     size = 19),
                          plot.caption = element_text(size = 12,
                                                      hjust = 0)
                          ) 
}

AUX_SEM <- as.character(c("2026/1",  "2026/2", "2026/3", 
                      "2026/4",  "2026/5",  "2026/6",  
                      "2026/7",  "2026/8",  "2026/9",  
                      "2026/10",  "2026/11",  "2026/12",  
                      "2026/13",  "2026/14",  "2026/15",  
                      "2026/16",  "2026/17",  "2026/18",  
                      "2026/19",  "2026/20",  "2026/21",  
                      "2026/22",  "2026/23",  
                      "2026/24",  "2026/25",  "2026/26",  
                      "2026/27",  "2026/28",  "2026/29",  
                      "2026/30",  "2026/31",  "2026/32", 
                      "2026/33",  "2026/34",  "2026/35",  
                      "2026/36",  "2026/37",  "2026/38",  
                      "2026/39",  "2026/40",  "2026/41",  
                      "2026/42",  "2026/43",  "2026/44",  
                      "2026/45",  "2026/46",  "2026/47",  
                      "2026/48",  "2026/49",  "2026/50",  
                      "2026/51",  "2026/52")
)

####################################################################################################################
################Trabalhando as tabelas base dos Canais Endêmicos####################################################
####################################################################################################################

#######     Eliminando a SE 53 Inexistente no período no ano 2026    #####

RS_CE_Notificados_Base <- RS_CE_Notificados_Base[, -54]
RS_2026_SE_Notificados <- RS_2026_SE_Notificados[, -54]

######     Canal Endêmico    NOTIFICADOS#####

RS_CE_Notificados_Base[(nrow(RS_CE_Notificados_Base) +1), 1] <- "2026"
RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base), 2:53] <- as.integer(data.frame(RS_2026_SE_Notificados[nrow(RS_2026_SE_Notificados), 2:53]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -7,]
AUX[2, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -6,]
AUX[3, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -5,]
AUX[4, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -4,]
AUX[5, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -3,]
AUX[6, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -2,]
AUX[7, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -1,]
AUX[8, ] <- RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -7, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -6, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -5, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -4, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -3, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base) -2, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base)-1, 1],
         RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base), 1])

colnames(AUX) <- AUX2

RS_CE_Notificados <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Notificados[, 1:(ncol(RS_CE_Notificados)-1)], 1 , Quartil_1)

RS_CE_Notificados <- as.data.frame(RS_CE_Notificados)

RS_CE_Notificados$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Notificados[, 1: (ncol(RS_CE_Notificados)-2)], 1 , median)

RS_CE_Notificados$Mediana <- AUX

######     Criando a coluna de Quartil_3 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Notificados[, 1:(ncol(RS_CE_Notificados)-3)], 1 , Quartil_3)

RS_CE_Notificados$Quartil_3 <- AUX

RS_CE_Notificados[, (ncol(RS_CE_Notificados)+1)] <- rownames(RS_CE_Notificados)

RS_CE_Notificados <- RS_CE_Notificados[, c(ncol(RS_CE_Notificados), 1:(ncol(RS_CE_Notificados) -1))]

RS_CE_Notificados[, 1] <- c(1:52)

colnames(RS_CE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados) <- c(1:nrow(RS_CE_Notificados))

# write.csv (RS_CE_Notificados, 
#           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados.csv"), 
#           row.names = FALSE)

AUX_GRAF <- RS_CE_Notificados

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados))

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Notificados <- ggplot(AUX_GRAF, aes(Ordem))  +
    labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos")  +
  Theme() +
  scale_x_continuous(breaks = c(1:52),
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
 
####         Canal Endêmico CONFIRMADOS              ####

#######     Eliminando a SE 53 Inexistente no período no ano 2026    #####

RS_CE_Confirmados_Base <- RS_CE_Confirmados_Base[, -54]
RS_2026_SE_Confirmados <- RS_2026_SE_Confirmados[, -54]

######     Canal Endêmico    CONFIRMADOS#####

RS_CE_Confirmados_Base[(nrow(RS_CE_Confirmados_Base) +1), 1] <- "2026"
RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base), 2:53] <- as.integer(data.frame(RS_2026_SE_Confirmados[nrow(RS_2026_SE_Confirmados), 2:53]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -7,]
AUX[2, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -6,]
AUX[3, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -5,]
AUX[4, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -4,]
AUX[5, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -3,]
AUX[6, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -2,]
AUX[7, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -1,]
AUX[8, ] <- RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -7, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -6, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -5, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -4, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -3, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base) -2, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base)-1, 1],
         RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base), 1])

colnames(AUX) <- AUX2

RS_CE_Confirmados <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Confirmados[, 1:(ncol(RS_CE_Confirmados)-1)], 1 , Quartil_1)

RS_CE_Confirmados <- as.data.frame(RS_CE_Confirmados)

RS_CE_Confirmados$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Confirmados[, 1: (ncol(RS_CE_Confirmados)-2)], 1 , median)

RS_CE_Confirmados$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Confirmados[, 1:(ncol(RS_CE_Confirmados)-3)], 1 , Quartil_3)

RS_CE_Confirmados$Quartil_3 <- AUX

RS_CE_Confirmados[, (ncol(RS_CE_Confirmados)+1)] <- rownames(RS_CE_Confirmados)

RS_CE_Confirmados <- RS_CE_Confirmados[, c(ncol(RS_CE_Confirmados), 1:(ncol(RS_CE_Confirmados) -1))]

RS_CE_Confirmados <- RS_CE_Confirmados[-53, ]

RS_CE_Confirmados[, 1] <- c(1:52)

colnames(RS_CE_Confirmados)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados) <- c(1:nrow(RS_CE_Confirmados))

rm(AUX, AUX2, RS_CE_Confirmados_Base)

# write.csv (RS_CE_Confirmados, 
#           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados.csv"), 
#           row.names = FALSE)

AUX_GRAF <- RS_CE_Confirmados

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Confirmados <- ggplot(AUX_GRAF, aes(Ordem))  +
   labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRMADOS - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####################################################################################################
###      Construindo um for loop para realizar a tabela de Prováveis por semana epidemiológica    ###
###          Será utilizado para os histogramas  e canais endêmicos de casos prováveis.           ###
#####################################################################################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 202601

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
    AUX[which(AUX == i), O] <- as.integer(SINAN_DENGUE_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == N)%>%
                                            count()
                                          -
                                            SINAN_DENGUE_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == N,
                                                   CLASSI_FIN == 5) %>%
                                            count()
    )
  }
  
  N <- N +1
  O <- O +1
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

assign(paste0("RS", "_2026_SE_Provaveis"), AUX)

assign(paste0("RS", RS, "_2026_SE_Provaveis"), AUX)

###     CANAL ENDÊMICO Prováveis         ####

####         Puxando os dados da tabela RS22_CE_Notificados

AUX_GRAF <- RS_CE_Confirmados[,]

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados))

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_SE_Provaveis <- RS_2026_SE_Provaveis[, -54]

AUX_GRAF[, 9] <- t(RS_2026_SE_Provaveis[nrow(RS_2026_SE_Provaveis), -1 ])

colnames(AUX_GRAF)[9] <- "2026"

RS_2026_GRAF_CE_Provaveis <- ggplot(AUX_GRAF, 
                                    aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - IVAIPORÃ      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_SEDE_Base[(nrow(RS_CE_Notificados_SEDE_Base) +1), 1] <- "2026"
RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_2026_SE_Notificados[6, 2:54]))

### Eliminando a SE 53

RS_CE_Notificados_SEDE_Base <- RS_CE_Notificados_SEDE_Base[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -7,]
AUX[2, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -6,]
AUX[3, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -5,]
AUX[4, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -4,]
AUX[5, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -3,]
AUX[6, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -2,]
AUX[7, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -1,]
AUX[8, ] <- RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -7, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -6, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -5, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -4, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -3, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base) -2, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base)-1, 1],
         RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base), 1])

colnames(AUX) <- AUX2

RS_CE_Notificados_SEDE <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Notificados_SEDE[, 1:(ncol(RS_CE_Notificados_SEDE)-1)], 1 , Quartil_1)

RS_CE_Notificados_SEDE <- as.data.frame(RS_CE_Notificados_SEDE)

RS_CE_Notificados_SEDE$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Notificados_SEDE[, 1: (ncol(RS_CE_Notificados_SEDE)-2)], 1 , median)

RS_CE_Notificados_SEDE$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Notificados_SEDE[, 1:(ncol(RS_CE_Notificados_SEDE)-3)], 1 , Quartil_3)

RS_CE_Notificados_SEDE$Quartil_3 <- AUX

RS_CE_Notificados_SEDE[, (ncol(RS_CE_Notificados_SEDE)+1)] <- rownames(RS_CE_Notificados_SEDE)

RS_CE_Notificados_SEDE <- RS_CE_Notificados_SEDE[, c(ncol(RS_CE_Notificados_SEDE), 1:(ncol(RS_CE_Notificados_SEDE) -1))]

RS_CE_Notificados_SEDE[, 1] <- c(1:52)

colnames(RS_CE_Notificados_SEDE)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados_SEDE) <- c(1:nrow(RS_CE_Notificados_SEDE))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO NOTIFICADOS - IVAIPORÃ####

AUX_GRAF <- RS_CE_Notificados_SEDE

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_SEDE))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Notificados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS Ivaiporã- 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####Canal Endêmico CONFIRMADOS####

RS_CE_Confirmados_SEDE_Base[(nrow(RS_CE_Confirmados_SEDE_Base) +1), 1] <- "2026"
RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_2026_SE_Confirmados[6, 2:54]))

### Eliminando a SE 53

RS_CE_Confirmados_SEDE_Base <- RS_CE_Confirmados_SEDE_Base[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -6,]
AUX[2, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -5,]
AUX[3, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -4,]
AUX[4, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -3,]
AUX[5, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -2,]
AUX[6, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -1,]
AUX[7, ] <- RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -6, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -5, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -4, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -3, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -2, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base) -1, 1],
         RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base), 1])

colnames(AUX) <- AUX2

RS_CE_Confirmados_SEDE <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Confirmados_SEDE[, 1:(ncol(RS_CE_Confirmados_SEDE)-1)], 1 , Quartil_1)

RS_CE_Confirmados_SEDE <- as.data.frame(RS_CE_Confirmados_SEDE)

RS_CE_Confirmados_SEDE$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Confirmados_SEDE[, 1: (ncol(RS_CE_Confirmados_SEDE)-2)], 1 , median)

RS_CE_Confirmados_SEDE$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Confirmados_SEDE[, 1:(ncol(RS_CE_Confirmados_SEDE)-3)], 1 , Quartil_3)

RS_CE_Confirmados_SEDE$Quartil_3 <- AUX

RS_CE_Confirmados_SEDE[, (ncol(RS_CE_Confirmados_SEDE)+1)] <- rownames(RS_CE_Confirmados_SEDE)

RS_CE_Confirmados_SEDE <- RS_CE_Confirmados_SEDE[, c(ncol(RS_CE_Confirmados_SEDE), 1:(ncol(RS_CE_Confirmados_SEDE) -1))]

RS_CE_Confirmados_SEDE[, 1] <- c(1:52)

colnames(RS_CE_Confirmados_SEDE)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados_SEDE) <- c(1:nrow(RS_CE_Confirmados_SEDE))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO CONFIRMADOS - IVAIPORÃ####

AUX_GRAF <- RS_CE_Confirmados_SEDE

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_SEDE))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Confirmados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS Ivaiporã- 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################################
####################################################################################################################################
###############Canal Endêmico Prováveis - IVAIPORÃ

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS_2026_Casos_Provaveis_SEDE <- (RS22_2026_SE_Provaveis[6, 2: 54])

rownames(RS_2026_Casos_Provaveis_SEDE)[1] <- "Provaveis"

RS_2026_Casos_Provaveis_SEDE <- t(as.data.frame(RS_2026_Casos_Provaveis_SEDE))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SEDE[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2026_Casos_Provaveis_SEDE[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2026_GRAF_CE_Provaveis_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Ivaiporã - 2026",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = Provaveis), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_2026_Casos_Provaveis_SEDE, AUX_GRAF)

####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - IVAIPORÃ      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_JARDIM_BASE[(nrow(RS_CE_Notificados_JARDIM_BASE) +1), 1] <- "2026"
RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Notificados[7, 2:54]))

### Eliminando a SE 53

RS_CE_Notificados_JARDIM_BASE <- RS_CE_Notificados_JARDIM_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -6,]
AUX[2, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -5,]
AUX[3, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -4,]
AUX[4, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -3,]
AUX[5, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -2,]
AUX[6, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -1,]
AUX[7, ] <- RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -6, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -5, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -4, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -3, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -2, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE) -1, 1],
         RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Notificados_JARDIM <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Notificados_JARDIM[, 1:(ncol(RS_CE_Notificados_JARDIM)-1)], 1 , Quartil_1)

RS_CE_Notificados_JARDIM <- as.data.frame(RS_CE_Notificados_JARDIM)

RS_CE_Notificados_JARDIM$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Notificados_JARDIM[, 1: (ncol(RS_CE_Notificados_JARDIM)-2)], 1 , median)

RS_CE_Notificados_JARDIM$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Notificados_JARDIM[, 1:(ncol(RS_CE_Notificados_JARDIM)-3)], 1 , Quartil_3)

RS_CE_Notificados_JARDIM$Quartil_3 <- AUX

RS_CE_Notificados_JARDIM[, (ncol(RS_CE_Notificados_JARDIM)+1)] <- rownames(RS_CE_Notificados_JARDIM)

RS_CE_Notificados_JARDIM <- RS_CE_Notificados_JARDIM[, c(ncol(RS_CE_Notificados_JARDIM), 1:(ncol(RS_CE_Notificados_JARDIM) -1))]

RS_CE_Notificados_JARDIM[, 1] <- c(1:52)

colnames(RS_CE_Notificados_JARDIM)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados_JARDIM) <- c(1:nrow(RS_CE_Notificados_JARDIM))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO NOTIFICADOS - IVAIPORÃ####

AUX_GRAF <- RS_CE_Notificados_JARDIM

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_JARDIM))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM 

RS_2026_GRAF_CE_Notificados_JARDIM <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS Jardim Alegre - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####Canal Endêmico CONFIRMADOS####

RS_CE_Confirmados_JARDIM_BASE[(nrow(RS_CE_Confirmados_JARDIM_BASE) +1), 1] <- "2026"
RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Confirmados[7, 2:54]))

### Eliminando a SE 53

RS_CE_Confirmados_JARDIM_BASE <- RS_CE_Confirmados_JARDIM_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -6,]
AUX[2, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -5,]
AUX[3, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -4,]
AUX[4, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -3,]
AUX[5, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -2,]
AUX[6, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -1,]
AUX[7, ] <- RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -6, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -5, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -4, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -3, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -2, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE) -1, 1],
         RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Confirmados_JARDIM <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Confirmados_JARDIM[, 1:(ncol(RS_CE_Confirmados_JARDIM)-1)], 1 , Quartil_1)

RS_CE_Confirmados_JARDIM <- as.data.frame(RS_CE_Confirmados_JARDIM)

RS_CE_Confirmados_JARDIM$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Confirmados_JARDIM[, 1: (ncol(RS_CE_Confirmados_JARDIM)-2)], 1 , median)

RS_CE_Confirmados_JARDIM$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Confirmados_JARDIM[, 1:(ncol(RS_CE_Confirmados_JARDIM)-3)], 1 , Quartil_3)

RS_CE_Confirmados_JARDIM$Quartil_3 <- AUX

RS_CE_Confirmados_JARDIM[, (ncol(RS_CE_Confirmados_JARDIM)+1)] <- rownames(RS_CE_Confirmados_JARDIM)

RS_CE_Confirmados_JARDIM <- RS_CE_Confirmados_JARDIM[, c(ncol(RS_CE_Confirmados_JARDIM), 1:(ncol(RS_CE_Confirmados_JARDIM) -1))]

RS_CE_Confirmados_JARDIM[, 1] <- c(1:52)

colnames(RS_CE_Confirmados_JARDIM)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados_JARDIM) <- c(1:nrow(RS_CE_Confirmados_JARDIM))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO CONFIRMADOS - Jardim Alegre ####

AUX_GRAF <- RS_CE_Confirmados_JARDIM

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_JARDIM))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Confirmados_JARDIM <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS Jardim Alegre - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################################
####################################################################################################################################
###############Canal Endêmico Prováveis - Jardim Alegre

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS_2026_Casos_Provaveis_JARDIM <- (RS22_2026_SE_Provaveis[7, 2: 54])

rownames(RS_2026_Casos_Provaveis_JARDIM)[1] <- "Provaveis"

RS_2026_Casos_Provaveis_JARDIM <- t(as.data.frame(RS_2026_Casos_Provaveis_JARDIM))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_JARDIM[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2026_Casos_Provaveis_JARDIM[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2026_GRAF_CE_Provaveis_JARDIM <- ggplot(AUX_GRAF, 
                                           aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Jardim Alegre - 2026",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = Provaveis), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_2026_Casos_Provaveis_JARDIM, AUX_GRAF)

####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - São João      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_SAO_JOAO_BASE[(nrow(RS_CE_Notificados_SAO_JOAO_BASE) +1), 1] <- "2026"
RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Notificados[16, 2:54]))

### Eliminando a SE 53

RS_CE_Notificados_SAO_JOAO_BASE <- RS_CE_Notificados_SAO_JOAO_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -6,]
AUX[2, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -5,]
AUX[3, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -4,]
AUX[4, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -3,]
AUX[5, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -2,]
AUX[6, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -1,]
AUX[7, ] <- RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -6, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -5, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -4, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -3, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -2, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE) -1, 1],
         RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Notificados_SAO_JOAO <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Notificados_SAO_JOAO[, 1:(ncol(RS_CE_Notificados_SAO_JOAO)-1)], 1 , Quartil_1)

RS_CE_Notificados_SAO_JOAO <- as.data.frame(RS_CE_Notificados_SAO_JOAO)

RS_CE_Notificados_SAO_JOAO$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Notificados_SAO_JOAO[, 1: (ncol(RS_CE_Notificados_SAO_JOAO)-2)], 1 , median)

RS_CE_Notificados_SAO_JOAO$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Notificados_SAO_JOAO[, 1:(ncol(RS_CE_Notificados_SAO_JOAO)-3)], 1 , Quartil_3)

RS_CE_Notificados_SAO_JOAO$Quartil_3 <- AUX

RS_CE_Notificados_SAO_JOAO[, (ncol(RS_CE_Notificados_SAO_JOAO)+1)] <- rownames(RS_CE_Notificados_SAO_JOAO)

RS_CE_Notificados_SAO_JOAO <- RS_CE_Notificados_SAO_JOAO[, c(ncol(RS_CE_Notificados_SAO_JOAO), 1:(ncol(RS_CE_Notificados_SAO_JOAO) -1))]

RS_CE_Notificados_SAO_JOAO[, 1] <- c(1:52)

colnames(RS_CE_Notificados_SAO_JOAO)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados_SAO_JOAO) <- c(1:nrow(RS_CE_Notificados_SAO_JOAO))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO NOTIFICADOS - São_João do Ivaí####

AUX_GRAF <- RS_CE_Notificados_SAO_JOAO

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_SAO_JOAO))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS São João do Ivaí - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####Canal Endêmico CONFIRMADOS####

RS_CE_Confirmados_SAO_JOAO_BASE[(nrow(RS_CE_Confirmados_SAO_JOAO_BASE) +1), 1] <- "2026"
RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Confirmados[16, 2:54]))

### Eliminando a SE 53

RS_CE_Confirmados_SAO_JOAO_BASE <- RS_CE_Confirmados_SAO_JOAO_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -6,]
AUX[2, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -5,]
AUX[3, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -4,]
AUX[4, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -3,]
AUX[5, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -2,]
AUX[6, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -1,]
AUX[7, ] <- RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -6, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -5, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -4, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -3, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -2, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE) -1, 1],
         RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Confirmados_SAO_JOAO_DO_IVAI <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, 1:(ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)-1)], 1 , Quartil_1)

RS_CE_Confirmados_SAO_JOAO_DO_IVAI <- as.data.frame(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)

RS_CE_Confirmados_SAO_JOAO_DO_IVAI$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, 1: (ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)-2)], 1 , median)

RS_CE_Confirmados_SAO_JOAO_DO_IVAI$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, 1:(ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)-3)], 1 , Quartil_3)

RS_CE_Confirmados_SAO_JOAO_DO_IVAI$Quartil_3 <- AUX

RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, (ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)+1)] <- rownames(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)

RS_CE_Confirmados_SAO_JOAO_DO_IVAI <- RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, c(ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI), 1:(ncol(RS_CE_Confirmados_SAO_JOAO_DO_IVAI) -1))]

RS_CE_Confirmados_SAO_JOAO_DO_IVAI[, 1] <- c(1:52)

colnames(RS_CE_Confirmados_SAO_JOAO_DO_IVAI)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados_SAO_JOAO_DO_IVAI) <- c(1:nrow(RS_CE_Confirmados_SAO_JOAO_DO_IVAI))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO CONFIRMADOS - Jardim Alegre ####

AUX_GRAF <- RS_CE_Confirmados_SAO_JOAO_DO_IVAI

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_SAO_JOAO_DO_IVAI))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, 
                                                       aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS São João do Ivaí - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################################
####################################################################################################################################
###############Canal Endêmico Prováveis - São João do Ivaí

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI <- (RS22_2026_SE_Provaveis[16, 2: 54])

rownames(RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI)[1] <- "Provaveis"

RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI <- t(as.data.frame(RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SAO_JOAO_DO_IVAI[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2026_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, 
                                                     aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS São João do Ivaí - 2026",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = Provaveis), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_2026_Casos_Provaveis_SAO_JOAO_DO_IVAI, AUX_GRAF)


####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - São João      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_LUNARDELLI_BASE[(nrow(RS_CE_Notificados_LUNARDELLI_BASE) +1), 1] <- "2026"
RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Notificados[9, 2:54]))

### Eliminando a SE 53

RS_CE_Notificados_LUNARDELLI_BASE <- RS_CE_Notificados_LUNARDELLI_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -6,]
AUX[2, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -5,]
AUX[3, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -4,]
AUX[4, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -3,]
AUX[5, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -2,]
AUX[6, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -1,]
AUX[7, ] <- RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -6, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -5, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -4, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -3, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -2, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE) -1, 1],
         RS_CE_Notificados_LUNARDELLI_BASE[nrow(RS_CE_Notificados_LUNARDELLI_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Notificados_LUNARDELLI <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Notificados_LUNARDELLI[, 1:(ncol(RS_CE_Notificados_LUNARDELLI)-1)], 1 , Quartil_1)

RS_CE_Notificados_LUNARDELLI <- as.data.frame(RS_CE_Notificados_LUNARDELLI)

RS_CE_Notificados_LUNARDELLI$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Notificados_LUNARDELLI[, 1: (ncol(RS_CE_Notificados_LUNARDELLI)-2)], 1 , median)

RS_CE_Notificados_LUNARDELLI$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Notificados_LUNARDELLI[, 1:(ncol(RS_CE_Notificados_LUNARDELLI)-3)], 1 , Quartil_3)

RS_CE_Notificados_LUNARDELLI$Quartil_3 <- AUX

RS_CE_Notificados_LUNARDELLI[, (ncol(RS_CE_Notificados_LUNARDELLI)+1)] <- rownames(RS_CE_Notificados_LUNARDELLI)

RS_CE_Notificados_LUNARDELLI <- RS_CE_Notificados_LUNARDELLI[, c(ncol(RS_CE_Notificados_LUNARDELLI), 1:(ncol(RS_CE_Notificados_LUNARDELLI) -1))]

RS_CE_Notificados_LUNARDELLI[, 1] <- c(1:52)

colnames(RS_CE_Notificados_LUNARDELLI)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados_LUNARDELLI) <- c(1:nrow(RS_CE_Notificados_LUNARDELLI))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO NOTIFICADOS - São_João do Ivaí####

AUX_GRAF <- RS_CE_Notificados_LUNARDELLI

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_LUNARDELLI))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Notificados_LUNARDELLI <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS Lunardelli - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####Canal Endêmico CONFIRMADOS####

RS_CE_Confirmados_LUNARDELLI_BASE[(nrow(RS_CE_Confirmados_LUNARDELLI_BASE) +1), 1] <- "2026"
RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE), 2:54] <- as.integer(data.frame(RS22_2026_SE_Confirmados[9, 2:54]))

### Eliminando a SE 53

RS_CE_Confirmados_LUNARDELLI_BASE <- RS_CE_Confirmados_LUNARDELLI_BASE[, -54]

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -6,]
AUX[2, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -5,]
AUX[3, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -4,]
AUX[4, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -3,]
AUX[5, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -2,]
AUX[6, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -1,]
AUX[7, ] <- RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) ,]

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX2 <-c(RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -6, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -5, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -4, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -3, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -2, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE) -1, 1],
         RS_CE_Confirmados_LUNARDELLI_BASE[nrow(RS_CE_Confirmados_LUNARDELLI_BASE), 1])

colnames(AUX) <- AUX2

RS_CE_Confirmados_LUNARDELLI <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_1 <- function(x) quantile(x, probs = 0.25)

AUX <- apply(RS_CE_Confirmados_LUNARDELLI[, 1:(ncol(RS_CE_Confirmados_LUNARDELLI)-1)], 1 , Quartil_1)

RS_CE_Confirmados_LUNARDELLI <- as.data.frame(RS_CE_Confirmados_LUNARDELLI)

RS_CE_Confirmados_LUNARDELLI$Quartil_1 <- AUX

######     Criando a coluna de médiana no data.frame     #####################

AUX <- apply(RS_CE_Confirmados_LUNARDELLI[, 1: (ncol(RS_CE_Confirmados_LUNARDELLI)-2)], 1 , median)

RS_CE_Confirmados_LUNARDELLI$Mediana <- AUX

######     Criando a coluna de Quartil_1 no data frame     ###############

Quartil_3 <- function(x) quantile(x, probs = 0.75)

AUX <- apply(RS_CE_Confirmados_LUNARDELLI[, 1:(ncol(RS_CE_Confirmados_LUNARDELLI)-3)], 1 , Quartil_3)

RS_CE_Confirmados_LUNARDELLI$Quartil_3 <- AUX

RS_CE_Confirmados_LUNARDELLI[, (ncol(RS_CE_Confirmados_LUNARDELLI)+1)] <- rownames(RS_CE_Confirmados_LUNARDELLI)

RS_CE_Confirmados_LUNARDELLI <- RS_CE_Confirmados_LUNARDELLI[, c(ncol(RS_CE_Confirmados_LUNARDELLI), 1:(ncol(RS_CE_Confirmados_LUNARDELLI) -1))]

RS_CE_Confirmados_LUNARDELLI[, 1] <- c(1:52)

colnames(RS_CE_Confirmados_LUNARDELLI)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados_LUNARDELLI) <- c(1:nrow(RS_CE_Confirmados_LUNARDELLI))

rm(AUX, AUX2)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO CONFIRMADOS - Lunardelli ####

AUX_GRAF <- RS_CE_Confirmados_LUNARDELLI

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_LUNARDELLI))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2026_GRAF_CE_Confirmados_LUNARDELLI <- ggplot(AUX_GRAF, 
                                                 aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS Lunardelli - 2026") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = `2026`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################################
####################################################################################################################################
###############Canal Endêmico Prováveis - Lunardelli

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS_2026_Casos_Provaveis_LUNARDELLI <- (RS22_2026_SE_Provaveis[9, 2: 54])

rownames(RS_2026_Casos_Provaveis_LUNARDELLI)[1] <- "Provaveis"

RS_2026_Casos_Provaveis_LUNARDELLI <- t(as.data.frame(RS_2026_Casos_Provaveis_LUNARDELLI))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_LUNARDELLI[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2026_Casos_Provaveis_LUNARDELLI[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2026_GRAF_CE_Provaveis_LUNARDELLI <- ggplot(AUX_GRAF, 
                                               aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Lunardelli - 2026",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#D2B48C",
            alpha = 1) +
  geom_line(aes(y = Mediana), 
            stat = "identity",
            color = "#2F4F4F",
            linewidth = 1) +
  geom_area(aes(y = Quartil_1), 
            fill = "#C0C0C0") +
  geom_line(aes(y = Provaveis), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  Theme() +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_2026_Casos_Provaveis_LUNARDELLI, AUX_GRAF)


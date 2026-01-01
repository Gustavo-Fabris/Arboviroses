Theme <- function(){theme(axis.text.x = element_text(angle = 85, 
                                                      vjust = .5,
                                                      face = "bold",
                                                      size = 12),
                           panel.grid.major = element_line(color = "#C0C0C0"),
                           panel.grid.minor = element_blank(),
                           panel.background = element_rect(fill = "#DC143C"),
                           plot.title = element_text(face = "bold",
                                                     size = 19),
                          plot.caption = element_text(size = 12,
                                                      hjust = 0)
                          ) 
}

AUX_SEM <- as.character(c("2025/1",  "2025/2", "2025/3", 
                      "2025/4",  "2025/5",  "2025/6",  
                      "2025/7",  "2025/8",  "2025/9",  
                      "2025/10",  "2025/11",  "2025/12",  
                      "2025/13",  "2025/14",  "2025/15",  
                      "2025/16",  "2025/17",  "2025/18",  
                      "2025/19",  "2025/20",  "2025/21",  
                      "2025/22",  "2025/23",  
                      "2025/24",  "2025/25",  "2025/26",  
                      "2025/27",  "2025/28",  "2025/29",  
                      "2025/30",  "2025/31",  "2025/32", 
                      "2025/33",  "2025/34",  "2025/35",  
                      "2025/36",  "2025/37",  "2025/38",  
                      "2025/39",  "2025/40",  "2025/41",  
                      "2025/42",  "2025/43",  "2025/44",  
                      "2025/45",  "2025/46",  "2025/47",  
                      "2025/48",  "2025/49",  "2025/50",  
                      "2025/51",  "2025/52")
)

####################################################################################################################
################Trabalhando as tabelas base dos Canais Endêmicos####################################################
####################################################################################################################

#######     Eliminando a SE 53 Inexistente no período no ano 2025    #####

RS_CE_Notificados_Base <- RS_CE_Notificados_Base[, -54]
RS_2025_SE_Notificados <- RS_2025_SE_Notificados[, -54]

######     Canal Endêmico    NOTIFICADOS#####

RS_CE_Notificados_Base[(nrow(RS_CE_Notificados_Base) +1), 1] <- "2025"
RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base), 2:53] <- as.integer(data.frame(RS_2025_SE_Notificados[nrow(RS_2025_SE_Notificados), 2:53]))

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

######     Criando a coluna de Quartil_1 no data frame     ###############

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

RS_2025_GRAF_CE_Notificados <- ggplot(AUX_GRAF, aes(Ordem))  +
    labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

#######     Eliminando a SE 53 Inexistente no período no ano 2025    #####

RS_CE_Confirmados_Base <- RS_CE_Confirmados_Base[, -54]
RS_2025_SE_Confirmados <- RS_2025_SE_Confirmados[, -54]

######     Canal Endêmico    CONFIRMADOS#####

RS_CE_Confirmados_Base[(nrow(RS_CE_Confirmados_Base) +1), 1] <- "2025"
RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base), 2:53] <- as.integer(data.frame(RS_2025_SE_Confirmados[nrow(RS_2025_SE_Confirmados), 2:53]))

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

RS_2025_GRAF_CE_Confirmados <- ggplot(AUX_GRAF, aes(Ordem))  +
   labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRMADOS - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

N <- 202501

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

assign(paste0("RS", "_2025_SE_Provaveis"), AUX)

assign(paste0("RS", RS, "_2025_SE_Provaveis"), AUX)

###     CANAL ENDÊMICO Prováveis         ####

####         Puxando os dados da tabela RS22_CE_Notificados

AUX_GRAF <- RS_CE_Confirmados[,]

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados))

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2025_SE_Provaveis <- RS_2025_SE_Provaveis[, -54]

AUX_GRAF[, 9] <- t(RS_2025_SE_Provaveis[nrow(RS_2025_SE_Provaveis), -1 ])

colnames(AUX_GRAF)[9] <- "2025"

RS_2025_GRAF_CE_Provaveis <- ggplot(AUX_GRAF, 
                                    aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_CE_Notificados_SEDE_Base[(nrow(RS_CE_Notificados_SEDE_Base) +1), 1] <- "2025"
RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_2025_SE_Notificados[6, 2:54]))

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

RS_2025_GRAF_CE_Notificados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS Ivaiporã- 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_CE_Confirmados_SEDE_Base[(nrow(RS_CE_Confirmados_SEDE_Base) +1), 1] <- "2025"
RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_2025_SE_Confirmados[6, 2:54]))

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

RS_2025_GRAF_CE_Confirmados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS Ivaiporã- 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_2025_Casos_Provaveis_SEDE <- (RS22_2025_SE_Provaveis[6, 2: 54])

rownames(RS_2025_Casos_Provaveis_SEDE)[1] <- "Provaveis"

RS_2025_Casos_Provaveis_SEDE <- t(as.data.frame(RS_2025_Casos_Provaveis_SEDE))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SEDE[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2025_Casos_Provaveis_SEDE[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2025_GRAF_CE_Provaveis_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Ivaiporã - 2025",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
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

rm(RS_2025_Casos_Provaveis_SEDE, AUX_GRAF)

####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - IVAIPORÃ      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_JARDIM_BASE[(nrow(RS_CE_Notificados_JARDIM_BASE) +1), 1] <- "2025"
RS_CE_Notificados_JARDIM_BASE[nrow(RS_CE_Notificados_JARDIM_BASE), 2:54] <- as.integer(data.frame(RS22_2025_SE_Notificados[7, 2:54]))

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

RS_2025_GRAF_CE_Notificados_JARDIM <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS Jardim Alegre - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_CE_Confirmados_JARDIM_BASE[(nrow(RS_CE_Confirmados_JARDIM_BASE) +1), 1] <- "2025"
RS_CE_Confirmados_JARDIM_BASE[nrow(RS_CE_Confirmados_JARDIM_BASE), 2:54] <- as.integer(data.frame(RS22_2025_SE_Confirmados[7, 2:54]))

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

RS_2025_GRAF_CE_Confirmados_JARDIM <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS Jardim Alegre - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_2025_Casos_Provaveis_JARDIM <- (RS22_2025_SE_Provaveis[7, 2: 54])

rownames(RS_2025_Casos_Provaveis_JARDIM)[1] <- "Provaveis"

RS_2025_Casos_Provaveis_JARDIM <- t(as.data.frame(RS_2025_Casos_Provaveis_JARDIM))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_JARDIM[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2025_Casos_Provaveis_JARDIM[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2025_GRAF_CE_Provaveis_JARDIM <- ggplot(AUX_GRAF, 
                                           aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Jardim Alegre - 2025",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
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

rm(RS_2025_Casos_Provaveis_JARDIM, AUX_GRAF)

####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - São João      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_SAO_JOAO_BASE[(nrow(RS_CE_Notificados_SAO_JOAO_BASE) +1), 1] <- "2025"
RS_CE_Notificados_SAO_JOAO_BASE[nrow(RS_CE_Notificados_SAO_JOAO_BASE), 2:54] <- as.integer(data.frame(RS22_2025_SE_Notificados[16, 2:54]))

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
###CANAL ENDÊMICO NOTIFICADOS - IVAIPORÃ####

AUX_GRAF <- RS_CE_Notificados_SAO_JOAO

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_SAO_JOAO))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$Sem_EPI <- AUX_SEM

RS_2025_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos NOTIFICADOS São João do Ivaí - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_CE_Confirmados_SAO_JOAO_BASE[(nrow(RS_CE_Confirmados_SAO_JOAO_BASE) +1), 1] <- "2025"
RS_CE_Confirmados_SAO_JOAO_BASE[nrow(RS_CE_Confirmados_SAO_JOAO_BASE), 2:54] <- as.integer(data.frame(RS22_2025_SE_Confirmados[16, 2:54]))

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

RS_2025_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, 
                                                       aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRADOS São João do Ivaí - 2025") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
  geom_line(aes(y = `2025`), 
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

RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI <- (RS22_2025_SE_Provaveis[7, 2: 54])

rownames(RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI)[1] <- "Provaveis"

RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI <- t(as.data.frame(RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SAO_JOAO_DO_IVAI[,]

AUX_GRAF$Sem_EPI <- AUX_SEM

AUX_GRAF[, 8] <- RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI[-53, 1]

AUX_GRAF$Ordem <- c(1: nrow(AUX_GRAF))

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_2025_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI <- ggplot(AUX_GRAF, 
                                                     aes(Ordem))  +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS São João do Ivaí - 2025",
       subtitle = "Casos Prováveis = Casos Notificados - Casos Descartados") +
  geom_area(aes(y = Quartil_3), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Mediana), 
            fill = "#556B2F") +
  geom_area(aes(y = Quartil_1), 
            fill = "#DCDCDC") +
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

rm(RS_2025_Casos_Provaveis_SAO_JOAO_DO_IVAI, AUX_GRAF)


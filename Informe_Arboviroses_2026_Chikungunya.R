##########################################################################################################################################
###################### CHIKUNGUNYA REGIONAL   ############################################################################################

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
    AUX[which(AUX == i), O] <- as.integer(SINAN_CHIK_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == N)%>%
                                            count()
                                          
    )
    
  }
  N <- N +1
  O <- O +1
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2025_SE_Notificados_CHIK"), AUX)

assign("RS_2025_SE_Notificados_CHIK", AUX)

########################################################################################################
###     Construindo um for loop para realizar a tabela de Confirmados por semana epidemiológica      ###
########################################################################################################

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
    AUX[which(AUX == i), O] <- as.integer(SINAN_CHIK_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   CLASSI_FIN == 13,
                                                   SEM_PRI == N)%>%
                                            count()
    )
  }
  N <- N +1
  O <- O +1
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2025_SE_Confirmados_CHIK"), AUX)

assign("RS_2025_SE_Confirmados_CHIK", AUX)

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
    AUX[which(AUX == i), O] <- as.integer(SINAN_CHIK_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == N)%>%
                                            count()
                                          -
                                            SINAN_CHIK_RS %>%
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

assign(paste0("RS", "_2025_SE_Provaveis_CHIK"), AUX)

assign(paste0("RS", RS, "_2025_SE_Provaveis_CHIK"), AUX)

# write.csv (assign(paste0("RS", RS, "_2025_SE_Provaveis_CHIK"), AUX), 
#           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2025_SE_Provaveis_CHIK.csv"), 
#           row.names = FALSE)

#####################################################################################################################
#####################  Dados Estaduais   ############################################################################
#####################################################################################################################
### Transformando os arquivos da base DBF em um único objeto referente ao período sazonal

PR_CHIK_2025_SINAN <- CHIKON2025 

####Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS###

PR_CHIK_2025_GERAL <- BASE_IBGE[, -c(4, 6)]

PR_CHIK_2025_GERAL$Notificados <- NA

PR_CHIK_2025_GERAL$Confirmados <- NA

PR_CHIK_2025_GERAL$Descartados <- NA

PR_CHIK_2025_GERAL$Autoctones <- NA

PR_CHIK_2025_GERAL$Importados <- NA

PR_CHIK_2025_GERAL$Obitos <- NA

PR_CHIK_2025_GERAL$Incidencia <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 5] <- as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                    filter(ID_MN_RESI == i) %>%   
                                                                                    count()
  )   
  
  ###Chikungunya###
  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 6] <-as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                   filter(CLASSI_FIN == 13, 
                                                                                          ID_MN_RESI == i) %>%
                                                                                   count() 
  )
  
  ###Descartados###
  
  
  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 7]<- as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                   filter(CLASSI_FIN == 5,
                                                                                          ID_MN_RESI == i) %>% 
                                                                                   count()
  )  
  
  ###Autóctones###
  
  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 8]<- as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          TPAUTOCTO == 1,
                                                                                          CLASSI_FIN == 13) %>% 
                                                                                   count() 
  )
  
  ###Importados###
  
  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 9]<- as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          TPAUTOCTO == 2,
                                                                                          CLASSI_FIN == 13) %>% 
                                                                                   count() 
  )
  
  ###Óbitos###
  
  PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL$Código_IBGE == i), 10] <- as.integer(PR_CHIK_2025_SINAN%>% 
                                                                                     filter(ID_MN_RESI == i, 
                                                                                            EVOLUCAO == 2) %>% 
                                                                                     count() 
  )
}

###Incidência###FORA DO LOOP###

PR_CHIK_2025_GERAL$Incidencia <- (PR_CHIK_2025_GERAL$Autoctones/PR_CHIK_2025_GERAL$População)*100000  
PR_CHIK_2025_GERAL$Incidencia <- format(round(PR_CHIK_2025_GERAL$Incidencia, 2))
PR_CHIK_2025_GERAL$Incidencia <- as.numeric(PR_CHIK_2025_GERAL$Incidencia)


PR_CHIK_2025_GERAL$Em_Investigacao <- as.integer(PR_CHIK_2025_GERAL$Notificados) - as.integer(PR_CHIK_2025_GERAL$Confirmados + PR_CHIK_2025_GERAL$Descartados)

PR_CHIK_2025_GERAL <- PR_CHIK_2025_GERAL[, c(1, 3, 4, 5, 6, 12, 7, 8, 9, 10, 11)]

PR_CHIK_2025_GERAL[400, 3:10] <- apply(PR_CHIK_2025_GERAL[, 3:10], 2, sum)

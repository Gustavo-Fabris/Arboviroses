##########################################################################################################################################
###################### CHIKUNGUNYA REGIONAL   ############################################################################################

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

assign(paste0("RS", RS, "_2026_SE_Notificados_CHIK"), AUX)

assign("RS_2026_SE_Notificados_CHIK", AUX)

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

N <- 202601

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

assign(paste0("RS", RS, "_2026_SE_Confirmados_CHIK"), AUX)

assign("RS_2026_SE_Confirmados_CHIK", AUX)

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

assign(paste0("RS", "_2026_SE_Provaveis_CHIK"), AUX)

assign(paste0("RS", RS, "_2026_SE_Provaveis_CHIK"), AUX)



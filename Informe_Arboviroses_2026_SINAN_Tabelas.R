####    Tabela de notificações SINAN    ###

SINAN_DENGUE_RS <- DENGON2026 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_2026_SINAN"), 
       SINAN_DENGUE_RS) 

###############   Fazendo o mesmo com as bases DBF de chikungunya   ################################################

SINAN_CHIK_RS <- CHIKON2026 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_2026_SINAN_CHIKUNGUNYA"), 
       SINAN_CHIK_RS) 

#################################################################################################################
###     Construindo um for loop para realizar a tabela de notificados por semana epidemiológica               ###
#################################################################################################################

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
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2026_SE_Notificados"), AUX)

assign("RS_2026_SE_Notificados", AUX)

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
    
    AUX[which(AUX == i), O] <- as.integer(SINAN_DENGUE_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   CLASSI_FIN == 10 
                                                   | 
                                                     CLASSI_FIN == 11 
                                                   |
                                                     CLASSI_FIN == 12,
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

assign(paste0("RS", RS, "_2026_SE_Confirmados"), AUX)

assign("RS_2026_SE_Confirmados", AUX)

###################################################################################################
####       Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS      ####
###################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Notificados <- NA

AUX$Dengue <- NA

AUX$D_S_A <- NA

AUX$Dengue_Grave <- NA

AUX$Descartados <- NA

AUX$Autoctones <- NA

AUX$Incidencia <- NA

AUX$Criterio_Encerramento_Lab <- NA

AUX$Criterio_Encerramento_Clin_Epid <- NA

AUX$DENV_I <- NA

AUX$DENV_II <- NA

AUX$DENV_III <- NA

AUX$DENV_IV <- NA

AUX$Hospitalizacao <- NA

AUX$Obitos <- NA

AUX$Inconclusivos <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  ###Notiicações###  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  ###Dengue###
  
  AUX[which(AUX$COD_IBGE == i), 6] <-as.integer(SINAN_DENGUE_RS %>% 
                                                  filter(CLASSI_FIN == 10, 
                                                         ID_MN_RESI == i) %>%
                                                  count() 
  )
  ###D.S.A.###
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%  
                                                   filter(CLASSI_FIN == 11, 
                                                          ID_MN_RESI == i) %>% 
                                                   count()
  )
  
  ###Dengue Grave###
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%  
                                                   filter(CLASSI_FIN == 12, 
                                                          ID_MN_RESI == i) %>% 
                                                   count()
  )
  
  ###Descartados###
  
  
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>% 
                                                  filter(CLASSI_FIN == 5,
                                                         ID_MN_RESI == i) %>% 
                                                  count()
  )  
  
  ###Autóctones###
  
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          TPAUTOCTO == 1,
                                                          CLASSI_FIN == 10 
                                                          |
                                                            CLASSI_FIN == 11
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  ###Encerrados Laboratório###
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CRITERIO == 1) %>% 
                                                    count() 
  )
  
  ###Encerrados Clínico-Epidemiológico###
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CRITERIO == 2) %>% 
                                                    count() 
  )
  
  ###DENV I###
  
  AUX[which(AUX$COD_IBGE == i), 14]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          SOROTIPO == 1) %>% 
                                                   count() 
  )
  
  ###DENV II###
  
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           SOROTIPO == 2) %>% 
                                                    count() 
  )
  
  ###DENV III###
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           SOROTIPO == 3) %>% 
                                                    count() 
  )
  ###DENV IV###                                     
  
  AUX[which(AUX$COD_IBGE == i), 17]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          SOROTIPO == 4) %>% 
                                                   count() 
  )
  ###Hospitalização###
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           HOSPITALIZ == 1) %>% 
                                                    count() 
  )
  ###Óbitos###
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           EVOLUCAO == 2) %>% 
                                                    count() 
  )
  
  ###Inconclusivos###
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CLASSI_FIN == 8) %>% 
                                                    count() 
  )
}

#########################
###    Incidência     ###
#########################

AUX$Incidencia <- (AUX$Autoctones/AUX$Populacao)*100000  
AUX$Incidencia <- format(round(AUX$Incidencia, 2))
AUX$Incidencia <- as.numeric(AUX$Incidencia)

#########################
###    Incidência  Prováveis   ###
#########################

AUX$Provaveis <- AUX$Notificados - AUX$Descartados

AUX$Incidencia_Provaveis <- (AUX$Provaveis/AUX$Populacao)*100000  
AUX$Incidencia_Provaveis <- format(round(AUX$Incidencia_Provaveis, 2))
AUX$Incidencia_Provaveis <- as.numeric(AUX$Incidencia_Provaveis)


###################################################################################################
####                Incluindo coluna de CASOS EM INVESTIGAÇÂO na tabela RS22_GERAL             ####
####                Esta coluna só tem sentido no período sazonal atual                        ####
####                Casos em investigação de períodos anteriores são                           ####
####                INCONCLUSIVOS.                                                             ####
###################################################################################################

AUX$Em_Investigacao <- as.integer(AUX$Notificados) - (as.integer(AUX$Dengue + AUX$D_S_A + AUX$Dengue_Grave + AUX$Descartados))

assign(paste0("RS", RS, "_2026_GERAL"), AUX)

assign("RS_2026_GERAL", AUX)

####################################################################################################
####      Elaborando Quadro com dados de sexo, idade, zona de moradia e escolaridade           #####
####################################################################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Menos_1_ano <- NA

AUX$Um_a_Cinco_Anos <- NA

AUX$Cinco_a_Doze_Anos <- NA

AUX$Doze_a_Dezoito_Anos <- NA

AUX$Dezoito_a_Cinq_Nove <- NA

AUX$Maior_Sessenta <- NA

AUX$Area_Urbana <- NA

AUX$Area_Rural <- NA

AUX$Sexo_Feminino <- NA

AUX$Sexo_Masculino <- NA

AUX$Analfabeto <- NA

AUX$Fundamental_Incompleto <- NA

AUX$Fundamental <- NA

AUX$Ens_Medio_Incompleto <- NA

AUX$Ens_Medio<- NA

AUX$Ens_Superior_Incompleto<- NA

AUX$Ens_Superior<- NA

AUX$Escolaridade_Ignorada<- NA

###      For Loop para geração da tabela RS22_Extra       ###

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N <=3012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N > 4000 
                                                          & 
                                                            NU_IDADE_N <=4005) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,
                                                          NU_IDADE_N > 4005 
                                                          & 
                                                            NU_IDADE_N <=4012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4059 ) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 1) %>% 
                                                    count() 
  )
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 2) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 12]  <- as.integer(SINAN_DENGUE_RS %>% 
                                                     filter(ID_MN_RESI == i, 
                                                            CS_SEXO == "F") %>% 
                                                     count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_SEXO == "M") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 14]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 0) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 1 
                                                           | 
                                                             CS_ESCOL_N == 2 
                                                           | 
                                                             CS_ESCOL_N == 3) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 4) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 5) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 6) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 7) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 8) %>% 
                                                    count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 21]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 9) %>% 
                                                   count() 
  )
}                                             

assign(paste0("RS", RS, "_2026_EXTRA"), AUX)

####################################################################################################
####      Elaborando Quadro com dados de sexo, idade, zona de moradia e escolaridade           #####
#####                              CONFIRMADOS                                                 #####
####################################################################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Menos_1_ano <- NA

AUX$Um_a_Cinco_Anos <- NA

AUX$Cinco_a_Doze_Anos <- NA

AUX$Doze_a_Dezoito_Anos <- NA

AUX$Dezoito_a_Cinq_Nove <- NA

AUX$Maior_Sessenta <- NA

AUX$Area_Urbana <- NA

AUX$Area_Rural <- NA

AUX$Sexo_Feminino <- NA

AUX$Sexo_Masculino <- NA

AUX$Analfabeto <- NA

AUX$Fundamental_Incompleto <- NA

AUX$Fundamental <- NA

AUX$Ens_Medio_Incompleto <- NA

AUX$Ens_Medio<- NA

AUX$Ens_Superior_Incompleto<- NA

AUX$Ens_Superior<- NA

AUX$Escolaridade_Ignorada<- NA

###      For Loop para geração da tabela RS22_Extra       ###

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N <=3012,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N > 4000 
                                                          & 
                                                            NU_IDADE_N <=4005,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,
                                                          NU_IDADE_N > 4005 
                                                          & 
                                                            NU_IDADE_N <=4012,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4059,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 1,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 2,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 12]  <- as.integer(SINAN_DENGUE_RS %>% 
                                                     filter(ID_MN_RESI == i, 
                                                            CS_SEXO == "F",
                                                            CLASSI_FIN == 10 
                                                            | 
                                                              CLASSI_FIN == 11 
                                                            |
                                                              CLASSI_FIN == 12) %>% 
                                                     count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_SEXO == "M",
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 14]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 0,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 1 
                                                           | 
                                                             CS_ESCOL_N == 2 
                                                           | 
                                                             CS_ESCOL_N == 3,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 4,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 5,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 6,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 7,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 8,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 21]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 9,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count() 
  )
}                                             

assign(paste0("RS", RS, "_2026_EXTRA_Confirmados"), AUX)

######################################################################################################
###         Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.            ###
######################################################################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Febre <- NA

AUX$Cefaleia <- NA

AUX$Mialgia <- NA

AUX$Exantema <- NA

AUX$Vomitos <- NA

AUX$Nausea <- NA

AUX$Dor_nas_Costas <- NA

AUX$Conjuntivite <- NA

AUX$Artrite  <- NA

AUX$Artralgia <- NA

AUX$Petequias <- NA

AUX$Leucopenia <- NA

AUX$Dor_Retroorbital <- NA

AUX$Prova_do_Laco_Positiva <- NA

###Elaborando for loop para sinais e sintomas.###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          FEBRE == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CEFALEIA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          MIALGIA == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          EXANTEMA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          VOMITO == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         NAUSEA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          DOR_COSTAS == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CONJUNTVIT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ARTRITE == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ARTRALGIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           PETEQUIA_N == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           LEUCOPENIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           DOR_RETRO == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           LACO == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_2026_SINAIS_Notificados"), AUX)

###    Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.         ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Febre <- NA

AUX$Cefaleia <- NA

AUX$Mialgia <- NA

AUX$Exantema <- NA

AUX$Vomitos <- NA

AUX$Nausea <- NA

AUX$Dor_nas_Costas <- NA

AUX$Conjuntivite <- NA

AUX$Artrite  <- NA

AUX$Artralgia <- NA

AUX$Petequias <- NA

AUX$Leucopenia <- NA

AUX$Dor_Retroorbital <- NA

AUX$Prova_do_Laco_Positiva <- NA

###   Elaborando for loop para sinais e sintomas.   ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          FEBRE == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          CEFALEIA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          MIALGIA == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          EXANTEMA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          VOMITO == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         CLASSI_FIN == 10 
                                                         | 
                                                           CLASSI_FIN == 11 
                                                         |
                                                           CLASSI_FIN == 12,
                                                         NAUSEA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          DOR_COSTAS == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           CONJUNTVIT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           ARTRITE == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           ARTRALGIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           PETEQUIA_N == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           LEUCOPENIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           DOR_RETRO == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           LACO == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_2026_SINAIS_Confirmados"), AUX)

###    Montando tabela de doenças pré-existentes    ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Município <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Diabetes <- NA

AUX$Doencas_Hematologicas <- NA

AUX$Hepatopatias <- NA

AUX$DRC <- NA

AUX$Hipertensao <- NA

AUX$Doenca_Acido_Peptica <- NA

AUX$Doenca_Auto_Imune <- NA

###    Construindo o for loop    ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i, 
                                                          DIABETES == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          HEMATOLOG == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          HEPATOPAT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         RENAL == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         HIPERTENSA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ACIDO_PEPT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           AUTO_IMUNE == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_2026_DOENCAS_PRE_EXISTENTES"), AUX)

###     Construindo tabela sinais de alarme     ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Hipotensao_Lipotimia <- NA

AUX$Queda_Abrupta_Plaquetas <- NA

AUX$Vomitos_Persistentes <- NA

AUX$Dor_Abdominal <- NA

AUX$Letargia <- NA

AUX$Aumento_Hematocrito <- NA

AUX$hemorragias <- NA

AUX$Hepatomegalia <- NA

AUX$Acumulo_Liquidos <- NA

###    Construindo o for loop     ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_HIPOT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_PLAQ == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_VOM == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_ABDOM == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_LETAR == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_HEMAT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_SANG == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_HEPAT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_LIQ == 1) %>%
                                                    count()
                                                  
  )
}

assign(paste0("RS", RS, "_2026_SINAIS_DE_ALARME"), AUX)

###     Construindo tabela Dengue Grave      ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Pulso_Debil <- NA

AUX$PA_Convergente <- NA

AUX$TPC <- NA

AUX$Acumulo_Liquidos_Insuf_Respiratoria <- NA

AUX$Taquicardia <- NA

AUX$Extremidades_Frias <- NA

AUX$Hipotensão_Arterial <- NA

AUX$Hematemese <- NA

AUX$Melena <- NA

AUX$Metrorragia <- NA

AUX$Sangramento_SNC <- NA

AUX$Aumento_ALT_AST <- NA

AUX$Miocardite <- NA

AUX$Alteracao_Consciencia <- NA

###      Construindo o for loop        ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_PULSO == 1) %>%
                                                   count()
  )     
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_CONV == 1) %>%
                                                   count()
  )   
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_ENCH == 1) %>%
                                                   count()
  )  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_INSUF == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_TAQUI == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_EXTRE == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_HIPOT == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_HEMAT == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 12]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_MELEN == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_METRO == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_SANG == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_AST == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_MIOC == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_CONSC == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_2026_DENGUE_GRAVE"), AUX)


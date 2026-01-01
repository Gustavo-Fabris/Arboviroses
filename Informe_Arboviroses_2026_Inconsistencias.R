DENGON2025 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2025.dbf", 
                       as.is = FALSE) %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

CHIKON2025 <- read.dbf(file = "Base_de_Dados/DBF/CHIKON2025.dbf", 
                       as.is = FALSE) %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

RS22_2025_SINAN <- rbind(DENGON2025,
                         CHIKON2025)

##### Dados de Residência

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Sem_Logradouro <- NA

AUX$Sem_Numero <- NA

AUX$Sem_Bairro <- NA

AUX$Sem_Zona <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2025_SINAN %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          is.na(RS22_2025_SINAN$NM_LOGRADO) 
                                                   )
                                                   )
 
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     is.na(RS22_2025_SINAN$NU_NUMERO) 
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2025_SINAN %>% 
                                                filter(ID_MN_RESI == i,  
                                                       is.na(RS22_2025_SINAN$NM_BAIRRO) 
                                                )
    )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     is.na(RS22_2025_SINAN$CS_ZONA) 
                                              )
  )
}

RS22_2025_Incons_Residencia <- AUX

##### Dados Laboratoriais

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Sorologia_Dengue <- NA

AUX$Sorologia_Chik <- NA

AUX$PCR <- NA

AUX$Dengue_S_Resultado <- NA

AUX$Chik_S_Resultado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_SORO < (DT_SIN_PRI +5)                  
                                                     )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_CHIK_S1 < (DT_SIN_PRI +5)                
                                                     )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2025_SINAN %>% 
                                                filter(ID_MN_RESI == i,  
                                                       DT_PCR > (DT_SIN_PRI +4)                      
                                                       )
    )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     DT_SORO != is.na(RS22_2025_SINAN$DT_SORO),
                                                     DT_SORO > (DT_SIN_PRI +5)
                                                     )
                                            ) - count(RS22_2025_SINAN %>% 
                                                      filter(ID_MN_RESI == i,
                                                             DT_SORO != is.na(RS22_2025_SINAN$DT_SORO),
                                                             DT_SORO > (DT_SIN_PRI +5),
                                                             Sys.Date() > (DT_SORO + 20),
                                                             RESUL_SORO !=is.na(RS22_2025_SINAN$RESUL_SORO)
                                                      )
                                            )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_CHIK_S1 != is.na(RS22_2025_SINAN$DT_CHIK_S1)
                                                     )
                                            ) -   
    count(RS22_2025_SINAN %>%             
            filter(ID_MN_RESI == i,
                   RES_CHIKS1 == is.na(RS22_2025_SINAN$RES_CHIKS1)
            )
    )
}

RS22_2025_Incons_Laboratorio <- AUX

##### Hospitalização

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Internacao_s_Data <- NA

AUX$Internacao_s_hosp <- NA

AUX$Internacao_s_DSA <- NA

AUX$Hospitalizado_S_Sorologia <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1 &
                                                       is.na(RS22_2025_SINAN$DT_INTERNA)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1 &
                                                       is.na(RS22_2025_SINAN$HOSPITAL)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1,
                                                     CLASSI_FIN != 5,
                                                     is.na(RS22_2025_SINAN$DT_ALRM),
                                                     is.na(RS22_2025_SINAN$DT_GRAV)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1,
                                                     CRITERIO == 2
                                                     )
  )
}

RS22_2025_Incons_Hospitalizacao <- AUX

##### Ausência de autoctonia

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$S_Autoc <- NA

AUX$S_Encerramento <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     CLASSI_FIN != 5,
                                                     is.na(RS22_2025_SINAN$TPAUTOCTO)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2025_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     CLASSI_FIN != is.na(RS22_2025_SINAN$CLASSI_FIN),
                                                     is.na(RS22_2025_SINAN$EVOLUCAO)
                                              )
  )
}

RS22_2025_Incons_Investigacao <- AUX
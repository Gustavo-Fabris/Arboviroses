DENGON2026 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2026.dbf", 
                       as.is = FALSE) %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

CHIKON2026 <- read.dbf(file = "Base_de_Dados/DBF/CHIKON2026.dbf", 
                       as.is = FALSE) %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

RS22_2026_SINAN <- rbind(DENGON2026,
                         CHIKON2026)

##### Dados de Residência

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Sem_Logradouro <- NA

AUX$Sem_Numero <- NA

AUX$Sem_Bairro <- NA

AUX$Sem_Zona <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2026_SINAN %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          is.na(RS22_2026_SINAN$NM_LOGRADO) 
                                                   )
                                                   )
 
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     is.na(RS22_2026_SINAN$NU_NUMERO) 
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2026_SINAN %>% 
                                                filter(ID_MN_RESI == i,  
                                                       is.na(RS22_2026_SINAN$NM_BAIRRO) 
                                                )
    )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     is.na(RS22_2026_SINAN$CS_ZONA) 
                                              )
  )
}

RS22_2026_Incons_Residencia <- AUX


AUX <- RS22_2026_Incons_Residencia[,-c(1, 3)]

RS22_2026_TAB_INCON_RESID_Municipios <- AUX %>%
  gt() %>%
  tab_header(title = md("**Qualifica SINAN - Dados de Residência (2026)**"),
             subtitle = "Campos 17 a 30 do SINAN online"
             ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Inconsistências",
              columns = c(2:5),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(2:5)) %>%
  cols_label(Municipio = "Município",
             Sem_Logradouro = "Logradouro Faltando",
             Sem_Numero = "Número Faltando",
             Sem_Bairro = "Bairro Faltando",
             Sem_Zona = "Zona Faltando") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##### Dados Laboratoriais

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Sorologia_Dengue <- NA

AUX$Sorologia_Chik <- NA

AUX$PCR <- NA

AUX$Dengue_S_Resultado <- NA

AUX$Chik_S_Resultado <- NA

AUX$PCR_S_sorotipo <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_SORO < (DT_SIN_PRI + 6)                  
                                                     )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_CHIK_S1 < (DT_SIN_PRI + 6)                
                                                     )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2026_SINAN %>% 
                                                filter(ID_MN_RESI == i,  
                                                       DT_PCR > (DT_SIN_PRI +4)                      
                                                       )
    )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
                                                     DT_SORO > (DT_SIN_PRI + 6)
                                                     )
                                            ) - count(RS22_2026_SINAN %>% 
                                                      filter(ID_MN_RESI == i,
                                                             DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
                                                             DT_SORO > (DT_SIN_PRI + 6),
                                                             Sys.Date() > (DT_SORO + 20),
                                                             RESUL_SORO !=is.na(RS22_2026_SINAN$RESUL_SORO)
                                                      )
                                            )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
                                                     DT_CHIK_S1 < (DT_SIN_PRI + 6)
                                                     )
                                            ) - count(RS22_2026_SINAN %>% 
                                                        filter(ID_MN_RESI == i,
                                                               DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_SORO),
                                                               DT_CHIK_S1 > (DT_SIN_PRI + 6),
                                                               Sys.Date() > (DT_SORO + 20),
                                                               RES_CHIKS1 != is.na(RS22_2026_SINAN$RES_CHIKS1)
            )
    )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     ID_AGRAVO == "A90",
                                                     RESUL_PCR_ == 1 &
                                                       is.na(SOROTIPO)
                                              )
                                            )
                                            }

RS22_2026_Incons_Laboratorio <- AUX

AUX <- RS22_2026_Incons_Laboratorio[,-c(1, 3)]

RS22_2026_TAB_INCON_LABORATORIO_Municipios <- AUX %>%
  gt() %>%
  tab_header(title = md("**Qualifica SINAN - Dados de Laboratoriais (2026)**"),
             subtitle = "Campos 35 a 49 do SINAN online"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Inconsistências",
              columns = c(2:7),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(2:7)) %>%
  cols_label(Municipio = "Município",
             Sorologia_Dengue = "Sorologia DENGUE Coletada < 6 dias",
             Sorologia_Chik = "Sorologia CHIKUNGUNYA Coletada < 6 dias",
             PCR = "Pesquisa Arbovírus Coletada > 5 dias",
             Dengue_S_Resultado = "Faltando Resultado Sorologia DENGUE",
             Chik_S_Resultado = "Faltando Resultado Sorologia CHIKUNGUNYA",
             PCR_S_sorotipo = "Faltando Sorotipo de Dengue (PCR Detectável)") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##### Hospitalização

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Internacao_s_Data <- NA

AUX$Internacao_s_hosp <- NA

AUX$Internacao_s_DSA <- NA

AUX$Hospitalizado_S_Sorologia <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1 &
                                                       is.na(RS22_2026_SINAN$DT_INTERNA)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1 &
                                                       is.na(RS22_2026_SINAN$HOSPITAL)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1,
                                                     CLASSI_FIN != 5,
                                                     is.na(RS22_2026_SINAN$DT_ALRM),
                                                     is.na(RS22_2026_SINAN$DT_GRAV)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i,  
                                                     HOSPITALIZ == 1,
                                                     CRITERIO == 2
                                                     )
  )
}

RS22_2026_Incons_Hospitalizacao <- AUX

AUX <- RS22_2026_Incons_Hospitalizacao[,-c(1, 3)]

RS22_2026_TAB_INCON_HOSPITALIZACAO <- AUX %>%
  gt() %>%
  tab_header(title = md("**Qualifica SINAN - Dados de Hospitalização (2026)**"),
             subtitle = "Campos 50 a 55, associados com campos 68 a 71 do SINAN online"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Inconsistências",
              columns = c(2:5),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(2:5)) %>%
  cols_label(Municipio = "Município",
             Internacao_s_Data = "Falta Data de Internação",
             Internacao_s_hosp = "Falta Hospital da Internação",
             Internacao_s_DSA = "Internamento sem Indicação de Dengue Grave ou D.S.A",
             Hospitalizado_S_Sorologia = "Internamento sem Coleta de Exames") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
                                            )
            ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##### Ausência de autoctonia

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$S_Autoc <- NA

AUX$S_Encerramento <- NA

AUX$Obitos_invest <- NA

AUX$Inconclusivo <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     CLASSI_FIN != 5,
                                                     is.na(RS22_2026_SINAN$TPAUTOCTO)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
                                                     is.na(RS22_2026_SINAN$EVOLUCAO)
                                              )
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     EVOLUCAO == 4)
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- count(RS22_2026_SINAN %>% 
                                              filter(ID_MN_RESI == i, 
                                                     CLASSI_FIN == 8)
  )
  
  }

RS22_2026_SINAN_Duplic <- RS22_2026_SINAN %>%
  filter(duplicated(RS22_2026_SINAN$NU_NOTIFIC))

AUX$Duplicidade <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  AUX[which(AUX$COD_IBGE == i), 8] <- count(RS22_2026_SINAN_Duplic %>% 
                                              filter(ID_MN_RESI == i,
                                                     ID_AGRAVO != 8)
  )
}

RS22_2026_Incons_Investigacao <- AUX

AUX <- RS22_2026_Incons_Investigacao[,-c(1, 3)]

RS22_2026_TAB_INCON_INVESTIGACAO <- AUX %>%
  gt() %>%
  tab_header(title = md("**Qualifica SINAN - Investigação Geral (2026)**"),
             subtitle = "Associação de diversos campos do SINAN online"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Inconsistências",
              columns = c(2:6),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(2:6)) %>%
  cols_label(Municipio = "Município",
             S_Autoc = "Sem Autoctonia",
             S_Encerramento = "Fichas em Aberto",
             Obitos_invest = "Óbitos em Investigação",
             Inconclusivo = "Fichas Inconclusivas",
             Duplicidade = "Fichas em Duplicidade") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############################################################################
####   Distribuindo as fichas para os municípios

###  Arapuã

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 410165,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


ARAPUA <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         DT_SORO < (DT_SIN_PRI + 6)) 

ARAPUA[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

ARAPUA[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         DT_PCR > (DT_SIN_PRI +4)) 

ARAPUA[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
         )
                
ARAPUA[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )

ARAPUA[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
         )

ARAPUA[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

ARAPUA[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

ARAPUA[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

ARAPUA[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

ARAPUA[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

ARAPUA[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         EVOLUCAO == 4)

ARAPUA[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410165,  
         CLASSI_FIN == 8)

ARAPUA[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 410165,
         ID_AGRAVO != 8)

ARAPUA[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARAPUA)[15] <- "Ficha_em_Duplicidade"

sheet_write(ARAPUA,
            ss = "https://docs.google.com/spreadsheets/d/1V3KV69giNzSZDjd7j3tCbPtnh6Z4jML_8-V6m0xxDAU/edit?gid=412172508#gid=412172508",
            sheet = "AUXILIAR02")

###  Ariranha do Ivaí

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 410185,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


ARIRANHA <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         DT_SORO < (DT_SIN_PRI + 6)) 

ARIRANHA[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

ARIRANHA[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         DT_PCR > (DT_SIN_PRI +4)) 

ARIRANHA[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


ARIRANHA[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


ARIRANHA[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

ARIRANHA[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

ARIRANHA[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

ARIRANHA[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

ARIRANHA[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

ARIRANHA[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

ARIRANHA[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         EVOLUCAO == 4)

ARIRANHA[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410185,  
         CLASSI_FIN == 8)

ARIRANHA[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 410185,
         ID_AGRAVO != 8)

ARIRANHA[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ARIRANHA)[15] <- "Ficha_em_Duplicidade"

sheet_write(ARIRANHA,
            ss = "https://docs.google.com/spreadsheets/d/1Og37J17lB08NSmOHKRFncVKl1pBi3DNzNsxW4S9hRZE/edit?gid=2092006565#gid=2092006565",
            sheet = "AUXILIAR02")

###  Cândido de Abreu

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 410440,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


CANDIDO <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         DT_SORO < (DT_SIN_PRI + 6)) 

CANDIDO[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

CANDIDO[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         DT_PCR > (DT_SIN_PRI +4)) 

CANDIDO[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


CANDIDO[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


CANDIDO[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

CANDIDO[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

CANDIDO[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

CANDIDO[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

CANDIDO[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

CANDIDO[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

CANDIDO[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         EVOLUCAO == 4)

CANDIDO[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410440,  
         CLASSI_FIN == 8)

CANDIDO[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 410440,
         ID_AGRAVO != 8)

CANDIDO[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CANDIDO)[15] <- "Ficha_em_Duplicidade"

sheet_write(CANDIDO,
            ss = "https://docs.google.com/spreadsheets/d/1xsOFwXYN_NTphr5iJh46iNskM1p_QKip54A3DPICugQ/edit?gid=1211575482#gid=1211575482",
            sheet = "AUXILIAR02")

###  Cruzmaltina

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 410685,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


CRUZMALTINA <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         DT_SORO < (DT_SIN_PRI + 6)) 

CRUZMALTINA[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

CRUZMALTINA[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         DT_PCR > (DT_SIN_PRI +4)) 

CRUZMALTINA[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


CRUZMALTINA[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


CRUZMALTINA[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

CRUZMALTINA[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

CRUZMALTINA[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

CRUZMALTINA[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

CRUZMALTINA[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

CRUZMALTINA[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

CRUZMALTINA[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         EVOLUCAO == 4)

CRUZMALTINA[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410685,  
         CLASSI_FIN == 8)

CRUZMALTINA[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 410685,
         ID_AGRAVO != 8)

CRUZMALTINA[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(CRUZMALTINA)[15] <- "Ficha_em_Duplicidade"

sheet_write(CRUZMALTINA,
            ss = "https://docs.google.com/spreadsheets/d/1EoBHM0s0fiIm6nYb940u49moQFPskg2WRsyQSeuVGl4/edit?gid=333775765#gid=333775765",
            sheet = "AUXILIAR02")

###  Godoy Moreira

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 410855,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


GODOY <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         DT_SORO < (DT_SIN_PRI + 6)) 

GODOY[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

GODOY[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         DT_PCR > (DT_SIN_PRI +4)) 

GODOY[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


GODOY[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


GODOY[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

GODOY[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

GODOY[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

GODOY[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

GODOY[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

GODOY[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

GODOY[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         EVOLUCAO == 4)

GODOY[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 410855,  
         CLASSI_FIN == 8)

GODOY[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 410855,
         ID_AGRAVO != 8)

GODOY[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(GODOY)[15] <- "Ficha_em_Duplicidade"

sheet_write(GODOY,
            ss = "https://docs.google.com/spreadsheets/d/11aaWzd7xAnkZ6uoF_pv1z8iFiFkKXM-zPkbgOK91QlE/edit?gid=17728500#gid=17728500",
            sheet = "AUXILIAR02")

###  Ivaiporã

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411150,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


IVAIPORA <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         DT_SORO < (DT_SIN_PRI + 6)) 

IVAIPORA[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

IVAIPORA[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         DT_PCR > (DT_SIN_PRI +4)) 

IVAIPORA[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO) &
         DT_SORO > (DT_SIN_PRI + 6) &
         Sys.Date() > (DT_SORO + 20) &
         is.na(RS22_2026_SINAN$RESUL_SORO))

IVAIPORA[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )

IVAIPORA[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

IVAIPORA[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

IVAIPORA[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

IVAIPORA[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

IVAIPORA[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

IVAIPORA[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

IVAIPORA[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         EVOLUCAO == 4)

IVAIPORA[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411150,  
         CLASSI_FIN == 8)

IVAIPORA[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411150,
         ID_AGRAVO != 8)

IVAIPORA[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(IVAIPORA)[15] <- "Ficha_em_Duplicidade"

sheet_write(IVAIPORA,
            ss = "https://docs.google.com/spreadsheets/d/1Mg10zhEVIWzXaeXC3aCDne8VgLYbVFM4d9FyZuVQd4U/edit?gid=1716314840#gid=1716314840",
            sheet = "AUXILIAR02")

###  Jardim Alegre

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411250,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


JARDIM <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         DT_SORO < (DT_SIN_PRI + 6)) 

JARDIM[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

JARDIM[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         DT_PCR > (DT_SIN_PRI +4)) 

JARDIM[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


JARDIM[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


JARDIM[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

JARDIM[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

JARDIM[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

JARDIM[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

JARDIM[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

JARDIM[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

JARDIM[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         EVOLUCAO == 4)

JARDIM[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411250,  
         CLASSI_FIN == 8)

JARDIM[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411250,
         ID_AGRAVO != 8)

JARDIM[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(JARDIM)[15] <- "Ficha_em_Duplicidade"

sheet_write(JARDIM,
            ss = "https://docs.google.com/spreadsheets/d/1Yt1gooatxY0Y7EWFBesJnp68xfCnR7EdOfUtAlnRnxY/edit?gid=1498997044#gid=1498997044",
            sheet = "AUXILIAR02")

###  Lidianópolis

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411342,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


LIDIANOPOLIS <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         DT_SORO < (DT_SIN_PRI + 6)) 

LIDIANOPOLIS[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

LIDIANOPOLIS[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         DT_PCR > (DT_SIN_PRI +4)) 

LIDIANOPOLIS[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


LIDIANOPOLIS[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


LIDIANOPOLIS[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

LIDIANOPOLIS[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

LIDIANOPOLIS[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

LIDIANOPOLIS[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

LIDIANOPOLIS[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

LIDIANOPOLIS[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

LIDIANOPOLIS[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         EVOLUCAO == 4)

LIDIANOPOLIS[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411342,  
         CLASSI_FIN == 8)

LIDIANOPOLIS[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411342,
         ID_AGRAVO != 8)

LIDIANOPOLIS[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LIDIANOPOLIS)[15] <- "Ficha_em_Duplicidade"

sheet_write(LIDIANOPOLIS,
            ss = "https://docs.google.com/spreadsheets/d/1a1_f7fPoNRMy0ASfY5Yu8xG6QniCbKsJ7FHeyeKXjN0/edit?gid=1897035102#gid=1897035102",
            sheet = "AUXILIAR02")

###  Lunardelli

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411375,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


LUNARDELLI <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         DT_SORO < (DT_SIN_PRI + 6)) 

LUNARDELLI[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

LUNARDELLI[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         DT_PCR > (DT_SIN_PRI +4)) 

LUNARDELLI[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


LUNARDELLI[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


LUNARDELLI[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

LUNARDELLI[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

LUNARDELLI[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

LUNARDELLI[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

LUNARDELLI[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

LUNARDELLI[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

LUNARDELLI[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         EVOLUCAO == 4)

LUNARDELLI[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411375,  
         CLASSI_FIN == 8)

LUNARDELLI[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411375,
         ID_AGRAVO != 8)

LUNARDELLI[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(LUNARDELLI)[15] <- "Ficha_em_Duplicidade"

sheet_write(LUNARDELLI,
            ss = "https://docs.google.com/spreadsheets/d/186vD9iqH-r8RIWUWYfC2EtYJAlr0Wb4N-FUSHxxpjQ0/edit?gid=1487561130#gid=1487561130",
            sheet = "AUXILIAR02")

###  Manoel Ribas

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411450,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


MRIBAS <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         DT_SORO < (DT_SIN_PRI + 6)) 

MRIBAS[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

MRIBAS[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         DT_PCR > (DT_SIN_PRI +4)) 

MRIBAS[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


MRIBAS[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


MRIBAS[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

MRIBAS[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

MRIBAS[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

MRIBAS[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

MRIBAS[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

MRIBAS[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

MRIBAS[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         EVOLUCAO == 4)

MRIBAS[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411450,  
         CLASSI_FIN == 8)

MRIBAS[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411450,
         ID_AGRAVO != 8)

MRIBAS[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRIBAS)[15] <- "Ficha_em_Duplicidade"

sheet_write(MRIBAS,
            ss = "https://docs.google.com/spreadsheets/d/1u3pn-3JHJ6Q4s4zley_9SPDVvOy3qAYhkG05BDQagwg/edit?gid=1188548387#gid=1188548387",
            sheet = "AUXILIAR02")

###  Mato Rico

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411573,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


MRICO <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         DT_SORO < (DT_SIN_PRI + 6)) 

MRICO[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

MRICO[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         DT_PCR > (DT_SIN_PRI +4)) 

MRICO[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


MRICO[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


MRICO[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

MRICO[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

MRICO[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

MRICO[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

MRICO[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

MRICO[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

MRICO[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         EVOLUCAO == 4)

MRICO[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411573,  
         CLASSI_FIN == 8)

MRICO[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411573,
         ID_AGRAVO != 8)

MRICO[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(MRICO)[15] <- "Ficha_em_Duplicidade"

sheet_write(MRICO,
            ss = "https://docs.google.com/spreadsheets/d/1FOhGcULbvyfrMgdRrC4ZAUpe8s5I-prreRaeuGwgac0/edit?gid=859150450#gid=859150450",
            sheet = "AUXILIAR02")

###  Nova Tebas

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 411727,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


NTEBAS <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         DT_SORO < (DT_SIN_PRI + 6)) 

NTEBAS[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

NTEBAS[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         DT_PCR > (DT_SIN_PRI +4)) 

NTEBAS[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


NTEBAS[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


NTEBAS[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

NTEBAS[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

NTEBAS[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

NTEBAS[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

NTEBAS[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

NTEBAS[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

NTEBAS[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         EVOLUCAO == 4)

NTEBAS[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 411727,  
         CLASSI_FIN == 8)

NTEBAS[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 411727,
         ID_AGRAVO != 8)

NTEBAS[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(NTEBAS)[15] <- "Ficha_em_Duplicidade"

sheet_write(NTEBAS,
            ss = "https://docs.google.com/spreadsheets/d/16EBfL9TyU8LFKQ89vwRY1LTt-4ZeLa0SuoguiuG_5Xg/edit?gid=1300182115#gid=1300182115",
            sheet = "AUXILIAR02")

###  Rio Branco do Ivaí

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 412217,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


RBRANCO <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         DT_SORO < (DT_SIN_PRI + 6)) 

RBRANCO[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

RBRANCO[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         DT_PCR > (DT_SIN_PRI +4)) 

RBRANCO[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


RBRANCO[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


RBRANCO[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

RBRANCO[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

RBRANCO[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

RBRANCO[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

RBRANCO[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

RBRANCO[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

RBRANCO[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         EVOLUCAO == 4)

RBRANCO[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412217,  
         CLASSI_FIN == 8)

RBRANCO[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 412217,
         ID_AGRAVO != 8)

RBRANCO[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(RBRANCO)[15] <- "Ficha_em_Duplicidade"

sheet_write(RBRANCO,
            ss = "https://docs.google.com/spreadsheets/d/1vewfyhZAfqHIPq4plyXJrGlJ-l28jUoppkSSyQIB50Q/edit?gid=1898810696#gid=1898810696",
            sheet = "AUXILIAR02")

###  Rosário do Ivaí

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 412265,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


ROSARIO <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         DT_SORO < (DT_SIN_PRI + 6)) 

ROSARIO[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

ROSARIO[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         DT_PCR > (DT_SIN_PRI +4)) 

ROSARIO[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


ROSARIO[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


ROSARIO[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

ROSARIO[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

ROSARIO[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

ROSARIO[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

ROSARIO[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

ROSARIO[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

ROSARIO[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         EVOLUCAO == 4)

ROSARIO[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412265,  
         CLASSI_FIN == 8)

ROSARIO[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 412265,
         ID_AGRAVO != 8)

ROSARIO[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(ROSARIO)[15] <- "Ficha_em_Duplicidade"

sheet_write(ROSARIO,
            ss = "https://docs.google.com/spreadsheets/d/1wrwAz1zplJj0tdEVCUHOVUhsmtX3EBPPn5_ji9d8eKg/edit?gid=1082383407#gid=1082383407",
            sheet = "AUXILIAR02")
###  Santa Maria do Oeste

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 412385,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


SMARIA <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         DT_SORO < (DT_SIN_PRI + 6)) 

SMARIA[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

SMARIA[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         DT_PCR > (DT_SIN_PRI +4)) 

SMARIA[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


SMARIA[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


SMARIA[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

SMARIA[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

SMARIA[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

SMARIA[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

SMARIA[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

SMARIA[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

SMARIA[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         EVOLUCAO == 4)

SMARIA[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412385,  
         CLASSI_FIN == 8)

SMARIA[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 412385,
         ID_AGRAVO != 8)

SMARIA[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SMARIA)[15] <- "Ficha_em_Duplicidade"

sheet_write(SMARIA,
            ss = "https://docs.google.com/spreadsheets/d/11KDwFpSyAu3MDSoODsCWiJhAdKmPBT-9Ow07IpbBJSU/edit?gid=1205683756#gid=1205683756",
            sheet = "AUXILIAR02")
###  São João do Ivaí

AUX <- RS22_2026_SINAN %>%
  filter(ID_MN_RESI == 412500,  
         is.na(RS22_2026_SINAN$NM_LOGRADO) |
           is.na(RS22_2026_SINAN$NU_NUMERO) |
           is.na(RS22_2026_SINAN$NM_BAIRRO) |
           is.na(RS22_2026_SINAN$CS_ZONA))


SJOAO <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[1] <- "Dados_de_Residência"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         DT_SORO < (DT_SIN_PRI + 6)) 

SJOAO[, 2] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[2] <- "Sorologia_DENGUE_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         DT_CHIK_S1 < (DT_SIN_PRI + 6)) 

SJOAO[, 3] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[3] <- "Sorologia_CHIKUNGUNYA_Coletada_com_Menos_de_5_dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         DT_PCR > (DT_SIN_PRI +4)) 

SJOAO[, 4] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[4] <- "PCR_Coletado_>_5_Dias"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         DT_SORO != is.na(RS22_2026_SINAN$DT_SORO),
         DT_SORO > (DT_SIN_PRI + 6),
         Sys.Date() > (DT_SORO + 20),
         is.na(RS22_2026_SINAN$RESUL_SORO)
  )


SJOAO[, 5] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[5] <- "Sorologia_Dengue_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         DT_CHIK_S1 != is.na(RS22_2026_SINAN$DT_CHIK_S1),
         DT_CHIK_S1 < (DT_SIN_PRI + 6),
         Sys.Date() > (DT_CHIK_S1 + 20),
         is.na(RS22_2026_SINAN$RES_CHIKS1)
  )


SJOAO[, 6] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[6] <- "Sorologia_CHIKUNGUNYA_sem_Resultado"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$DT_INTERNA)
  )

SJOAO[, 7] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[7] <- "Internamento_sem_Data"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         HOSPITALIZ == 1 &
           is.na(RS22_2026_SINAN$HOSPITAL)
  )

SJOAO[, 8] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[8] <- "Internamento_sem_Hospital"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         HOSPITALIZ == 1,
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$DT_ALRM),
         is.na(RS22_2026_SINAN$DT_GRAV)
  )

SJOAO[, 9] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[9] <- "Internamento_sem_DSA_GRAVE"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         HOSPITALIZ == 1,
         CRITERIO == 2)

SJOAO[, 10] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[10] <- "Internamento_sem_Coleta_Exames"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         CLASSI_FIN != 5,
         is.na(RS22_2026_SINAN$TPAUTOCTO)
  )

SJOAO[, 11] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[11] <- "Ausencia_Autoctonia"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         CLASSI_FIN != is.na(RS22_2026_SINAN$CLASSI_FIN),
         is.na(RS22_2026_SINAN$EVOLUCAO)
  )

SJOAO[, 12] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[12] <- "Ausencia_Evolucao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         EVOLUCAO == 4)

SJOAO[, 13] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[13] <- "Obito_em_Investigacao"

AUX <- RS22_2026_SINAN %>% 
  filter(ID_MN_RESI == 412500,  
         CLASSI_FIN == 8)

SJOAO[, 14] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[14] <- "Ficha_Inconclusiva"

AUX <- RS22_2026_SINAN_Duplic %>% 
  filter(ID_MN_RESI == 412500,
         ID_AGRAVO != 8)

SJOAO[, 15] <- as.data.frame(paste(AUX$NU_NOTIFIC, collapse = ", "))

colnames(SJOAO)[15] <- "Ficha_em_Duplicidade"

sheet_write(SJOAO,
            ss = "https://docs.google.com/spreadsheets/d/1XcOUVk-TBiVHURb8L13x0nueBo5Wveu0aIvVGdvUsdE/edit?gid=919578981#gid=919578981",
            sheet = "AUXILIAR02")

######      Vigilância de Populações Vulneráveis

## Aldeia Indígena Ivaí

AUX <- tibble(Notificados = NA,
              Prováveis = NA,
              Gestantes = NA,
              Menos_2a = NA,
              Mais_60 = NA,
              DENV_1 = NA,
              DENV_2 = NA,
              DENV_3 = NA,
              DENV_4 = NA,
              Chikungunya = NA)

AUX[1, 1] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
                                              )

AUX[1, 2] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)
AUX[1, 3] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            CS_GESTANT == 1 |
                              CS_GESTANT == 2|
                              CS_GESTANT == 3 |
                              CS_GESTANT == 4,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 4] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N <= 4002,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 5] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N >= 4060,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 6] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            SOROTIPO == 1,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 7] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            SOROTIPO == 2,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 8] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            SOROTIPO == 3,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 9] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            SOROTIPO == 4,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 10] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 411450,
                            CLASSI_FIN == 13,
                            NM_BAIRRO == "ALDEIA INDIGENA IVAI" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[1, 11]<- "MANOEL RIBAS"
AUX[1, 12]<- "Aldeia Indígena Ivaí"
AUX <- AUX[, c(11, 12, 1:10)]

## Aldeia Indígena Faxinal

AUX[2, 3] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 4] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)
AUX[2, 5] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            CS_GESTANT == 1 |
                              CS_GESTANT == 2|
                              CS_GESTANT == 3 |
                              CS_GESTANT == 4,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 6] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N <= 4002,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 7] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N >= 4060,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 8] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            SOROTIPO == 1,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 9] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            SOROTIPO == 2,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 10] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            SOROTIPO == 3,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 11] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 410440,
                            SOROTIPO == 4,
                            NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                              NM_BAIRRO == "ALDEIA INDÍGENA" |
                              NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 12] <- count(RS22_2026_SINAN %>% 
                      filter(ID_MN_RESI == 410440,
                             CLASSI_FIN == 13,
                             NM_BAIRRO == "ALDEIA INDIGENA FAXINAL" |
                               NM_BAIRRO == "ALDEIA INDÍGENA" |
                               NM_BAIRRO == "ALDEIA" ) 
)

AUX[2, 1]<- "CÂNDIDO DE ABREU"
AUX[2, 2]<- "Aldeia Indígena Faxinal"

## HORÁCIO MARTINS 

AUX[3, 3] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 4] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)
AUX[3, 5] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            CS_GESTANT == 1 |
                              CS_GESTANT == 2|
                              CS_GESTANT == 3 |
                              CS_GESTANT == 4,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 6] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N <= 4002,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 7] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            CLASSI_FIN != 5 |
                              is.na(CLASSI_FIN),
                            NU_IDADE_N >= 4060,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 8] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            SOROTIPO == 1,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 9] <- count(RS22_2026_SINAN %>% 
                     filter(ID_MN_RESI == 412217,
                            SOROTIPO == 2,
                            NM_BAIRRO == "HORACIO MARTINS" |
                              NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 10] <- count(RS22_2026_SINAN %>% 
                      filter(ID_MN_RESI == 412217,
                             SOROTIPO == 3,
                             NM_BAIRRO == "HORACIO MARTINS" |
                               NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 11] <- count(RS22_2026_SINAN %>% 
                      filter(ID_MN_RESI == 412217,
                             SOROTIPO == 4,
                             NM_BAIRRO == "HORACIO MARTINS" |
                               NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 12] <- count(RS22_2026_SINAN %>% 
                      filter(ID_MN_RESI == 412217,
                             CLASSI_FIN == 13,
                             NM_BAIRRO == "HORACIO MARTINS" |
                               NM_BAIRRO == "HORÁCIO MARTINS") 
)

AUX[3, 1]<- "RIO BRANCO DO IVAÍ"
AUX[3, 2]<- "Acampamento Horácio Martins"

colnames(AUX)[c(1, 2)] <- c("Município", "Território")

RS22_2026_TAB_Pop_Vulneravel <- AUX %>%
  gt() %>%
  tab_header(title = md("**Vigilância de Populações Vulneráveis (2026)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Prováveis",
              columns = c(4:7),
              id = "SINAN_2") %>%
  tab_spanner(label = "Confirmados",
              columns = c(8:12),
              id = "SINAN_3") %>%
  tab_spanner(label = "Casos",
              columns = c(3:12),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(3:12)) %>%
  cols_label(Menos_2a = "Menores 02 Anos",
             Mais_60 = "Maiores de 60 Anos",
             DENV_1 = "DENV I",
             DENV_2 = "DENV II",
             DENV_3 = "DENV III",
             DENV_4 = "DENV IV") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")


rm(ARAPUA, 
   ARIRANHA, 
   CANDIDO, 
   CRUZMALTINA, 
   GODOY, 
   IVAIPORA, 
   JARDIM, 
   LIDIANOPOLIS, 
   LUNARDELLI,
   MRIBAS,
   MRICO,
   NTEBAS,
   RBRANCO,
   ROSARIO,
   SMARIA,
   SJOAO)

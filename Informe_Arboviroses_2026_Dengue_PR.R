############################################################
################   Dengue Paraná   #########################
############################################################

####    Tabela de notificações SINAN    ###

PR_DENGUE_2025_SINAN <- DENGON2025 

PR_DENGUE_2025_GERAL <- BASE_IBGE[,-c(4,6)]

PR_DENGUE_2025_GERAL$Notificados <- NA

PR_DENGUE_2025_GERAL$Dengue <- NA

PR_DENGUE_2025_GERAL$D_S_A <- NA

PR_DENGUE_2025_GERAL$Dengue_Grave <- NA

PR_DENGUE_2025_GERAL$Descartados <- NA

PR_DENGUE_2025_GERAL$Autoctones <- NA

PR_DENGUE_2025_GERAL$Incidencia <- NA

PR_DENGUE_2025_GERAL$DENV_I <- NA

PR_DENGUE_2025_GERAL$DENV_II <- NA

PR_DENGUE_2025_GERAL$DENV_III <- NA

PR_DENGUE_2025_GERAL$DENV_IV <- NA

PR_DENGUE_2025_GERAL$Obitos <- NA

PR_DENGUE_2025_GERAL$Inconclusivos <- NA

PR_DENGUE_2025_GERAL$Importados <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 5] <- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i) %>%   
                                                                                        count()
  )   
  
  ###Dengue###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 6] <-as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                       filter(CLASSI_FIN == 10, 
                                                                                              ID_MN_RESI == i) %>%
                                                                                       count() 
  )
  
  ###D.S.A.###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 7] <- as.integer(PR_DENGUE_2025_SINAN %>%  
                                                                                        filter(CLASSI_FIN == 11, 
                                                                                               ID_MN_RESI == i) %>% 
                                                                                        count()
  )
  
  ###Dengue Grave###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 8] <- as.integer(PR_DENGUE_2025_SINAN %>%  
                                                                                        filter(CLASSI_FIN == 12, 
                                                                                               ID_MN_RESI == i) %>% 
                                                                                        count()
  )
  
  ###Descartados###
  
  
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 9]<- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                       filter(CLASSI_FIN == 5,
                                                                                              ID_MN_RESI == i) %>% 
                                                                                       count()
  )  
  
  ###Autóctones###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 10]<- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               TPAUTOCTO == 1,
                                                                                               CLASSI_FIN == 10) %>% 
                                                                                        count() 
  )
  
  ###DENV I###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 12]<- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SOROTIPO == 1) %>% 
                                                                                        count() 
  )
  
  ###DENV II###
  
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 13] <- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                SOROTIPO == 2) %>% 
                                                                                         count() 
  )
  
  ###DENV III###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 14] <- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                SOROTIPO == 3) %>% 
                                                                                         count() 
  )
  ###DENV IV###                                     
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 15]<- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SOROTIPO == 4) %>% 
                                                                                        count() 
  )
  
  ###Óbitos###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 16] <- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                EVOLUCAO == 2) %>% 
                                                                                         count() 
  )
  
  ###Inconclusivos###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 17] <-as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(CLASSI_FIN == 8, 
                                                                                               ID_MN_RESI == i) %>%
                                                                                        count() 
  )
  
  ###Importados###
  
  PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Código_IBGE == i), 18]<- as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               TPAUTOCTO == 2,
                                                                                               CLASSI_FIN == 10) %>% 
                                                                                        count() 
  )
}

###Incidência###FORA DO LOOP###

PR_DENGUE_2025_GERAL$Incidencia <- (PR_DENGUE_2025_GERAL$Autoctones/PR_DENGUE_2025_GERAL$População)*100000  
PR_DENGUE_2025_GERAL$Incidencia <- format(round(PR_DENGUE_2025_GERAL$Incidencia, 2))
PR_DENGUE_2025_GERAL$Incidencia <- as.numeric(PR_DENGUE_2025_GERAL$Incidencia)

PR_DENGUE_2025_GERAL$Provaveis <- as.integer(PR_DENGUE_2025_GERAL$Notificados) - as.integer(PR_DENGUE_2025_GERAL$Descartados)


PR_DENGUE_2025_GERAL$Incidencia_Provaveis <- (PR_DENGUE_2025_GERAL$Provaveis/PR_DENGUE_2025_GERAL$População)*100000  
PR_DENGUE_2025_GERAL$Incidencia_Provaveis <- format(round(PR_DENGUE_2025_GERAL$Incidencia_Provaveis, 2))
PR_DENGUE_2025_GERAL$Incidencia_Provaveis <- as.numeric(PR_DENGUE_2025_GERAL$Incidencia_Provaveis)

PR_DENGUE_2025_GERAL$Em_Investigacao <- as.integer(PR_DENGUE_2025_GERAL$Notificados) - as.integer(PR_DENGUE_2025_GERAL$Dengue + PR_DENGUE_2025_GERAL$D_S_A + PR_DENGUE_2025_GERAL$Dengue_Grave + PR_DENGUE_2025_GERAL$Descartados)

PR_DENGUE_2025_GERAL$Total <- as.integer(PR_DENGUE_2025_GERAL$Dengue + PR_DENGUE_2025_GERAL$D_S_A + PR_DENGUE_2025_GERAL$Dengue_Grave) 

PR_DENGUE_2025_GERAL <- PR_DENGUE_2025_GERAL[, c(1, 3, 4, 5, 19, 6, 7, 8, 22, 9, 21, 17, 16, 10, 18, 11, 20, 12, 13,14, 15)]

#### Correção
PR_DENGUE_2025_GERAL[170, 21] <- 0

######################################################################################################
###         Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.            ###
######################################################################################################

PR_2025_DENGUE_SINAIS_Notificados <- tibble(Febre = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                 filter(FEBRE == 1 ) %>%
                                                                 count()),
                                            Mialgia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                   filter(MIALGIA == 1 ) %>%
                                                                   count()),
                                            Cefaleia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                    filter(CEFALEIA == 1) %>%
                                                                    count()),
                                            Exantema = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                    filter(EXANTEMA == 1) %>%
                                                                    count()),
                                            Vomito = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                  filter(VOMITO == 1) %>%
                                                                  count()),
                                            Nausea = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                  filter(NAUSEA == 1) %>%
                                                                  count()),
                                            Dor_nas_Costas = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                          filter(DOR_COSTAS == 1) %>%
                                                                          count()),
                                            Conjuntivite = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                        filter(CONJUNTVIT == 1) %>%
                                                                        count()),
                                            Artrite = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                   filter(ARTRITE == 1) %>%
                                                                   count()),
                                            Artralgia_Intensa = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                             filter(ARTRALGIA == 1) %>%
                                                                             count()),
                                            Petequias = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                     filter(PETEQUIA_N == 1) %>%
                                                                     count()),
                                            Leucopenia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                      filter(LEUCOPENIA == 1) %>%
                                                                      count()),
                                            Prova_do_Laco_Positiva = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                                  filter(LACO == 1) %>%
                                                                                  count()),
                                            Dor_retroorbital = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                            filter(DOR_RETRO == 1) %>%
                                                                            count())
)
colnames(PR_2025_DENGUE_SINAIS_Notificados) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vômito", "Náusea", "Dor nas Costas", "Conjuntivite", "Artrite", "Artralgia Intensa", "Petéquias", "Leucopenia", "Prova do Laco Positiva", "Dor Retroorbital")

PR_2025_DENGUE_SINAIS_Confirmados <- tibble(Febre = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                                 filter(FEBRE == 1,
                                                                        CLASSI_FIN == 10 
                                                                        | 
                                                                          CLASSI_FIN == 11 
                                                                        |
                                                                          CLASSI_FIN == 12                                                            |                                                              CLASSI_FIN == 11                                                            |                                                             CLASSI_FIN == 12,) %>%
                                                                 count()),
                                            Mialgia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                   filter(MIALGIA == 1,
                                                                          CLASSI_FIN == 10 
                                                                          | 
                                                                            CLASSI_FIN == 11 
                                                                          |
                                                                            CLASSI_FIN == 12) %>%
                                                                   count()),
                                            Cefaleia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                    filter(CEFALEIA == 1,
                                                                           CLASSI_FIN == 10 
                                                                           | 
                                                                             CLASSI_FIN == 11 
                                                                           |
                                                                             CLASSI_FIN == 12) %>%
                                                                    count()),
                                            Exantema = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                    filter(EXANTEMA == 1,
                                                                           CLASSI_FIN == 10 
                                                                           | 
                                                                             CLASSI_FIN == 11 
                                                                           |
                                                                             CLASSI_FIN == 12) %>%
                                                                    count()),
                                            Vomito = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                  filter(VOMITO == 1,
                                                                         CLASSI_FIN == 10 
                                                                         | 
                                                                           CLASSI_FIN == 11 
                                                                         |
                                                                           CLASSI_FIN == 12) %>%
                                                                  count()),
                                            Nausea = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                  filter(NAUSEA == 1,
                                                                         CLASSI_FIN == 10 
                                                                         | 
                                                                           CLASSI_FIN == 11 
                                                                         |
                                                                           CLASSI_FIN == 12) %>%
                                                                  count()),
                                            Dor_nas_Costas = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                          filter(DOR_COSTAS == 1,
                                                                                 CLASSI_FIN == 10 
                                                                                 | 
                                                                                   CLASSI_FIN == 11 
                                                                                 |
                                                                                   CLASSI_FIN == 12) %>%
                                                                          count()),
                                            Conjuntivite = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                        filter(CONJUNTVIT == 1,
                                                                               CLASSI_FIN == 10 
                                                                               | 
                                                                                 CLASSI_FIN == 11 
                                                                               |
                                                                                 CLASSI_FIN == 12) %>%
                                                                        count()),
                                            Artrite = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                   filter(ARTRITE == 1,
                                                                          CLASSI_FIN == 10 
                                                                          | 
                                                                            CLASSI_FIN == 11 
                                                                          |
                                                                            CLASSI_FIN == 12) %>%
                                                                   count()),
                                            Artralgia_Intensa = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                             filter(ARTRALGIA == 1,
                                                                                    CLASSI_FIN == 10 
                                                                                    | 
                                                                                      CLASSI_FIN == 11 
                                                                                    |
                                                                                      CLASSI_FIN == 12) %>%
                                                                             count()),
                                            Petequias = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                     filter(PETEQUIA_N == 1,
                                                                            CLASSI_FIN == 10 
                                                                            | 
                                                                              CLASSI_FIN == 11 
                                                                            |
                                                                              CLASSI_FIN == 12) %>%
                                                                     count()),
                                            Leucopenia = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                      filter(LEUCOPENIA == 1,
                                                                             CLASSI_FIN == 10 
                                                                             | 
                                                                               CLASSI_FIN == 11 
                                                                             |
                                                                               CLASSI_FIN == 12) %>%
                                                                      count()),
                                            Prova_do_Laco_Positiva = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                                  filter(LACO == 1,
                                                                                         CLASSI_FIN == 10 
                                                                                         | 
                                                                                           CLASSI_FIN == 11 
                                                                                         |
                                                                                           CLASSI_FIN == 12) %>%
                                                                                  count()),
                                            Dor_retroorbital = as.integer(PR_DENGUE_2025_SINAN %>%
                                                                            filter(DOR_RETRO == 1,
                                                                                   CLASSI_FIN == 10 
                                                                                   | 
                                                                                     CLASSI_FIN == 11 
                                                                                   |
                                                                                     CLASSI_FIN == 12) %>%
                                                                            count())
)

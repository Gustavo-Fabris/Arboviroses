############################################################
################   Dengue Paraná   #########################
############################################################

####    Tabela de notificações SINAN    ###

PR_DENGUE_2026_SINAN <- DENGON2026 

PR_DENGUE_2026_GERAL <- BASE_IBGE[,-c(4,6)]

PR_DENGUE_2026_GERAL$Notificados <- NA

PR_DENGUE_2026_GERAL$Dengue <- NA

PR_DENGUE_2026_GERAL$D_S_A <- NA

PR_DENGUE_2026_GERAL$Dengue_Grave <- NA

PR_DENGUE_2026_GERAL$Descartados <- NA

PR_DENGUE_2026_GERAL$Autoctones <- NA

PR_DENGUE_2026_GERAL$Incidencia <- NA

PR_DENGUE_2026_GERAL$DENV_I <- NA

PR_DENGUE_2026_GERAL$DENV_II <- NA

PR_DENGUE_2026_GERAL$DENV_III <- NA

PR_DENGUE_2026_GERAL$DENV_IV <- NA

PR_DENGUE_2026_GERAL$Obitos <- NA

PR_DENGUE_2026_GERAL$Inconclusivos <- NA

PR_DENGUE_2026_GERAL$Importados <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 5] <- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i) %>%   
                                                                                        count()
  )   
  
  ###Dengue###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 6] <-as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                       filter(CLASSI_FIN == 10, 
                                                                                              ID_MN_RESI == i) %>%
                                                                                       count() 
  )
  
  ###D.S.A.###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 7] <- as.integer(PR_DENGUE_2026_SINAN %>%  
                                                                                        filter(CLASSI_FIN == 11, 
                                                                                               ID_MN_RESI == i) %>% 
                                                                                        count()
  )
  
  ###Dengue Grave###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 8] <- as.integer(PR_DENGUE_2026_SINAN %>%  
                                                                                        filter(CLASSI_FIN == 12, 
                                                                                               ID_MN_RESI == i) %>% 
                                                                                        count()
  )
  
  ###Descartados###
  
  
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 9]<- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                       filter(CLASSI_FIN == 5,
                                                                                              ID_MN_RESI == i) %>% 
                                                                                       count()
  )  
  
  ###Autóctones###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 10]<- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               TPAUTOCTO == 1,
                                                                                               CLASSI_FIN == 10) %>% 
                                                                                        count() 
  )
  
  ###DENV I###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 12]<- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SOROTIPO == 1) %>% 
                                                                                        count() 
  )
  
  ###DENV II###
  
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 13] <- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                SOROTIPO == 2) %>% 
                                                                                         count() 
  )
  
  ###DENV III###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 14] <- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                SOROTIPO == 3) %>% 
                                                                                         count() 
  )
  ###DENV IV###                                     
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 15]<- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               SOROTIPO == 4) %>% 
                                                                                        count() 
  )
  
  ###Óbitos###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 16] <- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                         filter(ID_MN_RESI == i, 
                                                                                                EVOLUCAO == 2) %>% 
                                                                                         count() 
  )
  
  ###Inconclusivos###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 17] <-as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(CLASSI_FIN == 8, 
                                                                                               ID_MN_RESI == i) %>%
                                                                                        count() 
  )
  
  ###Importados###
  
  PR_DENGUE_2026_GERAL[which(PR_DENGUE_2026_GERAL$Código_IBGE == i), 18]<- as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                                        filter(ID_MN_RESI == i, 
                                                                                               TPAUTOCTO == 2,
                                                                                               CLASSI_FIN == 10) %>% 
                                                                                        count() 
  )
}

###Incidência###FORA DO LOOP###

PR_DENGUE_2026_GERAL$Incidencia <- (PR_DENGUE_2026_GERAL$Autoctones/PR_DENGUE_2026_GERAL$População)*100000  
PR_DENGUE_2026_GERAL$Incidencia <- format(round(PR_DENGUE_2026_GERAL$Incidencia, 2))
PR_DENGUE_2026_GERAL$Incidencia <- as.numeric(PR_DENGUE_2026_GERAL$Incidencia)

PR_DENGUE_2026_GERAL$Provaveis <- as.integer(PR_DENGUE_2026_GERAL$Notificados) - as.integer(PR_DENGUE_2026_GERAL$Descartados)


PR_DENGUE_2026_GERAL$Incidencia_Provaveis <- (PR_DENGUE_2026_GERAL$Provaveis/PR_DENGUE_2026_GERAL$População)*100000  
PR_DENGUE_2026_GERAL$Incidencia_Provaveis <- format(round(PR_DENGUE_2026_GERAL$Incidencia_Provaveis, 2))
PR_DENGUE_2026_GERAL$Incidencia_Provaveis <- as.numeric(PR_DENGUE_2026_GERAL$Incidencia_Provaveis)

PR_DENGUE_2026_GERAL$Em_Investigacao <- as.integer(PR_DENGUE_2026_GERAL$Notificados) - as.integer(PR_DENGUE_2026_GERAL$Dengue + PR_DENGUE_2026_GERAL$D_S_A + PR_DENGUE_2026_GERAL$Dengue_Grave + PR_DENGUE_2026_GERAL$Descartados)

PR_DENGUE_2026_GERAL$Total <- as.integer(PR_DENGUE_2026_GERAL$Dengue + PR_DENGUE_2026_GERAL$D_S_A + PR_DENGUE_2026_GERAL$Dengue_Grave) 

PR_DENGUE_2026_GERAL <- PR_DENGUE_2026_GERAL[, c(1, 3, 4, 5, 19, 6, 7, 8, 22, 9, 21, 17, 16, 10, 18, 11, 20, 12, 13,14, 15)]

#### Correção
PR_DENGUE_2026_GERAL[170, 21] <- 0

######     Municípios Estratégicos fora da 22ª RS

RS22_2026_DENGUE_ESTRATEGICOS <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "PITANGA")
RS22_2026_DENGUE_ESTRATEGICOS[2, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "MARINGÁ")
RS22_2026_DENGUE_ESTRATEGICOS[3, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "APUCARANA")
RS22_2026_DENGUE_ESTRATEGICOS[4, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "FAXINAL")
RS22_2026_DENGUE_ESTRATEGICOS[5, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "GRANDES RIOS")
RS22_2026_DENGUE_ESTRATEGICOS[6, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "SÃO PEDRO DO IVAÍ")
RS22_2026_DENGUE_ESTRATEGICOS[7, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "RONCADOR")
RS22_2026_DENGUE_ESTRATEGICOS[8, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "CAMPO MOURÃO")
RS22_2026_DENGUE_ESTRATEGICOS[9, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "IRETAMA")
RS22_2026_DENGUE_ESTRATEGICOS[10, ] <- PR_DENGUE_2026_GERAL %>% filter(Município_sem_Código == "LONDRINA")

RS22_2026_DENGUE_ESTRATEGICOS <- RS22_2026_DENGUE_ESTRATEGICOS[, c(1, 2, 4, 5, 6, 7, 8, 14, 15, 16, 17, 18, 19, 20, 21)]

RS22_2026_TAB_Estrategicos <- RS22_2026_DENGUE_ESTRATEGICOS %>%
  gt() %>%
  tab_header(title = md("**Municípios Estratégicos Localizados Fora da 22ª RS**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Situação Epidemiológica - DENGUE",
              columns = c(3:15),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(3:15)) %>%
  cols_label(Município_sem_Código = "Município",
             Notificados = "Notificados",
             Provaveis = "Prováveis",
             Dengue = "Dengue",
             D_S_A = "D.S.A.",
             Dengue_Grave = "Dengue Grave",
             Incidencia = "Incidência",
             Incidencia_Provaveis = "Incidência Prováveis",
             DENV_I = "DENV I",
             DENV_II = "DENV II",
             DENV_III = "DENV III",
             DENV_IV = "DENV IV") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

######################################################################################################
###         Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.            ###
######################################################################################################

PR_2026_DENGUE_SINAIS_Notificados <- tibble(Febre = as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                 filter(FEBRE == 1 ) %>%
                                                                 count()),
                                            Mialgia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                   filter(MIALGIA == 1 ) %>%
                                                                   count()),
                                            Cefaleia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                    filter(CEFALEIA == 1) %>%
                                                                    count()),
                                            Exantema = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                    filter(EXANTEMA == 1) %>%
                                                                    count()),
                                            Vomito = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                  filter(VOMITO == 1) %>%
                                                                  count()),
                                            Nausea = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                  filter(NAUSEA == 1) %>%
                                                                  count()),
                                            Dor_nas_Costas = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                          filter(DOR_COSTAS == 1) %>%
                                                                          count()),
                                            Conjuntivite = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                        filter(CONJUNTVIT == 1) %>%
                                                                        count()),
                                            Artrite = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                   filter(ARTRITE == 1) %>%
                                                                   count()),
                                            Artralgia_Intensa = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                             filter(ARTRALGIA == 1) %>%
                                                                             count()),
                                            Petequias = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                     filter(PETEQUIA_N == 1) %>%
                                                                     count()),
                                            Leucopenia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                      filter(LEUCOPENIA == 1) %>%
                                                                      count()),
                                            Prova_do_Laco_Positiva = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                                  filter(LACO == 1) %>%
                                                                                  count()),
                                            Dor_retroorbital = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                            filter(DOR_RETRO == 1) %>%
                                                                            count())
)
colnames(PR_2026_DENGUE_SINAIS_Notificados) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vômito", "Náusea", "Dor nas Costas", "Conjuntivite", "Artrite", "Artralgia Intensa", "Petéquias", "Leucopenia", "Prova do Laco Positiva", "Dor Retroorbital")

PR_2026_DENGUE_SINAIS_Confirmados <- tibble(Febre = as.integer(PR_DENGUE_2026_SINAN %>% 
                                                                 filter(FEBRE == 1,
                                                                        CLASSI_FIN == 10 
                                                                        | 
                                                                          CLASSI_FIN == 11 
                                                                        |
                                                                          CLASSI_FIN == 12                                                            |                                                              CLASSI_FIN == 11                                                            |                                                             CLASSI_FIN == 12,) %>%
                                                                 count()),
                                            Mialgia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                   filter(MIALGIA == 1,
                                                                          CLASSI_FIN == 10 
                                                                          | 
                                                                            CLASSI_FIN == 11 
                                                                          |
                                                                            CLASSI_FIN == 12) %>%
                                                                   count()),
                                            Cefaleia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                    filter(CEFALEIA == 1,
                                                                           CLASSI_FIN == 10 
                                                                           | 
                                                                             CLASSI_FIN == 11 
                                                                           |
                                                                             CLASSI_FIN == 12) %>%
                                                                    count()),
                                            Exantema = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                    filter(EXANTEMA == 1,
                                                                           CLASSI_FIN == 10 
                                                                           | 
                                                                             CLASSI_FIN == 11 
                                                                           |
                                                                             CLASSI_FIN == 12) %>%
                                                                    count()),
                                            Vomito = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                  filter(VOMITO == 1,
                                                                         CLASSI_FIN == 10 
                                                                         | 
                                                                           CLASSI_FIN == 11 
                                                                         |
                                                                           CLASSI_FIN == 12) %>%
                                                                  count()),
                                            Nausea = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                  filter(NAUSEA == 1,
                                                                         CLASSI_FIN == 10 
                                                                         | 
                                                                           CLASSI_FIN == 11 
                                                                         |
                                                                           CLASSI_FIN == 12) %>%
                                                                  count()),
                                            Dor_nas_Costas = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                          filter(DOR_COSTAS == 1,
                                                                                 CLASSI_FIN == 10 
                                                                                 | 
                                                                                   CLASSI_FIN == 11 
                                                                                 |
                                                                                   CLASSI_FIN == 12) %>%
                                                                          count()),
                                            Conjuntivite = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                        filter(CONJUNTVIT == 1,
                                                                               CLASSI_FIN == 10 
                                                                               | 
                                                                                 CLASSI_FIN == 11 
                                                                               |
                                                                                 CLASSI_FIN == 12) %>%
                                                                        count()),
                                            Artrite = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                   filter(ARTRITE == 1,
                                                                          CLASSI_FIN == 10 
                                                                          | 
                                                                            CLASSI_FIN == 11 
                                                                          |
                                                                            CLASSI_FIN == 12) %>%
                                                                   count()),
                                            Artralgia_Intensa = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                             filter(ARTRALGIA == 1,
                                                                                    CLASSI_FIN == 10 
                                                                                    | 
                                                                                      CLASSI_FIN == 11 
                                                                                    |
                                                                                      CLASSI_FIN == 12) %>%
                                                                             count()),
                                            Petequias = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                     filter(PETEQUIA_N == 1,
                                                                            CLASSI_FIN == 10 
                                                                            | 
                                                                              CLASSI_FIN == 11 
                                                                            |
                                                                              CLASSI_FIN == 12) %>%
                                                                     count()),
                                            Leucopenia = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                      filter(LEUCOPENIA == 1,
                                                                             CLASSI_FIN == 10 
                                                                             | 
                                                                               CLASSI_FIN == 11 
                                                                             |
                                                                               CLASSI_FIN == 12) %>%
                                                                      count()),
                                            Prova_do_Laco_Positiva = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                                  filter(LACO == 1,
                                                                                         CLASSI_FIN == 10 
                                                                                         | 
                                                                                           CLASSI_FIN == 11 
                                                                                         |
                                                                                           CLASSI_FIN == 12) %>%
                                                                                  count()),
                                            Dor_retroorbital = as.integer(PR_DENGUE_2026_SINAN %>%
                                                                            filter(DOR_RETRO == 1,
                                                                                   CLASSI_FIN == 10 
                                                                                   | 
                                                                                     CLASSI_FIN == 11 
                                                                                   |
                                                                                     CLASSI_FIN == 12) %>%
                                                                            count())
)

#####################################################################################################################
#####################  Chikungunya   ############################################################################
#####################################################################################################################
### Transformando os arquivos da base DBF em um único objeto referente ao período sazonal

PR_CHIK_2026_SINAN <- CHIKON2026 

####Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS###

PR_CHIK_2026_GERAL <- BASE_IBGE[, -c(4, 6)]

PR_CHIK_2026_GERAL$Notificados <- NA

PR_CHIK_2026_GERAL$Confirmados <- NA

PR_CHIK_2026_GERAL$Descartados <- NA

PR_CHIK_2026_GERAL$Autoctones <- NA

PR_CHIK_2026_GERAL$Importados <- NA

PR_CHIK_2026_GERAL$Obitos <- NA

PR_CHIK_2026_GERAL$Incidencia <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 5] <- as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                    filter(ID_MN_RESI == i) %>%   
                                                                                    count()
  )   
  
  ###Chikungunya###
  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 6] <-as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                   filter(CLASSI_FIN == 13, 
                                                                                          ID_MN_RESI == i) %>%
                                                                                   count() 
  )
  
  ###Descartados###
  
  
  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 7]<- as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                   filter(CLASSI_FIN == 5,
                                                                                          ID_MN_RESI == i) %>% 
                                                                                   count()
  )  
  
  ###Autóctones###
  
  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 8]<- as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          TPAUTOCTO == 1,
                                                                                          CLASSI_FIN == 13) %>% 
                                                                                   count() 
  )
  
  ###Importados###
  
  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 9]<- as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          TPAUTOCTO == 2,
                                                                                          CLASSI_FIN == 13) %>% 
                                                                                   count() 
  )
  
  ###Óbitos###
  
  PR_CHIK_2026_GERAL[which(PR_CHIK_2026_GERAL$Código_IBGE == i), 10] <- as.integer(PR_CHIK_2026_SINAN%>% 
                                                                                     filter(ID_MN_RESI == i, 
                                                                                            EVOLUCAO == 2) %>% 
                                                                                     count() 
  )
}

###Incidência###FORA DO LOOP###

PR_CHIK_2026_GERAL$Incidencia <- (PR_CHIK_2026_GERAL$Autoctones/PR_CHIK_2026_GERAL$População)*100000  
PR_CHIK_2026_GERAL$Incidencia <- format(round(PR_CHIK_2026_GERAL$Incidencia, 2))
PR_CHIK_2026_GERAL$Incidencia <- as.numeric(PR_CHIK_2026_GERAL$Incidencia)


PR_CHIK_2026_GERAL$Em_Investigacao <- as.integer(PR_CHIK_2026_GERAL$Notificados) - as.integer(PR_CHIK_2026_GERAL$Confirmados + PR_CHIK_2026_GERAL$Descartados)

PR_CHIK_2026_GERAL <- PR_CHIK_2026_GERAL[, c(1, 3, 4, 5, 6, 12, 7, 8, 9, 10, 11)]

PR_CHIK_2026_GERAL[400, 3:10] <- apply(PR_CHIK_2026_GERAL[, 3:10], 2, sum)

######     Municípios Estratégicos fora da 22ª RS

RS22_2026_CHIK_ESTRATEGICOS <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "PITANGA")
RS22_2026_CHIK_ESTRATEGICOS[2, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "MARINGÁ")
RS22_2026_CHIK_ESTRATEGICOS[3, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "APUCARANA")
RS22_2026_CHIK_ESTRATEGICOS[4, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "FAXINAL")
RS22_2026_CHIK_ESTRATEGICOS[5, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "GRANDES RIOS")
RS22_2026_CHIK_ESTRATEGICOS[6, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "SÃO PEDRO DO IVAÍ")
RS22_2026_CHIK_ESTRATEGICOS[7, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "RONCADOR")
RS22_2026_CHIK_ESTRATEGICOS[8, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "CAMPO MOURÃO")
RS22_2026_CHIK_ESTRATEGICOS[9, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "IRETAMA")
RS22_2026_CHIK_ESTRATEGICOS[10, ] <- PR_CHIK_2026_GERAL %>% filter(Município_sem_Código == "LONDRINA")

RS22_2026_CHIK_ESTRATEGICOS <- RS22_2026_CHIK_ESTRATEGICOS[, -c(3, 6, 7, 10)]

RS22_2026_TAB_Estrategicos_Chik <- RS22_2026_CHIK_ESTRATEGICOS %>%
  gt() %>%
  tab_header(title = md("**Municípios Estratégicos Localizados Fora da 22ª RS**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Situação Epidemiológica - CHIKUNGUNYA",
              columns = c(3:7),
              id = "SINAN") %>%
  cols_align(align = "center", 
             columns = c(3:7)) %>%
  cols_label(Município_sem_Código = "Município",
             Notificados = "Notificados",
             Incidencia = "Incidência") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

